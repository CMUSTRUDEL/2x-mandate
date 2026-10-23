# =============================================================================
# Figure data layer. Builds the derived monthly / per-PR / per-reviewer series
# behind the figures from the same input tables as data_layer.R. AI authorship is
# the `created-by-ai` label; automated PRs are excluded via `is_automated`. For
# the descriptive review series a review event is automated when its account is
# automated OR the event is templated tool output (`is_templated`, e.g. a bot
# message posted through a human account); a human review is a non-automated
# review by someone other than the PR author.
# =============================================================================
if (!exists("prs")) source("R/data_layer.R")
suppressMessages({ library(tidyverse) })

OV_START <- as.Date("2024-01-01")
OV_END   <- as.Date("2026-04-30")   # PRs created through Apr 2026 (drop partial May)

SUBSTANTIVE_TYPES <- c("comment", "review_thread", "review:commented",
                       "review:changes_requested")

# ---- Raw PR frame for the descriptive figures -------------------------------
# Non-automated PRs in the window; AI-authored label; month floor.
ov_prs <- prs_raw %>%
  filter(!is_automated, created >= OV_START, created <= OV_END) %>%
  transmute(repository, number, pr_author = identity, month,
            state, is_revert = replace_na(is_revert, FALSE),
            total_cycle_time_hours,
            is_ai = str_detect(replace_na(labels, ""), regex("created-by-ai", ignore_case = TRUE)))

# ---- Raw review events (read once) ------------------------------------------
rev_raw <- read_csv(p("pr_reviews.csv"), show_col_types = FALSE,
                    col_types = cols(.default = col_character())) %>%
  transmute(repository,
            pr_number = suppressWarnings(as.integer(pr_number)),
            author, identity, type, comment_time,
            is_auto = tolower(replace_na(is_automated, "")) == "true" |
              tolower(replace_na(is_templated, "")) == "true") %>%
  filter(!is.na(pr_number))

# Per-PR review-coverage flags on the descriptive PR frame.
ov_rev <- rev_raw %>%
  inner_join(ov_prs %>% select(repository, number, pr_author, month),
             by = c("repository", "pr_number" = "number")) %>%
  mutate(human = !is_auto & author != pr_author,
         sub   = human & type %in% SUBSTANTIVE_TYPES)

pr_flags <- ov_rev %>% group_by(repository, pr_number) %>%
  summarise(has_human = any(human), has_sub = any(sub), has_auto = any(is_auto),
            .groups = "drop")

ov_prs <- ov_prs %>%
  left_join(pr_flags, by = c("repository", "number" = "pr_number")) %>%
  mutate(across(c(has_human, has_sub, has_auto), ~ replace_na(.x, FALSE)),
         has_silent = has_human & !has_sub)

# Human reviewers active per month (for the supply series), restricted to a set
# of retained PR authors. `keep` = the authors whose PRs define the sample.
monthly_reviewers <- function(keep) {
  ov_rev %>% filter(human, pr_author %in% keep) %>%
    group_by(month) %>% summarise(reviewers = n_distinct(author), .groups = "drop")
}

# ---- Monthly overview metrics for a PR subset -------------------------------
monthly_metrics <- function(p_sub) {
  keep <- unique(p_sub$pr_author)
  merged <- p_sub %>% filter(state == "MERGED")
  base <- p_sub %>% group_by(month) %>% summarise(
    n_prs       = n(),
    ai_share    = mean(is_ai),
    merge_rate  = mean(state == "MERGED"),
    revert_rate = mean(is_revert),
    share_human = mean(has_human),
    share_sub   = mean(has_sub),
    share_silent = mean(has_silent),
    share_auto  = mean(has_auto),
    authors     = n_distinct(pr_author),
    .groups = "drop")
  mrg <- merged %>% group_by(month) %>% summarise(
    cycle_p90          = quantile(total_cycle_time_hours, 0.90, na.rm = TRUE, names = FALSE),
    share_human_merged = mean(has_human),
    share_sub_merged   = mean(has_sub),
    .groups = "drop")
  mrg_sub <- merged %>% filter(has_sub) %>% group_by(month) %>% summarise(
    cycle_p90_sub = quantile(total_cycle_time_hours, 0.90, na.rm = TRUE, names = FALSE),
    .groups = "drop")
  base %>%
    left_join(monthly_reviewers(keep), by = "month") %>%
    left_join(mrg, by = "month") %>%
    left_join(mrg_sub, by = "month") %>%
    mutate(prs_per_author = n_prs / authors) %>%
    arrange(month)
}

# Estimation sample: PR authors with >= 3 active months (>=1 PR/month).
ov_est_authors <- ov_prs %>% distinct(pr_author, month) %>%
  count(pr_author) %>% filter(n >= 3) %>% pull(pr_author)

fig_ov      <- monthly_metrics(ov_prs %>% filter(pr_author %in% ov_est_authors))
fig_ov_full <- monthly_metrics(ov_prs)

# =============================================================================
# Per-developer reviews-given decomposition. One row per (reviewer, PR): the set
# of event types and the first engaged month; substantive = any comment-bearing
# type, bare_approval = approve/dismiss with no substantive event. Keyed on the
# review `identity` and excludes self-reviews and automated rows (is_automated).
# =============================================================================
APPROVE_TYPES <- c("review:approved", "review:dismissed")
pr_identity <- prs_raw %>% distinct(repository, number, pr_identity = identity)

rg_events <- rev_raw %>%
  filter(!is.na(identity), !is_auto) %>%
  left_join(pr_identity, by = c("repository", "pr_number" = "number")) %>%
  filter(is.na(pr_identity) | identity != pr_identity) %>%
  mutate(month = mfloor(as.Date(substr(comment_time, 1, 10)))) %>%
  filter(!is.na(month), month >= OV_START, month <= as.Date("2026-04-01"))

# collapse to (reviewer, PR): event-type set + first month engaged
rg_pr <- rg_events %>% group_by(identity, repository, pr_number) %>%
  summarise(month = min(month),
            substantive = any(type %in% SUBSTANTIVE_TYPES),
            approved    = any(type %in% APPROVE_TYPES), .groups = "drop") %>%
  mutate(bare_approval = approved & !substantive)

fig_rev_given <- rg_pr %>% group_by(developer = identity, month) %>%
  summarise(reviews_given  = sum(substantive),
            reviews_loose  = n(),
            approvals      = sum(approved),
            bare_approvals = sum(bare_approval), .groups = "drop")

# =============================================================================
# PR-level HUMAN-boundary review timing. The vendor pickup/review-lead split is
# set by the first review of ANY kind (an automated account that comments
# instantly collapses pickup); this recomputes both phases against the first
# non-author, non-automated review. is_ai is the NARROW created-by-ai label.
# =============================================================================
prh_raw <- read_csv(p("pull_requests.csv"), show_col_types = FALSE) %>%
  mutate(created = as.Date(created_at), month = mfloor(created),
         created_ts = as_datetime(created_at), merged_ts = as_datetime(merged_at)) %>%
  filter(!is_automated, created >= OV_START, created <= OV_END)

# First human review time per PR from the raw stream (author-keyed; a human
# review is a non-automated review by someone other than the PR author).
prh_author <- prh_raw %>% distinct(repository, number, pr_author = identity)
t_human <- rev_raw %>%
  inner_join(prh_author, by = c("repository", "pr_number" = "number")) %>%
  mutate(human = !is_auto & author != pr_author) %>%
  filter(human) %>%
  mutate(t = as_datetime(comment_time)) %>% filter(!is.na(t)) %>%
  group_by(repository, pr_number) %>% summarise(t_human = min(t), .groups = "drop")

hrs <- function(a, b) { d <- as.numeric(difftime(a, b, units = "hours")); ifelse(d >= 0, d, NA) }
fig_pr_human <- prh_raw %>%
  left_join(t_human, by = c("repository", "number" = "pr_number")) %>%
  transmute(repository, number, dev = identity, month, state,
            post = as.integer(month >= MANDATE),
            is_ai = as.integer(str_detect(replace_na(labels, ""), regex("created-by-ai", ignore_case = TRUE))),
            pr_size = replace_na(lines_added, 0) + replace_na(lines_removed, 0),
            files_changed, comment_count,
            coding_lead_time_hours, total_cycle_time_hours,
            pickup_human = hrs(t_human, created_ts),
            lead_human   = hrs(merged_ts, t_human),
            has_human_review = as.integer(!is.na(t_human)),
            is_revert = as.integer(replace_na(is_revert, FALSE)))

# =============================================================================
# In-text numbers read off the overview figures (Jan 2025 - Apr 2026 window;
# baseline = mean of Jan-Apr 2025, end = Apr 2026), recorded for check_paper.R.
# =============================================================================
local({
  win  <- function(d) d %>% filter(month >= as.Date("2025-01-01"), month <= as.Date("2026-04-01"))
  d <- win(fig_ov); f <- win(fig_ov_full)
  b <- function(x, col) mean(x[[col]][x$month <= as.Date("2025-04-01")], na.rm = TRUE)
  e <- function(x, col) x[[col]][x$month == as.Date("2026-04-01")]
  record("ov.ai_share_end_pct", 100 * e(d, "ai_share"))
  record("ov.prs_per_author_base", b(d, "prs_per_author")); record("ov.prs_per_author_end", e(d, "prs_per_author"))
  record("ov.prs_per_author_ratio", e(d, "prs_per_author") / b(d, "prs_per_author"))
  record("ov.volume_ratio", e(f, "n_prs") / b(f, "n_prs"))
  record("ov.reviewers_ratio", e(f, "reviewers") / b(f, "reviewers"))
  record("ov.load_ratio", (e(f, "n_prs") / e(f, "reviewers")) / (b(f, "n_prs") / b(f, "reviewers")))
  record("ov.human_base_pct", 100 * b(d, "share_human")); record("ov.human_end_pct", 100 * e(d, "share_human"))
  record("ov.auto_base_pct", 100 * b(d, "share_auto")); record("ov.auto_end_pct", 100 * e(d, "share_auto"))
  record("ov.sub_base_pct", 100 * b(d, "share_sub")); record("ov.sub_end_pct", 100 * e(d, "share_sub"))
  record("ov.silent_end_pct", 100 * e(d, "share_silent"))
  # Organization-level latency (app:org-latency).
  pre <- d %>% filter(month < MANDATE)
  record("lat.p90_peak_h", max(d$cycle_p90, na.rm = TRUE))
  record("lat.p90_sub_peak_h", max(d$cycle_p90_sub, na.rm = TRUE))
  record("lat.p90_sub_2026_h", mean(d$cycle_p90_sub[d$month >= as.Date("2026-01-01")], na.rm = TRUE))
  record("lat.p90_sub_base_h", b(d, "cycle_p90_sub"))
  record("lat.sub_merged_premandate_peak_pct", 100 * max(pre$share_sub_merged, na.rm = TRUE))
  record("lat.sub_merged_end_pct", 100 * e(d, "share_sub_merged"))
  cat(sprintf("[overview] PRs/dev %.1f -> %.1f (%.2fx) | volume %.1fx | reviewers %.1fx | load %.1fx\n",
              RESULTS$ov.prs_per_author_base, RESULTS$ov.prs_per_author_end, RESULTS$ov.prs_per_author_ratio,
              RESULTS$ov.volume_ratio, RESULTS$ov.reviewers_ratio, RESULTS$ov.load_ratio))
  cat(sprintf("[overview] human review %.0f%% -> %.0f%% | automated %.0f%% -> %.0f%% | substantive %.0f%% -> %.0f%% | silent %.0f%%\n",
              RESULTS$ov.human_base_pct, RESULTS$ov.human_end_pct, RESULTS$ov.auto_base_pct, RESULTS$ov.auto_end_pct,
              RESULTS$ov.sub_base_pct, RESULTS$ov.sub_end_pct, RESULTS$ov.silent_end_pct))
})
