# =============================================================================
# Canonical data layer. Single source of truth for the analysis.
#
# This file reads a small set of tidy input tables and applies every filter and
# derived-variable definition consistently, so that all downstream analyses
# (RQ1-RQ3 and the figures) consume the same canonical objects. The raw data is
# NOT distributed with this package (see README, "Data availability"); the tables
# below describe the schema an equivalent dataset must provide to re-run the code.
#
# Expected input files (place under `data/`; see README for the full schema):
#   data/pull_requests.csv     one row per pull request (PR-level facts)
#   data/pr_reviews.csv        one row per review/comment event, with reviewer id
#   data/contributors.csv      contributor -> role title (for the seniority ladder)
#   data/ai_tool_usage.csv     contributor x month AI-tool telemetry
#   data/commits.csv           one row per commit (author, timestamp)
#
# Automation: each pull_requests / pr_reviews row carries an `is_automated`
# boolean. Rows with is_automated == TRUE are excluded (PRs) or counted as
# non-human review (reviews). How automation is detected upstream is out of scope
# for this package (documented in the paper).
#
# Sourcing this file populates the global env with: prs, reviews, commits, telem,
# panel, roles, repo_cohort, RESULTS/record(), and the constants below. `source()`-safe.
# =============================================================================
suppressMessages({ library(tidyverse) })

DATA <- "data"
WIN_START <- as.Date("2024-01-01")   # panel window: Jan 2024 ..
WIN_END   <- as.Date("2026-04-30")   # .. Apr 2026 (PRs created in window)
MANDATE   <- as.Date("2025-06-01")   # organization-wide AI-adoption mandate
BASE_MONTHS <- seq(as.Date("2025-01-01"), as.Date("2025-04-01"), by = "month")
END_MONTH   <- as.Date("2026-04-01")
# Frontier-model release onsets (first full month after each public launch).
REL_SONNET45 <- as.Date("2025-09-01")
REL_OPUS45   <- as.Date("2025-11-01")
REL_GEN46    <- as.Date("2026-02-01")

p <- function(...) file.path(DATA, ...)

# Every number the paper reports is recorded under a stable key, so that
# R/check_paper.R can compare it with the value printed in the paper.
if (!exists("RESULTS")) RESULTS <- new.env()
record <- function(key, value) {
  assign(key, unname(as.numeric(value)[1]), envir = RESULTS)
  invisible(value)
}
# Floor to first-of-month. Accepts Date/datetime or "YYYY-MM"/"YYYY-MM-DD" strings.
mfloor <- function(d) {
  if (is.character(d)) d <- as.Date(ifelse(nchar(d) == 7, paste0(d, "-01"), d))
  as.Date(format(as.Date(d), "%Y-%m-01"))
}

# ---- 1. PR-level dataset ----------------------------------------------------
prs_raw <- read_csv(p("pull_requests.csv"), show_col_types = FALSE) %>%
  mutate(
    created = as.Date(created_at),
    month   = mfloor(created),
    merged  = state == "MERGED",
    ai      = str_detect(replace_na(labels, ""), "created-by-ai|created_by_ai"),
    size    = replace_na(lines_added, 0) + replace_na(lines_removed, 0)
  )

# Repo age cohort: a repo is "pre2022" iff its first-ever PR (over ALL history)
# predates 2022; else "2022plus".
repo_cohort <- prs_raw %>% group_by(repository) %>%
  summarise(first_pr = min(created), .groups = "drop") %>%
  mutate(cohort = ifelse(first_pr < as.Date("2022-01-01"), "pre2022", "2022plus"))

prs <- prs_raw %>%
  filter(!is_automated, created >= WIN_START, created <= WIN_END)

# ---- 2. Review / comment events (human = not automated) ----------------------
reviews <- read_csv(p("pr_reviews.csv"), show_col_types = FALSE) %>%
  mutate(human = !is_automated, comment_time = as_datetime(comment_time))

# Per-PR comment counts (PR/issue comments + inline review-thread comments).
# The raw PR-level `comment_count` folds in automated comments; we use the
# human-only count `com_hum` for the review comment outcome so it is consistent
# with the human-bounded review-timing and coverage measures.
comments_pp <- reviews %>%
  filter(type %in% c("review_thread", "comment")) %>%
  group_by(repository, pr_number) %>%
  summarise(com_hum = sum(human), com_all = n(), .groups = "drop")

# Commit events (for the working-time checks in 05_supplement.R): one row per
# commit with its author account and timestamp; automated authors are dropped.
commits <- read_csv(p("commits.csv"), show_col_types = FALSE) %>%
  filter(!is_automated)

# ---- 3. AI-tool telemetry per identity x month ------------------------------
# One tidy table keyed on (identity, month) with the per-tool line/spend columns.
telem <- read_csv(p("ai_tool_usage.csv"), show_col_types = FALSE) %>%
  mutate(month = mfloor(month),
         across(c(cursor_lines, claude_lines, claude_spend), ~ replace_na(.x, 0)),
         ai_lines = cursor_lines + claude_lines,
         ai_any   = as.integer(cursor_lines > 0 | claude_lines > 0 | claude_spend > 0)) %>%
  filter(month <= WIN_END)

# ---- 4. Seniority ladder from free-text role titles -------------------------
# Deterministic keyword classifier on the free-text `role` title -> the ladder
# {IC, Senior, Staff, Principal, Management}. Management folds in manager/lead/
# director/VP/C-level; IC folds in intern/grad and base ICs. Order matters:
# management titles are caught first (e.g. "Senior Engineering Manager" -> Mgmt).
classify_tier <- function(role) {
  r <- str_to_lower(replace_na(role, ""))
  case_when(
    str_detect(r, "manager|\\blead\\b|director|\\bvp\\b|vice president|head of|chief|\\bc[teo]o\\b|president|founder") ~ "Management",
    str_detect(r, "principal")                                   ~ "Principal",
    str_detect(r, "staff")                                       ~ "Staff",
    str_detect(r, "senior|\\bsr\\b|sr\\.")                       ~ "Senior",
    r == ""                                                       ~ NA_character_,
    TRUE                                                          ~ "IC"
  )
}
roles <- read_csv(p("contributors.csv"), show_col_types = FALSE) %>%
  mutate(tier = factor(classify_tier(role), levels = c("IC", "Senior", "Staff", "Principal", "Management")))

# ---- 5. Developer-month panel (the modelling table) -------------------------
pr_agg <- prs %>% group_by(identity, month) %>% summarise(
  n_prs        = n(),
  n_merged     = sum(merged),
  n_reverted   = sum(replace_na(is_revert, FALSE)),
  n_ai_prs     = sum(ai),
  avg_pr_size  = mean(size),
  avg_review_comments = mean(replace_na(comment_count, 0)),
  avg_review_time     = mean(replace_na(review_lead_time_hours, NA), na.rm = TRUE),
  .groups = "drop"
)

# The developer panel is the set of (non-automated) PR authors in the window; AI
# telemetry is joined onto them. Telemetry-only identities (no PRs in the window)
# are not developers in our sense and are excluded.
pr_ids <- unique(prs$identity)
panel <- pr_agg %>%
  full_join(filter(telem, identity %in% pr_ids), by = c("identity", "month")) %>%
  mutate(across(c(n_prs, n_merged, n_reverted, n_ai_prs,
                  cursor_lines, claude_lines, claude_spend, ai_lines, ai_any), ~ replace_na(.x, 0))) %>%
  filter(month >= WIN_START, month <= WIN_END) %>%
  arrange(identity, month) %>%
  mutate(adopted = ai_any, post = as.integer(month >= MANDATE))

# Estimation sample: developers with >= 3 active months, where a month is active
# iff they authored >= 1 PR. The inclusion criterion is deliberately INDEPENDENT
# of AI-tool use: it must apply symmetrically to adopters and never-adopters, or
# sample selection would correlate with treatment (an AI-heavy, low-PR developer
# would be kept while an equally low-PR never-adopter is dropped).
est_ids <- panel %>% filter(n_prs > 0) %>%
  count(identity) %>% filter(n >= 3) %>% pull(identity)

# Cumulative AI experience (lagged), per developer, on the estimation sample.
panel <- panel %>% group_by(identity) %>%
  mutate(cum_ai = cumsum(ai_lines),
         log_cum_ai = log1p(lag(cum_ai, default = 0))) %>% ungroup() %>%
  mutate(in_est = identity %in% est_ids)

# Convenience: active estimation-sample panel used by most models.
panel_est <- panel %>% filter(in_est, n_prs > 0) %>% mutate(log_n_prs = log(n_prs))

# ---- 6. Dataset summary (tab:data) -----------------------------------------
ado_flag <- panel %>% filter(in_est) %>% group_by(identity) %>% summarise(a = max(ai_any), .groups = "drop")
record("data.developers", n_distinct(panel$identity))
record("data.est_sample", length(est_ids))
record("data.adopters", sum(ado_flag$a == 1))
record("data.never", sum(ado_flag$a == 0))
record("data.prs", nrow(prs))
record("data.ai_share_pct", 100 * mean(prs$ai))
record("data.repos", n_distinct(prs$repository))
record("data.review_rows_automated_pct", 100 * mean(!reviews$human))
record("data.comment_events_automated_pct", 100 * (1 - sum(comments_pp$com_hum) / sum(comments_pp$com_all)))
record("data.role_coverage_pct", 100 * mean(est_ids %in% roles$identity[!is.na(roles$tier)]))

.summary <- function() {
  cat("==== Dataset summary ====\n")
  cat(sprintf("Developers (panel)            : %d\n", RESULTS$data.developers))
  cat(sprintf("  estimation sample (>=3 act) : %d\n", RESULTS$data.est_sample))
  cat(sprintf("  adopters / never-adopters   : %d / %d\n", RESULTS$data.adopters, RESULTS$data.never))
  cat(sprintf("Pull requests (non-automated) : %d\n", RESULTS$data.prs))
  cat(sprintf("  AI-authored (created-by-ai) : %.1f%%\n", RESULTS$data.ai_share_pct))
  cat(sprintf("Repositories (active)         : %d\n", RESULTS$data.repos))
  cat(sprintf("Review/comment events         : %d (automated %.0f%%)\n", nrow(reviews), RESULTS$data.review_rows_automated_pct))
  cat(sprintf("Comment events by automation  : %.0f%%\n", RESULTS$data.comment_events_automated_pct))
  cat(sprintf("Estimation sample with a role : %.0f%%\n", RESULTS$data.role_coverage_pct))
}
if (sys.nframe() == 0) .summary()
