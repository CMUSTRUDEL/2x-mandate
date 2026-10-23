# =============================================================================
# Writes a small SYNTHETIC dataset in the input schema to data/, so the whole
# pipeline can be run end to end without the confidential data. The numbers it
# produces are meaningless; it exists to exercise the code and document the
# schema by example. Nothing here is derived from the study data.
#
# Run from the package root:  Rscript R/make_synthetic_data.R [out_dir]
# =============================================================================
suppressMessages({ library(tidyverse) })
set.seed(42)
args <- commandArgs(trailingOnly = TRUE)
out <- if (length(args)) args[1] else "data"
dir.create(out, showWarnings = FALSE, recursive = TRUE)

months  <- seq(as.Date("2024-01-01"), as.Date("2026-04-01"), by = "month")
mandate <- as.Date("2025-06-01")
n_dev <- 160; n_repo <- 30
devs  <- sprintf("user-%04d", seq_len(n_dev))
repos <- sprintf("repo-%03d", seq_len(n_repo))
bots  <- c("bot-0001", "bot-0002")

# Developers: tenure window, base rate, adoption month (some never adopt).
dv <- tibble(identity = devs,
             start = sample(months[1:18], n_dev, replace = TRUE),
             end   = pmax(start, sample(months[10:28], n_dev, replace = TRUE)),
             rate  = rlnorm(n_dev, log(12), 0.6),
             adopt = if_else(runif(n_dev) < 0.8,
                             sample(months[12:26], n_dev, replace = TRUE), as.Date(NA)),
             role  = sample(c("Software Engineer", "Senior Software Engineer", "Staff Engineer",
                              "Principal Engineer", "Engineering Manager", NA), n_dev, replace = TRUE,
                            prob = c(.35, .25, .12, .05, .1, .13)))

# Developer-month counts with an adoption jump that grows with time on tool.
dm <- dv %>% rowwise() %>% reframe(identity, m = months[months >= start & months <= end],
                                    rate, adopt) %>%
  mutate(on = !is.na(adopt) & m >= adopt,
         tenure = ifelse(on, as.integer(round(as.numeric(m - adopt) / 30.44)), 0L),
         lam = rate * exp(0.15 * on + 0.04 * tenure),
         n = rpois(n(), lam))

ts_in <- function(m, k) as.POSIXct(m) + runif(k, 0, 27 * 86400)
titles <- c("feat: add export endpoint", "fix: handle empty payload", "chore: bump dependency to v2",
            "refactor billing service", "Add retry to webhook sender", "Update README", "tweak layout",
            "Revert \"add export endpoint\"", "[ABC-123] fix crash on load", "docs: clarify setup")
prs <- dm %>% filter(n > 0) %>% uncount(n) %>%
  mutate(repository = sample(repos, n(), replace = TRUE),
         created_at = ts_in(m, n()),
         ai = on & runif(n()) < pmin(0.9, 0.2 + 0.05 * tenure),
         state = sample(c("MERGED", "CLOSED", "OPEN"), n(), replace = TRUE, prob = c(.85, .12, .03)),
         merged_at = if_else(state == "MERGED", created_at + rexp(n(), 1 / 30) * 3600, as.POSIXct(NA)),
         title = sample(titles, n(), replace = TRUE),
         is_revert = str_detect(title, "^Revert"),
         head_ref = paste0(sample(c("feat/", "fix/", "chore/", ""), n(), replace = TRUE), "branch-", row_number()),
         lines_added = rnbinom(n(), mu = 60 * (1 + 0.5 * ai), size = 0.3),
         lines_removed = rnbinom(n(), mu = 20, size = 0.7),
         files_changed = 1L + rpois(n(), 2),
         commit_count = 1L + rpois(n(), 2),
         iteration_count = rpois(n(), 0.8),
         comment_count = rpois(n(), 2),
         coding_lead_time_hours = rexp(n(), 1 / 6),
         pickup_time_hours = rexp(n(), 1 / 4),
         review_lead_time_hours = rexp(n(), 1 / 10) * (1 + 0.2 * ai),
         total_cycle_time_hours = coding_lead_time_hours + pickup_time_hours + review_lead_time_hours,
         labels = if_else(ai, "created-by-ai", ""),
         is_automated = FALSE,
         is_generated = runif(n()) < 0.01) %>%
  group_by(repository) %>% mutate(number = row_number() + 100L) %>% ungroup()

# Old PRs so some repositories are "pre-2022", plus a few automated PRs.
old <- tibble(repository = repos[1:10], identity = sample(devs, 10),
              created_at = as.POSIXct("2019-06-01") + runif(10, 0, 3e7), number = 1L,
              state = "MERGED", labels = "", is_automated = FALSE, is_generated = FALSE)
botpr <- tibble(repository = sample(repos, 50, replace = TRUE), identity = sample(bots, 50, replace = TRUE),
                created_at = ts_in(sample(months, 50, replace = TRUE), 50), number = 10000L + 1:50,
                state = "MERGED", labels = "", is_automated = TRUE, is_generated = FALSE,
                title = "chore(deps): bump lodash")
pr_out <- bind_rows(prs, old, botpr) %>%
  mutate(merged_at = if_else(state == "MERGED" & is.na(merged_at), created_at + 3600, merged_at)) %>%
  select(repository, number, identity, created_at, merged_at, state, labels, title, head_ref,
         lines_added, lines_removed, files_changed, commit_count, iteration_count, is_revert,
         comment_count, coding_lead_time_hours, pickup_time_hours, review_lead_time_hours,
         total_cycle_time_hours, is_automated, is_generated)
write_csv(pr_out, file.path(out, "pull_requests.csv"), na = "")

# Review events: human reviewers (other developers) and automated reviewers.
rv_types <- c("comment", "review_thread", "review:commented", "review:changes_requested",
              "review:approved", "review:dismissed")
base_pr <- prs %>% select(repository, pr_number = number, pr_author = identity, created_at, m)
human_rev <- base_pr %>% filter(runif(n()) < 0.8) %>%
  mutate(k = 1L + rpois(n(), 1)) %>% uncount(k) %>%
  mutate(identity = sample(devs, n(), replace = TRUE), author = identity,
         type = sample(rv_types, n(), replace = TRUE, prob = c(.2, .15, .1, .05, .45, .05)),
         comment_time = created_at + rexp(n(), 1 / 5) * 3600, is_automated = FALSE, is_templated = runif(n()) < 0.02)
auto_rev <- base_pr %>% filter(runif(n()) < pmin(0.9, 0.1 + 0.8 * (m >= mandate))) %>%
  mutate(identity = sample(bots, n(), replace = TRUE), author = identity, type = "comment",
         comment_time = created_at + 60, is_automated = TRUE, is_templated = FALSE)
write_csv(bind_rows(human_rev, auto_rev) %>%
            select(repository, pr_number, identity, author, type, comment_time, is_automated, is_templated),
          file.path(out, "pr_reviews.csv"), na = "")

# Commits: a few per PR, around PR creation.
write_csv(prs %>% transmute(identity, k = commit_count, created_at) %>% uncount(k) %>%
            transmute(identity, committed_at = created_at - runif(n(), 0, 2 * 86400), is_automated = FALSE),
          file.path(out, "commits.csv"), na = "")

write_csv(dv %>% select(identity, role), file.path(out, "contributors.csv"), na = "")

# AI-tool telemetry from each developer's adoption month on.
tel <- dm %>% filter(on) %>%
  transmute(identity, month = format(m, "%Y-%m"),
            cursor_lines = if_else(m < as.Date("2026-02-01") & runif(n()) < 0.6, rpois(n(), 400), 0L),
            claude_lines = if_else(m >= as.Date("2025-07-01"), rpois(n(), 300 + 60 * tenure), 0L),
            claude_spend = if_else(m >= as.Date("2025-07-01"), round(rexp(n(), 1 / (40 + 60 * tenure)), 2), 0))
write_csv(tel, file.path(out, "ai_tool_usage.csv"), na = "")

cat(sprintf("wrote synthetic data to %s/: %d PRs, %d review events, %d developers\n",
            out, nrow(pr_out), nrow(human_rev) + nrow(auto_rev), n_dev))
