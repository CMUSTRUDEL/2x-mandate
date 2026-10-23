# =============================================================================
# RQ3: how AI volume reshaped review and whether quality degraded.
# Review-share reshaping + per-reviewer load (descriptive) and the per-PR
# effect of AI authorship (tab:review, tab:phases). Sources R/data_layer.R.
# =============================================================================
if (!exists("panel_est")) source("R/data_layer.R")
suppressMessages(library(fixest))
cat("\n################  RQ3  ################\n")

## ---- (1) Per-PR human vs automated review flags -----------------------------
# Human review = a non-automated review/comment by someone other than the PR author.
# Automated review = a review/comment row flagged is_automated.
rv <- reviews %>%
  inner_join(prs %>% select(repository, number, pr_author = identity, month, ai),
             by = c("repository", "pr_number" = "number")) %>%
  group_by(repository, pr_number) %>%
  summarise(human_rev = any(human & identity != pr_author),
            auto_rev  = any(!human),
            month = first(month), ai = first(ai), .groups = "drop")
prq <- prs %>% left_join(rv, by = c("repository", "number" = "pr_number"), suffix = c("", ".r")) %>%
  left_join(comments_pp, by = c("repository", "number" = "pr_number")) %>%
  mutate(human_rev = replace_na(human_rev, FALSE), auto_rev = replace_na(auto_rev, FALSE),
         com_hum = replace_na(com_hum, 0L))

## ---- (2) Review-share reshaping over time (all non-automated PRs) ------------
# The paper's overview figures and their in-text numbers are computed on the
# estimation sample in R/figure_data.R; this all-PR version is a cross-check.
shr <- prq %>% group_by(month) %>%
  summarise(human = mean(human_rev), auto = mean(auto_rev), merge_rate = mean(merged),
            revert = mean(replace_na(is_revert, FALSE)), .groups = "drop")
sh <- function(d) shr %>% filter(month == d)
b25 <- shr %>% filter(month %in% BASE_MONTHS) %>% summarise(across(-month, mean))
cat(sprintf("[review shares, all PRs] human %.0f%% -> %.0f%% | automated %.0f%% -> %.0f%%\n",
            100 * b25$human, 100 * sh(END_MONTH)$human, 100 * b25$auto, 100 * sh(END_MONTH)$auto))
cat(sprintf("                         merge rate %.2f -> %.2f | revert %.3f -> %.3f\n",
            b25$merge_rate, sh(END_MONTH)$merge_rate, b25$revert, sh(END_MONTH)$revert))

## ---- (3) Per-reviewer load (descriptive): volume vs reviewer pool ------------
vol <- prs %>% group_by(month) %>% summarise(n = n(), .groups = "drop")
revpool <- reviews %>% filter(human) %>% mutate(month = mfloor(comment_time)) %>%
  group_by(month) %>% summarise(reviewers = n_distinct(identity), .groups = "drop")
load_growth <- vol %>% inner_join(revpool, by = "month") %>% mutate(load = n / reviewers)
lg <- function(col) {
  b <- load_growth %>% filter(month %in% BASE_MONTHS) %>% summarise(m = mean(.data[[col]])) %>% pull(m)
  e <- load_growth %>% filter(month == END_MONTH) %>% pull(.data[[col]]); e / b
}
cat(sprintf("\n[per-reviewer] PR volume %.1fx | reviewer pool %.1fx | load per reviewer %.1fx\n",
            lg("n"), lg("reviewers"), lg("load")))
record("rq3.volume_ratio", lg("n")); record("rq3.reviewers_ratio", lg("reviewers"))
record("rq3.load_ratio", lg("load"))

## ---- (4) tab:review / tab:phases : per-PR effect of AI authorship -----------
# Author + month FE, size controls, author-clustered SEs. P(merged) is estimated
# on all PRs; every other outcome on merged PRs. Review rounds via Poisson, on
# merged PRs that received a review (has_review).
pr_all <- prq %>%
  mutate(lsize = log1p(size), lfiles = log1p(replace_na(files_changed, 0)),
         AI = as.integer(ai), postm = month >= MANDATE, merged_i = as.integer(merged))
pr_m <- pr_all %>% filter(merged) %>%
  mutate(lcom = log1p(com_hum),
         lcode = log1p(replace_na(coding_lead_time_hours, 0)),
         lpick = log1p(replace_na(pickup_time_hours, 0)),
         lrev = log1p(replace_na(review_lead_time_hours, 0)),
         lcyc = log1p(replace_na(total_cycle_time_hours, 0)),
         rounds = replace_na(iteration_count, 0L),
         reverted = as.integer(replace_na(is_revert, FALSE)),
         hcov = as.integer(human_rev))
st3 <- function(p) ifelse(p < .001, "***", ifelse(p < .01, "**", ifelse(p < .05, "*", ifelse(p < .1, "+", ""))))
cat("\n[tab:review / tab:phases]  outcome        pooled       post-mandate\n")
rows <- list(merged   = list("P(merged)",               pr_all, "ols"),
             reverted = list("P(is a revert)",          pr_m,   "ols"),
             lcom     = list("log(1+Comments)",         pr_m,   "ols"),
             lcode    = list("log(1+Coding Lead)",      pr_m,   "ols"),
             lpick    = list("log(1+Pickup)",           pr_m,   "ols"),
             lrev     = list("log(1+Review Lead)",      pr_m,   "ols"),
             lcyc     = list("log(1+Total Cycle)",      pr_m,   "ols"),
             rounds   = list("Review Rounds (Poisson)", filter(pr_m, has_review), "pois"),
             hcov     = list("Human Review Coverage",   pr_m,   "ols"))
for (k in names(rows)) {
  lab <- rows[[k]][[1]]; d <- rows[[k]][[2]]; fam <- rows[[k]][[3]]
  y <- if (k == "merged") "merged_i" else k
  fm <- as.formula(sprintf("%s ~ AI + lsize + lfiles | identity + month", y))
  fit <- function(x) if (fam == "pois") fepois(fm, x, cluster = ~identity) else feols(fm, x, cluster = ~identity)
  mp <- fit(d); mq <- fit(filter(d, postm))
  cat(sprintf("  %-26s %+0.3f%-3s     %+0.3f%-3s\n", lab,
              coef(mp)["AI"], st3(pvalue(mp)["AI"]), coef(mq)["AI"], st3(pvalue(mq)["AI"])))
  record(paste0("review.", k, ".pooled"), coef(mp)["AI"], pvalue(mp)["AI"])
  record(paste0("review.", k, ".post"), coef(mq)["AI"], pvalue(mq)["AI"])
}

# AI-authored PRs are larger: post-mandate size gap within author and month.
post_m <- filter(pr_m, postm)
msz <- feols(lsize ~ AI | identity + month, post_m, cluster = ~identity)
med_ai <- median(post_m$size[post_m$AI == 1]); med_other <- median(post_m$size[post_m$AI == 0])
cat(sprintf("\n[size] post-mandate AI-authored PRs: %+0.2f log points of lines changed (%+.0f%%); median %.0f vs %.0f lines\n",
            coef(msz)["AI"], 100 * (exp(coef(msz)["AI"]) - 1), med_ai, med_other))
record("rq3.ai_size_logpts", coef(msz)["AI"], pvalue(msz)["AI"]); record("rq3.ai_size_median", med_ai)
record("rq3.other_size_median", med_other)
