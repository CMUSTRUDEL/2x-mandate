# =============================================================================
# Runs the whole pipeline on whatever dataset is in data/ and writes:
#   output/results.csv   every number the paper reports: key, value, p-value,
#                        where it appears in the paper, and the script that
#                        computes it
#   output/tables/*.md   the paper's tables, rebuilt in the paper's layout
#   output/figures/*.pdf every figure of the paper and its supplement
#
# Run from the repository root:  Rscript R/run_all.R
# (On synthetic data from R/make_synthetic_data.R the numbers are meaningless;
# the run shows what each output looks like and that the code executes.)
# =============================================================================
RESULTS <- new.env(); PVALS <- new.env()
source("R/data_layer.R")
source("R/01_rq1.R")
source("R/02_rq2.R")
source("R/03_rq3.R")
source("R/05_supplement.R")
source("R/figure_data.R")
source("R/04_figures.R")
source("R/tables.R")

# Where each recorded number appears in the paper, and which script computes it,
# by key prefix (the longest matching prefix wins).
KEY_INDEX <- tribble(
  ~prefix,           ~where,                                        ~script,
  "data.",           "tab:data; sec:data",                          "R/data_layer.R",
  "rq1.",            "sec:throughput; app:spend",                   "R/01_rq1.R",
  "did.",            "tab:did",                                     "R/01_rq1.R",
  "decomp.",         "tab:decomp",                                  "R/01_rq1.R",
  "decomp_mfe.",     "sec:throughput (calendar-month FE)",          "R/01_rq1.R",
  "robust.",         "tab:robust",                                  "R/01_rq1.R",
  "denom.",          "app:throughput (denominator)",                "R/01_rq1.R",
  "prsignal.",       "app:prsignal",                                "R/01_rq1.R",
  "tier.",           "tab:tier (seniority); sec:het",               "R/02_rq2.R",
  "cohort.",         "tab:tier (repository cohort); sec:het",       "R/02_rq2.R",
  "ladder.",         "tab:ladder; app:capability",                  "R/02_rq2.R",
  "ladder_moy.",     "tab:ladder (month-of-year FE)",               "R/02_rq2.R",
  "ladder_variant.", "sec:het (robustness of the accumulated-use slope)", "R/02_rq2.R",
  "rq3.",            "sec:process",                                 "R/03_rq3.R",
  "review.",         "tab:review; tab:phases",                      "R/03_rq3.R",
  "split.",          "tab:splitting; app:splitting",                "R/05_supplement.R",
  "work.",           "tab:worktime; app:worktime",                  "R/05_supplement.R",
  "work_pr_review.", "app:worktime (PRs and reviews only)",         "R/05_supplement.R",
  "types.",          "tab:prtypes; app:prtypes",                    "R/05_supplement.R",
  "style.",          "app:prtypes (title-style check)",             "R/05_supplement.R",
  "followup.",       "app:followup; sec:process",                   "R/05_supplement.R",
  "ov.",             "fig:overview-*; sec:throughput; sec:process; app:review-composition", "R/figure_data.R",
  "lat.",            "app:org-latency; fig:overview-latency",       "R/figure_data.R")

keys <- sort(ls(RESULTS))
locate <- function(k) {
  hit <- KEY_INDEX[startsWith(k, KEY_INDEX$prefix), ]
  hit[which.max(nchar(hit$prefix)), ]
}
res <- bind_rows(lapply(keys, function(k) {
  loc <- locate(k)
  tibble(key = k, value = get(k, envir = RESULTS), p_value = get(k, envir = PVALS),
         where = if (nrow(loc)) loc$where else NA_character_,
         script = if (nrow(loc)) loc$script else NA_character_)
}))
dir.create("output", showWarnings = FALSE)
write_csv(res, file.path("output", "results.csv"), na = "")
cat(sprintf("\nwrote %d recorded numbers to output/results.csv\n", nrow(res)))
