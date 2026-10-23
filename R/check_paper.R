# =============================================================================
# Checks the pipeline's numbers against the values printed in the paper.
#
# Runs every analysis script, then compares each recorded number with
# expected/paper_values.csv (one row per number the paper reports, with the
# tolerance implied by the paper's rounding, and the LaTeX label where it
# appears). Writes output/check_results.csv and prints PASS / FAIL / MISSING.
#
# Run from the package root:  Rscript R/check_paper.R
# Set CHECK_FIGURES=1 to also regenerate the figures.
# =============================================================================
RESULTS <- new.env()
source("R/data_layer.R")
source("R/01_rq1.R")
source("R/02_rq2.R")
source("R/03_rq3.R")
source("R/figure_data.R")
source("R/05_supplement.R")
if (Sys.getenv("CHECK_FIGURES") == "1") source("R/04_figures.R")

exp_vals <- read_csv("expected/paper_values.csv", show_col_types = FALSE)
got <- tibble(key = ls(RESULTS), computed = vapply(ls(RESULTS), function(k) get(k, envir = RESULTS), numeric(1)))

chk <- exp_vals %>% left_join(got, by = "key") %>%
  mutate(status = case_when(
    is.na(computed)                                 ~ "MISSING",
    op == "eq" & abs(computed - expected) <= tol + 1e-9 ~ "PASS",
    op == "lt" & computed < expected                ~ "PASS",
    op == "gt" & computed > expected                ~ "PASS",
    TRUE                                            ~ "FAIL"))

dir.create("output", showWarnings = FALSE)
write_csv(chk, "output/check_results.csv")

cat("\n################  CHECK AGAINST THE PAPER  ################\n")
bad <- chk %>% filter(status != "PASS")
if (nrow(bad)) {
  cat(sprintf("  %-42s %10s %10s %4s  %s\n", "key", "paper", "computed", "op", "where"))
  for (i in seq_len(nrow(bad))) {
    cat(sprintf("  %-7s %-34s %10.4g %10.4g %4s  %s\n", bad$status[i], bad$key[i], bad$expected[i],
                bad$computed[i], bad$op[i], bad$where[i]))
  }
}
cat(sprintf("\n%d of %d paper numbers reproduced (%d fail, %d missing). Details: output/check_results.csv\n",
            sum(chk$status == "PASS"), nrow(chk), sum(chk$status == "FAIL"), sum(chk$status == "MISSING")))
unreported <- setdiff(got$key, exp_vals$key)
if (length(unreported)) cat(sprintf("Recorded but not compared (cross-checks): %s\n", paste(unreported, collapse = ", ")))
