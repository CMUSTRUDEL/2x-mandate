# =============================================================================
# Rebuilds the paper's tables, in the paper's layout, from the numbers the
# analysis scripts recorded (RESULTS, PVALS), and writes them as Markdown to
# output/tables/. Significance: + p<0.10, * p<0.05, ** p<0.01, *** p<0.001.
# Sourced by R/run_all.R after every analysis script has run.
# =============================================================================
TABLE_DIR <- file.path("output", "tables")
dir.create(TABLE_DIR, showWarnings = FALSE, recursive = TRUE)

val <- function(k) if (exists(k, envir = RESULTS, inherits = FALSE)) get(k, envir = RESULTS) else NA_real_
pv  <- function(k) if (exists(k, envir = PVALS, inherits = FALSE)) get(k, envir = PVALS) else NA_real_
stars <- function(p) ifelse(is.na(p), "", ifelse(p < .001, "***", ifelse(p < .01, "**",
                       ifelse(p < .05, "*", ifelse(p < .1, "+", "")))))
# A signed coefficient with stars; plain numbers for descriptive cells.
cf  <- function(k, d = 3) { v <- val(k); if (is.na(v)) "n/a" else paste0(sprintf(paste0("%+.", d, "f"), v), stars(pv(k))) }
num <- function(k, d = 1) { v <- val(k); if (is.na(v)) "n/a" else formatC(v, format = "f", digits = d, big.mark = ",") }
pct <- function(k) { v <- val(k); if (is.na(v)) "n/a" else sprintf("%+.0f%%", 100 * (exp(v) - 1)) }
sgn <- function(k, d = 2) { v <- val(k); if (is.na(v)) "n/a" else sprintf(paste0("%+.", d, "f"), v) }
mult <- function(k) { v <- val(k); if (is.na(v)) "n/a" else sprintf("%+.2f (%.1fx)", v, exp(v)) }

write_table <- function(name, caption, header, rows) {
  md <- c(paste0("**", name, "**: ", caption), "",
          paste0("| ", paste(header, collapse = " | "), " |"),
          paste0("|", paste(rep("---", length(header)), collapse = "|"), "|"),
          vapply(rows, function(r) paste0("| ", paste(r, collapse = " | "), " |"), ""))
  writeLines(md, file.path(TABLE_DIR, paste0(sub(":", "_", name), ".md")))
  invisible(md)
}

write_table("tab:data", "Summary of the datasets (Jan 2024 to Apr 2026).", c("Quantity", "Value"), list(
  c("Developers (panel)", num("data.developers", 0)),
  c("  estimation sample (>= 3 active months)", num("data.est_sample", 0)),
  c("  AI-tool adopters / never-adopters", paste(num("data.adopters", 0), "/", num("data.never", 0))),
  c("Pull requests (non-automated)", num("data.prs", 0)),
  c("  AI-authored (created-by-ai)", paste0(num("data.ai_share_pct", 1), "%")),
  c("Repositories (active)", num("data.repos", 0))))

did_rows <- c("log(Monthly PRs)" = "log_n_prs", "log(1+Avg PR Size)" = "l_size",
              "log(1+Avg Review Time)" = "l_rtime", "log(1+Avg Review Comments)" = "l_rcom",
              "Merge Rate" = "merge_rate", "Revert Rate" = "revert_rate")
write_table("tab:did", "Developer-level difference-in-differences (Eq. 1); developer FE, SEs clustered by developer.",
            c("Outcome", "AI Adopted", "Post Mandate"),
            lapply(names(did_rows), function(n) c(n, cf(paste0("did.", did_rows[[n]], ".adopted")),
                                                  cf(paste0("did.", did_rows[[n]], ".post")))))

write_table("tab:decomp", "Decomposing the within-developer throughput gain (Eq. 3); contributions at Apr-2026 usage.",
            c("Term", "Coefficient", "Contribution (log pts)"), list(
  c("Adoption jump (beta_1)", cf("decomp.jump"), sgn("decomp.jump", 2)),
  c("Accumulated use (beta)", cf("decomp.slope"), sgn("decomp.slope_contrib", 2)),
  c("Residual mandate (theta)", cf("decomp.post", 2), sgn("decomp.post", 2)),
  c("Total (within-developer change)", "", sgn("decomp.total", 2)),
  c("Preferred model, calendar-month FE: jump / slope / total", paste(cf("decomp_mfe.jump", 2), "/", cf("decomp_mfe.slope")),
    sprintf("%s (%sx)", sgn("decomp_mfe.total", 2), num("decomp_mfe.multiplier", 2)))))

tiers <- c("IC", "Senior", "Staff", "Principal", "Management")
write_table("tab:tier", "Heterogeneity in the AI adoption effect (n = developers; cells = dev-cohort-months).",
            c("Group", "n", "AI Adopted", "approx. %"), c(
  lapply(tiers, function(t) c(paste("Seniority:", t), num(paste0("tier.", t, ".n"), 0),
                              cf(paste0("tier.", t, ".adopted")), pct(paste0("tier.", t, ".adopted")))),
  list(c("Repository cohort: pre-2022 (legacy)", num("cohort.pre2022.cells", 0), cf("cohort.pre2022.adopted"), pct("cohort.pre2022.adopted")),
       c("Repository cohort: 2022+ (newer)", num("cohort.2022plus.cells", 0), cf("cohort.2022plus.adopted"), pct("cohort.2022plus.adopted")),
       c("Newer minus legacy (interaction)", "", cf("cohort.diff"), ""))))

rev_rows <- c("P(merged)" = "merged", "P(is a revert)" = "reverted", "log(1+Comments)" = "lcom",
              "log(1+Review Lead)" = "lrev", "log(1+Total Cycle)" = "lcyc",
              "Review Rounds (Poisson)" = "rounds", "Human Review Coverage (LPM)" = "hcov")
write_table("tab:review", "Per-PR effects of the created-by-ai label (Eq. 4); author and month FE, size controls.",
            c("Outcome", "Pooled", "Post-mandate"),
            lapply(names(rev_rows), function(n) c(n, cf(paste0("review.", rev_rows[[n]], ".pooled")),
                                                  cf(paste0("review.", rev_rows[[n]], ".post")))))
ph_rows <- c("log(1+Coding Lead)" = "lcode", "log(1+Pickup)" = "lpick",
             "log(1+Review Lead)" = "lrev", "log(1+Total Cycle)" = "lcyc")
write_table("tab:phases", "Per-PR effect of AI authorship by cycle-time phase (Eq. 4).",
            c("Phase", "Pooled", "Post-mandate"),
            lapply(names(ph_rows), function(n) c(n, cf(paste0("review.", ph_rows[[n]], ".pooled")),
                                                 cf(paste0("review.", ph_rows[[n]], ".post")))))

write_table("tab:robust", "AI-adoption coefficient on log(Monthly PRs) across estimators.",
            c("", "TWFE", "+Mo. FE", "Drop never", "C&S", "Borusyak"),
            list(c("log(PRs)", cf("robust.twfe"), cf("robust.month_fe"), cf("robust.drop_never"),
                   cf("robust.cs"), cf("robust.borusyak"))))

five_col <- function(prefix, label, d_desc = 2) c(label, num(paste0(prefix, ".base"), d_desc),
  num(paste0(prefix, ".end"), d_desc), cf(paste0(prefix, ".adopted"), 2), cf(paste0(prefix, ".month_fe"), 2),
  num(paste0(prefix, ".at_apr26"), 2))
five_hdr <- c("Outcome", "Base", "Apr 26", "Adopt.", "+Mo. FE", "Apr 26 use")

write_table("tab:splitting", "Pull-request splitting: volume rows Poisson (log points), shape rows OLS on the developer-month average.",
            five_hdr, list(
  five_col("split.prs", "PRs", 1), five_col("split.lines", "Lines changed", 0),
  five_col("split.files", "Files changed", 0), five_col("split.commits", "Commits", 0),
  five_col("split.lines_per_pr", "log(1+lines/PR)"), five_col("split.files_per_pr", "log(1+files/PR)"),
  five_col("split.commits_per_pr", "log(1+commits/PR)"), five_col("split.small10", "Share <= 10 lines"),
  five_col("split.big500", "Share >= 500 lines"),
  c("Median PR size (lines)", num("split.median_size_base", 0), num("split.median_size_end", 0), "", "", "")))

types <- c("All PRs" = "all_prs", "Feature" = "feature", "Bug fix" = "bug_fix",
           "Maintenance" = "maintenance", "Revert" = "revert", "Other" = "other")
write_table("tab:prtypes", "Pull requests by type: within-developer Poisson effects of AI adoption on the monthly count of each type.",
            c("Type", "Base", "Adopt.", "+Mo. FE", "Apr 26 use"),
            lapply(names(types), function(n) { k <- paste0("types.", types[[n]])
              c(n, num(paste0(k, ".base"), 1), cf(paste0(k, ".adopted"), 2), cf(paste0(k, ".month_fe"), 2),
                mult(paste0(k, ".at_apr26"))) }))

write_table("tab:worktime", "Working time: count rows Poisson (log points), share and span rows OLS.",
            five_hdr, list(
  five_col("work.active_days", "Active days / month", 1), five_col("work.weekend_days", "Weekend days / month"),
  five_col("work.off_share", "Off-hours share"), five_col("work.span", "Daily span, hours", 1),
  five_col("work.prs", "PRs / month", 1), five_col("work.prs_per_day", "PRs / active day (offset)", 1)))

write_table("tab:ladder", "Model capability ladder (each step incremental on the previous) versus accumulated use.",
            c("Term", "Ladder", "+ Month-of-year FE"), list(
  c("Sonnet 4.5 (gamma_S)", cf("ladder.sonnet45"), cf("ladder_moy.sonnet45")),
  c("Opus 4.5, incr. (gamma_O)", cf("ladder.opus45"), cf("ladder_moy.opus45")),
  c("4.6 generation, incr. (gamma_46)", cf("ladder.gen46"), cf("ladder_moy.gen46")),
  c("Accumulated use (beta)", cf("ladder.experience"), cf("ladder_moy.experience")),
  c("Post mandate (theta)", cf("ladder.post"), "---"),
  c("Placebo (Aug 2025)", cf("ladder.placebo"), "---")))

cat(sprintf("wrote %d tables to %s/\n", length(list.files(TABLE_DIR, pattern = "\\.md$")), TABLE_DIR))
