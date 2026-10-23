# =============================================================================
# RQ2: heterogeneity of the adoption effect across seniority tiers, repo-age
# cohorts, and frontier-model generations (the capability ladder + placebo).
# =============================================================================
if (!exists("panel_est")) source("R/data_layer.R")
suppressMessages(library(fixest))
cat("\n################  RQ2  ################\n")

## ---- (1) tab:tier upper block: per-seniority-tier adoption DiD ---------------
pet <- panel_est %>% inner_join(roles %>% select(identity, tier), by = "identity") %>% filter(!is.na(tier))
cat("\n[tab:tier seniority]\n")
for (tg in levels(pet$tier)) {
  d <- pet %>% filter(tier == tg)
  if (n_distinct(d$identity) < 5) next
  m <- feols(log_n_prs ~ adopted + post | identity, d, cluster = ~identity)
  cat(sprintf("  %-11s n=%3d  adopted %+0.3f  (%+d%%)\n",
              tg, n_distinct(d$identity), coef(m)["adopted"], round(100 * (exp(coef(m)["adopted"]) - 1))))
  record(paste0("tier.", tg, ".n"), n_distinct(d$identity)); record(paste0("tier.", tg, ".adopted"), coef(m)["adopted"], pvalue(m)["adopted"])
}
# Do adoption effects differ across the IC-to-principal ladder? Joint Wald test of
# the adopted x {Senior, Staff, Principal} interactions (IC = reference).
inter <- feols(log_n_prs ~ adopted * tier + post | identity, pet, cluster = ~identity)
w_ladder <- wald(inter, keep = "^adopted:tier(Senior|Staff|Principal)$", print = FALSE)
cat(sprintf("  (IC-to-principal interactions, joint Wald p = %.3f)\n", w_ladder$p))
record("tier.ladder_wald_p", w_ladder$p)

## ---- (2) tab:tier lower block: repo-age cohort DiD --------------------------
# developer x cohort x month panel: a developer's PRs split by repo age cohort.
coh <- prs %>% inner_join(repo_cohort %>% select(repository, cohort), by = "repository") %>%
  filter(identity %in% est_ids) %>%
  group_by(identity, month, cohort) %>% summarise(n_prs = n(), .groups = "drop") %>%
  left_join(panel %>% select(identity, month, adopted, post), by = c("identity", "month")) %>%
  mutate(log_n_prs = log(n_prs), ic = paste(identity, cohort),
         cohort = relevel(factor(cohort), "pre2022"))
cat("\n[tab:tier repo cohort]\n")
for (cg in c("pre2022", "2022plus")) {
  d <- coh %>% filter(cohort == cg)
  m <- feols(log_n_prs ~ adopted + post | identity, d, cluster = ~identity)
  cat(sprintf("  %-9s cells=%d  adopted %+0.3f  (%+d%%)\n",
              cg, nrow(d), coef(m)["adopted"], round(100 * (exp(coef(m)["adopted"]) - 1))))
  record(paste0("cohort.", cg, ".cells"), nrow(d)); record(paste0("cohort.", cg, ".adopted"), coef(m)["adopted"], pvalue(m)["adopted"])
}
cint <- feols(log_n_prs ~ adopted * cohort + post | ic, coh, cluster = ~identity)
ct <- summary(cint)$coeftable; ir <- grep("^adopted:cohort", rownames(ct), value = TRUE)[1]
cat(sprintf("  (newer - legacy difference: %+0.3f, p=%.3f)\n", ct[ir, 1], ct[ir, 4]))
record("cohort.diff", ct[ir, 1], ct[ir, 4]); record("cohort.diff_p", ct[ir, 4])

## ---- (3) tab:ladder: capability vs experience + placebo ---------------------
# ever-adopters with >= 6 active months; release indicators switch on the first
# full month after launch and are cumulative (so coefficients are incremental).
ever <- panel_est %>% group_by(identity) %>% filter(any(ai_any == 1), n() >= 6) %>% ungroup() %>%
  mutate(
    sonnet45 = as.integer(month >= REL_SONNET45),
    opus45   = as.integer(month >= REL_OPUS45),
    gen46    = as.integer(month >= REL_GEN46),
    placebo  = as.integer(month >= as.Date("2025-08-01")),
    moy      = factor(format(month, "%m"))
  )
lad <- feols(log_n_prs ~ sonnet45 + opus45 + gen46 + log_cum_ai + post | identity, ever, cluster = ~identity)
pla <- feols(log_n_prs ~ placebo + log_cum_ai + post | identity, ever, cluster = ~identity)
ladm <- feols(log_n_prs ~ sonnet45 + opus45 + gen46 + log_cum_ai + moy | identity, ever, cluster = ~identity)
cat("\n[tab:ladder] (incremental release steps)\n")
cat(sprintf("  Sonnet4.5 %+0.3f | Opus4.5 %+0.3f | Gen4.6 %+0.3f | experience(cumAI) %+0.3f\n",
            coef(lad)["sonnet45"], coef(lad)["opus45"], coef(lad)["gen46"], coef(lad)["log_cum_ai"]))
cat(sprintf("  + month-of-year FE: Sonnet %+0.3f | Opus %+0.3f | Gen46 %+0.3f | exp %+0.3f\n",
            coef(ladm)["sonnet45"], coef(ladm)["opus45"], coef(ladm)["gen46"], coef(ladm)["log_cum_ai"]))
cat(sprintf("  PLACEBO (Aug 2025) %+0.3f (p=%.3g)  <- should match real-release magnitude\n",
            coef(pla)["placebo"], summary(pla)$coeftable["placebo", 4]))
for (v in c("sonnet45", "opus45", "gen46", "log_cum_ai", "post")) {
  k <- if (v == "log_cum_ai") "experience" else v
  record(paste0("ladder.", k), coef(lad)[v], pvalue(lad)[v])
  if (v != "post") record(paste0("ladder_moy.", k), coef(ladm)[v], pvalue(ladm)[v])
}
record("ladder.placebo", coef(pla)["placebo"], pvalue(pla)["placebo"])
record("ladder.placebo_p", pvalue(pla)["placebo"])

# The accumulated-use slope under alternative samples and measures: post-mandate
# months only, Claude Code adopters only, and experience measured as the lagged
# cumulative count of AI-labeled PRs instead of AI-written lines.
claude_ids <- panel %>% filter(month >= REL_SONNET45, claude_lines > 0) %>% distinct(identity) %>% pull(identity)
ever <- ever %>% arrange(identity, month) %>% group_by(identity) %>%
  mutate(log_cum_ai_prs = log1p(lag(cumsum(n_ai_prs), default = 0))) %>% ungroup()
lad_f <- log_n_prs ~ sonnet45 + opus45 + gen46 + log_cum_ai + post | identity
variants <- list(
  post_mandate = list(feols(lad_f, filter(ever, month >= as.Date("2025-07-01")), cluster = ~identity), "log_cum_ai"),
  claude_only  = list(feols(lad_f, filter(ever, identity %in% claude_ids), cluster = ~identity), "log_cum_ai"),
  pr_count     = list(feols(log_n_prs ~ sonnet45 + opus45 + gen46 + log_cum_ai_prs + post | identity, ever,
                            cluster = ~identity), "log_cum_ai_prs"))
cat("  accumulated-use slope under variants (should stay positive and significant):\n")
for (nm in names(variants)) {
  m <- variants[[nm]][[1]]; v <- variants[[nm]][[2]]
  cat(sprintf("    %-13s %+0.3f (p=%.3g)\n", nm, coef(m)[v], pvalue(m)[v]))
  record(paste0("ladder_variant.", nm, ".slope"), coef(m)[v], pvalue(m)[v]); record(paste0("ladder_variant.", nm, ".p"), pvalue(m)[v])
}
