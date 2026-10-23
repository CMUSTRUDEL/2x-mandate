# =============================================================================
# RQ1: throughput, the within-developer DiD, decomposition, estimator robustness,
# dose-response, denominator and adoption-signal robustness.
# Sources R/data_layer.R.
# =============================================================================
if (!exists("panel_est")) source("R/data_layer.R")
suppressMessages({ library(fixest); library(did); library(didimputation) })

cat("\n################  RQ1  ################\n")

st <- function(p) ifelse(p < .001, "***", ifelse(p < .01, "**", ifelse(p < .05, "*", ifelse(p < .1, "+", ""))))
cc <- function(m, v = "adopted") sprintf("%+.3f%s", coef(m)[v], st(pvalue(m)[v]))

pe <- panel_est

## ---- (0) Descriptive throughput ratios --------------------------------------
ratio <- function(df, col) {
  m <- df %>% group_by(month) %>% summarise(v = mean(.data[[col]]), .groups = "drop")
  b <- m %>% filter(month %in% BASE_MONTHS) %>% summarise(x = mean(v)) %>% pull(x)
  e <- m %>% filter(month == END_MONTH) %>% pull(v)
  c(base = b, end = e, ratio = e / b)
}
r <- ratio(pe, "n_prs")
cat(sprintf("[throughput] per-active PRs %.1f -> %.1f = %.2fx\n", r["base"], r["end"], r["ratio"]))
record("rq1.prs_per_active_base", r["base"]); record("rq1.prs_per_active_end", r["end"])

# Developers active in both the Jan-Apr 2025 baseline and Apr 2026 ("stayers").
stay <- pe %>% group_by(identity) %>%
  filter(any(month %in% BASE_MONTHS), any(month == END_MONTH)) %>%
  summarise(b = mean(n_prs[month %in% BASE_MONTHS]), e = n_prs[month == END_MONTH][1], .groups = "drop")
cat(sprintf("[stayers] %d developers active throughout: %.1fx\n", nrow(stay), mean(stay$e) / mean(stay$b)))
record("rq1.stayers_n", nrow(stay)); record("rq1.stayers_ratio", mean(stay$e) / mean(stay$b))

# Claude Code spend per active PR author (all panel developers, as in the spend
# figure), Jul 2025 (telemetry start) -> Apr 2026.
sp <- panel %>% filter(n_prs > 0) %>% group_by(month) %>%
  summarise(spend = mean(claude_spend), prs = mean(n_prs), .groups = "drop")
s0 <- filter(sp, month == as.Date("2025-07-01")); s1 <- filter(sp, month == END_MONTH)
cat(sprintf("[spend] $%.0f -> $%.0f per developer-month (%.0fx); PRs/dev %.1fx over the same months\n",
            s0$spend, s1$spend, s1$spend / s0$spend, s1$prs / s0$prs))
record("rq1.spend_jul25", s0$spend); record("rq1.spend_apr26", s1$spend)
record("rq1.prs_ratio_jul25_apr26", s1$prs / s0$prs)

# Balance table: early (pre-mandate) adopters vs never-adopters, standardized
# mean differences of per-developer activity among developers with a role title.
onset <- panel %>% filter(ai_any == 1) %>% group_by(identity) %>% summarise(g = min(month), .groups = "drop")
bal <- pe %>% inner_join(roles %>% filter(!is.na(tier)) %>% distinct(identity), by = "identity") %>%
  left_join(onset, by = "identity") %>% group_by(identity) %>%
  summarise(n_months = n(), total_prs = sum(n_prs), mean_monthly_prs = mean(n_prs),
            cohort = ifelse(is.na(first(g)), "never", ifelse(first(g) < MANDATE, "early", "late")),
            .groups = "drop")
smd <- function(x1, x2) (mean(x1) - mean(x2)) / sqrt((sd(x1)^2 + sd(x2)^2) / 2)
for (v in c("n_months", "total_prs", "mean_monthly_prs")) {
  s <- smd(bal[[v]][bal$cohort == "early"], bal[[v]][bal$cohort == "never"])
  cat(sprintf("[balance] early vs never adopters, SMD of %-16s %.2f\n", v, s))
  record(paste0("rq1.balance_smd.", v), s)
}

## ---- (1) tab:did : developer-month DiD, six outcomes -------------------------
pe <- pe %>% mutate(
  l_size = log1p(avg_pr_size),
  l_rtime = log1p(replace_na(avg_review_time, 0)),
  l_rcom = log1p(avg_review_comments),
  merge_rate = n_merged / n_prs,
  revert_rate = n_reverted / n_prs,
  log_n_merged = ifelse(n_merged > 0, log(n_merged), NA_real_)
)
did_outcomes <- c(
  "log(Monthly PRs)"       = "log_n_prs",
  "log(1+Avg PR Size)"     = "l_size",
  "log(1+Avg Review Time)" = "l_rtime",
  "log(1+Avg Review Comm)" = "l_rcom",
  "Merge Rate"             = "merge_rate",
  "Revert Rate"            = "revert_rate"
)
cat("\n[tab:did]  outcome  |  AI Adopted  |  Post Mandate\n")
for (nm in names(did_outcomes)) {
  v <- did_outcomes[[nm]]
  m <- feols(as.formula(sprintf("%s ~ adopted + post | identity", v)), pe, cluster = ~identity)
  cat(sprintf("  %-24s %-10s  %s\n", nm, cc(m), cc(m, "post")))
  record(paste0("did.", v, ".adopted"), coef(m)["adopted"], pvalue(m)["adopted"])
  record(paste0("did.", v, ".post"), coef(m)["post"], pvalue(m)["post"])
}
mm <- feols(log_n_merged ~ adopted + post | identity, pe, cluster = ~identity)
cat(sprintf("  merged-PR outcome: log(Monthly merged PRs) adopted %s\n", cc(mm)))
record("rq1.merged_outcome_adopted", coef(mm)["adopted"], pvalue(mm)["adopted"])

## ---- (2) tab:decomp : adoption jump + accumulating use ----------------------
ee <- pe %>% filter(month == END_MONTH, ai_any == 1) %>% summarise(m = mean(log_cum_ai)) %>% pull(m)
dec <- feols(log_n_prs ~ adopted + log_cum_ai + post | identity, pe, cluster = ~identity)
b1 <- coef(dec)["adopted"]; bb <- coef(dec)["log_cum_ai"]; th <- coef(dec)["post"]
cat(sprintf("\n[tab:decomp] jump %s (contrib %+0.2f) | slope %s (contrib %+0.2f) | post %s\n",
            cc(dec), b1, cc(dec, "log_cum_ai"), bb * ee, cc(dec, "post")))
cat(sprintf("             total = %+0.2f  (%.2fx)\n", b1 + bb * ee + th, exp(b1 + bb * ee + th)))
record("decomp.jump", b1, pvalue(dec)["adopted"]); record("decomp.slope", bb, pvalue(dec)["log_cum_ai"])
record("decomp.post", th, pvalue(dec)["post"])
record("decomp.slope_contrib", bb * ee); record("decomp.total", b1 + bb * ee + th)

# Preferred model: calendar-month fixed effects instead of the Post term.
dm <- feols(log_n_prs ~ adopted + log_cum_ai | identity + factor(month), pe, cluster = ~identity)
tot <- coef(dm)["adopted"] + coef(dm)["log_cum_ai"] * ee
cat(sprintf("             month FE: jump %s | slope %s | total %+.2f (%.2fx)\n",
            cc(dm), cc(dm, "log_cum_ai"), tot, exp(tot)))
record("decomp_mfe.jump", coef(dm)["adopted"], pvalue(dm)["adopted"])
record("decomp_mfe.slope", coef(dm)["log_cum_ai"], pvalue(dm)["log_cum_ai"])
record("decomp_mfe.total", tot); record("decomp_mfe.multiplier", exp(tot))

## ---- (3) tab:robust : five estimators ---------------------------------------
# integer period + adoption cohort for the staggered estimators
t0 <- min(panel$month)
per <- function(d) as.integer((as.integer(format(d, "%Y")) - as.integer(format(t0, "%Y"))) * 12 +
                              (as.integer(format(d, "%m")) - as.integer(format(t0, "%m"))))
pe <- pe %>% left_join(onset, by = "identity") %>%
  mutate(t = as.integer(per(month)), gi = ifelse(is.na(g), 0L, as.integer(per(g))),
         idn = as.integer(factor(identity)))

tw <- feols(log_n_prs ~ adopted + post | identity, pe, cluster = ~identity)
mf <- feols(log_n_prs ~ adopted | identity + factor(month), pe, cluster = ~identity)
r1 <- feols(log_n_prs ~ adopted + post | identity, filter(pe, gi > 0), cluster = ~identity)
# Callaway-Sant'Anna: base_period = "universal" is the configuration that runs on
# this unbalanced panel (the "varying" base period errors out).
cs <- tryCatch({
  a <- att_gt(yname = "log_n_prs", tname = "t", idname = "idn", gname = "gi",
              data = pe, control_group = "notyettreated", base_period = "universal",
              allow_unbalanced_panel = TRUE, anticipation = 0, bstrap = FALSE, cband = FALSE)
  s <- suppressWarnings(aggte(a, type = "simple", na.rm = TRUE))
  c(att = s$overall.att, p = 2 * pnorm(-abs(s$overall.att / s$overall.se)))
}, error = function(e) { cat("  [C&S error]", conditionMessage(e), "\n"); c(att = NA, p = NA) })
bk <- tryCatch({
  bi <- did_imputation(data = pe, yname = "log_n_prs", gname = "gi", tname = "t", idname = "idn")
  c(att = bi$estimate[1], p = 2 * pnorm(-abs(bi$estimate[1] / bi$std.error[1])))
}, error = function(e) { cat("  [Borusyak error]", conditionMessage(e), "\n"); c(att = NA, p = NA) })
cat(sprintf("\n[tab:robust] TWFE %s | +MonthFE %s | Drop never %s | C&S %+.3f%s | Borusyak %+.3f%s\n",
            cc(tw), cc(mf), cc(r1), cs["att"], st(cs["p"]), bk["att"], st(bk["p"])))
record("robust.twfe", coef(tw)["adopted"], pvalue(tw)["adopted"])
record("robust.month_fe", coef(mf)["adopted"], pvalue(mf)["adopted"])
record("robust.drop_never", coef(r1)["adopted"], pvalue(r1)["adopted"])
record("robust.cs", cs["att"], cs["p"]); record("robust.borusyak", bk["att"], bk["p"])

## ---- (4) Event study: adoption effect at +3/+6/+9 months --------------------
es <- pe %>% filter(gi > 0) %>% mutate(rel = t - gi) %>% filter(rel >= -6, rel <= 9)
mes <- feols(log_n_prs ~ i(rel, ref = -1) + post | identity, es, cluster = ~identity)
for (h in c(3, 6, 9)) {
  k <- paste0("rel::", h)
  cat(sprintf("[event study] h=+%d  %.2fx\n", h, exp(coef(mes)[k])))
  record(paste0("rq1.event_h", h), exp(coef(mes)[k]), pvalue(mes)[k])
}

## ---- (5) Denominator robustness (per engineer, zeros incl.) -----------------
ten <- pe %>% group_by(identity) %>% summarise(f = min(month), l = max(month), .groups = "drop")
grid <- ten %>% rowwise() %>% do(tibble(identity = .$identity, month = seq(.$f, .$l, by = "month"))) %>% ungroup()
eng <- grid %>% left_join(panel %>% select(identity, month, n_prs, n_merged, ai_any), by = c("identity", "month")) %>%
  mutate(across(c(n_prs, n_merged, ai_any), ~ replace_na(.x, 0)), adopted = ai_any, mf = factor(month))
pz <- fepois(n_prs ~ adopted | identity + mf, eng, cluster = ~identity)
cat(sprintf("\n[denominator] per-engineer authored %.2fx | merged %.2fx | %.1f%% zero rows | Poisson jump %s\n",
            ratio(eng, "n_prs")["ratio"], ratio(eng, "n_merged")["ratio"], 100 * mean(eng$n_prs == 0), cc(pz)))
record("denom.authored_ratio", ratio(eng, "n_prs")["ratio"]); record("denom.merged_ratio", ratio(eng, "n_merged")["ratio"])
record("denom.zero_pct", 100 * mean(eng$n_prs == 0)); record("denom.poisson_jump", coef(pz)["adopted"], pvalue(pz)["adopted"])

## ---- (6) Adoption-signal robustness (app:prsignal) --------------------------
# Broaden adoption to telemetry OR a created-by-ai PR that month.
pe2 <- pe %>% mutate(adopted_b = as.integer(ai_any == 1 | n_ai_prs > 0))
mb <- feols(log_n_prs ~ adopted_b + post | identity, pe2, cluster = ~identity)
dev_sig <- pe2 %>% group_by(identity) %>%
  summarise(tel = any(ai_any == 1), lab = any(n_ai_prs > 0),
            first_tel = suppressWarnings(min(month[ai_any == 1])),
            first_lab = suppressWarnings(min(month[n_ai_prs > 0])), .groups = "drop")
n_reclass <- sum(!dev_sig$tel & dev_sig$lab)
n_earlier <- sum(dev_sig$tel & dev_sig$lab & dev_sig$first_lab < dev_sig$first_tel)
cat(sprintf("\n[app:prsignal] telemetry-only %s -> telemetry|AI-label %s; %d never-adopters reclassified, %d adoption dates earlier\n",
            cc(tw), cc(mb, "adopted_b"), n_reclass, n_earlier))
record("prsignal.adopted", coef(mb)["adopted_b"], pvalue(mb)["adopted_b"])
record("prsignal.post", coef(mb)["post"], pvalue(mb)["post"])
record("prsignal.reclassified", n_reclass); record("prsignal.earlier", n_earlier)
