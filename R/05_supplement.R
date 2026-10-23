# =============================================================================
# Supplement: is the throughput gain real work or a response to the target?
#   (A) PR splitting: code volume and PR shape per developer-month
#   (B) working time: active days, weekend and off-hours activity, daily span
#   (C) PR types: fix / feature / maintenance / revert / other from PR titles
#   (D) title-style check: are AI-labeled titles just easier to classify?
#   (E) follow-up fixes after AI-authored PRs (same author, same repository)
# Same within-developer designs as RQ1: Eq. (1) "adoption", its calendar-month FE
# variant "+month FE", and the decomposition evaluated at the Apr-2026 usage level.
# Sources R/data_layer.R.
# =============================================================================
if (!exists("panel_est")) source("R/data_layer.R")
suppressMessages(library(fixest))
cat("\n################  SUPPLEMENT  ################\n")

# Time zone in which dates, weekends, and hours are taken (the organization's
# home time zone). Set HOME_TZ to an Olson name, e.g. "America/New_York".
HOME_TZ <- Sys.getenv("HOME_TZ", "UTC")

# A few developer-months can carry two tool-seat rows; keep one row per dev-month.
pe_s <- panel_est %>% distinct(identity, month, .keep_all = TRUE)

star <- function(m, v) { p <- pvalue(m)[v]; ifelse(p < .001, "***", ifelse(p < .01, "**", ifelse(p < .05, "*", ifelse(p < .1, "+", "")))) }
ee <- pe_s %>% filter(month == END_MONTH, ai_any == 1) %>% summarise(m = mean(log_cum_ai)) %>% pull(m)
base_mean <- function(d, v) mean(d[[v]][d$month %in% BASE_MONTHS], na.rm = TRUE)
end_mean  <- function(d, v) mean(d[[v]][d$month == END_MONTH], na.rm = TRUE)

# One row per outcome: descriptive base -> Apr 2026, then the three designs.
# fam = "pois" for counts (coefficients read as log points), "ols" otherwise.
report <- function(d, v, label, fam = "ols", key = NULL) {
  fit <- function(f) if (fam == "pois") fepois(f, d, cluster = ~identity) else feols(f, d, cluster = ~identity)
  a <- fit(as.formula(sprintf("%s ~ adopted + post | identity", v)))
  b <- fit(as.formula(sprintf("%s ~ adopted | identity + month", v)))
  c <- fit(as.formula(sprintf("%s ~ adopted + log_cum_ai + post | identity", v)))
  tot <- coef(c)["adopted"] + coef(c)["log_cum_ai"] * ee + coef(c)["post"]
  cat(sprintf("  %-30s %9.2f -> %9.2f | %+7.3f%-3s | %+7.3f%-3s | %+7.3f (slope %+0.3f%s)\n",
              label, base_mean(d, v), end_mean(d, v),
              coef(a)["adopted"], star(a, "adopted"), coef(b)["adopted"], star(b, "adopted"),
              tot, coef(c)["log_cum_ai"], star(c, "log_cum_ai")))
  if (!is.null(key)) {
    record(paste0(key, ".base"), base_mean(d, v)); record(paste0(key, ".end"), end_mean(d, v))
    record(paste0(key, ".adopted"), coef(a)["adopted"]); record(paste0(key, ".month_fe"), coef(b)["adopted"])
    record(paste0(key, ".at_apr26"), tot)
  }
}
hdr <- function(t) {
  cat(sprintf("\n[%s]\n  %-30s %9s    %9s | %10s | %10s | %s\n", t, "outcome", "base", "Apr-26",
              "(A) adopt", "(B) +monFE", "(C) at Apr-26 usage"))
}

## ---- (A) tab:splitting -- code volume and PR shape ---------------------------
# Per-PR size and files winsorized at the 99th percentile: a handful of vendored or
# generated-file PRs otherwise dominate monthly sums.
Q_SIZE  <- quantile(prs$size, .99)
Q_FILES <- quantile(replace_na(prs$files_changed, 0L), .99)
pr_dm <- prs %>%
  mutate(size = pmin(size, Q_SIZE), files = pmin(replace_na(files_changed, 0L), Q_FILES),
         commits_n = replace_na(commit_count, 0L)) %>%
  group_by(identity, month) %>%
  summarise(lines = sum(size), files_tot = sum(files), commits_tot = sum(commits_n),
            med_size = median(size), med_files = median(files),
            small10 = mean(size <= 10), small50 = mean(size <= 50),
            big500 = mean(size >= 500), .groups = "drop")
pa <- pe_s %>% select(identity, month, adopted, post, log_cum_ai, ai_any, n_prs) %>%
  left_join(pr_dm, by = c("identity", "month")) %>%
  mutate(l_med_size = log1p(med_size), l_med_files = log1p(med_files),
         lines_per_pr = lines / n_prs, l_lines_per_pr = log1p(lines_per_pr),
         files_per_pr = files_tot / n_prs, l_files_per_pr = log1p(files_per_pr),
         l_commits_per_pr = log1p(commits_tot / n_prs))

hdr("tab:splitting -- monthly volume per developer (Poisson, log points)")
report(pa, "n_prs",       "PRs",               "pois", "split.prs")
report(pa, "lines",       "Lines changed",     "pois", "split.lines")
report(pa, "files_tot",   "Files changed",     "pois", "split.files")
report(pa, "commits_tot", "Commits",           "pois", "split.commits")
hdr("tab:splitting -- shape of a developer's PRs that month (OLS)")
report(pa, "l_lines_per_pr",   "log(1+lines per PR)", key = "split.lines_per_pr")
report(pa, "l_files_per_pr",   "log(1+files per PR)", key = "split.files_per_pr")
report(pa, "l_commits_per_pr", "log(1+commits per PR)", key = "split.commits_per_pr")
report(pa, "small10",          "Share PRs <=10 lines", key = "split.small10")
report(pa, "big500",           "Share PRs >=500 lines", key = "split.big500")
report(pa, "l_med_size",       "log(1+median PR lines)")
report(pa, "small50",          "Share PRs <=50 lines")

# Pooled PR-size distribution, baseline vs Apr 2026, among estimation-sample devs.
cat("\n[splitting] PR size quantiles, estimation-sample developers (median: 35 -> 61 lines)\n")
qs <- c(.1, .25, .5, .75, .9)
dist <- prs %>% filter(identity %in% est_ids) %>%
  mutate(w = case_when(month %in% BASE_MONTHS ~ "base Jan-Apr 2025", month == END_MONTH ~ "Apr 2026")) %>%
  filter(!is.na(w)) %>% group_by(w) %>%
  summarise(n = n(), lines = list(quantile(size, qs)), files = list(quantile(replace_na(files_changed, 0), qs)),
            .groups = "drop")
record("split.median_size_base", dist$lines[[which(dist$w == "base Jan-Apr 2025")]][3])
record("split.median_size_end", dist$lines[[which(dist$w == "Apr 2026")]][3])
for (i in seq_len(nrow(dist))) {
  cat(sprintf("  %-18s n=%6d  lines p10/25/50/75/90: %s | files: %s\n", dist$w[i], dist$n[i],
              paste(round(dist$lines[[i]]), collapse = "/"), paste(round(dist$files[[i]]), collapse = "/")))
}

## ---- (B) tab:worktime -- working time ----------------------------------------
# Activity events by the developer: PR opened, commit authored, review/comment
# written. Dates and weekends are taken in HOME_TZ; off-hours is measured against
# each developer's OWN typical hour in 2024, which does not depend on time zone.
ev <- bind_rows(
  prs %>% transmute(identity, ts = as_datetime(created_at), src = "pr"),
  commits %>% transmute(identity, ts = as_datetime(committed_at), src = "commit"),
  reviews %>% filter(human) %>% transmute(identity, ts = comment_time, src = "review")
) %>%
  filter(identity %in% est_ids, !is.na(ts)) %>%
  mutate(lt = with_tz(ts, HOME_TZ), day = as.Date(lt, tz = HOME_TZ), month = mfloor(day),
         hr = lubridate::hour(lt) + lubridate::minute(lt) / 60, wkend = lubridate::wday(lt, week_start = 1) >= 6) %>%
  filter(month >= WIN_START, month <= WIN_END)

# Each developer's reference hour: circular mean of their 2024 weekday activity.
ref <- ev %>% filter(format(month, "%Y") == "2024", !wkend) %>% group_by(identity) %>%
  summarise(ref_hr = (atan2(mean(sin(2 * pi * hr / 24)), mean(cos(2 * pi * hr / 24))) * 24 / (2 * pi)) %% 24,
            n_ref = n(), .groups = "drop") %>% filter(n_ref >= 50)
ev <- ev %>% left_join(ref, by = "identity") %>%
  mutate(dist = abs(((hr - ref_hr + 12) %% 24) - 12), offhrs = dist > 5)

wt <- function(e) {
  daily <- e %>% group_by(identity, day, month) %>%
    summarise(span = as.numeric(difftime(max(ts), min(ts), units = "hours")), n = n(), .groups = "drop")
  e %>% group_by(identity, month) %>%
    summarise(wkend_share = mean(wkend), off_share = mean(offhrs), .groups = "drop") %>%
    left_join(daily %>% group_by(identity, month) %>%
                summarise(active_days = n(), wkend_days = sum(lubridate::wday(day, week_start = 1) >= 6),
                          span = mean(span[n >= 2]), .groups = "drop"),
              by = c("identity", "month"))
}
mk <- function(w) pe_s %>% select(identity, month, adopted, post, log_cum_ai, ai_any) %>%
  inner_join(w, by = c("identity", "month"))

# All events, then PRs opened + reviews written only (excludes commits, which
# agents can author) as the robustness check reported in the text.
sets <- list("all events (PRs, commits, reviews)" = wt(ev),
             "PRs opened + reviews written only" = wt(filter(ev, src %in% c("pr", "review"))))
for (nm in names(sets)) {
  d <- mk(sets[[nm]])
  k <- if (nm == names(sets)[1]) "work" else "work_pr_review"
  hdr(paste("tab:worktime --", nm))
  report(d, "active_days", "Active days / month", "pois", paste0(k, ".active_days"))
  report(d, "wkend_days",  "Weekend days / month", "pois", paste0(k, ".weekend_days"))
  report(filter(d, !is.na(off_share)), "off_share", "Off-hours share (>5h from own)", key = paste0(k, ".off_share"))
  report(filter(d, !is.na(span)), "span", "Daily span, hours", key = paste0(k, ".span"))
}

# PRs per active day, holding days fixed: Poisson PRs with log(active days) offset.
d <- mk(sets[[1]]) %>% left_join(pa %>% distinct(identity, month, n_prs), by = c("identity", "month"))
hdr("tab:worktime -- PRs / month and PRs per active day (offset)")
report(d, "n_prs", "PRs / month", "pois", "work.prs")
off <- function(f) fepois(f, d, offset = ~log(active_days), cluster = ~identity)
a <- off(n_prs ~ adopted + post | identity)
b <- off(n_prs ~ adopted | identity + month)
c <- off(n_prs ~ adopted + log_cum_ai + post | identity)
tot <- coef(c)["adopted"] + coef(c)["log_cum_ai"] * ee + coef(c)["post"]
cat(sprintf("  %-30s %9.2f -> %9.2f | %+7.3f%-3s | %+7.3f%-3s | %+7.3f (slope %+0.3f%s)\n",
            "PRs / active day (offset)", base_mean(mutate(d, r = n_prs / active_days), "r"),
            end_mean(mutate(d, r = n_prs / active_days), "r"),
            coef(a)["adopted"], star(a, "adopted"), coef(b)["adopted"], star(b, "adopted"),
            tot, coef(c)["log_cum_ai"], star(c, "log_cum_ai")))
record("work.prs_per_day.base", base_mean(mutate(d, r = n_prs / active_days), "r"))
record("work.prs_per_day.end", end_mean(mutate(d, r = n_prs / active_days), "r"))
record("work.prs_per_day.adopted", coef(a)["adopted"]); record("work.prs_per_day.month_fe", coef(b)["adopted"])
record("work.prs_per_day.at_apr26", tot)

## ---- (C) tab:prtypes -- PR-type classifier ----------------------------------
strip_ticket <- function(t) {
  t %>% str_replace("^\\s*\\[[^\\]]*\\]\\s*", "") %>%          # [ABC-123] ...
    str_replace("^\\s*#?[a-z]*-?\\d{3,}\\s*[-:]\\s*", "") %>%    # 123456 - ... / ABC-12: ...
    str_squish()
}
CONV <- "^\\[?(feat|feature|fix|bugfix|hotfix|bug|chore|refactor|docs?|tests?|ci|build|perf|style|deps|revert)\\]?(\\([^)]*\\))?!?\\s*[:\\-]"
conv_map <- function(k) case_when(
  k %in% c("feat", "feature") ~ "feature",
  k %in% c("fix", "bugfix", "hotfix", "bug") ~ "fix",
  k == "revert" ~ "revert",
  !is.na(k) ~ "maint",
  TRUE ~ NA_character_)
KW_FIX   <- "\\b(fix(es|ed|ing)?|bugs?|bugfix|hotfix|regression|crash(es|ing)?|broken|prevent(s|ing)?)\\b"
KW_MAINT <- paste0("\\b(chore|bump(s|ed)?|upgrade(s|d)?|dependenc(y|ies)|deps|refactor(s|ed|ing)?|clean ?up|",
                   "remove(s|d)?|delete(s|d)?|deprecate(s|d)?|rename(s|d)?|docs?|documentation|readme|lint(ing)?|",
                   "rubocop|eslint|typos?|tests?|specs?|flaky|ci|logging|metrics|monitoring|terraform)\\b|",
                   "\\bupdate\\b.*\\bto\\s+v?\\d")
KW_FEAT  <- paste0("\\b(feat|features?|add(s|ed|ing)?|implement(s|ed|ing)?|introduce(s|d)?|support(s|ing)?|new|",
                   "enable(s|d)?|allow(s)?|create(s|d)?|build(s)?|launch(es)?|expose(s)?|extend(s)?|render(s)?|",
                   "show(s)?|display(s)?)\\b")

classify <- function(title, head_ref, is_revert) {
  t  <- str_to_lower(replace_na(title, ""))
  ts <- strip_ticket(t)
  ck <- str_match(ts, CONV)[, 2]
  br <- str_to_lower(replace_na(head_ref, ""))
  bk <- str_match(br, "(?:^|/)(feat|feature|fix|bugfix|hotfix|bug|chore|refactor|docs?)[-_/]")[, 2]
  cat <- case_when(
    replace_na(is_revert, FALSE) | str_detect(ts, "^revert\\b") ~ "revert",
    !is.na(ck) ~ conv_map(ck),
    str_detect(ts, KW_FIX)   ~ "fix",
    !is.na(bk) ~ conv_map(bk),
    str_detect(ts, KW_MAINT) ~ "maint",
    str_detect(ts, KW_FEAT)  ~ "feature",
    TRUE ~ "other")
  tibble(cat = cat, classified = as.integer(cat != "other"))
}

# Machine-generated PRs opened from human accounts (e.g. translation sync,
# batched infrastructure changes) are not developer work: is_generated == TRUE.
prs_c <- prs %>%
  filter(!replace_na(is_generated, FALSE)) %>%
  bind_cols(classify(.$title, .$head_ref, .$is_revert)) %>%
  mutate(cat = factor(cat, levels = c("feature", "fix", "maint", "revert", "other")))
cat(sprintf("\n[prtypes] %d PRs after excluding %d generated PRs; type shares (other ~33%%):\n",
            nrow(prs_c), nrow(prs) - nrow(prs_c)))
print(round(prop.table(table(prs_c$cat)), 3))
record("types.other_share_pct", 100 * mean(prs_c$cat == "other"))

cnt <- prs_c %>% count(identity, month, cat) %>%
  pivot_wider(names_from = cat, values_from = n, values_fill = 0, names_prefix = "n_")
pc <- pe_s %>% select(identity, month, adopted, post, log_cum_ai, ai_any) %>%
  left_join(cnt, by = c("identity", "month")) %>%
  mutate(across(starts_with("n_"), ~ replace_na(.x, 0L)),
         n_tot = n_feature + n_fix + n_maint + n_revert + n_other) %>%
  filter(n_tot > 0)
cats <- c("All PRs" = "n_tot", Feature = "n_feature", "Bug fix" = "n_fix", Maintenance = "n_maint",
          Revert = "n_revert", Other = "n_other")
ee_c <- pc %>% filter(month == END_MONTH, ai_any == 1) %>% summarise(m = mean(log_cum_ai)) %>% pull(m)
cat("\n[tab:prtypes] Poisson, developer FE; base | adoption | +month FE | at Apr-2026 usage\n")
for (nm in names(cats)) {
  v <- cats[[nm]]
  a <- fepois(as.formula(sprintf("%s ~ adopted + post | identity", v)), pc, cluster = ~identity)
  b <- fepois(as.formula(sprintf("%s ~ adopted | identity + month", v)), pc, cluster = ~identity)
  d <- fepois(as.formula(sprintf("%s ~ adopted + log_cum_ai + post | identity", v)), pc, cluster = ~identity)
  tot <- coef(d)["adopted"] + coef(d)["log_cum_ai"] * ee_c + coef(d)["post"]
  cat(sprintf("  %-12s %5.1f | %+0.2f%-3s | %+0.2f%-3s | %+0.2f (%.1fx)\n", nm,
              mean(pc[[v]][pc$month %in% BASE_MONTHS]),
              coef(a)["adopted"], star(a, "adopted"), coef(b)["adopted"], star(b, "adopted"),
              tot, exp(tot)))
  k <- paste0("types.", gsub("[^a-z]+", "_", tolower(nm)))
  record(paste0(k, ".base"), mean(pc[[v]][pc$month %in% BASE_MONTHS]))
  record(paste0(k, ".adopted"), coef(a)["adopted"]); record(paste0(k, ".month_fe"), coef(b)["adopted"])
  record(paste0(k, ".at_apr26"), tot)
}

## ---- (D) title-style check --------------------------------------------------
pr_s <- prs_c %>% mutate(AI = as.integer(ai), lsize = log1p(size),
                         lfiles = log1p(replace_na(files_changed, 0)),
                         is_fix = as.integer(cat == "fix"), is_feat = as.integer(cat == "feature"))
post_s <- filter(pr_s, month >= MANDATE)
ols_ai <- function(v, d) feols(as.formula(sprintf("%s ~ AI + lsize + lfiles | identity + month", v)), d, cluster = ~identity)
m_cl <- ols_ai("classified", post_s)
cat(sprintf("\n[title style] post-mandate, author + month FE: classifiable %.0f%% (non-AI) vs %.0f%% (AI); AI coef %+0.3f%s\n",
            100 * mean(post_s$classified[post_s$AI == 0]), 100 * mean(post_s$classified[post_s$AI == 1]),
            coef(m_cl)["AI"], star(m_cl, "AI")))
record("style.classified_nonai_pct", 100 * mean(post_s$classified[post_s$AI == 0]))
record("style.classified_ai_pct", 100 * mean(post_s$classified[post_s$AI == 1]))
record("style.classified_ai_pp", 100 * coef(m_cl)["AI"])
post_c <- filter(post_s, classified == 1)
m_fx <- ols_ai("is_fix", post_c)
cat(sprintf("              among classifiable titles: P(fix) AI coef %+0.3f%s\n", coef(m_fx)["AI"], star(m_fx, "AI")))
record("style.fix_given_classified_pp", 100 * coef(m_fx)["AI"])
drift <- pr_s %>% filter(AI == 0, format(month, "%m") <= "06", format(month, "%Y") %in% c("2024", "2026")) %>%
  group_by(half = paste0(format(month, "%Y"), "H1")) %>%
  summarise(fix = mean(is_fix), feature = mean(is_feat), .groups = "drop")
cat("              non-AI-labeled PRs, type mix by half-year (fix 15% both; feature 25% -> 27%):\n")
print(as.data.frame(drift), digits = 3, row.names = FALSE)
for (i in seq_len(nrow(drift))) {
  record(paste0("style.nonai_fix_pct_", drift$half[i]), 100 * drift$fix[i])
  record(paste0("style.nonai_feature_pct_", drift$half[i]), 100 * drift$feature[i])
}

## ---- (E) app:followup -- follow-up fixes after a merged PR -------------------
# No file paths, so we link on (author, repository): does a fix PR by the same
# author in the same repository open within K days after a PR merges? Feature
# follow-ups are a placebo outcome that defect-driven rework should not move.
followups <- function(K) {
  base <- pr_s %>% filter(merged, !is.na(merged_at)) %>%
    transmute(repository, number, identity, month, AI, lsize, lfiles,
              t0 = as_datetime(merged_at))
  nxt <- pr_s %>% transmute(repository, identity, n2 = number, t1 = as_datetime(created_at), cat)
  j <- base %>% inner_join(nxt, by = c("repository", "identity"), relationship = "many-to-many") %>%
    filter(t1 > t0, t1 <= t0 + days(K), n2 != number) %>%
    group_by(repository, number) %>%
    summarise(f_fix = sum(cat == "fix"), f_feat = sum(cat == "feature"), f_all = n(), .groups = "drop")
  base %>% left_join(j, by = c("repository", "number")) %>%
    mutate(across(c(f_fix, f_feat, f_all), ~ replace_na(.x, 0L)),
           any_fix = as.integer(f_fix > 0), any_feat = as.integer(f_feat > 0),
           l_other = log1p(f_all - f_fix))
}
cat("\n[app:followup] merged post-mandate PRs; author + month FE, size controls\n")
for (K in c(7, 14)) {
  fu <- followups(K) %>% filter(month >= MANDATE)
  mf <- feols(any_fix ~ AI + l_other + lsize + lfiles | identity + month, fu, cluster = ~identity)
  mg <- feols(any_feat ~ AI + log1p(f_all - f_feat) + lsize + lfiles | identity + month, fu, cluster = ~identity)
  # Stacked test: is the AI coefficient on fix follow-ups larger than on the placebo?
  st <- bind_rows(fu %>% transmute(identity, month, AI, lsize, lfiles, y = any_fix, fixo = 1L, lo = l_other),
                  fu %>% transmute(identity, month, AI, lsize, lfiles, y = any_feat, fixo = 0L, lo = log1p(f_all - f_feat)))
  ms <- feols(y ~ AI * fixo + lo + lsize + lfiles | identity^fixo + month^fixo, st, cluster = ~identity)
  cat(sprintf("  K=%2dd  fix: base %.1f%%, AI %+0.1fpp | feature (placebo): base %.1f%%, AI %+0.1fpp | fix - placebo %+0.1fpp (p=%.2f)\n",
              K, 100 * mean(fu$any_fix[fu$AI == 0]), 100 * coef(mf)["AI"],
              100 * mean(fu$any_feat[fu$AI == 0]), 100 * coef(mg)["AI"],
              100 * coef(ms)["AI:fixo"], pvalue(ms)["AI:fixo"]))
  k <- paste0("followup.k", K)
  record(paste0(k, ".fix_base_pct"), 100 * mean(fu$any_fix[fu$AI == 0])); record(paste0(k, ".fix_pp"), 100 * coef(mf)["AI"])
  record(paste0(k, ".feat_base_pct"), 100 * mean(fu$any_feat[fu$AI == 0])); record(paste0(k, ".feat_pp"), 100 * coef(mg)["AI"])
  record(paste0(k, ".diff_pp"), 100 * coef(ms)["AI:fixo"]); record(paste0(k, ".diff_p"), pvalue(ms)["AI:fixo"])
}
