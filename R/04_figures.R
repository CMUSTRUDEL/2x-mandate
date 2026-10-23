# =============================================================================
# All figures, generated from the canonical objects (data_layer.R) and the
# figure-data layer (figure_data.R) via the shared ggplot theme. Every figure is
# produced here and written to figures/*.pdf, so the figures and the in-text
# numbers come from one pipeline that reads the same input tables.
#
# The panel-derived figures (adoption, spend, adoption event study, capability)
# are drawn from the canonical developer panel -- the same source as every
# results table -- so figures and tables share one data layer.
# =============================================================================
if (!exists("panel_est")) source("R/data_layer.R")
if (!exists("fig_ov"))    source("R/figure_data.R")
source("R/theme.R")
suppressMessages({ library(tidyverse); library(patchwork); library(fixest) })
cat("\n################  FIGURES  ################\n")

# cowplot/patchwork measure text when they assemble grobs: patchwork on the open
# device, cowplot on its own null device (base pdf() by default, which does not
# know PAPER_FONT). Point both at PDF_DEVICE so those measurements match the
# font metrics of the saved PDFs; otherwise legend entries and labels collide.
PDF_DEVICE(tempfile(fileext = ".pdf"), width = 7, height = 7); metrics_dev <- dev.cur()
cowplot::set_null_device(function(width, height) {
  PDF_DEVICE(tempfile(fileext = ".pdf"), width = width, height = height)
  grDevices::dev.control("enable")
})

set.seed(1)
CUT <- PANEL_CUTOFF                       # 2026-04-01 (theme)

# Developer-panel frame for the panel-derived figures, from the canonical panel.
# Cohorts/first_ai are computed on each developer's full record before the
# >=3-active-month restriction (matches the estimation sample).
pf_all <- panel %>% filter(month <= CUT) %>%
  transmute(developer = identity, month, n_prs, n_ai_prs, ai_any,
            ai_cursor_lines = cursor_lines, ai_claude_lines = claude_lines,
            ai_claude_spend = claude_spend, ai_lines_generated = ai_lines,
            avg_review_comments)
first_ai <- pf_all %>% filter(ai_any == 1) %>% group_by(developer) %>%
  summarise(first_ai = min(month), .groups = "drop")
pf <- pf_all %>% left_join(first_ai, by = "developer") %>%
  mutate(cohort = factor(case_when(is.na(first_ai) ~ "never",
                                   first_ai < EV$mandate ~ "early",
                                   TRUE ~ "late"),
                         levels = c("early", "late", "never"))) %>%
  filter(!is.na(n_prs), n_prs > 0) %>%
  group_by(developer) %>% filter(n() >= 3) %>% ungroup() %>%
  mutate(log_n_prs = log(n_prs)) %>%
  arrange(developer, month) %>% group_by(developer) %>%
  mutate(cum_ai_lag = lag(cumsum(ai_lines_generated), default = 0)) %>% ungroup() %>%
  mutate(logcum = log1p(cum_ai_lag))

cohort_cols <- setNames(PAL[c("early", "late", "never")], c("early", "late", "never"))
cohort_labs <- c(early = "Adopted pre-mandate", late = "Adopted post-mandate", never = "Never adopted")

extract_es <- function(m, var) {
  ct <- coeftable(m); rn <- rownames(ct)
  idx <- grepl(paste0("^", var, "::"), rn)
  rt  <- as.numeric(gsub(paste0(var, "::(-?[0-9]+).*"), "\\1", rn[idx]))
  out <- rbind(data.frame(rel = rt, est = ct[idx, 1], se = ct[idx, 2]),
               data.frame(rel = -1, est = 0, se = 0))
  out$lo <- out$est - 1.96 * out$se; out$hi <- out$est + 1.96 * out$se
  out[order(out$rel), ]
}
rel_of <- function(m, d) as.integer(round(as.numeric(difftime(m, d, units = "days")) / 30.44))

# =============================================================================
# fig_identification -- pedagogical (simulated) calendar-vs-event-time panels.
# No data dependency (illustrative simulated developer timelines).
# =============================================================================
local({
  set.seed(42)
  devs <- tibble::tribble(
    ~id,    ~cohort,          ~adopt,        ~base,
    "Dev A","early (Cursor)", "2025-03-01",   17,
    "Dev B","mandate wave",   "2025-06-01",   12,
    "Dev C","mandate wave",   "2025-07-01",   21,
    "Dev D","mandate wave",   "2025-08-01",    9,
    "Dev E","late",           "2025-11-01",   14,
    "Dev F","never",           NA,            24) %>% mutate(adopt = as.Date(adopt))
  COH <- c("early (Cursor)" = "#0072B2", "mandate wave" = "#CC79A7",
           "late" = "#009E73", "never" = "#999999")
  coh_levels <- names(COH)
  col_scale <- scale_colour_manual(values = COH, limits = coh_levels, drop = FALSE,
                                   name = NULL, guide = guide_legend(nrow = 1, override.aes = list(shape = NA)))
  months <- seq(as.Date("2024-06-01"), PANEL_CUTOFF, by = "month")
  mnum <- function(d) as.integer(format(d, "%Y")) * 12L + as.integer(format(d, "%m"))
  panel_s <- tidyr::expand_grid(devs, date = months) %>%
    mutate(ms = ifelse(!is.na(adopt), mnum(date) - mnum(adopt), NA_integer_),
           adopted = !is.na(ms) & ms >= 0,
           mult = ifelse(adopted, 1 + 0.30 + 0.030 * pmin(ms, 14), 1),
           pr = base * mult * exp(rnorm(n(), 0, 0.05)),
           cohort = factor(cohort, levels = coh_levels))
  adopt_pts <- panel_s %>% filter(!is.na(ms) & ms == 0)
  base_lab <- theme_paper(base_size = 8) +
    theme(plot.subtitle = element_text(face = "bold", size = rel(1.0)),
          axis.title = element_text(size = rel(0.95)), legend.position = "none")
  # Open circles mark each developer's first AI use; they get their own legend
  # key via a constant shape aesthetic.
  adopt_marker <- function(d, ...) geom_point(data = d, aes(shape = "first AI use"), fill = "white",
                                              size = 1.6, stroke = 0.5, show.legend = TRUE, ...)
  shape_scale <- scale_shape_manual(values = c("first AI use" = 21), name = NULL,
                                    guide = guide_legend(override.aes = list(colour = INK, fill = "white")))
  pA <- ggplot(panel_s, aes(date, pr, colour = cohort, group = id)) +
    geom_vline(xintercept = EV$mandate, linetype = "dotted", colour = PAL["mandate"], linewidth = 0.4) +
    annotate("text", x = EV$mandate, y = Inf, label = "mandate", angle = 90,
             hjust = 1.05, vjust = -0.4, size = EVENT_LABEL_SIZE, colour = INK) +
    geom_line(linewidth = 0.5) + adopt_marker(adopt_pts) + col_scale + shape_scale +
    scale_x_date(date_breaks = "6 months", date_labels = "%b %y",
                 limits = c(min(months), max(months))) +
    labs(subtitle = "A. Calendar (wall-clock) time", x = NULL, y = "PRs / month") + base_lab
  pB_df <- panel_s %>% filter(!is.na(ms), ms >= -10, ms <= 12)
  pB <- ggplot(pB_df, aes(ms, pr, colour = cohort, group = id)) +
    geom_vline(xintercept = 0, linetype = "dashed", colour = "#555555", linewidth = 0.4) +
    annotate("text", x = 0, y = Inf, label = "adoption", angle = 90,
             hjust = 1.05, vjust = -0.4, size = EVENT_LABEL_SIZE, colour = INK) +
    geom_line(linewidth = 0.5) + adopt_marker(filter(pB_df, ms == 0)) + col_scale + shape_scale +
    scale_x_continuous(breaks = seq(-10, 10, 5)) +
    labs(subtitle = "B. Aligned on own adoption",
         x = "months relative to own adoption", y = NULL) + base_lab
  set.seed(123)
  mandate_mn <- mnum(EV$mandate); Npop <- 400
  pop <- data.frame(id = seq_len(Npop), base = runif(Npop, 8, 26),
                    off = pmax(-4, pmin(9, round(rnorm(Npop, 2, 4)))))
  pop_panel <- tidyr::expand_grid(pop, date = months) %>%
    mutate(mt = mnum(date) - mandate_mn, ms = mnum(date) - (mandate_mn + off),
           adopted = ms >= 0, mult = ifelse(adopted, 1 + 0.30 + 0.030 * pmin(ms, 14), 1),
           pr = base * mult * exp(rnorm(n(), 0, 0.15)))
  avg_curve <- function(df, rel, pre_hi) {
    df <- df %>% rename(rel = {{ rel }}) %>% group_by(rel) %>% summarise(pr = mean(pr), .groups = "drop")
    df$pr <- df$pr - mean(df$pr[df$rel <= pre_hi]); df
  }
  avgC <- bind_rows(
    avg_curve(filter(pop_panel, ms >= -10, ms <= 12), ms, -4) %>% mutate(align = "aligned on own adoption"),
    avg_curve(filter(pop_panel, mt >= -12, mt <= 10), mt, -6) %>% mutate(align = "aligned on the mandate"))
  ALN <- c("aligned on own adoption" = PAL[["experience"]], "aligned on the mandate" = PAL[["mandate"]])
  pC <- ggplot(avgC, aes(rel, pr, colour = align)) +
    geom_hline(yintercept = 0, colour = "#D0D0D0", linewidth = 0.3) +
    geom_vline(xintercept = 0, linetype = "dashed", colour = "#555555", linewidth = 0.4) +
    geom_line(linewidth = 0.7) + scale_colour_manual(values = ALN, name = NULL) +
    scale_x_continuous(breaks = seq(-10, 10, 5)) +
    labs(subtitle = "C. Same effect, averaged two ways",
         x = "months relative to alignment point", y = "mean PRs/mo\nvs pre-period") +
    base_lab +
    theme(legend.position = "inside", legend.position.inside = c(0.04, 0.96),
          legend.justification = c(0, 1), legend.key.size = unit(0.5, "lines"),
          legend.text = element_text(size = rel(0.85)),
          legend.background = element_rect(fill = "white", colour = NA))
  legend <- cowplot::get_plot_component(
    pA + theme(legend.position = "top", legend.justification = "center"), "guide-box-top")
  row <- cowplot::plot_grid(pA, pB, pC, nrow = 1, align = "h", axis = "tb")
  fig <- cowplot::plot_grid(legend, row, ncol = 1, rel_heights = c(0.1, 1))
  save_fig(fig, "identification", cols = 2, height = 1.65)
})

# =============================================================================
# fig_adoption -- monthly tool users by telemetry source + Claude spend inset,
# from the canonical developer panel.
# =============================================================================
local({
  START <- as.Date("2025-01-01"); END <- PANEL_CUTOFF; CURSOR_LAST <- as.Date("2026-02-01")
  adf <- panel %>%
    transmute(month, n_ai_prs, ai_cursor_lines = cursor_lines,
              ai_claude_lines = claude_lines, ai_claude_spend = claude_spend) %>%
    mutate(across(c(n_ai_prs, ai_cursor_lines, ai_claude_lines, ai_claude_spend), ~ replace_na(.x, 0))) %>%
    filter(month >= START, month <= END)
  users <- adf %>% group_by(month) %>% summarise(
    Cursor = sum(ai_cursor_lines > 0), `Claude Code` = sum(ai_claude_lines > 0),
    Other = sum(n_ai_prs > 0 & ai_cursor_lines == 0 & ai_claude_lines == 0), .groups = "drop") %>%
    pivot_longer(-month, names_to = "tool", values_to = "users") %>%
    mutate(tool = factor(tool, levels = c("Claude Code", "Cursor", "Other"))) %>%
    group_by(tool) %>% arrange(month) %>%
    mutate(users = ifelse(month < min(month[users > 0]), NA, users),
           users = ifelse(tool == "Cursor" & month > CURSOR_LAST, NA, users)) %>% ungroup()
  tool_cols <- c("Claude Code" = "#7C3AED", "Cursor" = "#95A5A6", "Other" = "#BDBDBD")
  tool_lty  <- c("Claude Code" = "solid", "Cursor" = "solid", "Other" = "dashed")
  ymax <- max(users$users, na.rm = TRUE)
  p_main <- ggplot(users, aes(month, users, colour = tool)) +
    event_lines(label = TRUE) +
    geom_line(aes(linetype = tool), linewidth = 0.5, na.rm = TRUE) +
    geom_point(data = filter(users, tool != "Other"), size = 0.7, na.rm = TRUE) +
    scale_colour_manual(values = tool_cols) +
    scale_linetype_manual(values = tool_lty, guide = "none") +
    scale_x_date(date_labels = "%b %y", date_breaks = "3 months", limits = c(START, END)) +
    coord_cartesian(clip = "off", ylim = c(0, ymax * 1.3)) +
    labs(x = NULL, y = "Developers using AI") + theme_paper() +
    theme(legend.box.spacing = unit(1, "pt"),
          plot.margin = margin(t = 0, r = 0, b = 0, l = 0))
  spend <- adf %>% group_by(month) %>% summarise(spend = sum(ai_claude_spend), .groups = "drop") %>%
    filter(spend > 0)
  p_inset <- ggplot(spend, aes(month, spend)) +
    geom_area(fill = unname(PAL["late"]), alpha = 0.18) +
    geom_line(colour = unname(PAL["late"]), linewidth = 0.45) +
    geom_point(colour = unname(PAL["late"]), size = 0.45) +
    scale_x_date(limits = c(min(spend$month), END), expand = expansion(0.02)) +
    scale_y_continuous(labels = label_number(prefix = "$", scale_cut = cut_short_scale()),
                       breaks = c(0, 2e5, 4e5)) +
    labs(x = NULL, y = NULL, subtitle = "Claude $/mo") +
    theme_paper(base_size = INSET_SIZE) +
    theme(panel.grid.major = element_line(linewidth = 0.15),
          plot.subtitle = element_text(size = rel(1.0), colour = INK, margin = margin(b = 1)),
          plot.background = element_rect(fill = "white", colour = "#CCCCCC", linewidth = 0.3),
          plot.margin = margin(2, 3, 1, 2),
          axis.text.x = element_blank(), axis.ticks.x = element_blank())
  p <- p_main + inset_element(p_inset, left = 0.02, bottom = 0.46, right = 0.30, top = 0.92,
                              align_to = "panel")
  save_fig(p, "adoption", cols = 1, height = 1.55)
})

# =============================================================================
# Overview subfigures (adoption / throughput / review / quality / latency /
# review-decomp). `d` is the estimation-
# sample monthly frame fig_ov; the throughput panel takes its per-developer line
# from the modelling sample and the gray volume from the full company-wide n_prs.
# =============================================================================
make_overview <- function(d, suffix = "", thru = NULL) {
  d <- d %>% mutate(month = as.Date(month)) %>%
    filter(month >= as.Date("2025-01-01"), month <= PANEL_CUTOFF)
  COL_HUMAN <- unname(PAL["early"]);   COL_AI  <- unname(PAL["heavy"])
  COL_SIL   <- unname(PAL["average"]); COL_CTX <- unname(PAL["mandate"])
  COL_VOL   <- unname(PAL["placebo"]); COL_LAT <- unname(PAL["experience"])
  xbreaks <- seq(as.Date("2025-01-01"), PANEL_CUTOFF, by = "3 months")
  xsc <- scale_x_date(date_labels = "%b %y", breaks = xbreaks,
                      limits = c(as.Date("2025-01-01"), as.Date("2026-05-01")),
                      expand = expansion(mult = c(0.02, 0)))
  xang <- theme()   # horizontal date labels: rotated ones cost ~0.2 in of height
  mar  <- theme(plot.margin = margin(t = 10, r = 6, b = 3, l = 3))
  ann  <- function(...) annotate("text", size = ANNO_SIZE, hjust = 1.1, ...)
  base <- d %>% filter(month <= as.Date("2025-04-01")) %>% summarise(across(where(is.numeric), mean))
  last <- tail(d, 1); lastm <- max(d$month); mx <- function(c) last[[c]] / base[[c]]
  dpp <- (last$share_human - base$share_human) * 100
  td <- if (is.null(thru)) d else thru %>% mutate(month = as.Date(month)) %>%
    filter(month >= as.Date("2025-01-01"), month <= PANEL_CUTOFF)
  base_t <- td %>% filter(month <= as.Date("2025-04-01")) %>% summarise(across(where(is.numeric), mean))
  last_t <- tail(td, 1); mxt <- function(c) last_t[[c]] / base_t[[c]]

  pAdopt <- ggplot(d, aes(month, ai_share)) +
    event_lines(label = TRUE) + geom_line(colour = COL_AI, linewidth = 0.5) +
    geom_point(colour = COL_AI, size = 0.6) +
    ann(x = lastm, y = last$ai_share, vjust = 1.6, label = sprintf("%.0f%%", last$ai_share * 100), colour = COL_AI) +
    scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1.15)) +
    coord_cartesian(clip = "off") + xsc + labs(x = NULL, y = "Share of AI PRs") +
    theme_paper() + xang + mar + theme(axis.title.y = element_text(colour = COL_AI))

  SCV <- max(td$prs_per_author) * 1.12 / max(td$n_prs)
  pThru <- ggplot(td, aes(month)) +
    geom_area(aes(y = n_prs * SCV), fill = COL_VOL, alpha = 0.45) + event_lines(label = TRUE) +
    geom_line(aes(y = prs_per_author), colour = COL_HUMAN, linewidth = 0.5) +
    geom_point(aes(y = prs_per_author), colour = COL_HUMAN, size = 0.6) +
    ann(x = lastm, y = last_t$prs_per_author, vjust = -0.9, label = sprintf("%.1f×", mxt("prs_per_author")), colour = COL_HUMAN) +
    ann(x = lastm, y = last_t$n_prs * SCV * 0.42, label = sprintf("vol\n%.1f×", mxt("n_prs")), lineheight = 0.85, colour = INK) +
    scale_y_continuous(limits = c(0, max(td$prs_per_author) * 1.15),
      sec.axis = sec_axis(~ . / SCV, name = "total PRs / month", labels = label_number(scale_cut = cut_short_scale()))) +
    coord_cartesian(clip = "off") + xsc + labs(x = NULL, y = "PRs / developer") +
    theme_paper() + xang + mar +
    theme(axis.title.y = element_text(colour = COL_HUMAN), axis.text.y = element_text(colour = COL_HUMAN),
          axis.title.y.right = element_text(colour = INK), axis.text.y.right = element_text(colour = INK))

  labs_r <- c(share_human = "any human", share_auto = "automated")
  dr <- d %>% select(month, share_human, share_auto) %>% pivot_longer(-month, names_to = "series", values_to = "val") %>%
    mutate(series = factor(labs_r[series], levels = labs_r))
  pRev <- ggplot(dr, aes(month, val, colour = series, shape = series)) +
    event_lines(label = FALSE) + geom_line(linewidth = 0.55) + geom_point(size = 0.85) +
    ann(x = lastm, y = last$share_human, vjust = 1.9, label = sprintf("%.0f%%", last$share_human * 100), colour = COL_HUMAN) +
    ann(x = lastm, y = last$share_auto, vjust = -0.7, label = sprintf("%.0f%%", last$share_auto * 100), colour = COL_AI) +
    scale_colour_manual(values = setNames(c(COL_HUMAN, COL_AI), labs_r)) +
    scale_shape_manual(values = setNames(c(16, 17), labs_r)) +
    scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
    coord_cartesian(clip = "off") + xsc + labs(x = NULL, y = "share of PRs") +
    theme_paper() + xang + mar + theme(legend.position = "top")

  labs_rd <- c(share_sub = "substantive", share_silent = "silent")
  drd <- d %>% select(month, share_sub, share_silent) %>% pivot_longer(-month, names_to = "series", values_to = "val") %>%
    mutate(series = factor(labs_rd[series], levels = labs_rd))
  pRevDecomp <- ggplot(drd, aes(month, val, colour = series, shape = series)) +
    event_lines(label = FALSE) + geom_line(linewidth = 0.55) + geom_point(size = 0.85) +
    ann(x = lastm, y = last$share_silent, vjust = 1.9, label = sprintf("%.0f%%", last$share_silent * 100), colour = COL_SIL) +
    ann(x = lastm, y = last$share_sub, vjust = 1.9, label = sprintf("%.0f%%", last$share_sub * 100), colour = COL_HUMAN) +
    scale_colour_manual(values = setNames(c(COL_HUMAN, COL_SIL), labs_rd)) +
    scale_shape_manual(values = setNames(c(16, 15), labs_rd)) +
    scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
    coord_cartesian(clip = "off") + xsc + labs(x = NULL, y = "share of PRs") +
    theme_paper() + xang + mar + theme(legend.position = "top")

  SCQ <- 0.05
  pQual <- ggplot(d, aes(month)) + event_lines(label = FALSE) +
    geom_line(aes(y = merge_rate), colour = COL_CTX, linewidth = 0.5) +
    geom_point(aes(y = merge_rate), colour = COL_CTX, size = 0.6) +
    geom_line(aes(y = revert_rate / SCQ), colour = COL_AI, linewidth = 0.5) +
    geom_point(aes(y = revert_rate / SCQ), colour = COL_AI, size = 0.6, shape = 15) +
    ann(x = lastm, y = last$merge_rate, vjust = 1.9, label = "merge", colour = COL_CTX) +
    ann(x = lastm, y = last$revert_rate / SCQ, vjust = 1.7, label = "revert", colour = COL_AI) +
    scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1.25), breaks = seq(0, 1, 0.25),
      sec.axis = sec_axis(~ . * SCQ, name = "revert rate", breaks = seq(0, 0.05, 0.01), labels = percent_format(accuracy = 1))) +
    coord_cartesian(clip = "off") + xsc + labs(x = NULL, y = "merge rate") +
    theme_paper() + xang + mar +
    theme(axis.title.y = element_text(colour = COL_CTX, margin = margin(r = 3)), axis.text.y = element_text(colour = COL_CTX),
          axis.title.y.right = element_text(colour = COL_AI, angle = -90, margin = margin(l = 3)),
          axis.text.y.right = element_text(colour = COL_AI))

  COV_LO <- 0.70; COV_HI <- 1.00; LAT_HI <- max(d$cycle_p90) * 1.18
  rescale_cov <- function(c) (c - COV_LO) / (COV_HI - COV_LO) * LAT_HI
  peak <- d[which.max(d$cycle_p90), ]
  pLat <- ggplot(d, aes(month)) + event_lines(label = FALSE) +
    geom_line(aes(y = cycle_p90), colour = COL_LAT, linewidth = 0.5) +
    geom_point(aes(y = cycle_p90), colour = COL_LAT, size = 0.6) +
    geom_line(aes(y = rescale_cov(share_human_merged)), colour = COL_HUMAN, linewidth = 0.5, linetype = "longdash") +
    geom_point(aes(y = rescale_cov(share_human_merged)), colour = COL_HUMAN, size = 0.6, shape = 15) +
    ann(x = peak$month, y = peak$cycle_p90, vjust = -0.7, label = sprintf("%.0fh", peak$cycle_p90), colour = COL_LAT) +
    ann(x = lastm, y = rescale_cov(last$share_human_merged), vjust = 1.8, label = sprintf("%.0f%%", last$share_human_merged * 100), colour = COL_HUMAN) +
    scale_y_continuous(limits = c(0, LAT_HI), name = "90th-pctile cycle time (h)",
      sec.axis = sec_axis(~ . / LAT_HI * (COV_HI - COV_LO) + COV_LO, name = "human-review coverage", labels = percent_format(accuracy = 1))) +
    coord_cartesian(clip = "off") + xsc + labs(x = NULL) + theme_paper() + xang + mar +
    theme(axis.title.y = element_text(colour = COL_LAT), axis.text.y = element_text(colour = COL_LAT),
          axis.title.y.right = element_text(colour = COL_HUMAN), axis.text.y.right = element_text(colour = COL_HUMAN))

  COV2_LO <- 0.00; COV2_HI <- 0.55; LAT2_HI <- max(d$cycle_p90_sub) * 1.50
  rescale_cov2 <- function(c) (c - COV2_LO) / (COV2_HI - COV2_LO) * LAT2_HI
  peak2 <- d[which.max(d$cycle_p90_sub), ]
  pLat2 <- ggplot(d, aes(month)) + event_lines(label = FALSE) +
    geom_line(aes(y = cycle_p90), colour = COL_CTX, linewidth = 0.45) +
    geom_point(aes(y = cycle_p90), colour = COL_CTX, size = 0.6) +
    geom_line(aes(y = cycle_p90_sub), colour = COL_LAT, linewidth = 0.5) +
    geom_point(aes(y = cycle_p90_sub), colour = COL_LAT, size = 0.6) +
    geom_line(aes(y = rescale_cov2(share_sub_merged)), colour = COL_HUMAN, linewidth = 0.5, linetype = "longdash") +
    geom_point(aes(y = rescale_cov2(share_sub_merged)), colour = COL_HUMAN, size = 0.6, shape = 15) +
    ann(x = peak2$month, y = peak2$cycle_p90_sub, vjust = -0.7, label = sprintf("substantive %.0fh", peak2$cycle_p90_sub), colour = COL_LAT) +
    ann(x = lastm, y = last$cycle_p90, vjust = 1.9, label = sprintf("all merged %.0fh", last$cycle_p90), colour = COL_CTX) +
    ann(x = lastm, y = rescale_cov2(last$share_sub_merged), vjust = -0.8, label = sprintf("%.0f%%", last$share_sub_merged * 100), colour = COL_HUMAN) +
    scale_y_continuous(limits = c(0, LAT2_HI), name = "90th-pctile cycle time (h)",
      sec.axis = sec_axis(~ . / LAT2_HI * (COV2_HI - COV2_LO) + COV2_LO, name = "substantive-review coverage", labels = percent_format(accuracy = 1))) +
    coord_cartesian(clip = "off") + xsc + labs(x = NULL) + theme_paper() + xang + mar +
    theme(axis.title.y = element_text(colour = COL_LAT), axis.text.y = element_text(colour = COL_LAT),
          axis.title.y.right = element_text(colour = COL_HUMAN), axis.text.y.right = element_text(colour = COL_HUMAN))

  aligned <- cowplot::align_plots(pAdopt, pThru, pRev, pQual, pLat, pLat2, pRevDecomp, align = "v", axis = "lr")
  save_fig(aligned[[1]], paste0("overview_adoption", suffix),   cols = 1, height = 1.12)
  save_fig(aligned[[2]], paste0("overview_throughput", suffix), cols = 1, height = 1.12)
  save_fig(aligned[[3]], paste0("overview_review", suffix),     cols = 1, height = 1.17)
  save_fig(aligned[[4]], paste0("overview_quality", suffix),    cols = 1, height = 1.17)
  save_fig(aligned[[6]], paste0("overview_latency_decomp", suffix), cols = 1, height = H_DUAL)
  save_fig(aligned[[7]], paste0("overview_review_decomp", suffix), cols = 1, height = H_STD)
  save_fig(aligned[[5]], paste0("overview_latency", suffix),    cols = 1, height = H_DUAL)
}

# Throughput: per-developer line from the estimation sample (fig_ov), gray volume
# from the full company-wide n_prs (fig_ov_full).
mdl_thru <- fig_ov %>% select(month, prs_per_author) %>%
  inner_join(fig_ov_full %>% select(month, n_prs), by = "month")
make_overview(fig_ov, "", thru = mdl_thru)

# fig_overview_spend -- per-capita AI token spend (Claude only, Jul 2025 on).
local({
  sp <- panel %>% transmute(developer = identity, month, n_prs,
                            ai_claude_spend = replace_na(claude_spend, 0)) %>%
    filter(month >= as.Date("2025-07-01"), month <= PANEL_CUTOFF, !is.na(n_prs), n_prs > 0) %>%
    group_by(month) %>% summarise(spend_dev = sum(ai_claude_spend) / n_distinct(developer), .groups = "drop")
  sp_lastm <- max(sp$month); sp_mx <- tail(sp$spend_dev, 1) / sp$spend_dev[1]
  xbreaks <- seq(as.Date("2025-01-01"), PANEL_CUTOFF, by = "3 months")
  xsc <- scale_x_date(date_labels = "%b %y", breaks = xbreaks,
                      limits = c(as.Date("2025-01-01"), as.Date("2026-05-01")), expand = expansion(mult = c(0.02, 0)))
  pSpend <- ggplot(sp, aes(month, spend_dev)) + event_lines(label = TRUE) +
    geom_line(colour = unname(PAL["late"]), linewidth = 0.5) +
    geom_point(colour = unname(PAL["late"]), size = 0.6) +
    annotate("text", size = ANNO_SIZE, hjust = 1.1, x = sp_lastm, y = tail(sp$spend_dev, 1),
             vjust = 1.5, label = sprintf("%.0f×", sp_mx), colour = unname(PAL["late"])) +
    scale_y_continuous(labels = dollar_format(accuracy = 1)) + coord_cartesian(clip = "off") +
    xsc + labs(x = NULL, y = "AI spend / developer") + theme_paper() +
    theme(plot.margin = margin(t = 10, r = 6, b = 3, l = 3),
          axis.title.y = element_text(colour = unname(PAL["late"])))
  save_fig(pSpend, "overview_spend", cols = 1, height = H_TS)
})

# =============================================================================
# fig_adoption_eventstudy (+ pretrends) -- dose-response event study on the
# canonical developer panel `pf`.
# =============================================================================
local({
  es <- pf %>% mutate(adopted = as.integer(ai_any == 1), post = as.integer(month >= EV$mandate)) %>%
    group_by(developer) %>% filter(n() >= 3) %>% ungroup()
  es_t <- es %>% filter(!is.na(first_ai)) %>% mutate(rel = rel_of(month, first_ai)) %>% filter(rel >= -6, rel <= 6)
  mA <- feols(log_n_prs ~ i(rel, ref = -1) + post | developer, es_t, cluster = ~developer)
  f5a <- extract_es(mA, "rel")
  p5a <- ggplot(f5a, aes(rel, est)) +
    geom_hline(yintercept = 0, colour = "#888888", linewidth = 0.3) +
    geom_vline(xintercept = -0.5, linetype = "dashed", colour = "#C0392B", linewidth = 0.35) +
    geom_ribbon(aes(ymin = lo, ymax = hi), fill = PAL[["heavy"]], alpha = 0.15) +
    geom_line(colour = PAL[["heavy"]], linewidth = 0.5) + geom_point(colour = PAL[["heavy"]], size = 0.8) +
    scale_x_continuous(breaks = seq(-6, 6, 2)) +
    labs(x = "Months relative to first AI tool use", y = "Effect on log(PRs)") + theme_paper()
  tier <- es %>% filter(!is.na(first_ai)) %>% group_by(developer) %>%
    summarise(avg_ai = mean(log1p(ai_lines_generated), na.rm = TRUE), .groups = "drop")
  q <- quantile(tier$avg_ai[tier$avg_ai > 0], c(.25, .75), na.rm = TRUE)
  tier <- tier %>% mutate(g = case_when(avg_ai >= q[2] ~ "Heavy", avg_ai <= q[1] ~ "Light", TRUE ~ "Average"))
  f5b <- map_dfr(c("Heavy", "Average", "Light"), function(gg) {
    d <- es_t %>% inner_join(tier %>% filter(g == gg) %>% select(developer), by = "developer")
    extract_es(feols(log_n_prs ~ i(rel, ref = -1) + post | developer, d, cluster = ~developer), "rel") %>% mutate(tier = gg)
  }) %>% mutate(tier = factor(tier, levels = c("Heavy", "Average", "Light")))
  p5b <- ggplot(f5b, aes(rel, est, colour = tier, fill = tier)) +
    geom_hline(yintercept = 0, colour = "#888888", linewidth = 0.3) +
    geom_vline(xintercept = -0.5, linetype = "dashed", colour = "#C0392B", linewidth = 0.35) +
    geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.10, colour = NA) +
    geom_line(linewidth = 0.5) + geom_point(size = 0.8) +
    scale_colour_manual(values = setNames(PAL[c("heavy", "average", "light")], c("Heavy", "Average", "Light"))) +
    scale_fill_manual(values = setNames(PAL[c("heavy", "average", "light")], c("Heavy", "Average", "Light"))) +
    scale_x_continuous(breaks = seq(-6, 6, 2)) +
    labs(x = "Months relative to first AI tool use", y = "Effect on log(PRs)") + theme_paper()
  save_fig(p5b, "adoption_eventstudy", cols = 1, height = 1.30)
  save_fig(p5a, "adoption_eventstudy_pretrends", cols = 1, height = H_TS)
})

# =============================================================================
# fig_capability_experience -- capability event studies + decomposition.
# =============================================================================
local({
  dec <- pf %>% mutate(post_mandate = as.integer(month >= EV$mandate),
                       sonnet45 = as.integer(month >= EV$sonnet45),
                       opus45   = as.integer(month >= EV$opus45),
                       gen46    = as.integer(month >= EV$gen46),
                       rel_s = rel_of(month, EV$sonnet45), rel_o = rel_of(month, EV$opus45),
                       rel_4 = rel_of(month, EV$gen46), rel_n = rel_of(month, as.Date("2025-08-01"))) %>%
    filter(cohort %in% c("early", "late")) %>% group_by(developer) %>% filter(n() >= 6) %>% ungroup()
  run_capes <- function(var, hmin, hmax) {
    d <- dec[!is.na(dec[[var]]) & dec[[var]] >= hmin & dec[[var]] <= hmax, ]
    feols(as.formula(paste0("log_n_prs ~ i(", var, ", ref=-1) + logcum + post_mandate | developer")), d, cluster = ~developer)
  }
  f6a <- bind_rows(
    extract_es(run_capes("rel_s", -6, 6), "rel_s") %>% mutate(spec = "Sonnet 4.5 (Oct '25)"),
    extract_es(run_capes("rel_o", -6, 4), "rel_o") %>% mutate(spec = "Opus 4.5 (Dec '25)"),
    extract_es(run_capes("rel_4", -6, 2), "rel_4") %>% mutate(spec = "4.6 gen (Feb '26)"),
    extract_es(run_capes("rel_n", -6, 8), "rel_n") %>% mutate(spec = "Placebo (Aug '25)")
  ) %>% mutate(spec = factor(spec, levels = c("Sonnet 4.5 (Oct '25)", "Opus 4.5 (Dec '25)", "4.6 gen (Feb '26)", "Placebo (Aug '25)")))
  cap_cols <- setNames(PAL[c("sonnet45", "opus45", "gen46", "placebo")], levels(f6a$spec))
  p6a <- ggplot(f6a, aes(rel, est, colour = spec, fill = spec)) +
    geom_hline(yintercept = 0, colour = "#888888", linewidth = 0.3) +
    geom_vline(xintercept = -0.5, linetype = "dashed", colour = "#C0392B", linewidth = 0.35) +
    geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.08, colour = NA) +
    geom_line(linewidth = 0.5) + geom_point(size = 0.7) +
    scale_colour_manual(values = cap_cols) + scale_fill_manual(values = cap_cols) +
    scale_x_continuous(breaks = seq(-6, 8, 2)) +
    labs(x = "Months relative to release", y = "Effect on log(PRs)",
         subtitle = "A. Capability event studies: sign-flipping + failing placebo") +
    guides(colour = guide_legend(nrow = 2), fill = guide_legend(nrow = 2)) + theme_paper()
  ml <- feols(log_n_prs ~ sonnet45 + opus45 + gen46 + logcum + post_mandate | developer, dec, cluster = ~developer)
  co <- coef(ml); cap_net <- unname(co["sonnet45"] + co["opus45"] + co["gen46"])
  delta <- pf %>% filter(cohort %in% c("early", "late"), month %in% c(as.Date("2025-05-01"), PANEL_CUTOFF)) %>%
    group_by(cohort, month) %>% summarise(m = mean(logcum, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = month, values_from = m)
  delta$d <- delta[[as.character(PANEL_CUTOFF)]] - delta[["2025-05-01"]]
  f6b <- map_dfr(seq_len(nrow(delta)), function(i) {
    tibble(cohort = as.character(delta$cohort[i]),
           channel = c("Mandate", "Experience", "Capability (not identified)"),
           share = c(unname(co["post_mandate"]), unname(co["logcum"]) * delta$d[i], cap_net))
  }) %>% mutate(cohort = factor(cohort, levels = c("early", "late"), labels = c("Early", "Late")),
               channel = factor(channel, levels = c("Mandate", "Experience", "Capability (not identified)")))
  decomp_cols <- c("Mandate" = PAL[["mandate"]], "Experience" = PAL[["experience"]], "Capability (not identified)" = "#D9D9D9")
  p6b <- ggplot(f6b, aes(cohort, share, fill = channel)) +
    geom_col(width = 0.65, colour = "white", linewidth = 0.2) +
    geom_hline(yintercept = 0, colour = "#888888", linewidth = 0.3) +
    scale_fill_manual(values = decomp_cols) +
    labs(x = "Adoption cohort", y = "Predicted Δ log(PRs)",
         subtitle = "B. Decomposition (capability shown but not identified)") +
    guides(fill = guide_legend(nrow = 1)) + theme_paper()
  save_fig(p6a | p6b, "capability_experience", cols = 2, height = H_WIDE)
})

# =============================================================================
# fig_review_reallocation (+ review_decomp_mean) -- ported from
# the canonical developer panel and the reviews-given decomposition.
# =============================================================================
local({
  event_date <- as.Date("2025-06-01")
  df <- panel %>% transmute(developer = identity, month, n_prs, n_ai_prs, ai_any) %>%
    mutate(across(c(n_ai_prs, ai_any), ~ replace_na(.x, 0)),
           adopted = as.integer(ai_any == 1), post = as.integer(month >= event_date)) %>%
    filter(!is.na(n_prs), n_prs > 0, month <= PANEL_CUTOFF) %>%
    group_by(developer) %>% filter(n() >= 3) %>% ungroup()
  rg <- fig_rev_given %>% mutate(month = as.Date(month))
  df <- df %>% left_join(rg, by = c("developer", "month")) %>%
    mutate(across(c(reviews_given, reviews_loose, approvals, bare_approvals), ~ replace_na(.x, 0)),
           silent_share = ifelse(approvals >= 3, bare_approvals / approvals, NA_real_))
  COL_SUB <- unname(PAL["early"]); COL_LITE <- unname(PAL["average"])
  xbk <- seq(as.Date("2024-01-01"), PANEL_CUTOFF, by = "6 months")
  xsc <- scale_x_date(date_labels = "%b %y", breaks = xbk,
                      limits = c(as.Date("2024-01-01"), as.Date("2026-05-01")), expand = expansion(mult = c(0.02, 0)))
  xang <- theme()
  labs_m <- c(commented = "commented (substantive)", silent = "silent (approve only)")
  decA <- rg %>% group_by(month) %>%
    summarise(commented = median(reviews_given), silent = median(bare_approvals), .groups = "drop") %>%
    filter(month <= PANEL_CUTOFF) %>% pivot_longer(-month, names_to = "series", values_to = "val") %>%
    mutate(series = factor(labs_m[series], levels = labs_m))
  lastm <- max(decA$month); end_lab <- decA %>% filter(month == lastm)
  pMain <- ggplot(decA, aes(month, val, colour = series, shape = series)) +
    event_lines(label = FALSE) + geom_line(linewidth = 0.55) + geom_point(size = 0.7) +
    annotate("text", x = lastm, y = end_lab$val[end_lab$series == labs_m["silent"]],
             label = sprintf("%.0f", end_lab$val[end_lab$series == labs_m["silent"]]),
             colour = COL_LITE, size = ANNO_SIZE, hjust = 1.1, vjust = -0.8) +
    annotate("text", x = lastm, y = end_lab$val[end_lab$series == labs_m["commented"]],
             label = sprintf("%.0f", end_lab$val[end_lab$series == labs_m["commented"]]),
             colour = COL_SUB, size = ANNO_SIZE, hjust = 1.1, vjust = 1.9) +
    scale_colour_manual(values = setNames(c(COL_SUB, COL_LITE), labs_m)) +
    scale_shape_manual(values = setNames(c(16, 15), labs_m)) +
    scale_y_continuous(limits = c(0, 24), breaks = seq(0, 16, 4), expand = expansion(mult = c(0, 0.02))) +
    coord_cartesian(clip = "off") + xsc + labs(x = NULL, y = "median reviews /\nreviewer / month") +
    theme_paper() + xang + theme(legend.position = "top", plot.margin = margin(t = 8, r = 6, b = 3, l = 3))
  first_ai_r <- df %>% filter(adopted == 1) %>% group_by(developer) %>% summarise(first_ai = min(month), .groups = "drop")
  es <- df %>% inner_join(first_ai_r, by = "developer") %>%
    mutate(rel = (lubridate::year(month) - lubridate::year(first_ai)) * 12 + (lubridate::month(month) - lubridate::month(first_ai)),
           rel = pmax(pmin(rel, 9), -6), log_loose = log1p(reviews_loose), log_sub = log1p(reviews_given))
  extract_esr <- function(y, lab, data = es) {
    m <- feols(as.formula(paste0(y, " ~ i(rel, ref = -1) + post | developer")), data, cluster = ~developer)
    ct <- broom::tidy(m) %>% filter(grepl("^rel::", term)) %>%
      transmute(rel = as.integer(sub("rel::", "", term)), est = estimate,
                lo = estimate - 1.96 * std.error, hi = estimate + 1.96 * std.error, series = lab)
    bind_rows(ct, tibble(rel = -1, est = 0, lo = 0, hi = 0, series = lab))
  }
  labs_es <- c(all = "all reviews", sub = "substantive")
  esc <- bind_rows(extract_esr("log_loose", labs_es["all"]), extract_esr("log_sub", labs_es["sub"])) %>%
    mutate(series = factor(series, levels = labs_es)) %>% arrange(series, rel)
  espal <- setNames(c(COL_LITE, COL_SUB), labs_es)
  pInset <- ggplot(esc, aes(rel, est, colour = series, fill = series)) +
    geom_hline(yintercept = 0, colour = "#999999", linewidth = 0.2) +
    geom_vline(xintercept = 0, linetype = "dotted", colour = PAL["mandate"], linewidth = 0.3) +
    geom_ribbon(aes(ymin = lo, ymax = hi), colour = NA, alpha = 0.10) +
    geom_line(linewidth = 0.4) + geom_point(size = 0.45) +
    scale_colour_manual(values = espal) + scale_fill_manual(values = espal) +
    scale_x_continuous(breaks = c(-6, 0, 6)) + scale_y_continuous(expand = expansion(mult = c(0.05, 0.45))) +
    labs(x = "months since adoption", y = NULL) +
    guides(fill = "none", colour = guide_legend(override.aes = list(linewidth = 0.8))) +
    theme_paper(base_size = INSET_SIZE) +
    theme(legend.position = c(0.02, 0.98), legend.justification = c(0, 1), legend.direction = "horizontal",
          legend.key.size = unit(0.35, "lines"), legend.key.spacing.y = unit(0, "pt"),
          legend.background = element_rect(fill = "white", colour = NA), legend.text = element_text(size = rel(1.0)),
          axis.title.x = element_text(size = rel(1.0)),
          plot.background = element_rect(fill = "white", colour = "grey75", linewidth = 0.3),
          plot.margin = margin(2, 3, 2, 2))
  p <- pMain + patchwork::inset_element(pInset, left = 0.08, bottom = 0.52, right = 0.62, top = 0.99)
  save_fig(p, "review_reallocation", cols = 1, height = 1.75)

  decA_mean <- rg %>% group_by(month) %>%
    summarise(`Commented (substantive)` = mean(reviews_given), `Silent (approve only)` = mean(bare_approvals), .groups = "drop") %>%
    filter(month <= PANEL_CUTOFF) %>% pivot_longer(-month, names_to = "series", values_to = "val")
  pAm <- ggplot(decA_mean, aes(month, val, colour = series)) +
    event_lines(label = FALSE) + geom_line(linewidth = 0.5) + geom_point(size = 0.7) +
    scale_colour_manual(values = c("Commented (substantive)" = unname(PAL["opus45"]), "Silent (approve only)" = unname(PAL["sonnet45"]))) +
    scale_x_date(date_labels = "%b %y") + labs(x = NULL, y = "Mean reviews / reviewer / month") + theme_paper()
  save_fig(pAm, "review_decomp_mean", cols = 1, height = H_STD)
})

# =============================================================================
# fig_review_latency -- per-PR effect of AI authorship across DORA phases (human boundary).
# from the human-boundary PR timing frame fig_pr_human.
# =============================================================================
local({
  pr <- fig_pr_human %>% mutate(month = as.Date(month), ym = factor(month),
    lpr_size = log1p(pr_size), lfiles = log1p(files_changed),
    l_coding = log1p(coding_lead_time_hours), l_comments = log1p(comment_count),
    l_pickup = log1p(pickup_human), l_reviewlead = log1p(lead_human),
    l_cycle = log1p(total_cycle_time_hours))
  merged_reviewed <- pr %>% filter(state == "MERGED", has_human_review == 1)
  post <- merged_reviewed %>% filter(post == 1)
  fe <- "is_ai + lpr_size + lfiles | dev + ym"
  log_outcomes <- c(`Coding lead` = "l_coding", Comments = "l_comments", Pickup = "l_pickup",
                    `Review lead` = "l_reviewlead", `Total cycle` = "l_cycle")
  kind_of <- c(`Coding lead` = "Authoring", Comments = "Commentary", Pickup = "Latency",
               `Review lead` = "Latency", `Total cycle` = "Latency")
  order <- c("Total cycle", "Review lead", "Pickup", "Comments", "Coding lead")
  prem <- imap_dfr(log_outcomes, function(col, lab) {
    m <- feols(as.formula(paste(col, "~", fe)), post, cluster = ~dev)
    ct <- coeftable(m)["is_ai", ]
    tibble(outcome = lab, pct = 100 * (exp(ct[1]) - 1), p = ct[4])
  }) %>% mutate(outcome = factor(outcome, levels = order),
               kind = factor(kind_of[as.character(outcome)], levels = c("Authoring", "Commentary", "Latency")),
               lbl = sprintf("%+.0f%%%s", pct, ifelse(p >= 0.05, " (n.s.)", "")), lblx = pmax(pct, 0))
  p <- ggplot(prem, aes(pct, outcome, fill = kind)) +
    geom_vline(xintercept = 0, colour = "#888888", linewidth = 0.3) +
    geom_col(width = 0.66, colour = "white", linewidth = 0.15) +
    geom_text(aes(x = lblx, label = lbl), hjust = -0.12, size = ANNO_SIZE, colour = "#333333") +
    scale_fill_manual(values = c("Authoring" = PAL[["never"]], "Commentary" = PAL[["mandate"]], "Latency" = PAL[["experience"]])) +
    scale_x_continuous(expand = expansion(mult = c(0.10, 0.16))) +
    labs(x = "Effect of AI authorship (%), post-mandate", y = NULL) + theme_paper() +
    theme(panel.grid.major.y = element_blank())
  save_fig(p, "review_latency", cols = 1, height = H_STD)
})

cat("wrote all figures to output/figures/\n")
dev.off(metrics_dev)
