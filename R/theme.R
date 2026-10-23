# Shared publication theme, palette, and helpers for the figures. Sourced by
# 04_figures.R so every figure uses one consistent visual system.
#
# Sizing contract (IEEEtran two-column layout):
#   * Figures are SAVED at the exact IEEEtran target width so LaTeX applies no
#     scaling (\includegraphics[width=\columnwidth|\textwidth] => scale 1.0).
#       - 1 column: 3.487 in (252 pt)   - 2 columns: 7.14 in (516 pt)
#   * In-figure text renders at ~7-8 pt: at or just below the 8 pt IEEE caption
#     font, and clearly smaller than the 10 pt body. base_size = 8 delivers this
#     at scale 1.0; do not scale figures in LaTeX.
#   * Heights are kept tight so no single float dominates a page.

suppressMessages({
  library(ggplot2)
  library(scales)
})

# ── Exact IEEEtran target widths, in inches (1 in = 72.27 TeX pt) ──────────────
COL_W  <- 252 / 72.27   # 3.487 in  (\columnwidth)
TEXT_W <- 516 / 72.27   # 7.140 in  (\textwidth)

# ── Unified typography ─────────────────────────────────────────────────────────
# One sans-serif font for every figure: Open Sans (SIL Open Font License, free
# from Google Fonts). The PDF device (below) embeds (subsets) the face, so the
# PDFs are self-contained. On a host without Open Sans installed, swap to "sans" -- the
# only line to change -- and expect some labels to shift. base_size = 8 pt keeps
# in-figure text at/below the 8 pt IEEE caption.
PAPER_FONT <- "Open Sans"
INSET_SIZE <- 6      # base_size for small inset panels (vs 8 for main panels)
INK        <- "#1A1A1A"  # near-black for ALL in-figure text/labels (readability:
                         # never set annotation text in gray)
# In-figure text (geom_text / annotate) sizes, in ggplot size units:
ANNO_SIZE        <- 2.1   # end-of-line value labels and callouts
EVENT_LABEL_SIZE <- 1.8   # rotated event-rule / reference-line labels

# ── Unified, space-saving figure heights (inches) ──────────────────────────────
# Four tokens replace the ad-hoc per-figure heights; ~15% tighter than before so
# no float dominates a page. Pick by structure, not by figure.
H_TS    <- 1.55   # 1-col single-panel time series / bars, no legend
H_STD   <- 1.80   # 1-col with a top legend or inset
H_DUAL  <- 1.95   # 1-col dual-axis or two-series panel
H_WIDE  <- 2.15   # 2-col single row of side-by-side panels
H_STACK <- 3.00   # 2-col with vertically stacked panels

# ── Fixed semantic palette (colorblind-safe; reused across every figure) ───────
PAL <- c(
  mandate    = "#7F8C8D",  # mandate / organization-wide
  experience = "#16A085",  # developer experience channel
  sonnet45   = "#D35400",  # capability: Sonnet 4.5
  opus45     = "#2980B9",  # capability: Opus 4.5
  gen46      = "#27AE60",  # capability: 4.6 generation
  placebo    = "#B0B0B0",  # null placebo
  early      = "#2C3E50",  # cohort: pre-mandate adopters
  late       = "#16A085",  # cohort: post-mandate adopters
  never      = "#BDC3C7",  # cohort: never-adopters
  heavy      = "#54278F",  # usage intensity (ordered)
  average    = "#807DBA",
  light      = "#BCBDDC"
)

# ── Key calendar events (UTC month starts) ─────────────────────────────────────
EV <- list(
  mandate   = as.Date("2025-06-01"),  # CTO "2x" mandate
  sonnet45  = as.Date("2025-10-01"),  # Sonnet 4.5 first full month
  opus45    = as.Date("2025-12-01"),  # Opus 4.5 first full month
  gen46     = as.Date("2026-02-01")   # 4.6 generation (Feb 2026)
)
PANEL_CUTOFF <- as.Date("2026-04-01")  # drop the partial May 2026 month

# ── Publication theme ──────────────────────────────────────────────────────────
# base_size = 8 (pt) so that, at scale 1.0, axis text ~= 7 pt, axis titles and
# panel labels ~= 8 pt -- at or just below the IEEE caption font. base_family
# defaults to the shared sans-serif PAPER_FONT so every figure uses one face.
theme_paper <- function(base_size = 8, base_family = PAPER_FONT) {
  theme_minimal(base_size = base_size, base_family = base_family) %+replace%
    theme(
      panel.background  = element_rect(fill = "white", colour = NA),
      plot.background   = element_rect(fill = "white", colour = NA),
      panel.grid.major  = element_line(colour = "#E8E8E8", linewidth = 0.22),
      panel.grid.minor  = element_blank(),
      axis.line         = element_line(colour = "#444444", linewidth = 0.25),
      axis.ticks        = element_line(colour = "#444444", linewidth = 0.25),
      axis.ticks.length = unit(2, "pt"),
      axis.text         = element_text(colour = "#333333", size = rel(0.9)),
      axis.title        = element_text(colour = "#222222", size = rel(1.0)),
      strip.text        = element_text(face = "bold", size = rel(0.95),
                                       margin = margin(2, 2, 2, 2)),
      strip.background  = element_rect(fill = "#F2F2F2", colour = NA),
      legend.position   = "top",
      legend.title      = element_blank(),
      legend.key.size   = unit(0.5, "lines"),
      legend.key.spacing.x = unit(7, "pt"),   # gap between legend entries
      legend.text       = element_text(size = rel(0.9), margin = margin(l = 2)),
      legend.margin     = margin(0, 0, 0, 0),
      legend.box.spacing = unit(2, "pt"),
      plot.title        = element_blank(),  # the LaTeX \caption carries the title
      plot.subtitle     = element_text(size = rel(1.0), colour = "#333333", hjust = 0,
                                       margin = margin(b = 3)),
      plot.margin       = margin(2, 4, 2, 2),
      panel.spacing     = unit(0.6, "lines")
    )
}

# theme_*() controls axis/legend/strip text, but in-figure geom_text() and
# annotate("text"/"label") read their family/size from the geom defaults, not the
# theme. Set those once here so every annotation in every figure uses PAPER_FONT too.
update_geom_defaults("text",  list(family = PAPER_FONT))
update_geom_defaults("label", list(family = PAPER_FONT))

# Vertical rules for the mandate + the three model releases, with a one-row label
# strip at the top. Add to any time-series plot with `+ event_lines()`.
event_lines <- function(label = TRUE, y = Inf) {
  layers <- list(
    geom_vline(xintercept = EV$mandate,  linetype = "dotted", colour = PAL["mandate"],  linewidth = 0.35),
    geom_vline(xintercept = EV$sonnet45, linetype = "dashed", colour = PAL["sonnet45"], linewidth = 0.35),
    geom_vline(xintercept = EV$opus45,   linetype = "dashed", colour = PAL["opus45"],   linewidth = 0.35),
    geom_vline(xintercept = EV$gen46,    linetype = "dashed", colour = PAL["gen46"],    linewidth = 0.35)
  )
  if (label) {
    lab <- data.frame(
      x = c(EV$mandate, EV$sonnet45, EV$opus45, EV$gen46),
      l = c("Mandate", "Sonnet 4.5", "Opus 4.5", "4.6 gen")
    )
    layers <- c(layers, list(
      geom_text(data = lab, aes(x = x, y = y, label = l),
                inherit.aes = FALSE, angle = 90, vjust = -0.3, hjust = 1.02,
                size = EVENT_LABEL_SIZE, colour = INK)
    ))
  }
  layers
}

# ── PDF device ─────────────────────────────────────────────────────────────────
# Both devices embed (subset) the font. On macOS use Quartz: cairo_pdf rounds
# glyph advances at 6-8 pt, which squeezes spaces ("Opus4.5") and opens gaps
# inside words ("m erge"). cairo_pdf remains the fallback on other platforms.
PDF_DEVICE <- if (Sys.info()[["sysname"]] == "Darwin" && capabilities("aqua")) {
  function(filename, width, height, ...)
    grDevices::quartz(file = filename, type = "pdf", width = width, height = height, ...)
} else grDevices::cairo_pdf

# ── Save helper: vector PDF at the exact IEEE target width ─────────────────────
# cols = 1 -> 3.487 in (\columnwidth); cols = 2 -> 7.14 in (\textwidth). Heights
# are deliberately tight; pass `height` to override. Saved width MUST match the
# \includegraphics width in main.tex so the figure is not rescaled (which would
# distort the font sizes calibrated above).
save_fig <- function(plot, name, cols = 1, height = NULL, dir = "figures") {
  width <- if (cols == 2) TEXT_W else COL_W
  if (is.null(height)) height <- if (cols == 2) H_WIDE else H_STD
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  path <- file.path(dir, paste0("fig_", name, ".pdf"))
  ggsave(path, plot, width = width, height = height, device = PDF_DEVICE)
  message(sprintf("saved %s (%.2f x %.2f in)", path, width, height))
  invisible(path)
}
