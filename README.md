# Statistical analysis code

This repository contains the statistical analysis and figure-generation code
for the accompanying paper "AI Writes Faster Than Humans Can Review: A Longitudinal Study of an Enterprise ``2x'' Mandate", including its supplementary material. It lets
readers **see exactly how every table, figure, and in-text number is computed**
(see [Where each result is computed](#where-each-result-is-computed)) and **run
the full pipeline** on a dataset in the documented input schema.

## Data availability

**No data is included.** The study data was provided under a confidentiality
agreement with the participating organization and cannot be redistributed in any
form. This repository therefore contains code, a documented input schema, and a
synthetic-data generator.

The synthetic data exists only so the code can be run end to end: it shows what
each output looks like and that every script executes. **Numbers computed from it
are meaningless and are not expected to resemble the paper.** To run the
pipeline on real data, place a conforming dataset under `data/` (see
[Input schema](#input-schema)).

All identifiers in the schema are opaque account and repository IDs; none of the
code depends on real names, and no organization-, person-, or
vendor-contract-identifying strings are present in this repository.

## Relationship to the paper

Anonymizing the code meant simplifying parts of the data pipeline, most notably:
automated accounts and templated bot messages are taken as given flags
(`is_automated`, `is_templated`, `is_generated`), whereas the paper describes the
detection procedure that produced them. The paper reflects the actual research
process; this code is a faithful but deliberately simplified rendering of the
statistical analyses built on top of it.

## Quick start

```sh
Rscript R/make_synthetic_data.R   # optional: writes a synthetic dataset to data/
Rscript R/run_all.R               # runs every analysis on data/ and writes output/
```

`R/run_all.R` writes:

- `output/results.csv`: every number the paper reports, one row each, with its
  value, p-value (for regression coefficients), where it appears in the paper,
  and the script that computes it;
- `output/tables/*.md`: the paper's tables, rebuilt in the paper's layout with
  significance stars;
- `output/figures/*.pdf`: every figure of the paper and its supplement.

Each analysis script also runs on its own and prints its results (e.g.
`Rscript R/02_rq2.R`); it sources `R/data_layer.R` if needed. `reproduce.Rmd`
knits the whole pipeline into one HTML report. Run every command from the repository
root.

## Where each result is computed

Table, figure, and section numbers refer to the paper; "Suppl." is its
supplementary material. Script sections are marked in the code with
`## ---- (n)` headings.

| Paper item | Script, section |
|---|---|
| Table I (developer-level DiD) | `R/01_rq1.R` (1) |
| Table II (decomposition), and the calendar-month FE version in Sec. IV-A | `R/01_rq1.R` (2) |
| Table III, seniority tiers | `R/02_rq2.R` (1) |
| Table III, repository cohorts | `R/02_rq2.R` (2) |
| Table IV (per-PR effects of AI authorship) | `R/03_rq3.R` (4) |
| Table V (dataset summary) | `R/data_layer.R` (6) |
| Table VI (cycle-time phases) | `R/03_rq3.R` (4) |
| Table VII (estimators) | `R/01_rq1.R` (3) |
| Table VIII (PR splitting) | `R/05_supplement.R` (A) |
| Table IX (PR types) | `R/05_supplement.R` (C) |
| Table X (working time) | `R/05_supplement.R` (B) |
| Table XI (capability ladder, placebo, seasonality) | `R/02_rq2.R` (3) |
| Sec. IV-A in-text numbers (throughput ratio, developers active throughout, spend, event-study multipliers, merged-PR outcome) | `R/01_rq1.R` (0), (1), (4); `R/figure_data.R` (end) |
| Sec. IV-B robustness of the accumulated-use slope | `R/02_rq2.R` (3) |
| Sec. IV-C review volume, reviewer pool, review shares, AI-PR size | `R/figure_data.R` (end); `R/03_rq3.R` (3), (4) |
| Suppl. C-D (organization-level latency) | `R/figure_data.R` (end) |
| Suppl. D-B (adoption-signal robustness) | `R/01_rq1.R` (6) |
| Suppl. D-D (denominator, zero-output months) | `R/01_rq1.R` (5) |
| Suppl. E-B title-style check | `R/05_supplement.R` (D) |
| Suppl. E-D (follow-up fixes) | `R/05_supplement.R` (E) |
| Fig. 1 (`fig_adoption`) | `R/04_figures.R`, "fig_adoption" |
| Fig. 2, 4, 6, 8, 10, 12 (`fig_overview_*`) | `R/04_figures.R`, `make_overview()`; series built in `R/figure_data.R` |
| Fig. 3 (`fig_identification`, simulated) | `R/04_figures.R`, "fig_identification" |
| Fig. 5 and 14 (`fig_adoption_eventstudy`, `..._pretrends`) | `R/04_figures.R`, "fig_adoption_eventstudy" |
| Fig. 7 and 9 (`fig_review_reallocation`, `fig_review_decomp_mean`) | `R/04_figures.R`, "fig_review_reallocation" |
| Fig. 11 (`fig_review_latency`) | `R/04_figures.R`, "fig_review_latency" |
| Fig. 13 (`fig_overview_spend`) | `R/04_figures.R`, "fig_overview_spend" |
| Fig. 15 (`fig_capability_experience`) | `R/04_figures.R`, "fig_capability_experience" |

The keys in `output/results.csv` carry the same mapping in their `where` and
`script` columns.

## Layout

```
R/
  data_layer.R           Reads the input tables; applies the automation/window/
                         estimation-sample filters; builds the canonical objects
                         every analysis uses.
  01_rq1.R               RQ1: throughput, DiD, decomposition, estimators, event
                         study, denominator and adoption-signal robustness.
  02_rq2.R               RQ2: seniority tiers, repository cohorts, capability ladder.
  03_rq3.R               RQ3: review shares, reviewer load, per-PR effects.
  05_supplement.R        Supplement: splitting, working time, PR types, title style,
                         follow-up fixes.
  figure_data.R          Descriptive monthly / per-PR / per-reviewer series behind
                         the figures, and the in-text numbers read off them.
  04_figures.R           All figures.
  theme.R                Shared ggplot theme, palette, and figure-saving helpers.
  tables.R               Rebuilds the paper's tables from the recorded numbers.
  run_all.R              Runs everything and writes output/.
  make_synthetic_data.R  Writes a synthetic dataset in the input schema to data/.
reproduce.Rmd            Knits the whole pipeline into one HTML report.
```

## Software

R (>= 4.2) with: `tidyverse`, `fixest`, `did`, `didimputation`, `patchwork`,
`cowplot`, `scales`, `broom` (and `rmarkdown`, `rprojroot` to knit
`reproduce.Rmd`). Figures embed Open Sans (free from Google Fonts). On macOS they
are saved with the Quartz PDF device; elsewhere they fall back to `cairo_pdf`. On
a host without Open Sans installed, set `PAPER_FONT <- "sans"` at the top of
`R/theme.R`.

Set the environment variable `HOME_TZ` to the organization's home time zone (an
Olson name such as `America/New_York`; default `UTC`). It only affects the
working-time analysis (dates, weekends, and hours of activity).

## Input schema

Place these UTF-8 CSVs under `data/`. Extra columns are ignored; the columns below
are the ones the code reads. `identity` is an opaque account ID (the same ID space
across all tables); `repository` is an opaque repository ID. Timestamps are
ISO-8601; `month` is any first-of-month or `YYYY-MM` string. Booleans are
`TRUE`/`FALSE`.

### `pull_requests.csv`: one row per pull request (all history)

Include PRs from before the study window: a repository's age cohort is set by its
first-ever PR.

| column | type | notes |
|---|---|---|
| `repository` | string | opaque repo ID |
| `number` | int | PR number within the repo (`repository`+`number` is the PR key) |
| `identity` | string | PR author account ID |
| `created_at` | timestamp | PR creation time |
| `merged_at` | timestamp | merge time (empty if never merged) |
| `state` | string | `MERGED` / `CLOSED` / `OPEN` |
| `labels` | string | label names; a `created-by-ai` label marks an AI-authored PR |
| `ai_any_label` | bool | `TRUE` if the PR carries any AI-attribution label (`created-by-ai` or a tool-specific one); used only for the descriptive AI-share series |
| `title` | string | PR title (used only by the PR-type classifier) |
| `head_ref` | string | source branch name (used only by the PR-type classifier) |
| `lines_added`, `lines_removed` | int | PR size components |
| `files_changed` | int | files touched |
| `commit_count` | int | commits in the PR |
| `iteration_count` | int | review rounds (review-and-revise iterations) |
| `is_revert` | bool | whether the PR reverts another |
| `has_review` | bool | whether the PR received any review (used for the review-rounds model) |
| `comment_count` | int | total review/PR comments |
| `coding_lead_time_hours`, `pickup_time_hours`, `review_lead_time_hours`, `total_cycle_time_hours` | float | DORA-style cycle-time phases (hours) |
| `is_automated` | bool | `TRUE` if authored by an automated account; such PRs are excluded |
| `is_generated` | bool | `TRUE` for machine-generated PRs opened from human accounts (e.g. translation sync, batched infrastructure changes); excluded from the PR-type analysis only |

### `pr_reviews.csv`: one row per review / comment event

| column | type | notes |
|---|---|---|
| `repository`, `pr_number` | string, int | the PR this event belongs to |
| `identity` | string | reviewer account ID |
| `author` | string | reviewer account ID as recorded on the event (set equal to `identity` if you have one) |
| `type` | string | `comment`, `review_thread`, `review:commented`, `review:changes_requested`, `review:approved`, `review:dismissed` |
| `comment_time` | timestamp | event time |
| `is_automated` | bool | `TRUE` if the reviewer account is automated (bot account) |
| `is_templated` | bool | `TRUE` if the event itself is automated: templated tool output (e.g. a bot message posted through a human account) or flagged as a bot in the source export; used by the descriptive review series and the review-timing figure |

### `commits.csv`: one row per commit

| column | type | notes |
|---|---|---|
| `identity` | string | commit author account ID |
| `committed_at` | timestamp | commit time |
| `is_automated` | bool | `TRUE` for automated authors; such rows are dropped |

### `contributors.csv`: role titles (for the seniority ladder)

| column | type | notes |
|---|---|---|
| `identity` | string | account ID |
| `role` | string | free-text job title; classified into IC/Senior/Staff/Principal/Management by keyword |

### `ai_tool_usage.csv`: AI-tool telemetry, one row per (identity, month)

| column | type | notes |
|---|---|---|
| `identity` | string | account ID |
| `month` | month | usage month |
| `cursor_lines` | int | lines accepted from the editor-assistant tool |
| `claude_lines` | int | lines generated from the agentic tool |
| `claude_spend` | float | tool spend that month (currency units) |

Adoption is defined from this telemetry: a developer-month is *adopted* if any of
`cursor_lines`, `claude_lines`, or `claude_spend` is positive.

## Definitions and conventions (documented in the code)

- **Window**: PRs created Jan 2024 to Apr 2026; pre-mandate baseline Jan to Apr 2025.
- **Active month**: a developer-month with at least one authored PR (independent of
  treatment, so adopters and never-adopters are filtered symmetrically).
- **Estimation sample**: developers with at least 3 active months.
- **AI authorship**: a PR is AI-authored when its `labels` contain `created-by-ai`
  (the descriptive AI-share series uses the broader `ai_any_label`).
- **Automation**: bot exclusion and the human-vs-automated review split are driven
  by the flags above. A *human review* is a non-automated review by someone other
  than the PR author.
