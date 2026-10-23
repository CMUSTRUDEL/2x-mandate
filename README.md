# Replication package: statistical analyses and figures

This package contains the full statistical analysis and figure-generation code
for the accompanying paper on the impact of AI coding tools on developer
throughput and code review, including its supplementary material. It lets
reviewers **inspect every analysis**, **run the pipeline end to end on synthetic
data**, and, given the study data or a comparable dataset, **re-run it and check
the results against the numbers printed in the paper**.

## Data availability

**No data is included.** The study data was provided under a confidentiality
agreement with the participating organization and cannot be redistributed in any
form. This package therefore ships code, a documented input schema, and a
synthetic-data generator. To run the pipeline on real data, place a conforming
dataset under `data/` (see [Input schema](#input-schema)).

All identifiers in the schema are opaque account and repository IDs; none of the
code depends on real names, and no organization-, person-, or
vendor-contract-identifying strings are present in this package.

## Relationship to the paper

Anonymizing the code meant simplifying parts of the data pipeline, most notably:
automated accounts and templated bot messages are taken as given flags
(`is_automated`, `is_templated`, `is_generated`), whereas the paper describes the
detection procedure that produced them. The paper reflects the actual research
process; this package is a faithful but deliberately simplified rendering of the
statistical analyses built on top of it.

## Quick start

```sh
Rscript R/make_synthetic_data.R      # writes a small synthetic dataset to data/
Rscript R/check_paper.R              # runs every analysis, compares with the paper
CHECK_FIGURES=1 Rscript R/check_paper.R   # ... and also writes figures/*.pdf
```

On synthetic data almost every comparison fails by design; the run shows that the
code executes end to end. On the study data, `R/check_paper.R` reports how many of
the paper's numbers are reproduced and lists any that are not.

Run every command from the package root. Each analysis script can also be run on
its own (e.g. `Rscript R/02_rq2.R`); it sources `R/data_layer.R` if needed.
`reproduce.Rmd` knits the whole pipeline into one HTML report.

## Layout

```
R/
  data_layer.R           Reads the input tables; applies the automation/window/
                         estimation-sample filters and builds the canonical objects
                         every analysis uses (also builds tab:data).
  01_rq1.R               RQ1: throughput, within-developer DiD (tab:did), the
                         decomposition (tab:decomp), five staggered-adoption
                         estimators (tab:robust), event study, denominator and
                         adoption-signal robustness.
  02_rq2.R               RQ2: seniority tiers and repository cohorts (tab:tier), the
                         capability ladder with placebo and seasonality (tab:ladder)
                         and its robustness variants.
  03_rq3.R               RQ3: review shares, reviewer load, and the per-PR effect of
                         AI authorship (tab:review, tab:phases).
  05_supplement.R        Supplement: PR splitting (tab:splitting), working time
                         (tab:worktime), PR types (tab:prtypes), title-style check,
                         and follow-up fixes.
  figure_data.R          Descriptive per-month / per-PR / per-reviewer series behind
                         the figures, and the in-text numbers read off them.
  04_figures.R           All figures of the paper and supplement, written to
                         figures/*.pdf.
  theme.R                Shared ggplot theme, palette, and figure-saving helpers.
  check_paper.R          Runs everything and compares each recorded number with
                         expected/paper_values.csv; writes output/check_results.csv.
  make_synthetic_data.R  Writes a synthetic dataset in the input schema to data/.
expected/
  paper_values.csv       Every number the paper reports: value, tolerance implied by
                         the paper's rounding, comparison, and LaTeX label.
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
| `title` | string | PR title (used only by the PR-type classifier) |
| `head_ref` | string | source branch name (used only by the PR-type classifier) |
| `lines_added`, `lines_removed` | int | PR size components |
| `files_changed` | int | files touched |
| `commit_count` | int | commits in the PR |
| `iteration_count` | int | review rounds (review-and-revise iterations) |
| `is_revert` | bool | whether the PR reverts another |
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
| `is_templated` | bool | `TRUE` if the event is templated tool output, e.g. a bot message posted through a human account; used by the descriptive review series |

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
- **AI authorship**: a PR is AI-authored when its `labels` contain `created-by-ai`.
- **Automation**: bot exclusion and the human-vs-automated review split are driven
  by the flags above. A *human review* is a non-automated review by someone other
  than the PR author.
- **Recorded numbers**: every number the paper reports is stored with
  `record(key, value)`; `R/check_paper.R` compares them with
  `expected/paper_values.csv`.
