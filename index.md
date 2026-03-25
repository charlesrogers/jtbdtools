# jtbdtools ![](reference/figures/logo.png)

**Quantitative Jobs-to-Be-Done analysis in R.** Opportunity scoring,
segment comparison, and publication-ready visualizations using the
Outcome-Driven Innovation methodology.

## The Formula

The ODI opportunity score measures unmet customer needs:

    opportunity = importance + max(0, importance - satisfaction)

When importance exceeds satisfaction, the gap amplifies the score. When
users are already satisfied, opportunity equals importance (the floor).
Scores range 0-20; anything above 10 is a high-opportunity outcome.

## What You Get

### Segment comparison tables

Compare opportunity scores across user segments to find where specific
groups are underserved:

![Segment comparison table](reference/figures/readme-segment-table.png)

### Opportunity matrix

The classic ODI scatter plot: high importance + low satisfaction = high
opportunity (red):

![Opportunity score
matrix](reference/figures/readme-opportunity-matrix.png)

### Ranked opportunity scores

Instantly see which outcomes have the highest unmet need:

![Top opportunity
scores](reference/figures/readme-top-opportunities.png)

## Quick Start

``` r
# install.packages("pak")
pak::pak("charlesrogers/jtbdtools")

library(jtbdtools)
data(jtbd_sample)

# Calculate opportunity scores
scores <- get_jtbd_scores(jtbd_sample)

# Compare across segments
comparison <- get_jtbd_scores.comparison(jtbd_sample, "segment")

# Visualize
plot_opportunity_matrix(scores)
```

## Functions

| Category          | Function                                                                                                                  | Description                                |
|-------------------|---------------------------------------------------------------------------------------------------------------------------|--------------------------------------------|
| **Scoring**       | [`get_jtbd_scores()`](https://charlesrogers.github.io/jtbdtools/reference/get_jtbd_scores.md)                             | Calculate imp/sat/opp scores for a dataset |
|                   | [`get_jtbd_scores.comparison()`](https://charlesrogers.github.io/jtbdtools/reference/get_jtbd_scores.comparison.md)       | Compare scores across segments             |
|                   | [`get_jtbd_scores.pairwise()`](https://charlesrogers.github.io/jtbdtools/reference/get_jtbd_scores.pairwise.md)           | Head-to-head comparison of two segments    |
|                   | [`calculate_opportunity_score()`](https://charlesrogers.github.io/jtbdtools/reference/calculate_opportunity_score.md)     | Core ODI formula                           |
| **Visualization** | [`plot_opportunity_matrix()`](https://charlesrogers.github.io/jtbdtools/reference/plot_opportunity_matrix.md)             | Importance x Satisfaction scatter          |
|                   | [`plot_cleveland()`](https://charlesrogers.github.io/jtbdtools/reference/plot_cleveland.md)                               | Lollipop chart for segment comparison      |
|                   | [`plot_this.graph.rel_score()`](https://charlesrogers.github.io/jtbdtools/reference/plot_this.graph.rel_score.md)         | Relative score bump chart                  |
|                   | [`plot_this.graph.abs_score()`](https://charlesrogers.github.io/jtbdtools/reference/plot_this.graph.abs_score.md)         | Absolute score bump chart                  |
| **Tables**        | [`theme.job_step()`](https://charlesrogers.github.io/jtbdtools/reference/theme.job_step.md)                               | Publication-ready gt table                 |
|                   | [`create.job_step.table()`](https://charlesrogers.github.io/jtbdtools/reference/create.job_step.table.md)                 | Filter + format + save as PNG              |
|                   | [`create.pct.table()`](https://charlesrogers.github.io/jtbdtools/reference/create.pct.table.md)                           | Frequency table with bar charts            |
| **Data Prep**     | [`prep_data()`](https://charlesrogers.github.io/jtbdtools/reference/prep_data.md)                                         | Full SPSS data cleaning pipeline           |
|                   | [`build_imp_column_names()`](https://charlesrogers.github.io/jtbdtools/reference/build_imp_column_names.md)               | Rename columns to `imp__step.objective`    |
|                   | [`build_sat_column_names()`](https://charlesrogers.github.io/jtbdtools/reference/build_sat_column_names.md)               | Rename columns to `sat__step.objective`    |
| **Analysis**      | [`get.normalized_scores()`](https://charlesrogers.github.io/jtbdtools/reference/get.normalized_scores.md)                 | Min-max normalize within segments          |
|                   | [`get.percent_of_max()`](https://charlesrogers.github.io/jtbdtools/reference/get.percent_of_max.md)                       | Percent-of-segment-max scoring             |
|                   | [`get_jtbd_segment.comp.ordinal()`](https://charlesrogers.github.io/jtbdtools/reference/get_jtbd_segment.comp.ordinal.md) | Rank-based segment comparison              |

## Data Format

Your data needs columns in this format:

    imp__job_step.objective_name    # importance (factor, 1-5)
    sat__job_step.objective_name    # satisfaction (factor, 1-5)

Example: - `imp__researching.minimize_time_to_evaluate_options` -
`sat__researching.minimize_time_to_evaluate_options`

See
[`?jtbd_sample`](https://charlesrogers.github.io/jtbdtools/reference/jtbd_sample.md)
for a complete working dataset and
[`vignette("data-preparation")`](https://charlesrogers.github.io/jtbdtools/articles/data-preparation.md)
for SPSS import instructions.

## Methodology

Based on Tony Ulwick’s [Outcome-Driven
Innovation](https://www.amazon.com/What-Customers-Want-Outcome-Driven-Breakthrough/dp/0071408673).
The package:

1.  Converts Likert-scale (1-5) survey responses to 0-10 scores using
    **top-2-box** scoring (% rating 4 or 5)
2.  Applies the ODI formula:
    `opportunity = importance + max(0, importance - satisfaction)`
3.  Compares scores across user segments to identify where specific
    groups are underserved
4.  Generates publication-ready visualizations and tables

## License

MIT
