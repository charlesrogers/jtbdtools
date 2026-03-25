# Opportunity matrix plot

Creates the classic ODI opportunity matrix: importance on x-axis,
satisfaction on y-axis, with high-opportunity objectives highlighted.
Optionally shows diagonal zone lines dividing the plot into
Under-Served, Appropriately-Served, Over-Served, and Table Stakes
regions (from quantjtbd).

## Usage

``` r
plot_opportunity_matrix(
  scores,
  title = "Opportunity Score Matrix",
  subtitle = NULL,
  highlight_threshold = 10,
  show_zones = FALSE
)
```

## Arguments

- scores:

  A data frame from
  [`get_jtbd_scores()`](https://charlesrogers.github.io/jtbdtools/reference/get_jtbd_scores.md)
  with imp, sat, and opp columns

- title:

  Plot title (default: "Opportunity Score Matrix")

- subtitle:

  Plot subtitle (default: NULL)

- highlight_threshold:

  Opportunity score threshold for highlighting (default: 10)

- show_zones:

  Show diagonal reference lines and zone labels (default: FALSE)

## Value

A ggplot object

## See also

Other visualization:
[`plot.job_step()`](https://charlesrogers.github.io/jtbdtools/reference/plot.job_step.md),
[`plot_cleveland()`](https://charlesrogers.github.io/jtbdtools/reference/plot_cleveland.md),
[`plot_this.graph.abs_score()`](https://charlesrogers.github.io/jtbdtools/reference/plot_this.graph.abs_score.md),
[`plot_this.graph.rel.abs_score()`](https://charlesrogers.github.io/jtbdtools/reference/plot_this.graph.rel.abs_score.md),
[`plot_this.graph.rel_score()`](https://charlesrogers.github.io/jtbdtools/reference/plot_this.graph.rel_score.md),
[`theme_jtbd()`](https://charlesrogers.github.io/jtbdtools/reference/theme_jtbd.md)

## Examples

``` r
data(jtbd_sample)
scores <- get_jtbd_scores(jtbd_sample)
# plot_opportunity_matrix(scores)
# plot_opportunity_matrix(scores, show_zones = TRUE)
```
