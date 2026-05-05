# Plot job step scatter

Creates an importance vs satisfaction scatter plot for a job step.

## Usage

``` r
# S3 method for class 'job_step'
plot(x, step_title = "", n = NULL, study = NULL, ...)
```

## Arguments

- x:

  A data frame containing job step data

- step_title:

  The title for the plot

- n, study:

  Sample size and study label for the plot footer (see
  [`jtbd_footer()`](https://charlesrogers.github.io/jtbdtools/reference/jtbd_footer.md)).

- ...:

  Additional arguments (unused)

## Value

A ggplot object

## See also

Other visualization:
[`jtbd_footer()`](https://charlesrogers.github.io/jtbdtools/reference/jtbd_footer.md),
[`plot_cleveland()`](https://charlesrogers.github.io/jtbdtools/reference/plot_cleveland.md),
[`plot_opportunity_matrix()`](https://charlesrogers.github.io/jtbdtools/reference/plot_opportunity_matrix.md),
[`plot_this.graph.abs_score()`](https://charlesrogers.github.io/jtbdtools/reference/plot_this.graph.abs_score.md),
[`plot_this.graph.rel.abs_score()`](https://charlesrogers.github.io/jtbdtools/reference/plot_this.graph.rel.abs_score.md),
[`plot_this.graph.rel_score()`](https://charlesrogers.github.io/jtbdtools/reference/plot_this.graph.rel_score.md),
[`theme_jtbd()`](https://charlesrogers.github.io/jtbdtools/reference/theme_jtbd.md)
