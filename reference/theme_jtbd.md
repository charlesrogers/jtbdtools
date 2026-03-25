# jtbdtools ggplot2 theme

A clean, minimal theme for JTBD visualizations. Based on
`theme_minimal()` with refined typography and grid lines.

## Usage

``` r
theme_jtbd(base_size = 12)
```

## Arguments

- base_size:

  Base font size (default: 12)

## Value

A ggplot2 theme object

## See also

Other visualization:
[`plot.job_step()`](https://charlesrogers.github.io/jtbdtools/reference/plot.job_step.md),
[`plot_cleveland()`](https://charlesrogers.github.io/jtbdtools/reference/plot_cleveland.md),
[`plot_opportunity_matrix()`](https://charlesrogers.github.io/jtbdtools/reference/plot_opportunity_matrix.md),
[`plot_this.graph.abs_score()`](https://charlesrogers.github.io/jtbdtools/reference/plot_this.graph.abs_score.md),
[`plot_this.graph.rel.abs_score()`](https://charlesrogers.github.io/jtbdtools/reference/plot_this.graph.rel.abs_score.md),
[`plot_this.graph.rel_score()`](https://charlesrogers.github.io/jtbdtools/reference/plot_this.graph.rel_score.md)

## Examples

``` r
library(ggplot2)
ggplot(mtcars, aes(mpg, wt)) + geom_point() + theme_jtbd()
```
