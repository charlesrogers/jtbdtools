# Build a standardized JTBD footer caption

Returns a one-line string suitable for use in `labs(caption = ...)` on
ggplot2 plots or
[`gt::tab_source_note()`](https://gt.rstudio.com/reference/tab_source_note.html)
on gt tables. Combines sample size, study name, and any existing caption
text into a single, consistently formatted footer.

## Usage

``` r
jtbd_footer(n = NULL, study = NULL, extra = NULL)
```

## Arguments

- n:

  Sample size. Either a single integer or a named numeric vector (e.g.
  `c(all = 200, casual = 80, power = 120)`). When a vector is supplied,
  each element is rendered as `name=count`.

- study:

  Study label (e.g. "GradeOptimizer Q1 2026"). Optional.

- extra:

  Extra text to append after the n/study block (e.g. a methodology
  note). Optional.

## Value

A single character string. Returns `NULL` if all inputs are NULL/empty.

## See also

Other visualization:
[`plot.job_step()`](https://charlesrogers.github.io/jtbdtools/reference/plot.job_step.md),
[`plot_cleveland()`](https://charlesrogers.github.io/jtbdtools/reference/plot_cleveland.md),
[`plot_opportunity_matrix()`](https://charlesrogers.github.io/jtbdtools/reference/plot_opportunity_matrix.md),
[`plot_this.graph.abs_score()`](https://charlesrogers.github.io/jtbdtools/reference/plot_this.graph.abs_score.md),
[`plot_this.graph.rel.abs_score()`](https://charlesrogers.github.io/jtbdtools/reference/plot_this.graph.rel.abs_score.md),
[`plot_this.graph.rel_score()`](https://charlesrogers.github.io/jtbdtools/reference/plot_this.graph.rel_score.md),
[`theme_jtbd()`](https://charlesrogers.github.io/jtbdtools/reference/theme_jtbd.md)

## Examples

``` r
jtbd_footer(n = 250, study = "GradeOptimizer Q1 2026")
#> [1] "n = 250  ·  Study: GradeOptimizer Q1 2026"
jtbd_footer(n = c(all = 250, casual = 80, power = 170))
#> [1] "n = 500 (all=250, casual=80, power=170)"
jtbd_footer(n = 250, study = "Pilot", extra = "T2B scoring; 95% Wilson CI")
#> [1] "n = 250  ·  Study: Pilot  ·  T2B scoring; 95% Wilson CI"
```
