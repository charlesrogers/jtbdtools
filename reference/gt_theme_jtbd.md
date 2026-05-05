# jtbdtools gt table theme

A branded gt theme for JTBD tables. Clean, left-aligned, with
heat-mapped numeric columns and professional typography. Inspired by the
quantjtbd `gt_theme.yellow` but using the jtbdtools color palette.

## Usage

``` r
gt_theme_jtbd(gt_object, n = NULL, study = NULL, ...)
```

## Arguments

- gt_object:

  A gt table object

- n, study:

  Sample size and study label rendered as a
  [`tab_source_note()`](https://gt.rstudio.com/reference/tab_source_note.html)
  footer (see
  [`jtbd_footer()`](https://charlesrogers.github.io/jtbdtools/reference/jtbd_footer.md)).

- ...:

  Additional arguments (unused)

## Value

A styled gt table object

## See also

Other tables:
[`create.job_step.table()`](https://charlesrogers.github.io/jtbdtools/reference/create.job_step.table.md),
[`create.pct.table()`](https://charlesrogers.github.io/jtbdtools/reference/create.pct.table.md),
[`theme.job_step()`](https://charlesrogers.github.io/jtbdtools/reference/theme.job_step.md)

## Examples

``` r
library(gt)
data(jtbd_sample)
scores <- get_jtbd_scores(jtbd_sample)
scores[1:5, c("job_step", "objective", "imp.all", "sat.all", "opp.all")] |>
  gt() |>
  gt_theme_jtbd(n = 250, study = "Pilot")


  

job_step
```
