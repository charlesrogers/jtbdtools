# Format job step as gt table

Creates a publication-ready gt table for a job step's opportunity
scores, with color-coded index values and highlighted median row.

## Usage

``` r
theme.job_step(your.df, step_title)
```

## Arguments

- your.df:

  A data frame from
  [`get_jtbd_scores()`](https://charlesrogers.github.io/jtbdtools/reference/get_jtbd_scores.md)
  containing scores for one job step

- step_title:

  Title for the table header

## Value

A gt table object

## See also

Other tables:
[`create.job_step.table()`](https://charlesrogers.github.io/jtbdtools/reference/create.job_step.table.md),
[`create.pct.table()`](https://charlesrogers.github.io/jtbdtools/reference/create.pct.table.md),
[`gt_theme_jtbd()`](https://charlesrogers.github.io/jtbdtools/reference/gt_theme_jtbd.md)
