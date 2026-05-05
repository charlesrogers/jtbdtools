# Format job step as gt table

Creates a publication-ready gt table for a job step's opportunity
scores, with color-coded index values and highlighted median row. When
`n` is supplied (or `imp_se.all` / `sat_se.all` columns are already
present), the Importance and Satisfaction values are rendered as
`score ± SE` to make the precision of each estimate visible.

## Usage

``` r
theme.job_step(your.df, step_title, n = NULL, study = NULL)
```

## Arguments

- your.df:

  A data frame from
  [`get_jtbd_scores()`](https://charlesrogers.github.io/jtbdtools/reference/get_jtbd_scores.md)
  containing scores for one job step

- step_title:

  Title for the table header

- n:

  Sample size for the segment (used to compute Wilson SEs if not
  present).

- study:

  Study label rendered in the footer.

## Value

A gt table object

## See also

Other tables:
[`create.job_step.table()`](https://charlesrogers.github.io/jtbdtools/reference/create.job_step.table.md),
[`create.pct.table()`](https://charlesrogers.github.io/jtbdtools/reference/create.pct.table.md),
[`gt_theme_jtbd()`](https://charlesrogers.github.io/jtbdtools/reference/gt_theme_jtbd.md)
