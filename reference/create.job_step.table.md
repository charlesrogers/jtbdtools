# Create and save job step table

Filters a scores data frame to a specific job step, formats it with
[`theme.job_step()`](https://charlesrogers.github.io/jtbdtools/reference/theme.job_step.md),
and saves as PNG.

## Usage

``` r
create.job_step.table(df, job.string, job_step.string, path.table = tempdir())
```

## Arguments

- df:

  A data frame from
  [`get_jtbd_scores()`](https://charlesrogers.github.io/jtbdtools/reference/get_jtbd_scores.md)

- job.string:

  Job name for the filename

- job_step.string:

  Job step to filter to

- path.table:

  Directory to save the PNG (default:
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html))

## Value

The gt table object (also saved as PNG)

## See also

Other tables:
[`create.pct.table()`](https://charlesrogers.github.io/jtbdtools/reference/create.pct.table.md),
[`gt_theme_jtbd()`](https://charlesrogers.github.io/jtbdtools/reference/gt_theme_jtbd.md),
[`theme.job_step()`](https://charlesrogers.github.io/jtbdtools/reference/theme.job_step.md)
