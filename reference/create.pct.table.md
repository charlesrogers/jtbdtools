# Create percentage table

Creates a gt table showing frequency percentages with bar charts for
categorical data (e.g., tool usage, channel preference).

## Usage

``` r
create.pct.table(
  df,
  job.string,
  var_name.string,
  job_step.string,
  path.table = tempdir(),
  n = NULL,
  study = NULL
)
```

## Arguments

- df:

  Data frame containing the data

- job.string:

  Job name for the filename

- var_name.string:

  Column name(s) to analyze

- job_step.string:

  Job step label for the header

- path.table:

  Directory to save the PNG (default:
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html))

- n, study:

  Sample size and study label rendered as a footer (see
  [`jtbd_footer()`](https://charlesrogers.github.io/jtbdtools/reference/jtbd_footer.md)).

## Value

The gt table object (also saved as PNG)

## See also

Other tables:
[`create.job_step.table()`](https://charlesrogers.github.io/jtbdtools/reference/create.job_step.table.md),
[`gt_theme_jtbd()`](https://charlesrogers.github.io/jtbdtools/reference/gt_theme_jtbd.md),
[`theme.job_step()`](https://charlesrogers.github.io/jtbdtools/reference/theme.job_step.md)
