# Prepare Qualtrics survey data for JTBD analysis

Convenience wrapper that combines
[`read_qualtrics()`](https://charlesrogers.github.io/jtbdtools/reference/read_qualtrics.md),
[`detect_imp_sat()`](https://charlesrogers.github.io/jtbdtools/reference/detect_imp_sat.md),
and
[`prep_survey()`](https://charlesrogers.github.io/jtbdtools/reference/prep_survey.md)
into a single call. Goes from Qualtrics CSV to analysis-ready data frame
in one step.

## Usage

``` r
prep_qualtrics(
  file,
  job_steps = NULL,
  imp_pattern = "important|importance",
  sat_pattern = "satisf",
  imp_cols = NULL,
  sat_cols = NULL,
  segment_col = NULL
)
```

## Arguments

- file:

  Path to the Qualtrics CSV file

- job_steps:

  A named list mapping job step names to objective indices. Example:
  `list(researching = 1:4, purchasing = 5:8)`

- imp_pattern:

  Regex pattern to identify importance columns

- sat_pattern:

  Regex pattern to identify satisfaction columns

- imp_cols:

  Explicit importance column names (overrides auto-detection)

- sat_cols:

  Explicit satisfaction column names (overrides auto-detection)

- segment_col:

  Column name(s) to keep as segmentation variable(s)

## Value

A data frame ready for
[`get_jtbd_scores()`](https://charlesrogers.github.io/jtbdtools/reference/get_jtbd_scores.md)

## See also

Other import:
[`detect_imp_sat()`](https://charlesrogers.github.io/jtbdtools/reference/detect_imp_sat.md),
[`prep_survey()`](https://charlesrogers.github.io/jtbdtools/reference/prep_survey.md),
[`read_qualtrics()`](https://charlesrogers.github.io/jtbdtools/reference/read_qualtrics.md),
[`validate_jtbd_data()`](https://charlesrogers.github.io/jtbdtools/reference/validate_jtbd_data.md)

## Examples

``` r
sample_file <- system.file("extdata", "sample_qualtrics.csv", package = "jtbdtools")
ready <- prep_qualtrics(sample_file,
  job_steps = list(researching = 1:4),
  segment_col = "Q3"
)
#> Detected Qualtrics format: removed 2 metadata rows (10 responses).
#> Removed 4 Qualtrics metadata columns.
#> ✔ Detected 4 importance columns
#> ✔ Detected 4 satisfaction columns
#> ℹ 2 other columns (segmentation, demographics, etc.)
#> ✔ Prepared 4 objectives across 1 job step.
#> ℹ Data is ready for `get_jtbd_scores()`.
get_jtbd_scores(ready)
#> # A tibble: 4 × 7
#>   job_step    objective           imp.all sat.all opp.all rank.all opp_index.all
#>   <chr>       <fct>                 <dbl>   <dbl>   <dbl>    <dbl>         <dbl>
#> 1 researching evaluate_options          8       1      15      1            1.07
#> 2 researching find_options              8       2      14      2.5          1   
#> 3 researching understand_pricing        8       2      14      2.5          1   
#> 4 researching minimize_the_likel…       8       4      12      4            0.86
```
