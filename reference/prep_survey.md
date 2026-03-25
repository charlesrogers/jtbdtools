# Prepare survey data for JTBD analysis

The universal data prep function. Takes any data frame and renames
columns to the required `imp__job_step.objective` /
`sat__job_step.objective` format. Works with data from any source
(Qualtrics, SurveyMonkey, Google Forms, CSV, etc.).

## Usage

``` r
prep_survey(
  df,
  imp_cols,
  sat_cols,
  job_steps = NULL,
  objective_names = NULL,
  segment_col = NULL
)
```

## Arguments

- df:

  A data frame containing survey responses

- imp_cols:

  Character vector of importance column names

- sat_cols:

  Character vector of satisfaction column names (must be same length and
  order as `imp_cols`)

- job_steps:

  A named list mapping job step names to column indices or names.
  Example: `list(researching = 1:4, purchasing = 5:8)` means the first 4
  imp/sat pairs belong to "researching" and the next 4 to "purchasing".
  If NULL, all objectives are assigned to a single job step called
  "all".

- objective_names:

  Optional character vector of clean objective names. If NULL, names are
  auto-generated from column names by cleaning punctuation and spaces.

- segment_col:

  Optional name of a column to keep as segmentation variable. Can also
  be a character vector of column names to keep.

## Value

A data frame ready for
[`get_jtbd_scores()`](https://charlesrogers.github.io/jtbdtools/reference/get_jtbd_scores.md),
with properly named `imp__`/`sat__` columns and factor values (1-5)

## See also

Other import:
[`detect_imp_sat()`](https://charlesrogers.github.io/jtbdtools/reference/detect_imp_sat.md),
[`prep_qualtrics()`](https://charlesrogers.github.io/jtbdtools/reference/prep_qualtrics.md),
[`read_qualtrics()`](https://charlesrogers.github.io/jtbdtools/reference/read_qualtrics.md),
[`validate_jtbd_data()`](https://charlesrogers.github.io/jtbdtools/reference/validate_jtbd_data.md)

## Examples

``` r
sample_file <- system.file("extdata", "sample_qualtrics.csv", package = "jtbdtools")
df <- read_qualtrics(sample_file)
#> Detected Qualtrics format: removed 2 metadata rows (10 responses).
#> Removed 4 Qualtrics metadata columns.
detected <- detect_imp_sat(df)
#> ✔ Detected 4 importance columns
#> ✔ Detected 4 satisfaction columns
#> ℹ 2 other columns (segmentation, demographics, etc.)

ready <- prep_survey(df,
  imp_cols = detected$imp,
  sat_cols = detected$sat,
  job_steps = list(researching = 1:4),
  segment_col = "Q3"
)
#> ✔ Prepared 4 objectives across 1 job step.
#> ℹ Data is ready for `get_jtbd_scores()`.
head(ready)
#>   caseid         Q3 imp__researching.find_options sat__researching.find_options
#> 1      1 Power User                             5                             3
#> 2      2     Casual                             4                             2
#> 3      3 Power User                             5                             3
#> 4      4   New User                             3                             4
#> 5      5     Casual                             4                             2
#> 6      6 Power User                             5                             1
#>   imp__researching.evaluate_options sat__researching.evaluate_options
#> 1                                 4                                 2
#> 2                                 5                                 1
#> 3                                 5                                 3
#> 4                                 4                                 3
#> 5                                 3                                 2
#> 6                                 5                                 2
#>   imp__researching.minimize_the_likelihood_of_missing_relevant_options
#> 1                                                                    3
#> 2                                                                    4
#> 3                                                                    5
#> 4                                                                    4
#> 5                                                                    5
#> 6                                                                    4
#>   sat__researching.minimize_the_likelihood_of_missing_relevant_options
#> 1                                                                    4
#> 2                                                                    3
#> 3                                                                    2
#> 4                                                                    5
#> 5                                                                    3
#> 6                                                                    2
#>   imp__researching.understand_pricing sat__researching.understand_pricing
#> 1                                   5                                   3
#> 2                                   4                                   2
#> 3                                   4                                   4
#> 4                                   5                                   3
#> 5                                   3                                   1
#> 6                                   4                                   3
```
