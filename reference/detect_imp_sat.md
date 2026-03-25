# Detect importance and satisfaction columns

Scans column names or question labels for text patterns indicating
importance or satisfaction questions. Returns the detected groupings.

## Usage

``` r
detect_imp_sat(
  df,
  imp_pattern = "important|importance",
  sat_pattern = "satisf"
)
```

## Arguments

- df:

  A data frame (optionally with a `question_labels` attribute from
  [`read_qualtrics()`](https://charlesrogers.github.io/jtbdtools/reference/read_qualtrics.md))

- imp_pattern:

  Regex pattern to identify importance columns (default:
  "important\|importance")

- sat_pattern:

  Regex pattern to identify satisfaction columns (default: "satisf")

## Value

A list with `$imp` and `$sat` (character vectors of column names)

## See also

Other import:
[`prep_qualtrics()`](https://charlesrogers.github.io/jtbdtools/reference/prep_qualtrics.md),
[`prep_survey()`](https://charlesrogers.github.io/jtbdtools/reference/prep_survey.md),
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
detected$imp
#> [1] "Q1_1" "Q1_2" "Q1_3" "Q1_4"
detected$sat
#> [1] "Q2_1" "Q2_2" "Q2_3" "Q2_4"
```
