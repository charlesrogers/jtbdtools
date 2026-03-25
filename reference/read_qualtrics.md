# Read a Qualtrics CSV export

Reads a Qualtrics CSV file, automatically detecting and removing the 2
metadata header rows that Qualtrics adds below the column names. Also
strips auto-generated Qualtrics columns (ResponseId, dates, status,
etc.).

## Usage

``` r
read_qualtrics(file, keep_metadata_cols = FALSE)
```

## Arguments

- file:

  Path to the Qualtrics CSV file

- keep_metadata_cols:

  If TRUE, keep Qualtrics metadata columns like ResponseId and dates
  (default: FALSE)

## Value

A data frame with question response columns

## See also

Other import:
[`detect_imp_sat()`](https://charlesrogers.github.io/jtbdtools/reference/detect_imp_sat.md),
[`prep_qualtrics()`](https://charlesrogers.github.io/jtbdtools/reference/prep_qualtrics.md),
[`prep_survey()`](https://charlesrogers.github.io/jtbdtools/reference/prep_survey.md),
[`validate_jtbd_data()`](https://charlesrogers.github.io/jtbdtools/reference/validate_jtbd_data.md)

## Examples

``` r
sample_file <- system.file("extdata", "sample_qualtrics.csv", package = "jtbdtools")
df <- read_qualtrics(sample_file)
#> Detected Qualtrics format: removed 2 metadata rows (10 responses).
#> Removed 4 Qualtrics metadata columns.
head(df)
#>   Q1_1 Q1_2 Q1_3 Q1_4 Q2_1 Q2_2 Q2_3 Q2_4         Q3          Q4
#> 1    5    4    3    5    3    2    4    3 Power User    2+ years
#> 2    4    5    4    4    2    1    3    2     Casual 6-12 months
#> 3    5    5    5    4    3    3    2    4 Power User   1-2 years
#> 4    3    4    4    5    4    3    5    3   New User  < 6 months
#> 5    4    3    5    3    2    2    3    1     Casual 6-12 months
#> 6    5    5    4    4    1    2    2    3 Power User    2+ years
```
