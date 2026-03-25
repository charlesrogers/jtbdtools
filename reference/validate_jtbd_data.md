# Validate JTBD data format

Checks that a data frame meets the requirements for JTBD scoring:
matching imp/sat column pairs, factor format, valid levels. Prints a
diagnostic report.

## Usage

``` r
validate_jtbd_data(df)
```

## Arguments

- df:

  A data frame to validate

## Value

Invisible TRUE if valid, FALSE with warnings if issues found

## See also

Other import:
[`detect_imp_sat()`](https://charlesrogers.github.io/jtbdtools/reference/detect_imp_sat.md),
[`prep_qualtrics()`](https://charlesrogers.github.io/jtbdtools/reference/prep_qualtrics.md),
[`prep_survey()`](https://charlesrogers.github.io/jtbdtools/reference/prep_survey.md),
[`read_qualtrics()`](https://charlesrogers.github.io/jtbdtools/reference/read_qualtrics.md)

## Examples

``` r
data(jtbd_sample)
validate_jtbd_data(jtbd_sample)
#> ✔ Data is valid for JTBD analysis.
#> ℹ 12 matched imp/sat objective pairs
#> ℹ 200 respondents
#> ℹ 0 missing values across all columns
```
