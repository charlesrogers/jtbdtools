# Prepare data for analysis

Runs the full data preparation pipeline: converts SPSS labels to column
names, removes prefixes/suffixes, replaces spaces with underscores, and
converts labelled data to factors.

## Usage

``` r
prep_data(df)
```

## Arguments

- df:

  A data frame (typically imported from SPSS via
  [`haven::read_sav()`](https://haven.tidyverse.org/reference/read_spss.html))

## Value

A processed data frame ready for JTBD scoring

## See also

Other data-prep:
[`build_imp_column_names()`](https://charlesrogers.github.io/jtbdtools/reference/build_imp_column_names.md),
[`build_sat_column_names()`](https://charlesrogers.github.io/jtbdtools/reference/build_sat_column_names.md),
[`change_labeles_to_factors()`](https://charlesrogers.github.io/jtbdtools/reference/change_labeles_to_factors.md),
[`convert_labels_to_row_names()`](https://charlesrogers.github.io/jtbdtools/reference/convert_labels_to_row_names.md),
[`remove_data_prefix()`](https://charlesrogers.github.io/jtbdtools/reference/remove_data_prefix.md),
[`remove_data_suffix()`](https://charlesrogers.github.io/jtbdtools/reference/remove_data_suffix.md),
[`replace_spaces_with_underscores()`](https://charlesrogers.github.io/jtbdtools/reference/replace_spaces_with_underscores.md)

## Examples

``` r
# Typically used with SPSS data:
# df <- haven::read_sav("survey.sav")
# df_clean <- prep_data(df)
```
