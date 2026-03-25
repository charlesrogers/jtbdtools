# Replace spaces with underscores in column names

Uses
[`janitor::clean_names()`](https://sfirke.github.io/janitor/reference/clean_names.html)
to standardize column names.

## Usage

``` r
replace_spaces_with_underscores(df)
```

## Arguments

- df:

  A data frame to process

## Value

A data frame with cleaned column names

## See also

Other data-prep:
[`build_imp_column_names()`](https://charlesrogers.github.io/jtbdtools/reference/build_imp_column_names.md),
[`build_sat_column_names()`](https://charlesrogers.github.io/jtbdtools/reference/build_sat_column_names.md),
[`change_labeles_to_factors()`](https://charlesrogers.github.io/jtbdtools/reference/change_labeles_to_factors.md),
[`convert_labels_to_row_names()`](https://charlesrogers.github.io/jtbdtools/reference/convert_labels_to_row_names.md),
[`prep_data()`](https://charlesrogers.github.io/jtbdtools/reference/prep_data.md),
[`remove_data_prefix()`](https://charlesrogers.github.io/jtbdtools/reference/remove_data_prefix.md),
[`remove_data_suffix()`](https://charlesrogers.github.io/jtbdtools/reference/remove_data_suffix.md)
