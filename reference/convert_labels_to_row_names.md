# Convert labels to column names

Swaps SPSS variable labels into column names using
[`sjlabelled::label_to_colnames()`](https://strengejacke.github.io/sjlabelled/reference/label_to_colnames.html).

## Usage

``` r
convert_labels_to_row_names(df)
```

## Arguments

- df:

  A data frame with SPSS labels

## Value

A data frame with labels as column names

## See also

Other data-prep:
[`build_imp_column_names()`](https://charlesrogers.github.io/jtbdtools/reference/build_imp_column_names.md),
[`build_sat_column_names()`](https://charlesrogers.github.io/jtbdtools/reference/build_sat_column_names.md),
[`change_labeles_to_factors()`](https://charlesrogers.github.io/jtbdtools/reference/change_labeles_to_factors.md),
[`prep_data()`](https://charlesrogers.github.io/jtbdtools/reference/prep_data.md),
[`remove_data_prefix()`](https://charlesrogers.github.io/jtbdtools/reference/remove_data_prefix.md),
[`remove_data_suffix()`](https://charlesrogers.github.io/jtbdtools/reference/remove_data_suffix.md),
[`replace_spaces_with_underscores()`](https://charlesrogers.github.io/jtbdtools/reference/replace_spaces_with_underscores.md)
