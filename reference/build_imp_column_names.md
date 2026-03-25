# Build importance column names

Renames columns to the required `imp__job_step.objective` format. Runs
[`prep_data()`](https://charlesrogers.github.io/jtbdtools/reference/prep_data.md)
first, then prefixes each column with `imp__` and the job section.

## Usage

``` r
build_imp_column_names(df, job_section)
```

## Arguments

- df:

  A data frame to process

- job_section:

  The job section name (e.g., "researching")

## Value

A data frame with columns renamed to `imp__job_section.original_name`

## See also

Other data-prep:
[`build_sat_column_names()`](https://charlesrogers.github.io/jtbdtools/reference/build_sat_column_names.md),
[`change_labeles_to_factors()`](https://charlesrogers.github.io/jtbdtools/reference/change_labeles_to_factors.md),
[`convert_labels_to_row_names()`](https://charlesrogers.github.io/jtbdtools/reference/convert_labels_to_row_names.md),
[`prep_data()`](https://charlesrogers.github.io/jtbdtools/reference/prep_data.md),
[`remove_data_prefix()`](https://charlesrogers.github.io/jtbdtools/reference/remove_data_prefix.md),
[`remove_data_suffix()`](https://charlesrogers.github.io/jtbdtools/reference/remove_data_suffix.md),
[`replace_spaces_with_underscores()`](https://charlesrogers.github.io/jtbdtools/reference/replace_spaces_with_underscores.md)
