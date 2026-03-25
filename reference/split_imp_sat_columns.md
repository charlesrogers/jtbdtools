# Split importance and satisfaction columns

Separates the `objective_name` column into `imp_sat` (imp/sat indicator)
and `objective` (the actual objective name) by splitting on `__`.

## Usage

``` r
split_imp_sat_columns(data_frame_imp_sat)
```

## Arguments

- data_frame_imp_sat:

  A data frame with an `objective_name` column

## Value

A data frame with `imp_sat` and `objective` columns

## See also

Other scoring:
[`calculate_opportunity_score()`](https://charlesrogers.github.io/jtbdtools/reference/calculate_opportunity_score.md),
[`calculate_pop_pct_score()`](https://charlesrogers.github.io/jtbdtools/reference/calculate_pop_pct_score.md),
[`find_imp_sat_columns()`](https://charlesrogers.github.io/jtbdtools/reference/find_imp_sat_columns.md),
[`get_jtbd_scores()`](https://charlesrogers.github.io/jtbdtools/reference/get_jtbd_scores.md),
[`get_jtbd_scores.individual()`](https://charlesrogers.github.io/jtbdtools/reference/get_jtbd_scores.individual.md),
[`get_sample_size()`](https://charlesrogers.github.io/jtbdtools/reference/get_sample_size.md)
