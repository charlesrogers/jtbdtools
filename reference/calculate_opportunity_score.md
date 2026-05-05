# Calculate opportunity score

Applies the ODI opportunity formula:
`opportunity = importance + max(0, importance - satisfaction)`. When
importance \>= satisfaction, the gap amplifies opportunity. When
satisfaction \> importance, opportunity equals importance (the floor).

## Usage

``` r
calculate_opportunity_score(data_frame_split)
```

## Arguments

- data_frame_split:

  A data frame with `imp_sat`, `objective`, and `imp_sat_score` columns

## Value

A data frame with `imp`, `sat`, and `opp` columns per objective

## See also

Other scoring:
[`add_score_cis()`](https://charlesrogers.github.io/jtbdtools/reference/add_score_cis.md),
[`calculate_pop_pct_score()`](https://charlesrogers.github.io/jtbdtools/reference/calculate_pop_pct_score.md),
[`find_imp_sat_columns()`](https://charlesrogers.github.io/jtbdtools/reference/find_imp_sat_columns.md),
[`get_jtbd_scores()`](https://charlesrogers.github.io/jtbdtools/reference/get_jtbd_scores.md),
[`get_jtbd_scores.individual()`](https://charlesrogers.github.io/jtbdtools/reference/get_jtbd_scores.individual.md),
[`get_sample_size()`](https://charlesrogers.github.io/jtbdtools/reference/get_sample_size.md),
[`split_imp_sat_columns()`](https://charlesrogers.github.io/jtbdtools/reference/split_imp_sat_columns.md)
