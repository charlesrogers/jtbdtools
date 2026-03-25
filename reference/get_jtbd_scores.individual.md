# Calculate individual JTBD scores

Calculates opportunity scores at the individual respondent level rather
than aggregated across the population. Useful for clustering and
segmentation.

## Usage

``` r
get_jtbd_scores.individual(df.uid_imp_sat)
```

## Arguments

- df.uid_imp_sat:

  A data frame with a `caseid` column and `imp__`/`sat__` columns

## Value

A data frame with individual-level scores

## See also

Other scoring:
[`calculate_opportunity_score()`](https://charlesrogers.github.io/jtbdtools/reference/calculate_opportunity_score.md),
[`calculate_pop_pct_score()`](https://charlesrogers.github.io/jtbdtools/reference/calculate_pop_pct_score.md),
[`find_imp_sat_columns()`](https://charlesrogers.github.io/jtbdtools/reference/find_imp_sat_columns.md),
[`get_jtbd_scores()`](https://charlesrogers.github.io/jtbdtools/reference/get_jtbd_scores.md),
[`get_sample_size()`](https://charlesrogers.github.io/jtbdtools/reference/get_sample_size.md),
[`split_imp_sat_columns()`](https://charlesrogers.github.io/jtbdtools/reference/split_imp_sat_columns.md)
