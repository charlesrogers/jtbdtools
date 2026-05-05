# Get sample size

Returns the number of non-NA respondents based on the last importance
column.

## Usage

``` r
get_sample_size(your_data_frame)
```

## Arguments

- your_data_frame:

  A data frame containing columns starting with "imp\_\_"

## Value

Integer: number of non-NA rows

## See also

Other scoring:
[`add_score_cis()`](https://charlesrogers.github.io/jtbdtools/reference/add_score_cis.md),
[`calculate_opportunity_score()`](https://charlesrogers.github.io/jtbdtools/reference/calculate_opportunity_score.md),
[`calculate_pop_pct_score()`](https://charlesrogers.github.io/jtbdtools/reference/calculate_pop_pct_score.md),
[`find_imp_sat_columns()`](https://charlesrogers.github.io/jtbdtools/reference/find_imp_sat_columns.md),
[`get_jtbd_scores()`](https://charlesrogers.github.io/jtbdtools/reference/get_jtbd_scores.md),
[`get_jtbd_scores.individual()`](https://charlesrogers.github.io/jtbdtools/reference/get_jtbd_scores.individual.md),
[`split_imp_sat_columns()`](https://charlesrogers.github.io/jtbdtools/reference/split_imp_sat_columns.md)

## Examples

``` r
data(jtbd_sample)
get_sample_size(jtbd_sample)
#> [1] 200
```
