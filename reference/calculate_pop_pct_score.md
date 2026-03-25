# Calculate population percentage score

Converts raw Likert-scale survey responses (1-5) into a 0-10 score by
calculating the percentage of respondents who rated 4 or 5 (top-2 box).

## Usage

``` r
calculate_pop_pct_score(objectives)
```

## Arguments

- objectives:

  A data frame containing factor columns to be scored

## Value

A data frame with objective names and their calculated scores

## See also

Other scoring:
[`calculate_opportunity_score()`](https://charlesrogers.github.io/jtbdtools/reference/calculate_opportunity_score.md),
[`find_imp_sat_columns()`](https://charlesrogers.github.io/jtbdtools/reference/find_imp_sat_columns.md),
[`get_jtbd_scores()`](https://charlesrogers.github.io/jtbdtools/reference/get_jtbd_scores.md),
[`get_jtbd_scores.individual()`](https://charlesrogers.github.io/jtbdtools/reference/get_jtbd_scores.individual.md),
[`get_sample_size()`](https://charlesrogers.github.io/jtbdtools/reference/get_sample_size.md),
[`split_imp_sat_columns()`](https://charlesrogers.github.io/jtbdtools/reference/split_imp_sat_columns.md)
