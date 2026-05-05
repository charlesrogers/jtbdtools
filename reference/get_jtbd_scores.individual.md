# Calculate individual JTBD scores

Calculates opportunity scores at the individual respondent level rather
than aggregated across the population. Useful for clustering and
segmentation. Applies the ODI formula per respondent:
`opp = imp + max(0, imp - sat)`.

## Usage

``` r
get_jtbd_scores.individual(df)
```

## Arguments

- df:

  A data frame with `imp__`/`sat__` factor columns (1-5 scale)

## Value

A long-format data frame with columns: caseid, objective, imp, sat, opp

## See also

Other scoring:
[`add_score_cis()`](https://charlesrogers.github.io/jtbdtools/reference/add_score_cis.md),
[`calculate_opportunity_score()`](https://charlesrogers.github.io/jtbdtools/reference/calculate_opportunity_score.md),
[`calculate_pop_pct_score()`](https://charlesrogers.github.io/jtbdtools/reference/calculate_pop_pct_score.md),
[`find_imp_sat_columns()`](https://charlesrogers.github.io/jtbdtools/reference/find_imp_sat_columns.md),
[`get_jtbd_scores()`](https://charlesrogers.github.io/jtbdtools/reference/get_jtbd_scores.md),
[`get_sample_size()`](https://charlesrogers.github.io/jtbdtools/reference/get_sample_size.md),
[`split_imp_sat_columns()`](https://charlesrogers.github.io/jtbdtools/reference/split_imp_sat_columns.md)

## Examples

``` r
data(jtbd_sample)
individual <- get_jtbd_scores.individual(jtbd_sample)
#> Individual scores calculated: 200 respondents x 12 objectives.
head(individual)
#> # A tibble: 6 × 5
#>   caseid objective                                               imp   sat   opp
#>    <int> <chr>                                                 <dbl> <dbl> <dbl>
#> 1      1 researching.minimize_time_to_find_options                 5     3     7
#> 2      1 researching.minimize_time_to_evaluate_options             5     4     6
#> 3      1 researching.minimize_likelihood_of_missing_relevant_…     3     4     3
#> 4      1 researching.minimize_time_to_understand_pricing           3     3     3
#> 5      1 purchasing.minimize_time_to_complete_transaction          4     3     5
#> 6      1 purchasing.minimize_likelihood_of_unexpected_costs        4     1     7
```
