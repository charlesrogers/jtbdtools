# Calculate JTBD opportunity scores

The core scoring function. Takes a data frame with `imp__` and `sat__`
columns and calculates importance, satisfaction, and opportunity scores
using the Outcome-Driven Innovation formula.

## Usage

``` r
get_jtbd_scores(your_data_frame, col_suffix = "all")
```

## Arguments

- your_data_frame:

  A data frame with columns following the `imp__job_step.objective` and
  `sat__job_step.objective` naming convention. Values should be factors
  (1-5).

- col_suffix:

  Label for this segment (default: "all")

## Value

A data frame with columns: job_step, objective, imp, sat, opp, rank,
opp_index (suffixed by `col_suffix`)

## See also

Other scoring:
[`calculate_opportunity_score()`](https://charlesrogers.github.io/jtbdtools/reference/calculate_opportunity_score.md),
[`calculate_pop_pct_score()`](https://charlesrogers.github.io/jtbdtools/reference/calculate_pop_pct_score.md),
[`find_imp_sat_columns()`](https://charlesrogers.github.io/jtbdtools/reference/find_imp_sat_columns.md),
[`get_jtbd_scores.individual()`](https://charlesrogers.github.io/jtbdtools/reference/get_jtbd_scores.individual.md),
[`get_sample_size()`](https://charlesrogers.github.io/jtbdtools/reference/get_sample_size.md),
[`split_imp_sat_columns()`](https://charlesrogers.github.io/jtbdtools/reference/split_imp_sat_columns.md)

## Examples

``` r
data(jtbd_sample)
scores <- get_jtbd_scores(jtbd_sample)
head(scores)
#> # A tibble: 6 × 7
#>   job_step    objective           imp.all sat.all opp.all rank.all opp_index.all
#>   <chr>       <fct>                 <dbl>   <dbl>   <dbl>    <dbl>         <dbl>
#> 1 researching minimize_time_to_e…    8.5     2.2     14.8        1          1.64
#> 2 purchasing  minimize_likelihoo…    7.6     1.5     13.7        2          1.51
#> 3 purchasing  minimize_time_to_r…    8.4     4.2     12.6        3          1.39
#> 4 onboarding  minimize_likelihoo…    7.35    2.1     12.6        4          1.39
#> 5 researching minimize_time_to_u…    8.3     6.05    10.6        5          1.17
#> 6 researching minimize_time_to_f…    7.9     6.7      9.1        6          1.01
```
