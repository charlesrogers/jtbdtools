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
[`add_score_cis()`](https://charlesrogers.github.io/jtbdtools/reference/add_score_cis.md),
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
#> 1 researching minimize_time_to_f…    7.75    2.6     12.9        1          1.53
#> 2 purchasing  minimize_likelihoo…    8.65    6.1     11.2        2          1.33
#> 3 purchasing  minimize_likelihoo…    7.4     4.55    10.2        3          1.21
#> 4 onboarding  minimize_likelihoo…    6.25    2.3     10.2        4          1.21
#> 5 researching minimize_likelihoo…    7.7     6        9.4        5          1.11
#> 6 purchasing  minimize_time_to_c…    7.45    6.3      8.6        6          1.02
```
