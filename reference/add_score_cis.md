# Add Wilson confidence intervals to JTBD scores

Augments a scores data frame from
[`get_jtbd_scores()`](https://charlesrogers.github.io/jtbdtools/reference/get_jtbd_scores.md)
or
[`get_jtbd_scores.comparison()`](https://charlesrogers.github.io/jtbdtools/reference/get_jtbd_scores.comparison.md)
with confidence-interval bounds for each importance, satisfaction, and
opportunity column. Because raw scores are top-2-box proportions scaled
to 0–10, the imp/sat bounds use the Wilson interval for binomial
proportions. Opportunity bounds propagate via the delta method on
`opp = imp + max(0, imp - sat)`.

## Usage

``` r
add_score_cis(scores, n, conf_level = 0.95)
```

## Arguments

- scores:

  A scores data frame with `imp.<seg>`, `sat.<seg>`, `opp.<seg>`
  columns.

- n:

  Sample size. Either a single integer applied to every segment, or a
  named integer vector keyed by segment (e.g.
  `c(all = 200, casual = 80)`).

- conf_level:

  Confidence level (default 0.95).

## Value

The input data frame with new `<imp|sat|opp>_lo.<seg>`,
`<imp|sat|opp>_hi.<seg>`, and `<imp|sat|opp>_se.<seg>` columns.

## See also

Other scoring:
[`calculate_opportunity_score()`](https://charlesrogers.github.io/jtbdtools/reference/calculate_opportunity_score.md),
[`calculate_pop_pct_score()`](https://charlesrogers.github.io/jtbdtools/reference/calculate_pop_pct_score.md),
[`find_imp_sat_columns()`](https://charlesrogers.github.io/jtbdtools/reference/find_imp_sat_columns.md),
[`get_jtbd_scores()`](https://charlesrogers.github.io/jtbdtools/reference/get_jtbd_scores.md),
[`get_jtbd_scores.individual()`](https://charlesrogers.github.io/jtbdtools/reference/get_jtbd_scores.individual.md),
[`get_sample_size()`](https://charlesrogers.github.io/jtbdtools/reference/get_sample_size.md),
[`split_imp_sat_columns()`](https://charlesrogers.github.io/jtbdtools/reference/split_imp_sat_columns.md)

## Examples

``` r
data(jtbd_sample)
scores <- get_jtbd_scores(jtbd_sample)
add_score_cis(scores, n = get_sample_size(jtbd_sample))
#> # A tibble: 12 × 16
#>    job_step  objective imp.all sat.all opp.all rank.all opp_index.all imp_lo.all
#>    <chr>     <fct>       <dbl>   <dbl>   <dbl>    <dbl>         <dbl>      <dbl>
#>  1 research… minimize…    7.75    2.6    12.9         1          1.53       7.12
#>  2 purchasi… minimize…    8.65    6.1    11.2         2          1.33       8.11
#>  3 purchasi… minimize…    7.4     4.55   10.2         3          1.21       6.75
#>  4 onboardi… minimize…    6.25    2.3    10.2         4          1.21       5.56
#>  5 research… minimize…    7.7     6       9.4         5          1.11       7.07
#>  6 purchasi… minimize…    7.45    6.3     8.6         6          1.02       6.8 
#>  7 research… minimize…    7.35    6.4     8.3         7          0.98       6.7 
#>  8 purchasi… minimize…    6.05    5.2     6.9         8          0.82       5.36
#>  9 onboardi… minimize…    6.55    6.95    6.55        9          0.78       5.87
#> 10 onboardi… minimize…    6.3     7.2     6.3        10          0.75       5.61
#> 11 onboardi… minimize…    4.15    2.15    6.15       11          0.73       3.49
#> 12 research… minimize…    5       4.65    5.35       12          0.63       4.31
#> # ℹ 8 more variables: imp_hi.all <dbl>, imp_se.all <dbl>, sat_lo.all <dbl>,
#> #   sat_hi.all <dbl>, sat_se.all <dbl>, opp_lo.all <dbl>, opp_hi.all <dbl>,
#> #   opp_se.all <dbl>
```
