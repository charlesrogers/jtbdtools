# Test statistical significance between two segments

Runs Wilcoxon rank-sum (Mann-Whitney U) tests on raw Likert responses
for each importance and satisfaction objective, comparing two segments.
This is a non-parametric test appropriate for ordinal survey data.

## Usage

``` r
test_segment_significance(
  data_frame,
  segmentation_column,
  segment_a,
  segment_b,
  alpha = 0.05
)
```

## Arguments

- data_frame:

  The full data frame with `imp__`/`sat__` columns and a segmentation
  column

- segmentation_column:

  The name of the segmentation column (as string)

- segment_a:

  First segment value to compare

- segment_b:

  Second segment value to compare

- alpha:

  Significance level (default: 0.05)

## Value

A tibble with columns: objective, p.imp, p.sat, sig.imp, sig.sat

## See also

Other segmentation:
[`get_jtbd_scores.batch()`](https://charlesrogers.github.io/jtbdtools/reference/get_jtbd_scores.batch.md),
[`get_jtbd_scores.comparison()`](https://charlesrogers.github.io/jtbdtools/reference/get_jtbd_scores.comparison.md),
[`get_jtbd_scores.pairwise()`](https://charlesrogers.github.io/jtbdtools/reference/get_jtbd_scores.pairwise.md),
[`get_jtbd_segment.comp.ordinal()`](https://charlesrogers.github.io/jtbdtools/reference/get_jtbd_segment.comp.ordinal.md),
[`get_jtbd_var_values.list()`](https://charlesrogers.github.io/jtbdtools/reference/get_jtbd_var_values.list.md),
[`get_seg_comp.build.linear()`](https://charlesrogers.github.io/jtbdtools/reference/get_seg_comp.build.linear.md)

## Examples

``` r
data(jtbd_sample)
sig <- test_segment_significance(jtbd_sample, "segment", "casual", "power_user")
sig
#> # A tibble: 12 × 6
#>    job_step    objective                            p.imp  p.sat sig.imp sig.sat
#>    <chr>       <chr>                                <dbl>  <dbl> <lgl>   <lgl>  
#>  1 researching minimize_time_to_find_options       0.630  0.0936 FALSE   FALSE  
#>  2 researching minimize_time_to_evaluate_options   0      0.0033 TRUE    TRUE   
#>  3 researching minimize_likelihood_of_missing_rel… 0.903  0.0398 FALSE   TRUE   
#>  4 researching minimize_time_to_understand_pricing 0.166  0.793  FALSE   FALSE  
#>  5 purchasing  minimize_time_to_complete_transact… 0.0021 0.076  TRUE    FALSE  
#>  6 purchasing  minimize_likelihood_of_unexpected_… 0      0      TRUE    TRUE   
#>  7 purchasing  minimize_time_to_receive_confirmat… 0.879  0.0024 FALSE   TRUE   
#>  8 purchasing  minimize_likelihood_of_errors_in_o… 0.245  0.0004 FALSE   TRUE   
#>  9 onboarding  minimize_time_to_get_started        0.0001 0.0018 TRUE    TRUE   
#> 10 onboarding  minimize_likelihood_of_confusion_d… 0.0012 0.0267 TRUE    TRUE   
#> 11 onboarding  minimize_time_to_reach_first_value  0.133  0.148  FALSE   FALSE  
#> 12 onboarding  minimize_likelihood_of_needing_sup… 0.015  0.0336 TRUE    TRUE   
```
