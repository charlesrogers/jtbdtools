# Calculate JTBD scores for multiple segments

Compares opportunity scores across all values of a segmentation
variable. This is the main function for segment comparison analysis.
Optionally runs Wilcoxon rank-sum tests to identify statistically
significant differences between each segment and the overall population.

## Usage

``` r
get_jtbd_scores.comparison(
  data_frame,
  segmentation_column,
  test_sig = FALSE,
  alpha = 0.05
)
```

## Arguments

- data_frame:

  A data frame with `imp__`/`sat__` columns and a segmentation column

- segmentation_column:

  The name of the column to segment by (as string)

- test_sig:

  If TRUE, run statistical significance tests and add p-value columns
  (default: FALSE)

- alpha:

  Significance level for tests (default: 0.05)

## Value

A data frame with imp/sat/opp scores for each segment. When
`test_sig = TRUE`, additional columns `p.imp.<segment>`,
`p.sat.<segment>`, `sig.imp.<segment>`, `sig.sat.<segment>` are
included.

## See also

Other segmentation:
[`get_jtbd_scores.batch()`](https://charlesrogers.github.io/jtbdtools/reference/get_jtbd_scores.batch.md),
[`get_jtbd_scores.pairwise()`](https://charlesrogers.github.io/jtbdtools/reference/get_jtbd_scores.pairwise.md),
[`get_jtbd_segment.comp.ordinal()`](https://charlesrogers.github.io/jtbdtools/reference/get_jtbd_segment.comp.ordinal.md),
[`get_jtbd_var_values.list()`](https://charlesrogers.github.io/jtbdtools/reference/get_jtbd_var_values.list.md),
[`get_seg_comp.build.linear()`](https://charlesrogers.github.io/jtbdtools/reference/get_seg_comp.build.linear.md),
[`test_segment_significance()`](https://charlesrogers.github.io/jtbdtools/reference/test_segment_significance.md)

## Examples

``` r
data(jtbd_sample)
# Without significance testing
comparison <- get_jtbd_scores.comparison(jtbd_sample, "segment")
#> Found 3 segments with n > 30.

# With significance testing
comparison_sig <- get_jtbd_scores.comparison(jtbd_sample, "segment", test_sig = TRUE)
#> Found 3 segments with n > 30.
# Look at p-values
comparison_sig[, grep("^(objective|p\\.|sig\\.)", names(comparison_sig))]
#> # A tibble: 12 × 13
#>    objective             p.imp.casual p.sat.casual sig.imp.casual sig.sat.casual
#>    <chr>                        <dbl>        <dbl> <lgl>          <lgl>         
#>  1 minimize_time_to_fin…       0.964        0.939  FALSE          FALSE         
#>  2 minimize_likelihood_…       0.0198       0.0049 TRUE           TRUE          
#>  3 minimize_likelihood_…       0            0      TRUE           TRUE          
#>  4 minimize_likelihood_…       0.248        0.001  FALSE          TRUE          
#>  5 minimize_likelihood_…       0.0222       0.675  TRUE           FALSE         
#>  6 minimize_time_to_com…       0.304        0.0101 FALSE          TRUE          
#>  7 minimize_time_to_eva…       0.0001       0.0006 TRUE           TRUE          
#>  8 minimize_time_to_rec…       0.479        0.181  FALSE          FALSE         
#>  9 minimize_likelihood_…       0.313        0.0001 FALSE          TRUE          
#> 10 minimize_time_to_rea…       0.0389       0.188  TRUE           FALSE         
#> 11 minimize_time_to_get…       0.0027       0.0564 TRUE           FALSE         
#> 12 minimize_time_to_und…       0.580        0.702  FALSE          FALSE         
#> # ℹ 8 more variables: p.imp.new_user <dbl>, p.sat.new_user <dbl>,
#> #   sig.imp.new_user <lgl>, sig.sat.new_user <lgl>, p.imp.power_user <dbl>,
#> #   p.sat.power_user <dbl>, sig.imp.power_user <lgl>, sig.sat.power_user <lgl>
```
