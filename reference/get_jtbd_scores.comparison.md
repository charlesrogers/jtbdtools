# Calculate JTBD scores for multiple segments

Compares opportunity scores across all values of a segmentation
variable. This is the main function for segment comparison analysis.

## Usage

``` r
get_jtbd_scores.comparison(data_frame, segmentation_column)
```

## Arguments

- data_frame:

  A data frame with `imp__`/`sat__` columns and a segmentation column

- segmentation_column:

  The name of the column to segment by (as string)

## Value

A data frame with imp/sat/opp scores for each segment, joined together

## See also

Other segmentation:
[`get_jtbd_scores.batch()`](https://charlesrogers.github.io/jtbdtools/reference/get_jtbd_scores.batch.md),
[`get_jtbd_scores.pairwise()`](https://charlesrogers.github.io/jtbdtools/reference/get_jtbd_scores.pairwise.md),
[`get_jtbd_segment.comp.ordinal()`](https://charlesrogers.github.io/jtbdtools/reference/get_jtbd_segment.comp.ordinal.md),
[`get_jtbd_var_values.list()`](https://charlesrogers.github.io/jtbdtools/reference/get_jtbd_var_values.list.md),
[`get_seg_comp.build.linear()`](https://charlesrogers.github.io/jtbdtools/reference/get_seg_comp.build.linear.md)

## Examples

``` r
data(jtbd_sample)
comparison <- get_jtbd_scores.comparison(jtbd_sample, "segment")
#> Found 3 segments with n > 30.
head(comparison)
#> # A tibble: 6 × 22
#>   job_step   objective imp.all sat.all opp.all rank.all opp_index.all imp.casual
#>   <chr>      <fct>       <dbl>   <dbl>   <dbl>    <dbl>         <dbl>      <dbl>
#> 1 researchi… minimize…    8.5     2.2     14.8        1          1.64       8.02
#> 2 purchasing minimize…    7.6     1.5     13.7        2          1.51       5.8 
#> 3 purchasing minimize…    8.4     4.2     12.6        3          1.39       8.77
#> 4 onboarding minimize…    7.35    2.1     12.6        4          1.39       7.16
#> 5 researchi… minimize…    8.3     6.05    10.6        5          1.17       8.52
#> 6 researchi… minimize…    7.9     6.7      9.1        6          1.01       7.78
#> # ℹ 14 more variables: sat.casual <dbl>, opp.casual <dbl>, rank.casual <dbl>,
#> #   opp_index.casual <dbl>, imp.new_user <dbl>, sat.new_user <dbl>,
#> #   opp.new_user <dbl>, rank.new_user <dbl>, opp_index.new_user <dbl>,
#> #   imp.power_user <dbl>, sat.power_user <dbl>, opp.power_user <dbl>,
#> #   rank.power_user <dbl>, opp_index.power_user <dbl>
```
