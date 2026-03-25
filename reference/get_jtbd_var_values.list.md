# Get unique segment values

Extracts unique values from a segmentation column, filtering to segments
with at least `min_n` respondents.

## Usage

``` r
get_jtbd_var_values.list(data_frame, segmentation_column, min_n = 30)
```

## Arguments

- data_frame:

  A data frame containing the segmentation column

- segmentation_column:

  The name of the segmentation column (as string)

- min_n:

  Minimum number of respondents to include a segment (default: 30)

## Value

A character vector of segment values

## See also

Other segmentation:
[`get_jtbd_scores.batch()`](https://charlesrogers.github.io/jtbdtools/reference/get_jtbd_scores.batch.md),
[`get_jtbd_scores.comparison()`](https://charlesrogers.github.io/jtbdtools/reference/get_jtbd_scores.comparison.md),
[`get_jtbd_scores.pairwise()`](https://charlesrogers.github.io/jtbdtools/reference/get_jtbd_scores.pairwise.md),
[`get_jtbd_segment.comp.ordinal()`](https://charlesrogers.github.io/jtbdtools/reference/get_jtbd_segment.comp.ordinal.md),
[`get_seg_comp.build.linear()`](https://charlesrogers.github.io/jtbdtools/reference/get_seg_comp.build.linear.md)

## Examples

``` r
data(jtbd_sample)
get_jtbd_var_values.list(jtbd_sample, "segment", min_n = 30)
#> Found 3 segments with n > 30.
#> [1] casual     new_user   power_user
#> Levels: casual new_user power_user
```
