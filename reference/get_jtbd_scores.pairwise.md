# Calculate JTBD scores for a pair of segments

Compares two specific segment values head-to-head.

## Usage

``` r
get_jtbd_scores.pairwise(
  your_data_frame,
  column_to_split_on,
  factor_a,
  factor_b
)
```

## Arguments

- your_data_frame:

  The data frame containing JTBD data

- column_to_split_on:

  The column used for segmentation

- factor_a:

  The first segment value

- factor_b:

  The second segment value

## Value

A data frame with imp/sat/opp for both segments

## See also

Other segmentation:
[`get_jtbd_scores.batch()`](https://charlesrogers.github.io/jtbdtools/reference/get_jtbd_scores.batch.md),
[`get_jtbd_scores.comparison()`](https://charlesrogers.github.io/jtbdtools/reference/get_jtbd_scores.comparison.md),
[`get_jtbd_segment.comp.ordinal()`](https://charlesrogers.github.io/jtbdtools/reference/get_jtbd_segment.comp.ordinal.md),
[`get_jtbd_var_values.list()`](https://charlesrogers.github.io/jtbdtools/reference/get_jtbd_var_values.list.md),
[`get_seg_comp.build.linear()`](https://charlesrogers.github.io/jtbdtools/reference/get_seg_comp.build.linear.md)
