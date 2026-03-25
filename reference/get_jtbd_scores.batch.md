# Batch calculate JTBD scores for multiple segments

Iterates over a list of segments, calculating pairwise scores against a
static reference segment and joining results into a master table.

## Usage

``` r
get_jtbd_scores.batch(
  master_table,
  data_frame,
  merged_df_and_segmentation_column,
  static_segment,
  list_of_unique_segments
)
```

## Arguments

- master_table:

  The main data frame to update

- data_frame:

  The data frame containing JTBD data

- merged_df_and_segmentation_column:

  The column used for segmentation

- static_segment:

  The static segment to compare against

- list_of_unique_segments:

  A list of unique segments to analyze

## Value

An updated master table with JTBD scores for each segment

## See also

Other segmentation:
[`get_jtbd_scores.comparison()`](https://charlesrogers.github.io/jtbdtools/reference/get_jtbd_scores.comparison.md),
[`get_jtbd_scores.pairwise()`](https://charlesrogers.github.io/jtbdtools/reference/get_jtbd_scores.pairwise.md),
[`get_jtbd_segment.comp.ordinal()`](https://charlesrogers.github.io/jtbdtools/reference/get_jtbd_segment.comp.ordinal.md),
[`get_jtbd_var_values.list()`](https://charlesrogers.github.io/jtbdtools/reference/get_jtbd_var_values.list.md),
[`get_seg_comp.build.linear()`](https://charlesrogers.github.io/jtbdtools/reference/get_seg_comp.build.linear.md),
[`test_segment_significance()`](https://charlesrogers.github.io/jtbdtools/reference/test_segment_significance.md)
