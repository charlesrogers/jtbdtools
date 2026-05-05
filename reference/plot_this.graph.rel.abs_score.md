# Plot relative and absolute score graphs

Generates both relative (percent-of-segment-max) and absolute score bump
charts for segment comparison. Convenience wrapper around
[`plot_this.graph.rel_score()`](https://charlesrogers.github.io/jtbdtools/reference/plot_this.graph.rel_score.md)
and
[`plot_this.graph.abs_score()`](https://charlesrogers.github.io/jtbdtools/reference/plot_this.graph.abs_score.md).

## Usage

``` r
plot_this.graph.rel.abs_score(
  your_data_frame,
  seg_value_title,
  last_value,
  save_path = tempdir(),
  n = NULL,
  study = NULL
)
```

## Arguments

- your_data_frame:

  A data frame from
  [`get_jtbd_segment.comp.ordinal()`](https://charlesrogers.github.io/jtbdtools/reference/get_jtbd_segment.comp.ordinal.md)

- seg_value_title:

  Title label for the segment variable

- last_value:

  The segment value to label on the right side of the chart

- save_path:

  Directory to save plots (default:
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html))

- n, study:

  Sample size and study label for the plot footer (see
  [`jtbd_footer()`](https://charlesrogers.github.io/jtbdtools/reference/jtbd_footer.md)).

## Value

NULL (called for side effects: generates and saves plots)

## See also

Other visualization:
[`jtbd_footer()`](https://charlesrogers.github.io/jtbdtools/reference/jtbd_footer.md),
[`plot.job_step()`](https://charlesrogers.github.io/jtbdtools/reference/plot.job_step.md),
[`plot_cleveland()`](https://charlesrogers.github.io/jtbdtools/reference/plot_cleveland.md),
[`plot_opportunity_matrix()`](https://charlesrogers.github.io/jtbdtools/reference/plot_opportunity_matrix.md),
[`plot_this.graph.abs_score()`](https://charlesrogers.github.io/jtbdtools/reference/plot_this.graph.abs_score.md),
[`plot_this.graph.rel_score()`](https://charlesrogers.github.io/jtbdtools/reference/plot_this.graph.rel_score.md),
[`theme_jtbd()`](https://charlesrogers.github.io/jtbdtools/reference/theme_jtbd.md)
