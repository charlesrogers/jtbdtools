# Plot absolute score bump chart

Shows absolute opportunity scores across segments with linear
significance labeling.

## Usage

``` r
plot_this.graph.abs_score(
  your_data_frame,
  seg_value_title,
  last_value,
  save_path = tempdir()
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

  Directory to save the plot (default:
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html))

## Value

A ggplot object (also saved as PNG)

## See also

Other visualization:
[`plot.job_step()`](https://charlesrogers.github.io/jtbdtools/reference/plot.job_step.md),
[`plot_cleveland()`](https://charlesrogers.github.io/jtbdtools/reference/plot_cleveland.md),
[`plot_opportunity_matrix()`](https://charlesrogers.github.io/jtbdtools/reference/plot_opportunity_matrix.md),
[`plot_this.graph.rel.abs_score()`](https://charlesrogers.github.io/jtbdtools/reference/plot_this.graph.rel.abs_score.md),
[`plot_this.graph.rel_score()`](https://charlesrogers.github.io/jtbdtools/reference/plot_this.graph.rel_score.md),
[`theme_jtbd()`](https://charlesrogers.github.io/jtbdtools/reference/theme_jtbd.md)
