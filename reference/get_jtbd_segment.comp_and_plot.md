# Compare and plot JTBD segment scores

Generates all-scores and opportunity-only plots for a segment
comparison.

## Usage

``` r
get_jtbd_segment.comp_and_plot(
  your_data_frame,
  save_path = tempdir(),
  n = NULL,
  study = NULL
)
```

## Arguments

- your_data_frame:

  A data frame containing JTBD scores

- save_path:

  Directory to save plots (default:
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html))

- n, study:

  Sample size and study label for the plot footer (see
  [`jtbd_footer()`](https://charlesrogers.github.io/jtbdtools/reference/jtbd_footer.md)).

## Value

NULL (called for side effects)
