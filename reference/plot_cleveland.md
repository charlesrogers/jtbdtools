# Cleveland (lollipop) comparison plot

Compares two segments side-by-side using a Cleveland dot plot. Ported
from the quantjtbd package. If `data` carries `<col>_lo`/`<col>_hi`
bounds for either group (matching the `group_1`/`group_2` column names),
horizontal error bars are drawn on each dot.

## Usage

``` r
plot_cleveland(
  data,
  objective,
  group_1,
  group_2,
  title_string,
  subtitle_string,
  n = NULL,
  study = NULL
)
```

## Arguments

- data:

  A data frame with objective and segment score columns

- objective:

  Unquoted column name for objectives

- group_1:

  Unquoted column name for first segment scores

- group_2:

  Unquoted column name for second segment scores

- title_string:

  Plot title

- subtitle_string:

  Plot subtitle

- n, study:

  Sample size and study label for the plot footer (see
  [`jtbd_footer()`](https://charlesrogers.github.io/jtbdtools/reference/jtbd_footer.md)).

## Value

A ggplot object

## See also

Other visualization:
[`jtbd_footer()`](https://charlesrogers.github.io/jtbdtools/reference/jtbd_footer.md),
[`plot.job_step()`](https://charlesrogers.github.io/jtbdtools/reference/plot.job_step.md),
[`plot_opportunity_matrix()`](https://charlesrogers.github.io/jtbdtools/reference/plot_opportunity_matrix.md),
[`plot_this.graph.abs_score()`](https://charlesrogers.github.io/jtbdtools/reference/plot_this.graph.abs_score.md),
[`plot_this.graph.rel.abs_score()`](https://charlesrogers.github.io/jtbdtools/reference/plot_this.graph.rel.abs_score.md),
[`plot_this.graph.rel_score()`](https://charlesrogers.github.io/jtbdtools/reference/plot_this.graph.rel_score.md),
[`theme_jtbd()`](https://charlesrogers.github.io/jtbdtools/reference/theme_jtbd.md)
