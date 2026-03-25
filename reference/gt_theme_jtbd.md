# jtbdtools gt table theme

A branded gt theme for JTBD tables. Clean, left-aligned, with
heat-mapped numeric columns and professional typography. Inspired by the
quantjtbd `gt_theme.yellow` but using the jtbdtools color palette.

## Usage

``` r
gt_theme_jtbd(gt_object, ...)
```

## Arguments

- gt_object:

  A gt table object

- ...:

  Additional arguments (unused)

## Value

A styled gt table object

## See also

Other tables:
[`create.job_step.table()`](https://charlesrogers.github.io/jtbdtools/reference/create.job_step.table.md),
[`create.pct.table()`](https://charlesrogers.github.io/jtbdtools/reference/create.pct.table.md),
[`theme.job_step()`](https://charlesrogers.github.io/jtbdtools/reference/theme.job_step.md)

## Examples

``` r
library(gt)
data(jtbd_sample)
scores <- get_jtbd_scores(jtbd_sample)
scores[1:5, c("job_step", "objective", "imp.all", "sat.all", "opp.all")] |>
  gt() |>
  gt_theme_jtbd()


  

job_step
```

objective

imp.all

sat.all

opp.all

researching

minimize_time_to_find_options

7.75

2.60

12.90

purchasing

minimize_likelihood_of_errors_in_order

8.65

6.10

11.20

purchasing

minimize_likelihood_of_unexpected_costs

7.40

4.55

10.25

onboarding

minimize_likelihood_of_needing_support

6.25

2.30

10.20

researching

minimize_likelihood_of_missing_relevant_options

7.70

6.00

9.40
