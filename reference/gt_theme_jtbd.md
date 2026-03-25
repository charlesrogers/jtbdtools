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

minimize_time_to_evaluate_options

8.50

2.20

14.80

purchasing

minimize_likelihood_of_unexpected_costs

7.60

1.50

13.70

purchasing

minimize_time_to_receive_confirmation

8.40

4.20

12.60

onboarding

minimize_likelihood_of_confusion_during_setup

7.35

2.10

12.60

researching

minimize_time_to_understand_pricing

8.30

6.05

10.55
