# Introduction to jtbdtools

## What is Outcome-Driven Innovation?

Jobs-to-Be-Done (JTBD) theory says customers “hire” products to get a
job done. **Outcome-Driven Innovation** (ODI), developed by Tony Ulwick,
turns this into a quantitative framework: survey users on how
*important* each outcome is and how *satisfied* they are with current
solutions, then calculate where the biggest gaps are.

The **opportunity score** formula:

    opportunity = importance + max(0, importance - satisfaction)

When importance exceeds satisfaction, the gap amplifies the opportunity.
When satisfaction exceeds importance, opportunity equals importance (the
floor). Scores range from 0-20, with anything above 10 considered a
high-opportunity outcome.

## Quick Start

``` r
library(jtbdtools)
data(jtbd_sample)
```

The sample dataset contains 200 survey respondents rating 12 objectives
across 3 job steps:

``` r
# See the column structure
names(jtbd_sample)[1:8]
#> [1] "caseid"                                                          
#> [2] "segment"                                                         
#> [3] "imp__researching.minimize_time_to_find_options"                  
#> [4] "sat__researching.minimize_time_to_find_options"                  
#> [5] "imp__researching.minimize_time_to_evaluate_options"              
#> [6] "sat__researching.minimize_time_to_evaluate_options"              
#> [7] "imp__researching.minimize_likelihood_of_missing_relevant_options"
#> [8] "sat__researching.minimize_likelihood_of_missing_relevant_options"
```

### Calculate Scores

``` r
scores <- get_jtbd_scores(jtbd_sample)
scores[, c("job_step", "objective", "imp.all", "sat.all", "opp.all")]
#> # A tibble: 12 × 5
#>    job_step    objective                                 imp.all sat.all opp.all
#>    <chr>       <fct>                                       <dbl>   <dbl>   <dbl>
#>  1 researching minimize_time_to_evaluate_options            8.5     2.2    14.8 
#>  2 purchasing  minimize_likelihood_of_unexpected_costs      7.6     1.5    13.7 
#>  3 purchasing  minimize_time_to_receive_confirmation        8.4     4.2    12.6 
#>  4 onboarding  minimize_likelihood_of_confusion_during_…    7.35    2.1    12.6 
#>  5 researching minimize_time_to_understand_pricing          8.3     6.05   10.6 
#>  6 researching minimize_time_to_find_options                7.9     6.7     9.1 
#>  7 onboarding  minimize_likelihood_of_needing_support       7.85    6.7     9   
#>  8 onboarding  minimize_time_to_get_started                 7.2     6.15    8.25
#>  9 researching minimize_likelihood_of_missing_relevant_…    5.65    3.7     7.6 
#> 10 purchasing  minimize_time_to_complete_transaction        6.6     6.95    6.6 
#> 11 onboarding  minimize_time_to_reach_first_value           6       6.45    6   
#> 12 purchasing  minimize_likelihood_of_errors_in_order       3.85    3.85    3.85
```

The highest-opportunity objectives have high importance and low
satisfaction.

### Compare Segments

``` r
comparison <- get_jtbd_scores.comparison(jtbd_sample, "segment")
#> Found 3 segments with n > 30.
# Show opportunity scores by segment
opp_cols <- grep("^opp\\.", names(comparison), value = TRUE)
comparison[, c("job_step", "objective", opp_cols)]
#> # A tibble: 12 × 6
#>    job_step    objective          opp.all opp.casual opp.new_user opp.power_user
#>    <chr>       <fct>                <dbl>      <dbl>        <dbl>          <dbl>
#>  1 researching minimize_time_to_…   14.8       12.7         15.8           16.6 
#>  2 purchasing  minimize_likeliho…   13.7        8.52        16.2           18.1 
#>  3 purchasing  minimize_time_to_…   12.6       13.7         12.9           11.0 
#>  4 onboarding  minimize_likeliho…   12.6       11.2         11.9           14.8 
#>  5 researching minimize_time_to_…   10.6       11.7          9.42          10   
#>  6 researching minimize_time_to_…    9.1        8.15         9.42          10   
#>  7 onboarding  minimize_likeliho…    9          8.02         6.35          12.2 
#>  8 onboarding  minimize_time_to_…    8.25       9.88        10.8            6.12
#>  9 researching minimize_likeliho…    7.6       10.1          6.92           5.52
#> 10 purchasing  minimize_time_to_…    6.6        5.8          4.81          11.8 
#> 11 onboarding  minimize_time_to_…    6          5.19         8.46           5.82
#> 12 purchasing  minimize_likeliho…    3.85       5.8          2.31           4.33
```

### Opportunity Matrix

``` r
plot_opportunity_matrix(scores, subtitle = "Sample JTBD Survey (N=200)")
```

![Opportunity Score
Matrix](jtbdtools-introduction_files/figure-html/opportunity-matrix-1.png)

Opportunity Score Matrix

## Data Format

Your data must follow this column naming convention:

- `imp__job_step.objective_name` for importance columns
- `sat__job_step.objective_name` for satisfaction columns
- Values should be factors with levels 1-5

Example column names: -
`imp__researching.minimize_time_to_evaluate_options` -
`sat__researching.minimize_time_to_evaluate_options`

See
[`?jtbd_sample`](https://charlesrogers.github.io/jtbdtools/reference/jtbd_sample.md)
for a complete example dataset and
[`vignette("data-preparation")`](https://charlesrogers.github.io/jtbdtools/articles/data-preparation.md)
for SPSS import instructions.
