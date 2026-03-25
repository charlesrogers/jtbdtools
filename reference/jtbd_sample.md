# Sample JTBD survey data

A synthetic dataset containing 200 survey respondents who rated the
importance and satisfaction of 12 objectives across 3 job steps on a 1-5
Likert scale. Includes a segmentation column with three user types that
have different response patterns.

## Usage

``` r
jtbd_sample
```

## Format

A data frame with 200 rows and 31 columns. Key columns include `caseid`
(respondent ID), `segment` (user type factor), demographic profiling
columns (`gender`, `age_group`, `income`, `education`, `tenure`), and 24
paired importance/satisfaction columns following the
`imp__step.objective` / `sat__step.objective` naming convention.

## Examples

``` r
data(jtbd_sample)
head(jtbd_sample)
#>   caseid    segment gender age_group   income    education      tenure
#> 1      1   new_user Female     18-24  $50-75k  High School   1-2 years
#> 2      2   new_user Female     25-34  $30-50k   Bachelor's 6-12 months
#> 3      3     casual   Male     25-34  $30-50k   Bachelor's 6-12 months
#> 4      4   new_user Female     25-34 $75-100k Some College  < 6 months
#> 5      5 power_user   Male       55+   $100k+     Graduate   1-2 years
#> 6      6 power_user   Male       55+ $75-100k     Graduate    2+ years
#>   imp__researching.minimize_time_to_find_options
#> 1                                              5
#> 2                                              5
#> 3                                              2
#> 4                                              3
#> 5                                              5
#> 6                                              5
#>   sat__researching.minimize_time_to_find_options
#> 1                                              3
#> 2                                              2
#> 3                                              4
#> 4                                              3
#> 5                                              2
#> 6                                              2
#>   imp__researching.minimize_time_to_evaluate_options
#> 1                                                  5
#> 2                                                  2
#> 3                                                  2
#> 4                                                  4
#> 5                                                  5
#> 6                                                  5
#>   sat__researching.minimize_time_to_evaluate_options
#> 1                                                  4
#> 2                                                  3
#> 3                                                  4
#> 4                                                  3
#> 5                                                  4
#> 6                                                  4
#>   imp__researching.minimize_likelihood_of_missing_relevant_options
#> 1                                                                3
#> 2                                                                4
#> 3                                                                4
#> 4                                                                4
#> 5                                                                4
#> 6                                                                5
#>   sat__researching.minimize_likelihood_of_missing_relevant_options
#> 1                                                                4
#> 2                                                                4
#> 3                                                                5
#> 4                                                                3
#> 5                                                                5
#> 6                                                                5
#>   imp__researching.minimize_time_to_understand_pricing
#> 1                                                    3
#> 2                                                    5
#> 3                                                    3
#> 4                                                    4
#> 5                                                    4
#> 6                                                    3
#>   sat__researching.minimize_time_to_understand_pricing
#> 1                                                    3
#> 2                                                    3
#> 3                                                    5
#> 4                                                    5
#> 5                                                    4
#> 6                                                    4
#>   imp__purchasing.minimize_time_to_complete_transaction
#> 1                                                     4
#> 2                                                     3
#> 3                                                     5
#> 4                                                     3
#> 5                                                     5
#> 6                                                     5
#>   sat__purchasing.minimize_time_to_complete_transaction
#> 1                                                     3
#> 2                                                     5
#> 3                                                     4
#> 4                                                     4
#> 5                                                     5
#> 6                                                     2
#>   imp__purchasing.minimize_likelihood_of_unexpected_costs
#> 1                                                       4
#> 2                                                       5
#> 3                                                       4
#> 4                                                       5
#> 5                                                       5
#> 6                                                       4
#>   sat__purchasing.minimize_likelihood_of_unexpected_costs
#> 1                                                       1
#> 2                                                       3
#> 3                                                       5
#> 4                                                       2
#> 5                                                       2
#> 6                                                       4
#>   imp__purchasing.minimize_time_to_receive_confirmation
#> 1                                                     3
#> 2                                                     4
#> 3                                                     5
#> 4                                                     3
#> 5                                                     5
#> 6                                                     5
#>   sat__purchasing.minimize_time_to_receive_confirmation
#> 1                                                     3
#> 2                                                     4
#> 3                                                     2
#> 4                                                     2
#> 5                                                     5
#> 6                                                     5
#>   imp__purchasing.minimize_likelihood_of_errors_in_order
#> 1                                                      5
#> 2                                                      4
#> 3                                                      4
#> 4                                                      5
#> 5                                                      4
#> 6                                                      4
#>   sat__purchasing.minimize_likelihood_of_errors_in_order
#> 1                                                      3
#> 2                                                      4
#> 3                                                      4
#> 4                                                      3
#> 5                                                      5
#> 6                                                      4
#>   imp__onboarding.minimize_time_to_get_started
#> 1                                            4
#> 2                                            4
#> 3                                            5
#> 4                                            4
#> 5                                            3
#> 6                                            3
#>   sat__onboarding.minimize_time_to_get_started
#> 1                                            2
#> 2                                            3
#> 3                                            2
#> 4                                            2
#> 5                                            3
#> 6                                            4
#>   imp__onboarding.minimize_likelihood_of_confusion_during_setup
#> 1                                                             4
#> 2                                                             5
#> 3                                                             4
#> 4                                                             3
#> 5                                                             4
#> 6                                                             4
#>   sat__onboarding.minimize_likelihood_of_confusion_during_setup
#> 1                                                             3
#> 2                                                             2
#> 3                                                             4
#> 4                                                             3
#> 5                                                             5
#> 6                                                             5
#>   imp__onboarding.minimize_time_to_reach_first_value
#> 1                                                  4
#> 2                                                  5
#> 3                                                  3
#> 4                                                  3
#> 5                                                  4
#> 6                                                  4
#>   sat__onboarding.minimize_time_to_reach_first_value
#> 1                                                  4
#> 2                                                  3
#> 3                                                  3
#> 4                                                  2
#> 5                                                  5
#> 6                                                  3
#>   imp__onboarding.minimize_likelihood_of_needing_support
#> 1                                                      4
#> 2                                                      5
#> 3                                                      2
#> 4                                                      4
#> 5                                                      3
#> 6                                                      4
#>   sat__onboarding.minimize_likelihood_of_needing_support
#> 1                                                      2
#> 2                                                      1
#> 3                                                      3
#> 4                                                      3
#> 5                                                      1
#> 6                                                      2

# Calculate opportunity scores
scores <- get_jtbd_scores(jtbd_sample)

# Compare segments
comparison <- get_jtbd_scores.comparison(jtbd_sample, "segment")
#> Found 3 segments with n > 30.
```
