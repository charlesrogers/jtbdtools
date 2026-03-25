# Data Preparation Guide

## Column Naming Convention

jtbdtools requires a specific column naming format:

    imp__job_step.objective_name
    sat__job_step.objective_name

Rules:

- **Prefix**: `imp__` for importance, `sat__` for satisfaction (double
  underscore)
- **Job step and objective** separated by `.` (period)
- **No spaces** anywhere in the name
- Each importance column must have a matching satisfaction column
- Data must be **factor** format with levels 1-5

### Example Column Names

    imp__researching.minimize_time_to_evaluate_options
    sat__researching.minimize_time_to_evaluate_options
    imp__purchasing.minimize_time_to_receive_confirmation
    sat__purchasing.minimize_time_to_receive_confirmation

## Importing SPSS Data

If your data is in SPSS format (.sav):

``` r
library(haven)
library(labelled)

# Import
df_spss <- haven::read_sav("your_survey.sav")

# Inspect labels
dictionary <- labelled::generate_dictionary(df_spss)
```

### Using jtbdtools Rename Functions

The
[`build_imp_column_names()`](https://charlesrogers.github.io/jtbdtools/reference/build_imp_column_names.md)
and
[`build_sat_column_names()`](https://charlesrogers.github.io/jtbdtools/reference/build_sat_column_names.md)
functions handle SPSS data that follows the standard survey instrument
format:

    DistinctQuestionId_Question_Text - How Important...
    DistinctQuestionId_Question_Text - How Satisfied...

They will:

1.  Swap SPSS labels into column names
2.  Remove the prefix (before `--`)
3.  Remove the suffix (after `-`)
4.  Replace spaces with underscores
5.  Add the `imp__`/`sat__` prefix and job section
6.  Convert labelled data to factors

``` r
library(jtbdtools)

# Separate importance and satisfaction columns first
df_imp <- df_spss[, imp_column_indices]
df_sat <- df_spss[, sat_column_indices]

# Rename with job section
df_imp_renamed <- build_imp_column_names(df_imp, "researching")
df_sat_renamed <- build_sat_column_names(df_sat, "researching")

# Combine
df_ready <- cbind(df_imp_renamed, df_sat_renamed)
```

### Manual Renaming

If your data isn’t in SPSS format, rename columns manually:

``` r
library(dplyr)

df <- your_data %>%
  rename(
    imp__researching.minimize_time_to_find_options = q1_importance,
    sat__researching.minimize_time_to_find_options = q1_satisfaction,
    imp__researching.minimize_time_to_evaluate_options = q2_importance,
    sat__researching.minimize_time_to_evaluate_options = q2_satisfaction
  ) %>%
  mutate(across(starts_with("imp__"), ~ factor(., levels = 1:5))) %>%
  mutate(across(starts_with("sat__"), ~ factor(., levels = 1:5)))
```

## Common Pitfalls

1.  **Invisible characters**: SurveyMonkey exports sometimes include
    Unicode no-break spaces (U+00A0) instead of regular spaces. These
    are invisible in Excel but cause column matching to fail.

2.  **Factor levels**: Columns must be factors with levels 1-5, not
    numeric or character.

3.  **Matching pairs**: Every `imp__` column must have an exactly
    matching `sat__` column (same job step and objective name).
