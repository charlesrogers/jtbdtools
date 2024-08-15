# jtbdtools: Jobs to be Done (JTBD) Analysis Tools

## Overview

`jtbdtools` is an R package that provides a comprehensive set of tools for analyzing Jobs to be Done (JTBD) data. It includes functions for calculating opportunity scores, performing segmentation analysis, visualizing JTBD data, and more.

## Installation

You can install the development version of jtbdtools from GitHub with:

devtools::install_github("[charlesrogers/jtbdtools](https://github.com/charlesrogers/jtbdtools)")

## Main Functions

- `get_jtbd_scores()`: Calculate JTBD scores
- `get_jtbd_scores.comparison()`: Calculate JTBD scores for multiple segments
- `plot_this.graph.rel.abs_score()`: Visualize relative and absolute scores

## JTBDtools Package Usage

Here's a basic example of how to use the `jtbdtools` package:


library(jtbdtools)

### Data Prep
``` r
data <- read.csv("your_jtbd_data.csv")
```
#### Data Format
This package is currently designed to work with data exported in the SPSS fileformat ".SAV". If you data is in some other format, you'll need to handle the data preparationfor analysis (this can be done a variety of ways, even with google sheets).
#### Import your SPSS data into R

```{r eval=FALSE}

 df_spss  <-  haven::read_sav("YOUR_FILE.sav")
# Check out the data and their labels via the Labeled package's Dictionary functionality
 dictionary <- labelled::generate_dictionary(df_spss)
```

#### Renaming Your Columns

To be honest, the hardest part of this package is getting the data in a format that the package can accept. 

Each column needs to be labelled in the following format:

* "imp__"/"sat__" + "your_job_step_name" + ".name_of_job"
* No spaces anywhere
* Job Step and Name of Jobs must literally __exactly__ match^[I have a MAJOR trauma because somehow a "no break space" character (unicode U+00A0, "space" + "option" key on mac) got inserted instead of spaces for the satisfaction section when exporting data from SurveyMonkey. It took me hours (days?) of debugging to find out why they weren't matching as these are invisible in excel format.] between the importance & satisfaction columns--the only thing that should be different is the "imp" or "sat" prefix.

Here are a few sample column names:

* imp__researching.minimize_time_to_evaluate_options
* sat__researching.minimize_time_to_evaluate_options
* imp__purchasing.minimize_time_to_receive_payment_confirmation
* sat__purchasing.minimize_time_to_receive_payment_confirmation

Here "researching" and "purchasing" are the job steps, and the strings that start with "minimize_time_to..." are the objectives

##### Labeling SPSS (.sav) Data Using QuantJTBD

Load the data (ideally via SPSS) because we want to use the column labels in the graphics so you know which scores correspond to which objectives.
The `build_imp_column_names` and `build_sat_column_names` functions primarily renames the column names (it also change it to factor).

Currently it accepts a standard format given from survey instruments which is:
`DistinctQuestionId_Question_Text - How Important...` for importance columns and `DistinctQuestionId_Question_Text - How Satisfied...` for satisfaction.

If your data isn't in this format, you'll have to rename them manually (which isn't so bad likely using `mutate`)

Here is specifically what `build_imp_column_names` and `build_sat_column_names` do:

+ Swap the labels and the column names
+ Remove the prefix by splitting on space
+ Remove the suffix by splitting on "-"
+ Change any spaces in the names to underscores
+ Prefix the updated columns with the needed `imp__`, `sat__`, and `.job_ _section` portions of the column name
+ Change all `haven` data labels to factors (not a renaming task, but it's part of this function for now)

```{r eval=FALSE}
# Example
df_renamed_imp_cols <- build_sat_column_names(df_imp_only_cols,"Job_Step_Name")
df_renamed_sat_cols <- build_imp_column_names(df_sat_only_cols,"Job_Step_Name")
df_merged_imp_sat_cols <- cbind(df_renamed_imp_cols,df_renamed_sat_cols) 
```

#### Renaming the columns some other way

Column naming __RULES__ in order to be able to use the package:

* Each job step (`imp__JOB_STEP.job_name`) must be DISTINCT from other job steps
* Each job name (`imp__job_step.JOB_NAME`) must be DISTINCT from other job names
* Each job step and job name (`imp__JOB_STEP.JOB_NAME`) must be IDENTICAL between the importance and satisfaction columns--the only thing that changes is the prefix: `imp__researching.minimize_time_to_evaluate_options`  `sat__researching.minimize_time_to_evaluate_options`
* You must have an importance and satisfaction column for every job you want to calculate  
* No spaces in the column name
* __Data must be in factor format__ 

### Data Analysis
#### Calculate JTBD Scores for 1 Group
If you would like to calcuate the scores for one group (no comparisons between groups), use the `get_jtbd_scores` function
``` r
scores <- get_jtbd_scores(data, "column_suffix")
```
The `column_suffix` parameter is optional and will default to "all", and if left blank, your columns will read:
| Job Step | Objective | imp.all | sat.all | opp.all |
| -------- | ------- | -------- | ------- | -------- |
| Researching | Minimize time to ... | 8.4 | 6.5 |  10.3|

#### Calculate JTBD Scores for 1 Group
##### get_jtbd_scores.comparison()

This function calculates JTBD scores for multiple values of a factor, for example:
Let's say you want to compare the jtbd scores for males vs females.
If your dataframe is `data` and the column name containing gender is called `var.gender`, you would use the `get_jtbd_scores.comparison()` as follows:
```r
get_jtbd_scores.comparison(data,"var.gender")
```
This would print a table containing 
| Job Step | Objective | imp.male | imp.female | sat.male | sat.female | opp.male | opp.female |
| -------- | ------- | -------- | ------- | -------- | -------- | -------- | -------- | 
| Researching | Minimize time to ... | 7.4 | 8.6 | 4.5 |7.7| 10.4 | 9.5|

### Quant JTBD Methodology

The scores are calculated in the traditional Outcome Driven Innovation way:
```{r eval=FALSE}
importance + importance - satisfaction
example: 8.4 + 8.4 - 6.7 = 10.1
```
To get a bit more specific, since someone's satisfaction with a job doesn't change how important it is, importance should be the floor for opportunity, so we just use the importance score as the opportunity score:
```{r eval=FALSE}
importance + (if importance < satisfaction, 0, - satisfaction)
incorrect calculation: 6.7 + 6.7 - 8.4 = 5
correct calculation: 6.7 + (if 8.4 > 6.7, 0, else 6.7 - 8.4) = 6.7

```

## Contributing

Contributions to `jtbdtools` are welcome. Please feel free to submit a Pull Request.

## License

This project is licensed under the MIT License - see the LICENSE file for details.
