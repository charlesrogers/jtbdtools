# jtbdtools: Jobs to be Done (JTBD) Analysis Tools

## Overview

`jtbdtools` is an R package that provides a comprehensive set of tools for analyzing Jobs to be Done (JTBD) data. It includes functions for calculating opportunity scores, performing segmentation analysis, visualizing JTBD data, and more.

## Installation

You can install the development version of jtbdtools from GitHub with:

devtools::install_github("[charlesrogers/jtbdtools](https://github.com/charlesrogers/jtbdtools)")

## Main Functions

- `get_jtbd_scores()`: Calculate JTBD scores
- `get_jtbd_scores.batch()`: Calculate JTBD scores for multiple segments
- `get_jtbd_clusters()`: Perform cluster analysis on JTBD data
- `plot_this.graph.rel.abs_score()`: Visualize relative and absolute scores
- `create.job_step.table()`: Create a table for job step analysis
- `get_jtbd_segment.comp()`: Compare JTBD segments

## Usage

Here's a basic example of how to use the `jtbdtools` package:


library(jtbdtools)

# Load your JTBD data
``` r
data <- read.csv("your_jtbd_data.csv")
```
# Key Functions
## Calculate JTBD scores
``` r
scores <- get_jtbd_scores(data, "column_suffix")
```
(I recommend using "all")

 
### get_jtbd_scores.batch()

This function calculates JTBD scores for multiple segments. It uses the output of `get_jtbd_var_values.list()` as input.

#### Usage:

##### First, get the list of segments

##### Then, use this list in the batch function 
###### Assuming 'master_table' is your initial data frame to update and 'static_segment' is the segment you want to compare against
``` r

batch_scores <- get_jtbd_scores.batch(
master_table = master_table,
data_frame = your_data_frame,
merged_df_and_segmentation_column = "segmentation_column",
static_segment = "your_static_segment",
list_of_unique_segments = segment_list
)

print(batch_scores)
```

## Contributing

Contributions to `jtbdtools` are welcome. Please feel free to submit a Pull Request.

## License

This project is licensed under the MIT License - see the LICENSE file for details.