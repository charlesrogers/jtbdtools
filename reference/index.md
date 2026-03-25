# Package index

## Scoring

Calculate opportunity scores from survey data

- [`get_jtbd_scores()`](https://charlesrogers.github.io/jtbdtools/reference/get_jtbd_scores.md)
  : Calculate JTBD opportunity scores
- [`get_jtbd_scores.comparison()`](https://charlesrogers.github.io/jtbdtools/reference/get_jtbd_scores.comparison.md)
  : Calculate JTBD scores for multiple segments
- [`get_jtbd_scores.pairwise()`](https://charlesrogers.github.io/jtbdtools/reference/get_jtbd_scores.pairwise.md)
  : Calculate JTBD scores for a pair of segments
- [`get_jtbd_scores.batch()`](https://charlesrogers.github.io/jtbdtools/reference/get_jtbd_scores.batch.md)
  : Batch calculate JTBD scores for multiple segments
- [`get_jtbd_scores.individual()`](https://charlesrogers.github.io/jtbdtools/reference/get_jtbd_scores.individual.md)
  : Calculate individual JTBD scores
- [`calculate_opportunity_score()`](https://charlesrogers.github.io/jtbdtools/reference/calculate_opportunity_score.md)
  : Calculate opportunity score
- [`calculate_pop_pct_score()`](https://charlesrogers.github.io/jtbdtools/reference/calculate_pop_pct_score.md)
  : Calculate population percentage score
- [`find_imp_sat_columns()`](https://charlesrogers.github.io/jtbdtools/reference/find_imp_sat_columns.md)
  : Find importance and satisfaction columns
- [`split_imp_sat_columns()`](https://charlesrogers.github.io/jtbdtools/reference/split_imp_sat_columns.md)
  : Split importance and satisfaction columns
- [`get_sample_size()`](https://charlesrogers.github.io/jtbdtools/reference/get_sample_size.md)
  : Get sample size

## Visualization

Plot opportunity data

- [`plot_opportunity_matrix()`](https://charlesrogers.github.io/jtbdtools/reference/plot_opportunity_matrix.md)
  : Opportunity matrix plot
- [`plot_cleveland()`](https://charlesrogers.github.io/jtbdtools/reference/plot_cleveland.md)
  : Cleveland (lollipop) comparison plot
- [`plot_this.graph.rel.abs_score()`](https://charlesrogers.github.io/jtbdtools/reference/plot_this.graph.rel.abs_score.md)
  : Plot relative and absolute score graphs
- [`plot_this.graph.rel_score()`](https://charlesrogers.github.io/jtbdtools/reference/plot_this.graph.rel_score.md)
  : Plot relative score bump chart
- [`plot_this.graph.abs_score()`](https://charlesrogers.github.io/jtbdtools/reference/plot_this.graph.abs_score.md)
  : Plot absolute score bump chart
- [`plot(`*`<job_step>`*`)`](https://charlesrogers.github.io/jtbdtools/reference/plot.job_step.md)
  : Plot job step scatter
- [`theme_jtbd()`](https://charlesrogers.github.io/jtbdtools/reference/theme_jtbd.md)
  : jtbdtools ggplot2 theme
- [`jtbd_colors()`](https://charlesrogers.github.io/jtbdtools/reference/jtbd_colors.md)
  : JTBD color palette

## Tables

Publication-ready gt tables

- [`gt_theme_jtbd()`](https://charlesrogers.github.io/jtbdtools/reference/gt_theme_jtbd.md)
  : jtbdtools gt table theme
- [`theme.job_step()`](https://charlesrogers.github.io/jtbdtools/reference/theme.job_step.md)
  : Format job step as gt table
- [`create.job_step.table()`](https://charlesrogers.github.io/jtbdtools/reference/create.job_step.table.md)
  : Create and save job step table
- [`create.pct.table()`](https://charlesrogers.github.io/jtbdtools/reference/create.pct.table.md)
  : Create percentage table

## Import & Data Prep

Import survey data and prepare for analysis

- [`prep_qualtrics()`](https://charlesrogers.github.io/jtbdtools/reference/prep_qualtrics.md)
  : Prepare Qualtrics survey data for JTBD analysis
- [`prep_survey()`](https://charlesrogers.github.io/jtbdtools/reference/prep_survey.md)
  : Prepare survey data for JTBD analysis
- [`read_qualtrics()`](https://charlesrogers.github.io/jtbdtools/reference/read_qualtrics.md)
  : Read a Qualtrics CSV export
- [`detect_imp_sat()`](https://charlesrogers.github.io/jtbdtools/reference/detect_imp_sat.md)
  : Detect importance and satisfaction columns
- [`validate_jtbd_data()`](https://charlesrogers.github.io/jtbdtools/reference/validate_jtbd_data.md)
  : Validate JTBD data format
- [`prep_data()`](https://charlesrogers.github.io/jtbdtools/reference/prep_data.md)
  : Prepare data for analysis
- [`build_imp_column_names()`](https://charlesrogers.github.io/jtbdtools/reference/build_imp_column_names.md)
  : Build importance column names
- [`build_sat_column_names()`](https://charlesrogers.github.io/jtbdtools/reference/build_sat_column_names.md)
  : Build satisfaction column names
- [`convert_labels_to_row_names()`](https://charlesrogers.github.io/jtbdtools/reference/convert_labels_to_row_names.md)
  : Convert labels to column names
- [`remove_data_prefix()`](https://charlesrogers.github.io/jtbdtools/reference/remove_data_prefix.md)
  : Remove data prefix
- [`remove_data_suffix()`](https://charlesrogers.github.io/jtbdtools/reference/remove_data_suffix.md)
  : Remove data suffix
- [`replace_spaces_with_underscores()`](https://charlesrogers.github.io/jtbdtools/reference/replace_spaces_with_underscores.md)
  : Replace spaces with underscores in column names
- [`change_labeles_to_factors()`](https://charlesrogers.github.io/jtbdtools/reference/change_labeles_to_factors.md)
  : Change labelled data to factors

## Segmentation & Significance

Compare scores across segments with statistical testing

- [`get_jtbd_var_values.list()`](https://charlesrogers.github.io/jtbdtools/reference/get_jtbd_var_values.list.md)
  : Get unique segment values
- [`test_segment_significance()`](https://charlesrogers.github.io/jtbdtools/reference/test_segment_significance.md)
  : Test statistical significance between two segments
- [`get_jtbd_segment.comp.ordinal()`](https://charlesrogers.github.io/jtbdtools/reference/get_jtbd_segment.comp.ordinal.md)
  : Get JTBD segment comparison: ordinal
- [`get_jtbd_segment.comp_and_plot()`](https://charlesrogers.github.io/jtbdtools/reference/get_jtbd_segment.comp_and_plot.md)
  : Compare and plot JTBD segment scores
- [`get_seg_comp.build.linear()`](https://charlesrogers.github.io/jtbdtools/reference/get_seg_comp.build.linear.md)
  : Build linear segment comparison

## Utilities

- [`get_count()`](https://charlesrogers.github.io/jtbdtools/reference/get_count.md)
  : Get count of factor levels
- [`make_data_long.pairwise()`](https://charlesrogers.github.io/jtbdtools/reference/make_data_long.pairwise.md)
  : Convert JTBD scores to long format for pairwise comparison
- [`get_min_max()`](https://charlesrogers.github.io/jtbdtools/reference/get_min_max.md)
  : Get min and max values for JTBD scores
- [`get.percent_of_max()`](https://charlesrogers.github.io/jtbdtools/reference/get.percent_of_max.md)
  : Get percent of max for each segment
- [`get.normalized_scores()`](https://charlesrogers.github.io/jtbdtools/reference/get.normalized_scores.md)
  : Get normalized scores
- [`remove_weird_text_formatting.jtbd()`](https://charlesrogers.github.io/jtbdtools/reference/remove_weird_text_formatting.jtbd.md)
  : Remove JTBD text formatting
- [`plot_this.pairwise.plotable()`](https://charlesrogers.github.io/jtbdtools/reference/plot_this.pairwise.plotable.md)
  : Plot JTBD scores for pairwise comparison (long format helper)
- [`plot_this.group.plotable()`](https://charlesrogers.github.io/jtbdtools/reference/plot_this.group.plotable.md)
  : Plot JTBD scores for group comparison (long format helper)

## Data

- [`jtbd_sample`](https://charlesrogers.github.io/jtbdtools/reference/jtbd_sample.md)
  : Sample JTBD survey data
