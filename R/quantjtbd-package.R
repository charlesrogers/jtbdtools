#' jtbdtools: A package for Jobs to be Done (JTBD) analysis
#'
#' This package provides functions for analyzing Jobs to be Done (JTBD) data,
#' including opportunity scoring, segmentation, and visualization.
#'
#' @section Main functions:
#' \itemize{
#'   \item \code{\link{get_jtbd_scores}}: Calculate JTBD scores
#'   \item \code{\link{get_jtbd_scores.batch}}: Calculate JTBD scores for multiple segments
#'   \item \code{\link{get_jtbd_clusters}}: Perform cluster analysis on JTBD data
#'   \item \code{\link{plot_this.graph.rel.abs_score}}: Visualize relative and absolute scores
#'   \item \code{\link{create.job_step.table}}: Create a table for job step analysis
#'   \item \code{\link{get_jtbd_segment.comp}}: Compare JTBD segments
#' }
#'
#' @docType package
#' @name jtbdtools
#' @import ggplot2
#' @importFrom dplyr mutate group_by count select filter rename %>%
#' @importFrom tidyr pivot_wider pivot_longer separate
#' @importFrom stats median sd
#' @importFrom utils head tail
#' @importFrom forcats fct_count
#' @importFrom sjlabelled label_to_colnames
#' @importFrom janitor clean_names
#' @importFrom haven is.labelled
#' @importFrom stringr str_replace_all str_wrap
#' @importFrom scales percent
#' @importFrom gt gt tab_header fmt_percent tab_options tab_footnote cols_width
#' @importFrom gtExtras gt_theme_nytimes gt_highlight_rows
NULL
#' Get count of factor levels
#'
#' @param data_frame A data frame containing the column to be counted
#' @param col_name The name of the column to count
#'
#' @return A data frame with counts for each factor level
#' @export
#'
#' @importFrom labelled to_factor
get_count <- function(data_frame, col_name) {
  count <- data_frame %>%
    mutate(col_name = labelled::to_factor(col_name)) %>%
    group_by(col_name) %>% 
    count()
  return(count)
}

#' Get sample size
#'
#' @param your_data_frame A data frame containing columns starting with "imp__"
#'
#' @return The number of non-NA rows in the last column starting with "imp__"
#' @export
get_sample_size <- function(your_data_frame) {
  sample_size <- your_data_frame %>%
    select(dplyr::starts_with("imp__")) %>%
    select(last_col()) %>%
    filter(!is.na(.)) %>%
    nrow()
  return(sample_size)
}

#' Build importance column names
#'
#' @param df A data frame to process
#' @param job_section The job section to add to the column names
#'
#' @return A data frame with updated column names
#' @export
build_imp_column_names <- function(df, job_section) {
  df <- prep_data(df)
  new_col_name <- paste0("imp__", job_section, ".", colnames(df))
  df <- setNames(df, new_col_name) %>%
    rename(caseid = 1)
  
  return(df)
}

#' Build satisfaction column names
#'
#' @param df A data frame to process
#' @param job_section The job section to add to the column names
#'
#' @return A data frame with updated column names
#' @export
build_sat_column_names <- function(df, job_section) {
  df <- prep_data(df)
  new_col_name <- paste0("sat__", job_section, ".", colnames(df))
  df <- setNames(df, new_col_name) %>%
    rename(caseid = 1)
  
  return(df)
}

#' Prepare data for analysis
#'
#' @param df A data frame to process
#'
#' @return A processed data frame
#' @export
prep_data <- function(df) {
  df <- convert_labels_to_row_names(df)
  df <- remove_data_prefix(df)
  df <- remove_data_suffix(df)
  df <- replace_spaces_with_underscores(df)
  df <- change_labeles_to_factors(df)
  return(df)
}

#' Convert labels to row names
#'
#' @param df A data frame to process
#'
#' @return A data frame with labels converted to column names
#' @export
#'
#' @importFrom sjlabelled label_to_colnames
convert_labels_to_row_names <- function(df) {
  df <- df %>%
    sjlabelled::label_to_colnames()
}

#' Remove data prefix
#'
#' @param df A data frame to process
#'
#' @return A data frame with prefixes removed from column names
#' @export
remove_data_prefix <- function(df) {
  df <- df %>% setNames(gsub(".*?--", "", names(.)))
}

#' Remove data suffix
#'
#' @param df A data frame to process
#'
#' @return A data frame with suffixes removed from column names
#' @export
remove_data_suffix <- function(df) {
  df <- df %>% setNames(sub("-.*$","", names(.)))
}

#' Replace spaces with underscores in column names
#'
#' @param df A data frame to process
#'
#' @return A data frame with spaces replaced by underscores in column names
#' @export
#'
#' @importFrom janitor clean_names
replace_spaces_with_underscores <- function(df){
  df <- df %>%
    janitor::clean_names()
}

#' Change labelled data to factors
#'
#' @param df A data frame to process
#'
#' @return A data frame with labelled columns converted to factors
#' @export
#'
#' @importFrom haven is.labelled
change_labeles_to_factors <- function(df){
  df <- df %>%
    mutate_if(haven::is.labelled, as_factor)
}

#' Calculate JTBD scores
#'
#' @param your_data_frame A data frame containing importance and satisfaction data
#' @param col_suffix The segmentation variable
#'
#' @return A data frame with calculated JTBD scores
#' @export
get_jtbd_scores <- function(your_data_frame, col_suffix = "all"){
  opportunity_columns_group_1 <- find_imp_sat_columns(your_data_frame)
  opportunity_score_group_1 <- calculate_pop_pct_score(opportunity_columns_group_1)
  opportunity_score_group_1 <- split_imp_sat_columns(opportunity_score_group_1)
  opportunity_score_group_1 <- calculate_opportunity_score(opportunity_score_group_1)
  opportunity_score_group_1 <- opportunity_score_group_1 %>%
    mutate(opp = if_else(imp < sat, imp, imp + imp - sat)) %>%
    arrange(desc(opp)) %>%
    mutate(segment_name = ifelse(col_suffix == "all", "all", col_suffix),
           rank = rank(desc(opp)))
  opportunity_score_group_1 <- opportunity_score_group_1 %>%
    mutate(opp_index = round(opp / median(opp), 2))
  
  importance_satisfaction_opportunity <- opportunity_score_group_1 %>%
    pivot_wider(objective,
                names_from = c(segment_name),
                values_from = c(imp, sat, opp, rank, opp_index),
                names_sep = ".") %>%
    mutate(objective = as_factor(objective)) %>%
    separate(objective, sep = "([.])", into = c("job_step", "objective")) %>%
    mutate_if(is.numeric, round, 2) %>%
    mutate(objective = factor(objective, levels = objective))
  
  return(importance_satisfaction_opportunity)
}

#' Find importance and satisfaction columns
#'
#' @param your_data_frame A data frame containing importance and satisfaction columns
#'
#' @return A data frame with only importance and satisfaction columns
#' @export
find_imp_sat_columns <- function(your_data_frame){
  imp_columns <- your_data_frame %>%
    select(starts_with("imp_"))
  sat_columns <- your_data_frame %>%
    select(starts_with("sat_"))
  data_frame_imp_sat <- cbind(imp_columns, sat_columns)
  return(data_frame_imp_sat)
}

#' Calculate population percentage score
#'
#' @param objectives A data frame containing objectives to be scored
#'
#' @return A data frame with calculated scores for each objective
#' @export
#'
#' @importFrom forcats fct_count
calculate_pop_pct_score <- function(objectives){
  all_data <- NULL
  for (objective in seq_along(objectives)){
    namez <- names(objectives)[[objective]]
    
    objective_score <- fct_count(objectives[[objective]]) %>%
      mutate(objective_name = namez)
    
    objective_score_tibble <- objective_score %>%
      mutate(user_rating = f) %>%
      filter(user_rating %in% c(1,2,3,4,5)) %>%
      select(objective_name, user_rating, n)
    
    individual_data <-  objective_score_tibble %>%
      summarize(objective_name = unique(objective_name),
                total_sum = sum(n),
                imp_sat_sum = sum(n[user_rating == 5 | user_rating == 4]))  %>%
      mutate(imp_sat_score = ((imp_sat_sum/total_sum)*10))
    all_data <- rbind(all_data, individual_data)
  }
  return(all_data)
}

#' Split importance and satisfaction columns
#'
#' @param data_frame_imp_sat A data frame with importance and satisfaction columns
#'
#' @return A data frame with split importance and satisfaction columns
#' @export
split_imp_sat_columns <- function(data_frame_imp_sat){
  data_frame_imp_sat_split <-  data_frame_imp_sat %>%
    separate(objective_name, "__", into = c("imp_sat", "objective"), remove = FALSE)
  return(data_frame_imp_sat_split)
}

#' Calculate opportunity score
#'
#' @param data_frame_split A data frame with split importance and satisfaction columns
#'
#' @return A data frame with calculated opportunity scores
#' @export
calculate_opportunity_score <- function(data_frame_split) {
  opportunity_scores <- data_frame_split %>%
    pivot_wider(objective,
                names_from = c(imp_sat),
                values_from = imp_sat_score) %>%
    mutate(opp = if_else(imp < sat, imp, imp + imp - sat)) %>%
    arrange(desc(opp))
  
  return(opportunity_scores)
}

#' Get JTBD variable values list
#'
#' @param data_frame A data frame containing the segmentation column
#' @param segmentation_column The name of the segmentation column
#'
#' @return A list of unique values from the segmentation column with more than 90 occurrences
#' @export
get_jtbd_var_values.list <- function(data_frame, segmentation_column){
  list <- data_frame %>%
    group_by(!!as.name(segmentation_column)) %>%
    count() %>%
    filter(n > 90) %>%
    filter(!is.na(!!as.name(segmentation_column))) %>%
    select(!!as.name(segmentation_column)) %>%
    unique() %>% deframe()
  print(list[1])
  list.cut <- list[-1]
  return(list.cut)
}

#' Calculate JTBD scores for multiple segments
#'
#' @param master_table The main data frame to update
#' @param data_frame The data frame containing JTBD data
#' @param merged_df_and_segmentation_column The column used for segmentation
#' @param static_segment The static segment to compare against
#' @param list_of_unique_segments A list of unique segments to analyze
#'
#' @return An updated master table with JTBD scores for each segment
#' @export
get_jtbd_scores.batch <- function(master_table, data_frame, merged_df_and_segmentation_column, static_segment, list_of_unique_segments){
  for (i in 1:length(list_of_unique_segments)) {
    section  <- deparse(substitute(list_of_unique_segments))
    df_name <- list_of_unique_segments[i]
    df_name <- df_name %>%
      str_replace_all(.," ","_")
    df_name <- paste0("df.",section,".",df_name)
    print(df_name)
    df_of_scores <- get_jtbd_scores.pairwise(data_frame, merged_df_and_segmentation_column, static_segment, list_of_unique_segments[i])
    
    assign(df_name, df_of_scores, envir = .GlobalEnv)
    truncated_list <- df_of_scores %>%
      select(objective, starts_with("imp"), starts_with("sat"), starts_with("opp"))
    
    master_table <-  master_table %>%
      left_join(truncated_list)
  }
  return(master_table)
}

#' Calculate JTBD scores for comparison
#'
#' This function combines the functionality of get_jtbd_var_values.list and get_jtbd_scores
#' to simplify the process of calculating JTBD scores for comparison.
#'
#' @param data_frame The data frame containing JTBD data
#' @param segmentation_column The name of the column used for segmentation
#'
#' @return An updated master table with JTBD scores for each segment
#' @export
get_jtbd_scores.comparison <- function(data_frame, segmentation_column) {
  # Get the list of unique segments
  segments_list <- get_jtbd_var_values.list(data_frame, segmentation_column)
  
  # Get the static segment (first item in the list)
  static_segment <- data_frame %>%
    pull(!!as.name(segmentation_column)) %>%
    unique() %>%
    .[1]
  
  # Calculate scores for all segments
  result <- get_jtbd_scores(data_frame, col_suffix = "all")
  
  # Calculate scores for each segment
  for (segment in segments_list) {
    segment_data <- data_frame %>% filter(!!as.name(segmentation_column) == segment)
    segment_scores <- get_jtbd_scores(segment_data, col_suffix = segment)
    result <- result %>% 
      left_join(segment_scores %>% select(-starts_with("all")), by = c("job_step", "objective"))
  }
  
  return(result)
}

#' Calculate JTBD scores for a pair of segments
#'
#' @param your_data_frame The data frame containing JTBD data
#' @param column_to_split_on The column used for segmentation
#' @param factor_a The first segment to compare
#' @param factor_b The second segment to compare
#'
#' @return A data frame with JTBD scores for the two segments
#' @export
get_jtbd_scores.pairwise <- function(your_data_frame, column_to_split_on, factor_a, factor_b) {
  file_type <- "opportunity_list_table"
  opportunity_calc_group_1 <- your_data_frame %>%
    filter(column_to_split_on == factor_a)
  
  sample_size_factor_a <- get_sample_size(opportunity_calc_group_1)
  
  opportunity_columns_group_1 <- find_imp_sat_columns(opportunity_calc_group_1)
  
  opportunity_score_group_1 <- calculate_pop_pct_score(opportunity_columns_group_1)
  
  opportunity_score_group_1 <- split_imp_sat_columns(opportunity_score_group_1)
  
  opportunity_score_group_1 <- calculate_opportunity_score(opportunity_score_group_1) %>%
    mutate(segment_name = factor_a)
  
  opportunity_calc_group_2 <- your_data_frame %>%
    filter(column_to_split_on == factor_b)
  
  sample_size_factor_b <- get_sample_size(opportunity_calc_group_2)
  
  opportunity_columns_group_2 <- find_imp_sat_columns(opportunity_calc_group_2)
  
  opportunity_score_group_2 <- calculate_pop_pct_score(opportunity_columns_group_2)
  
  opportunity_score_group_2 <- split_imp_sat_columns(opportunity_score_group_2)
  
  opportunity_score_group_2 <- calculate_opportunity_score(opportunity_score_group_2) %>%
    mutate(segment_name = factor_b)
  
  merged_opportunity_data_frame <- rbind(opportunity_score_group_1, opportunity_score_group_2)

  importance_satisfaction_opportunity <- merged_opportunity_data_frame %>%
    pivot_wider(objective,
                names_from = c(segment_name),
                values_from = c(imp, sat, opp),
                names_sep = ".") %>%
    mutate(objective = as_factor(objective)) %>%
    separate(objective, sep = "([.])", into = c("job_step", "objective"))
  
  deparsed_column_name <- deparse(substitute(column_to_split_on))
  deparsed_column_name <- str_split_fixed(deparsed_column_name[1], "([$])", 2) %>%
    as.data.frame(.) %>%
    select(deparsed_column_name = V2) %>%
    pluck(., 1)
  
  importance_satisfaction_opportunity <- importance_satisfaction_opportunity %>%
    mutate(objective = factor(objective, levels = objective)) %>%
    mutate_if(is.numeric, round, 1) %>%
    select(-starts_with("rank"))
  
  return(importance_satisfaction_opportunity)
}

#' Plot JTBD scores for pairwise comparison
#'
#' @param your_data_frame A data frame containing JTBD scores
#' 
#' @return A long format data frame suitable for plotting
#' @export
plot_this.pairwise.plotable <- function(your_data_frame) {
  long.your_data_frame <- your_data_frame %>%
    pivot_longer(cols = -c(job_step, objective),
                 names_to = c("measure", "segment"),
                 names_pattern = "^(imp|opp|sat)\\.(.*)$",
                 values_to = "score") %>%
    pivot_wider(names_from = "measure", values_from = "score") %>% 
    separate_wider_delim(segment, "_", names = c("segment", "seg_value"))
  return(long.your_data_frame)
}

#' Plot JTBD scores for group comparison
#'
#' @param your_data_frame A data frame containing JTBD scores
#' 
#' @return A long format data frame suitable for plotting
#' @export
plot_this.group.plotable <- function(your_data_frame) {
  long.your_data_frame <- your_data_frame %>%
    pivot_longer(cols = -c(job_step, objective, opp.all), 
                 names_to = c("measure"), 
                 values_to = "score") %>%
    separate_wider_delim(measure, ".", names = c("measure", "segment")) 
  return(long.your_data_frame)
}

#' Convert JTBD scores to long format for pairwise comparison
#'
#' @param your_data_frame A data frame containing JTBD scores
#' 
#' @return A long format data frame
#' @export
make_data_long.pairwise <- function(your_data_frame) {
  long.your_data_frame <- your_data_frame %>%
    pivot_longer(cols = -c(job_step, objective), 
                 names_to = c("measure"), 
                 values_to = "score") %>% 
    separate_wider_delim(measure, ".", names = c("measure", "segment"))
  return(long.your_data_frame)
}

#' Get min and max values for JTBD scores
#'
#' @param your_long_data_frame A long format data frame containing JTBD scores
#' 
#' @return A data frame with min, max, and difference calculations
#' @export
get_min_max <- function(your_long_data_frame){
  df.diff <- your_long_data_frame %>%
    filter(!segment == "all") %>%
    filter(job_step %in% c("finding_sellers")) %>%
    group_by(measure, objective) %>%
    mutate(measure.max = max(score),
           measure.min = min(score),
           measure.diff = measure.max - measure.min) %>%
    ungroup() %>%
    group_by(measure) %>%
    mutate(diff.total = sum(measure.diff) / 2) %>%
    ungroup()
  
  return(df.diff)
}

#' Compare and plot JTBD segment scores
#'
#' @param your_data_frame A data frame containing JTBD scores
#' 
#' @return Plots and saves JTBD score comparisons
#' @export
get_jtbd_segment.comp_and_plot <- function(your_data_frame){
  plot_title <- deparse(substitute(your_data_frame)) %>%
    str_replace_all(., "output.scores.", "")
  print(plot_title)
  
  df.long <- your_data_frame %>%
    make_data_long.pairwise()
  
  max_diff.imp <- df.long %>%
    get_min_max() %>%
    filter(measure == "imp") %>%
    select(diff.total) %>%
    distinct() 
  
  max_diff.imp <- pluck(max_diff.imp$diff.total[1])
  
  max_diff.sat <- df.long %>%
    get_min_max() %>%
    filter(measure == "sat") %>%
    select(diff.total) %>%
    distinct() 
  
  max_diff.sat <- pluck(max_diff.sat$diff.total[1])
  
  print(max_diff.sat)
  
  # ALL Scores Plotted
  df.long %>%
    mutate(objective = fct_reorder(objective, score, .fun = 'max')) %>%
    remove_weird_text_formatting.jtbd() %>%
    ggplot(aes(x = reorder(objective, score), y = score, shape = measure, color = segment)) +
    labs(title = paste0("Opportunities: ", plot_title), x = "Objective", y = "Score", color = "Segment", shape = "Measure") +
    geom_point(size = 4, alpha = .8) + 
    scale_x_discrete(labels = wrap_format(10)) +
    coord_cartesian(ylim = c(0, 11)) +
    scale_y_continuous(breaks = seq(0, 12, 1), expand = c(0, 0))
  
  ggsave(filename = paste0("plot-trio_scores_", plot_title, ".png"), 
         path = "/Users/charlesrogers/Documents/work/analysis/JTBD_Commerce/outputs/graphs", 
         width = 20, height = 10)
  
  # Opp Plot
  df_opp.linear <- your_data_frame %>%
    get_jtbd_segment.comp.index_vs_rank()
  
  df_opp.linear %>%
    remove_weird_text_formatting.jtbd() %>%
    ggplot(aes(x = reorder(objective, score), y = score, shape = job_step, color = segment, label = score_show)) +
    labs(title = paste0("Opportunities: ", plot_title), 
         subtitle = paste0("Sum of Diff in Imp: ", max_diff.imp, ", SAT: ", max_diff.sat), 
         x = "Objective", y = "Objective", color = "Segment", shape = "Measure") +
    geom_line(aes(group = objective)) +
    geom_point(size = 4, alpha = .8) + 
    geom_text(hjust = -.25) +
    coord_flip()
  
  ggsave(filename = paste0("plot-OPP_scores_", plot_title, ".png"), 
         path = "/Users/charlesrogers/Documents/work/analysis/JTBD_Commerce/outputs/graphs", 
         width = 20, height = 10)
}

#' Remove weird text formatting for JTBD
#'
#' @param your_data_frame A data frame to process
#'
#' @return A data frame with cleaned text formatting
#' @export
remove_weird_text_formatting.jtbd <- function(your_data_frame){
  your_data_frame.changed <- your_data_frame  %>%
    # STRINGS
    mutate_if(is.character,~str_replace_all(.,"minimize_time_to_","")) %>%
    mutate_if(is.factor,~str_replace_all(.,"minimize_time_to_","")) %>%
    mutate_if(is.character,~str_replace_all(.,"minimize_likelihood_of_","Avoid ")) %>%
    # FACTORS
    mutate_if(is.factor,~str_replace_all(.,"minimize_likelihood_of_","Avoid ")) %>%
    mutate_if(is.factor,~str_replace_all(.,"_"," ")) %>%
    mutate_if(is.character,~str_replace_all(.,"_"," ")) %>%
    mutate_if(is.factor,~str_to_sentence(.)) %>%
    mutate_if(is.character,~str_to_sentence(.))
  return(your_data_frame.changed)
}

#' Get percent of max for each function
#'
#' @param your_df A data frame containing JTBD scores
#'
#' @return A data frame with percent of max calculations
#' @export
get.percent_of_max <- function(your_df){
  df.maxxed <- your_df %>%
    make_data_long.pairwise()%>%
    filter(segment!="all") %>% 
    group_by(segment,measure)%>%
    mutate(seg.max=max(score))%>%
    ungroup()%>%
    mutate(pct_max.seg=score/seg.max) %>%
    select(-c(score,seg.max))%>%
    pivot_wider(names_from = measure,values_from = pct_max.seg)
  
  return(df.maxxed)
}

#' Get normalized scores
#'
#' @param your_df A data frame containing JTBD scores
#'
#' @return A data frame with normalized scores
#' @export
get.normalized_scores <- function(your_df){
  df.maxxed <- your_df %>%
    make_data_long.pairwise()%>%
    filter(segment!="all") %>%
    group_by(segment,measure)%>%
    mutate(seg.max=max(score),
           seg.min=min(score),
           seg.range=seg.max-seg.min,
           score.normd=round((score-seg.min)/seg.range,2))%>%
    ungroup()%>%
    mutate(pct_max.seg=score/seg.max) %>%
    select(-c(score,seg.max,seg.min,seg.range,pct_max.seg))
  
  return(df.maxxed)
}

#' Theme for job step visualization
#'
#' @param your.df A data frame containing job step data
#' @param step_title The title for the job step
#'
#' @return A gt table with formatted job step data
#' @export
#'
#' @importFrom gt gt tab_header fmt_percent gtExtras::gt_theme_nytimes tab_options tab_footnote gt_highlight_rows data_color cols_width
#' @importFrom scales col_numeric
theme.job_step <- function(your.df,step_title){
  output <- your.df %>%
    arrange(-opp.all)%>%
    relocate(c(opp.all,opp_index.all,imp.all,sat.all),.after = c("objective"))%>%
    select(-job_step)%>%
    remove_weird_text_formatting.jtbd()%>%
    rename(Importance=imp.all,
           Satisfaction=sat.all,
           Opportunity=opp.all)%>%
    select(-c(Opportunity,opp_index.all))%>%
    gt() %>%
    tab_header(
      title = step_title) %>%
    fmt_percent(
      columns = Index,
      decimals = 0
    ) |> 
    gtExtras::gt_theme_nytimes() %>%
    tab_options(data_row.padding = px(1)) %>%
    tab_footnote(
      locations = cells_column_labels(columns = Index),
      footnote = 'Index is the relative rank of an objective\'s opportunity score; It\'s calculated by taking the percentage of the median opportunity'
    )   %>% 
    gt_highlight_rows(
      rows = objective=="Median objective",
      fill = "lightgrey",
      bold_target_only = FALSE,
      font_weight = "normal"
    ) %>%
    data_color(columns = c(Index),
               colors = scales::col_numeric(
                 palette =  c("#990099FF","white","#066230"),
                 domain = c(.76,1.26))
    ) %>%
    cols_width(
      objective ~ px(250),
      Index ~ px(50),
      Importance ~ px(75),
      Satisfaction ~ px(75),
    )
  
  return(output)
}

#' Plot job step
#'
#' @param your.df A data frame containing job step data
#' @param step_title The title for the job step
#'
#' @return A ggplot object with job step visualization
#' @export
plot.job_step <- function(your.df,step_title){
  your_plot <- your.df %>%
    ggplot(aes(x=imp.all,y=sat.all))+geom_point(size=3)
  return(your_plot)
}

#' Create job step table
#'
#' @param df A data frame containing job step data
#' @param job.string The job string
#' @param job_step.string The job step string
#' @param path.table The path to save the table
#'
#' @return Saves a png file of the job step table
#' @export
#'
#' @importFrom gt gtsave
create.job_step.table <- function(df,job.string,job_step.string,path.table){
  table_title <- paste0(job.string," - ",job_step.string)
  table.png <- df %>%
    filter(job_step==job_step.string) %>%
    theme.job_step(.,table_title)
  
  file_name.table <- paste("table-opportunities-",job.string,"-",job_step.string,".png") 
  
  gtsave(table.png, filename = file_name.table,path = path.table, vwidth = 600, vheight = 2500, selector='.gt_table')
}



#' Get JTBD segment comparison: ordinal
#'
#' @param your_df A data frame containing JTBD scores
#'
#' @return A data frame with ordinal comparison results
#' @export
get_jtbd_segment.comp.ordinal <- function(your_df){
  df.linear <- your_df %>%
    filter(!segment=="other")%>%
    filter(measure=="opp")%>%
    filter(!segment=="all") %>%
    mutate(
      max.all=max(opp.all),
      pct_max.all=round(opp.all/max.all,2)
    ) %>%
    mutate(
      max.segment=max(score)
    )%>%
    group_by(segment)%>%
    mutate(
      rank.seg=rank(-score),
      max.seg_value=max(score)
    )%>%
    ungroup()%>%
    group_by(objective)%>%
    mutate(
      mean=mean(score),
      sd=sd(score),
      count=n(),
      max_value=max(score),
      min_value=min(score),
      rank.delta=(rank.all-rank.seg),
      rank.max=min(rank.all,rank.seg)
    )%>%
    ungroup()%>%
    mutate(
      vs.max_all=round(score/max.all,2),
      delta.vs.max_all=vs.max_all-pct_max.all,
      pct_max.seg=round(score/max.segment,2),
      vs.ave_seg.obj=round(score/opp.all,2),
      rank.delta.abs=abs(rank.delta),
      pct_max.seg_value=round(score/max.seg_value,2),
      pct_max.delta=round(pct_max.seg-pct_max.all,2),
    )%>%
    mutate(
      range=round(max_value-min_value,2),
      sig_level=round(rank.delta.abs,2),
    ) %>% 
    group_by(objective)%>%
    mutate(
      max_value.pct_of_seg_val=max(pct_max.seg_value),
      min_value.pct_of_seg_val=min(pct_max.seg_value),
    )%>%
    ungroup()%>%
    mutate(seg.value = labelled::to_factor(segment),
           range.pct_of_seg_val=max_value.pct_of_seg_val-min_value.pct_of_seg_val
    )

  df.linear %>%
    select(job_step,objective,seg.value,score,rank.delta,rank.seg,rank.all,pct_max.delta,rank.max,vs.max_all,pct_max.seg_value,pct_max.seg,vs.ave_seg.obj,range.pct_of_seg_val,range,sig_level,opp.all,max_value,min_value)
}

#' Get JTBD segment comparison
#'
#' @param your_df A data frame containing JTBD scores
#'
#' @return A data frame with segment comparison results
#' @export
get_jtbd_segment.comp <- function(your_df){
  your_df.linear <- your_df %>% get_jtbd_segment.comp.ordinal()
  
  your_df.outlier <- your_df %>% get_jtbd_segment.comp.outlier()
  
  your_df.merged <- your_df.linear %>%
    select(objective,binary.linear,p.value) %>%
    left_join(your_df.outlier)
}

#' Build group score segmentation
#'
#' @param total_pop Total population data frame
#' @param original_df Original data frame
#' @param new_table New table to be added
#'
#' @return An updated data frame with new segmentation scores
#' @export
get_seg_comp.build.linear <- function(total_pop, original_df, new_table){
  median.all <- total_pop %>%
    mutate(median.all = median(opp.all))
  
  section <- deparse(substitute(new_table)) %>%
    str_replace_all("output.segment.master_table.", "")
  
  new_table %>%
    summarise(segment = section,
              sd = sd(range),
              n = n(),
              count.linear.1 = sum(sig_level < .1),
              count.linear.05 = sum(sig_level < .05),
              count.outlier.sal = sum(sig_level > 0 & range > 1.5)) %>%
    mutate(
      pval.1.lin = scales::percent(count.linear.1 / n, accuracy = 1),
      pval.05.lin = scales::percent(count.linear.05 / n, accuracy = 1)) %>%
    select(-c(n, count.linear.1, count.linear.05), everything(), count.outlier.sal) %>%
    bind_rows(original_df)
}

#' Calculate individual JTBD scores
#'
#' @param df.uid_imp_sat A data frame containing importance and satisfaction scores with user IDs
#'
#' @return A data frame with individual JTBD scores
#' @export
get_jtbd_scores.individual <- function(df.uid_imp_sat){
  step.1 <- df.uid_imp_sat %>%
    pivot_longer(cols = -caseid, names_to = "objective_string", values_to = "score") %>% 
    separate(objective_string, "__", into = c("imp_sat", "objective"), remove = FALSE) %>%
    mutate_if(is.factor, as.numeric) %>%
    mutate_if(is.numeric, as.factor)
  
  step.2 <- step.1 %>%
    select(caseid, objective, imp_sat, score) %>%
    pivot_wider(names_from = imp_sat, values_from = score)
  
  step.3 <- step.2 %>%
    mutate(opp = case_when(
      (imp == "5" & sat %in% c("11", "12")) | (imp == "4" & sat == "11") | (imp %in% c("4", "5") & !sat %in% c("11", "12")) ~ 1,
      TRUE ~ 0))
  
  step.4 <- step.3 %>%
    group_by(objective) %>%
    summarize(opp_sum = sum(opp == 1),
              total_count = n(),
              opp.score = opp_sum / total_count)
  
  print(step.4)
  
  return(step.3)
}

#' Create percentage table
#'
#' @param df Data frame containing the data
#' @param job.string Job string
#' @param var_name.string Variable name string
#' @param job_step.string Job step string
#' @param path.table Path to save the table
#'
#' @return Saves a PNG file of the percentage table
#' @export
#'
#' @importFrom gt gt tab_header gtExtras::gt_plt_bar_pct gtExtras::gt_theme_nytimes gtsave
#' @importFrom dplyr select pivot_longer filter group_by mutate count ungroup rename
#' @importFrom tidyr pivot_longer
#' @importFrom forcats fct_lump_prop
#' @importFrom stringr str_replace_all
create.pct.table <- function(df, job.string, var_name.string, job_step.string, path.table){
  table.png <- df %>%
    select(var_name.string) %>%
    pivot_longer(cols = everything()) %>% 
    filter(!is.na(value)) %>% 
    group_by(name) %>%
    mutate(value = fct_lump_prop(value, .031)) %>%
    count(name, value) %>% 
    mutate(frequency = round(n / sum(n), 2) * 100) %>%
    remove_weird_text_formatting.jtbd() %>% 
    select(-n) %>% 
    arrange(-frequency) %>%
    ungroup() %>%
    select(-c(name)) %>%
    rename(Tool = value) %>%
    gt() %>%
    tab_header(
      title = paste0(job_step.string)) %>%
    gtExtras::gt_plt_bar_pct(column = frequency, 
                             scaled = TRUE,
                             labels = TRUE,
                             width = 1000,
                             height = 50,
                             font_size = "28px") %>%
    cols_align(
      align = c("left"),
      columns = everything()) %>%
    gtExtras::gt_theme_nytimes() %>%
    tab_style(
      cell_text(weight = "bold",
                size = "xx-large"),
      locations = cells_row_groups(everything())) %>%
    tab_options(
      table.font.size = "24px",
      column_labels.font.size = "18px")
  
  file_name.table <- paste("table-tool_usage-", job.string, "-", job_step.string, ".png") 
  
  gtsave(table.png, filename = file_name.table, path = path.table, vwidth = 1500, vheight = 500)
}

#' Plot relative score graph
#'
#' @param your_data_frame A data frame containing JTBD scores
#' @param seg_value_title The title for the segment value
#' @param last_value The last value to be labeled
#'
#' @return A ggplot object
#' @export
#'
#' @importFrom ggplot2 ggplot aes geom_bump geom_point facet_wrap geom_text_repel labs theme scale_x_discrete
#' @importFrom ggrepel geom_text_repel
#' @importFrom forcats fct_inorder
#' @importFrom stringr str_wrap
plot_this.graph.rel_score <- function(your_data_frame,seg_value_title,last_value){
  #https://github.com/davidsjoberg/ggbump
  your_data_frame %>%
    arrange(seg.value) %>%
    remove_weird_text_formatting.jtbd()%>%
    ggplot(aes(y=pct_max.seg_value,x=fct_inorder(seg.value),group=objective,color=objective))+
    geom_bump(size = 2, smooth = 4) +
    geom_point(size = 7) +
    facet_wrap(~job_step)+
    geom_text_repel(data = . %>%   remove_weird_text_formatting.jtbd() %>% filter(seg.value==last_value&range.pct_of_seg_val>.1),
                    aes(x = seg.value, label = stringr::str_wrap(objective,15)), size = 2.5,nudge_x = 1) +
    labs(title= paste0("Percent of Segment Max by: ",seg_value_title), y="Percent of Segment Max (Relative Score)",x="",caption = "Labelled objectives have a >= 10% difference in relative value within the objective")+
    theme(legend.position = "none",
          panel.grid.major = element_blank(),
          text=element_text(family = "Roboto"),
          title = element_text (size = 15,margin = margin(b = 10))) +
    scale_x_discrete(expand=c(.05,0))
  
  ggsave(filename = paste0("plot-rel_score_by-",seg_value_title,".png"),path ="/Users/charlesrogers/Documents/work/analysis/JTBD_Commerce/outputs/graphs",width=20, height=10.68)
}

#' Plot absolute score graph
#'
#' @param your_data_frame A data frame containing JTBD scores
#' @param seg_value_title The title for the segment value
#' @param last_value The last value to be labeled
#'
#' @return A ggplot object
#' @export
#'
#' @importFrom ggplot2 ggplot aes geom_bump geom_point facet_wrap geom_text_repel labs theme scale_x_discrete
#' @importFrom ggrepel geom_text_repel
#' @importFrom forcats fct_inorder
#' @importFrom stringr str_wrap
plot_this.graph.abs_score <- function(your_data_frame,seg_value_title,last_value){
  #https://github.com/davidsjoberg/ggbump
  your_data_frame %>%
    arrange(seg.value) %>%
    remove_weird_text_formatting.jtbd()%>%
    ggplot(aes(y=score,x=fct_inorder(seg.value),group=objective,color=objective,shape=binary.linear))+
    geom_bump(size = 2, smooth = 4) +
    geom_point(size = 7) +
    facet_wrap(~job_step)+
    geom_text_repel(data = . %>%   remove_weird_text_formatting.jtbd() %>% filter(seg.value==last_value&binary.linear=="1"),
                    aes(x = seg.value, label = stringr::str_wrap(objective,15)), size = 2.5,nudge_x = 1) +
    labs(title= paste0("Linear Opportunities by: ",seg_value_title), y="Percent of Max (Absolute Score)",x="",caption = "Labelled objectives have a linear relationship at a .05 pvalue")+
    theme(legend.position = "none",
          panel.grid.major = element_blank(),
          text=element_text(family = "Roboto"),
          title = element_text (size = 15,margin = margin(b = 10))) +
    scale_x_discrete(expand=c(.05,0))
  
  ggsave(filename = paste0("plot-abs_score_by-",seg_value_title,".png"),path ="/Users/charlesrogers/Documents/work/analysis/JTBD_Commerce/outputs/graphs",width=20, height=10.68)
}

#' Plot both relative and absolute score graphs
#'
#' @param your_data_frame A data frame containing JTBD scores
#' @param seg_value_title The title for the segment value
#' @param last_value The last value to be labeled
#'
#' @return NULL
#' @export
plot_this.graph.rel.abs_score <- function(your_data_frame,seg_value_title,last_value){
  plot_this.graph.rel_score(your_data_frame,seg_value_title,last_value)
  plot_this.graph.abs_score(your_data_frame,seg_value_title,last_value)
}