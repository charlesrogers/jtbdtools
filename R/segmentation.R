#' Get unique segment values
#'
#' Extracts unique values from a segmentation column, filtering to segments
#' with at least `min_n` respondents.
#'
#' @param data_frame A data frame containing the segmentation column
#' @param segmentation_column The name of the segmentation column (as string)
#' @param min_n Minimum number of respondents to include a segment (default: 30)
#'
#' @return A character vector of segment values
#' @export
#'
#' @family segmentation
#'
#' @examples
#' data(jtbd_sample)
#' get_jtbd_var_values.list(jtbd_sample, "segment", min_n = 30)
get_jtbd_var_values.list <- function(data_frame, segmentation_column, min_n = 30) {
  if (!segmentation_column %in% names(data_frame)) {
    cli::cli_abort(c(
      "Column {.val {segmentation_column}} not found in data frame.",
      "i" = "Available columns: {.val {names(data_frame)}}"
    ))
  }

  list_vals <- data_frame %>%
    group_by(.data[[segmentation_column]]) %>%
    count() %>%
    filter(n > min_n) %>%
    filter(!is.na(.data[[segmentation_column]])) %>%
    select(all_of(segmentation_column)) %>%
    unique() %>% deframe()

  cli::cli_inform("Found {length(list_vals)} segments with n > {min_n}.")
  return(list_vals)
}

#' Calculate JTBD scores for multiple segments
#'
#' Compares opportunity scores across all values of a segmentation variable.
#' This is the main function for segment comparison analysis.
#'
#' @param data_frame A data frame with `imp__`/`sat__` columns and a segmentation column
#' @param segmentation_column The name of the column to segment by (as string)
#'
#' @return A data frame with imp/sat/opp scores for each segment, joined together
#' @export
#'
#' @family segmentation
#'
#' @examples
#' data(jtbd_sample)
#' comparison <- get_jtbd_scores.comparison(jtbd_sample, "segment")
#' head(comparison)
get_jtbd_scores.comparison <- function(data_frame, segmentation_column) {
  segments_list <- get_jtbd_var_values.list(data_frame, segmentation_column)

  result <- get_jtbd_scores(data_frame, col_suffix = "all")

  for (seg_value in segments_list) {
    segment_data <- data_frame %>% filter(.data[[segmentation_column]] == seg_value)
    segment_scores <- get_jtbd_scores(segment_data, col_suffix = seg_value)
    result <- result %>%
      left_join(segment_scores %>% select(-starts_with("all")), by = c("job_step", "objective"))
  }

  return(result)
}

#' Calculate JTBD scores for a pair of segments
#'
#' Compares two specific segment values head-to-head.
#'
#' @param your_data_frame The data frame containing JTBD data
#' @param column_to_split_on The column used for segmentation
#' @param factor_a The first segment value
#' @param factor_b The second segment value
#'
#' @return A data frame with imp/sat/opp for both segments
#' @export
#'
#' @family segmentation
get_jtbd_scores.pairwise <- function(your_data_frame, column_to_split_on, factor_a, factor_b) {
  opportunity_calc_group_1 <- your_data_frame %>%
    filter(!!as.name(column_to_split_on) == factor_a)

  sample_size_factor_a <- get_sample_size(opportunity_calc_group_1)
  opportunity_columns_group_1 <- find_imp_sat_columns(opportunity_calc_group_1)
  opportunity_score_group_1 <- calculate_pop_pct_score(opportunity_columns_group_1)
  opportunity_score_group_1 <- split_imp_sat_columns(opportunity_score_group_1)
  opportunity_score_group_1 <- calculate_opportunity_score(opportunity_score_group_1) %>%
    mutate(segment_name = factor_a)

  opportunity_calc_group_2 <- your_data_frame %>%
    filter(!!as.name(column_to_split_on) == factor_b)

  sample_size_factor_b <- get_sample_size(opportunity_calc_group_2)
  opportunity_columns_group_2 <- find_imp_sat_columns(opportunity_calc_group_2)
  opportunity_score_group_2 <- calculate_pop_pct_score(opportunity_columns_group_2)
  opportunity_score_group_2 <- split_imp_sat_columns(opportunity_score_group_2)
  opportunity_score_group_2 <- calculate_opportunity_score(opportunity_score_group_2) %>%
    mutate(segment_name = factor_b)

  merged_opportunity_data_frame <- rbind(opportunity_score_group_1, opportunity_score_group_2)

  importance_satisfaction_opportunity <- merged_opportunity_data_frame %>%
    pivot_wider(id_cols = objective,
                names_from = c(segment_name),
                values_from = c(imp, sat, opp),
                names_sep = ".") %>%
    mutate(objective = as_factor(objective)) %>%
    separate(objective, sep = "([.])", into = c("job_step", "objective"))

  importance_satisfaction_opportunity <- importance_satisfaction_opportunity %>%
    mutate(objective = factor(objective, levels = objective)) %>%
    mutate_if(is.numeric, round, 1) %>%
    select(-starts_with("rank"))

  return(importance_satisfaction_opportunity)
}

#' Batch calculate JTBD scores for multiple segments
#'
#' Iterates over a list of segments, calculating pairwise scores against a
#' static reference segment and joining results into a master table.
#'
#' @param master_table The main data frame to update
#' @param data_frame The data frame containing JTBD data
#' @param merged_df_and_segmentation_column The column used for segmentation
#' @param static_segment The static segment to compare against
#' @param list_of_unique_segments A list of unique segments to analyze
#'
#' @return An updated master table with JTBD scores for each segment
#' @export
#'
#' @family segmentation
get_jtbd_scores.batch <- function(master_table, data_frame, merged_df_and_segmentation_column, static_segment, list_of_unique_segments) {
  for (i in seq_along(list_of_unique_segments)) {
    section <- deparse(substitute(list_of_unique_segments))
    df_name <- list_of_unique_segments[i]
    df_name <- df_name %>%
      str_replace_all(., " ", "_")
    df_name <- paste0("df.", section, ".", df_name)
    cli::cli_inform("Processing segment: {df_name}")
    df_of_scores <- get_jtbd_scores.pairwise(data_frame, merged_df_and_segmentation_column, static_segment, list_of_unique_segments[i])

    truncated_list <- df_of_scores %>%
      select(objective, starts_with("imp"), starts_with("sat"), starts_with("opp"))

    master_table <- master_table %>%
      left_join(truncated_list)
  }
  return(master_table)
}

#' Get JTBD segment comparison: ordinal
#'
#' Calculates ordinal (rank-based) comparisons between segments, including
#' rank deltas, percent-of-max scores, and significance levels.
#'
#' @param your_df A data frame in long format containing JTBD scores
#'
#' @return A data frame with ordinal comparison metrics
#' @export
#'
#' @family segmentation
get_jtbd_segment.comp.ordinal <- function(your_df) {
  df.linear <- your_df %>%
    filter(!segment == "other") %>%
    filter(measure == "opp") %>%
    filter(!segment == "all") %>%
    mutate(
      max.all = max(opp.all),
      pct_max.all = round(opp.all / max.all, 2)
    ) %>%
    mutate(
      max.segment = max(score)
    ) %>%
    group_by(segment) %>%
    mutate(
      rank.seg = rank(-score),
      max.seg_value = max(score)
    ) %>%
    ungroup() %>%
    group_by(objective) %>%
    mutate(
      mean = mean(score),
      sd = sd(score),
      count = n(),
      max_value = max(score),
      min_value = min(score),
      rank.delta = (rank.all - rank.seg),
      rank.max = min(rank.all, rank.seg)
    ) %>%
    ungroup() %>%
    mutate(
      vs.max_all = round(score / max.all, 2),
      delta.vs.max_all = vs.max_all - pct_max.all,
      pct_max.seg = round(score / max.segment, 2),
      vs.ave_seg.obj = round(score / opp.all, 2),
      rank.delta.abs = abs(rank.delta),
      pct_max.seg_value = round(score / max.seg_value, 2),
      pct_max.delta = round(pct_max.seg - pct_max.all, 2),
    ) %>%
    mutate(
      range = round(max_value - min_value, 2),
      sig_level = round(rank.delta.abs, 2),
    ) %>%
    group_by(objective) %>%
    mutate(
      max_value.pct_of_seg_val = max(pct_max.seg_value),
      min_value.pct_of_seg_val = min(pct_max.seg_value),
    ) %>%
    ungroup() %>%
    mutate(seg.value = labelled::to_factor(segment),
           range.pct_of_seg_val = max_value.pct_of_seg_val - min_value.pct_of_seg_val
    )

  df.linear %>%
    select(job_step, objective, seg.value, score, rank.delta, rank.seg, rank.all,
           pct_max.delta, rank.max, vs.max_all, pct_max.seg_value, pct_max.seg,
           vs.ave_seg.obj, range.pct_of_seg_val, range, sig_level, opp.all,
           max_value, min_value)
}

#' Build linear segment comparison
#'
#' Summarizes segment comparison results with significance counts and percentages.
#'
#' @param total_pop Total population data frame
#' @param original_df Original data frame to append to
#' @param new_table New segment comparison table
#'
#' @return An updated data frame with segment comparison summary
#' @export
#'
#' @family segmentation
get_seg_comp.build.linear <- function(total_pop, original_df, new_table) {
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
