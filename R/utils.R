#' Get count of factor levels
#'
#' @param data_frame A data frame containing the column to be counted
#' @param col_name The name of the column to count
#'
#' @return A data frame with counts for each factor level
#' @export
get_count <- function(data_frame, col_name) {
  count <- data_frame %>%
    mutate(col_name = labelled::to_factor(col_name)) %>%
    group_by(col_name) %>%
    count()
  return(count)
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
#' @param your_long_data_frame A long format data frame
#' @param job_steps Character vector of job steps to filter (default: all)
#'
#' @return A data frame with min, max, and difference calculations
#' @export
get_min_max <- function(your_long_data_frame, job_steps = NULL) {
  df.diff <- your_long_data_frame %>%
    filter(!segment == "all")

  if (!is.null(job_steps)) {
    df.diff <- df.diff %>%
      filter(job_step %in% job_steps)
  }

  df.diff <- df.diff %>%
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

#' Get percent of max for each segment
#'
#' @param your_df A data frame containing JTBD scores
#'
#' @return A data frame with percent-of-max calculations
#' @export
get.percent_of_max <- function(your_df) {
  df.maxxed <- your_df %>%
    make_data_long.pairwise() %>%
    filter(segment != "all") %>%
    group_by(segment, measure) %>%
    mutate(seg.max = max(score)) %>%
    ungroup() %>%
    mutate(pct_max.seg = score / seg.max) %>%
    select(-c(score, seg.max)) %>%
    pivot_wider(names_from = measure, values_from = pct_max.seg)

  return(df.maxxed)
}

#' Get normalized scores
#'
#' Min-max normalizes scores within each segment and measure.
#'
#' @param your_df A data frame containing JTBD scores
#'
#' @return A data frame with normalized scores (0-1 range)
#' @export
get.normalized_scores <- function(your_df) {
  df.maxxed <- your_df %>%
    make_data_long.pairwise() %>%
    filter(segment != "all") %>%
    group_by(segment, measure) %>%
    mutate(seg.max = max(score),
           seg.min = min(score),
           seg.range = seg.max - seg.min,
           score.normd = round((score - seg.min) / seg.range, 2)) %>%
    ungroup() %>%
    mutate(pct_max.seg = score / seg.max) %>%
    select(-c(score, seg.max, seg.min, seg.range, pct_max.seg))

  return(df.maxxed)
}

#' Remove JTBD text formatting
#'
#' Cleans up objective names for display: removes common prefixes like
#' "minimize_time_to_" and replaces underscores with spaces.
#'
#' @param your_data_frame A data frame with objective text columns
#'
#' @return A data frame with cleaned text
#' @export
remove_weird_text_formatting.jtbd <- function(your_data_frame) {
  your_data_frame.changed <- your_data_frame %>%
    mutate_if(is.character, ~str_replace_all(., "minimize_time_to_", "")) %>%
    mutate_if(is.factor, ~str_replace_all(., "minimize_time_to_", "")) %>%
    mutate_if(is.character, ~str_replace_all(., "minimize_likelihood_of_", "Avoid ")) %>%
    mutate_if(is.factor, ~str_replace_all(., "minimize_likelihood_of_", "Avoid ")) %>%
    mutate_if(is.factor, ~str_replace_all(., "_", " ")) %>%
    mutate_if(is.character, ~str_replace_all(., "_", " ")) %>%
    mutate_if(is.factor, ~str_to_sentence(.)) %>%
    mutate_if(is.character, ~str_to_sentence(.))
  return(your_data_frame.changed)
}

#' Plot JTBD scores for pairwise comparison (long format helper)
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

#' Plot JTBD scores for group comparison (long format helper)
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

#' Compare and plot JTBD segment scores
#'
#' Generates all-scores and opportunity-only plots for a segment comparison.
#'
#' @param your_data_frame A data frame containing JTBD scores
#' @param save_path Directory to save plots (default: [tempdir()])
#' @param n,study Sample size and study label for the plot footer (see [jtbd_footer()]).
#'
#' @return NULL (called for side effects)
#' @export
get_jtbd_segment.comp_and_plot <- function(your_data_frame, save_path = tempdir(),
                                           n = NULL, study = NULL) {
  plot_title <- deparse(substitute(your_data_frame)) %>%
    str_replace_all(., "output.scores.", "")
  cli::cli_inform("Plotting: {plot_title}")

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

  # ALL Scores Plotted
  df.long %>%
    mutate(objective = fct_reorder(objective, score, .fun = 'max')) %>%
    remove_weird_text_formatting.jtbd() %>%
    ggplot(aes(x = reorder(objective, score), y = score, shape = measure, color = segment)) +
    labs(title = paste0("Opportunities: ", plot_title), x = "Objective", y = "Score",
         color = "Segment", shape = "Measure",
         caption = jtbd_footer(n = n, study = study)) +
    geom_point(size = 4, alpha = .8) +
    scale_x_discrete(labels = scales::wrap_format(10)) +
    coord_cartesian(ylim = c(0, 11)) +
    scale_y_continuous(breaks = seq(0, 12, 1), expand = c(0, 0)) +
    theme_jtbd()

  ggsave(filename = paste0("plot-trio_scores_", plot_title, ".png"),
         path = save_path, width = 20, height = 10)
}
