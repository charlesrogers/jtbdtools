#' Plot relative and absolute score graphs
#'
#' Generates both relative (percent-of-segment-max) and absolute score bump charts
#' for segment comparison. Convenience wrapper around [plot_this.graph.rel_score()]
#' and [plot_this.graph.abs_score()].
#'
#' @param your_data_frame A data frame from [get_jtbd_segment.comp.ordinal()]
#' @param seg_value_title Title label for the segment variable
#' @param last_value The segment value to label on the right side of the chart
#' @param save_path Directory to save plots (default: [tempdir()])
#'
#' @return NULL (called for side effects: generates and saves plots)
#' @export
#'
#' @family visualization
plot_this.graph.rel.abs_score <- function(your_data_frame, seg_value_title, last_value, save_path = tempdir()) {
  plot_this.graph.rel_score(your_data_frame, seg_value_title, last_value, save_path = save_path)
  plot_this.graph.abs_score(your_data_frame, seg_value_title, last_value, save_path = save_path)
}

#' Plot relative score bump chart
#'
#' Shows how each objective's opportunity score ranks relative to the segment maximum,
#' making cross-segment comparison easier.
#'
#' @param your_data_frame A data frame from [get_jtbd_segment.comp.ordinal()]
#' @param seg_value_title Title label for the segment variable
#' @param last_value The segment value to label on the right side of the chart
#' @param save_path Directory to save the plot (default: [tempdir()])
#'
#' @return A ggplot object (also saved as PNG)
#' @export
#'
#' @family visualization
plot_this.graph.rel_score <- function(your_data_frame, seg_value_title, last_value, save_path = tempdir()) {
  p <- your_data_frame %>%
    arrange(seg.value) %>%
    remove_weird_text_formatting.jtbd() %>%
    ggplot(aes(y = pct_max.seg_value, x = fct_inorder(seg.value), group = objective, color = objective)) +
    geom_line(linewidth = 1.5) +
    geom_point(size = 5) +
    facet_wrap(~job_step) +
    geom_text_repel(data = . %>% remove_weird_text_formatting.jtbd() %>% filter(seg.value == last_value & range.pct_of_seg_val > .1),
                    aes(x = seg.value, label = stringr::str_wrap(objective, 15)), size = 2.5, nudge_x = 1) +
    labs(title = paste0("Percent of Segment Max by: ", seg_value_title),
         y = "Percent of Segment Max (Relative Score)", x = "",
         caption = "Labelled objectives have a >= 10% difference in relative value within the objective") +
    theme_jtbd() +
    theme(legend.position = "none") +
    scale_x_discrete(expand = c(.05, 0))

  ggsave(filename = paste0("plot-rel_score_by-", seg_value_title, ".png"),
         path = save_path, width = 20, height = 10.68)

  return(p)
}

#' Plot absolute score bump chart
#'
#' Shows absolute opportunity scores across segments with linear significance labeling.
#'
#' @param your_data_frame A data frame from [get_jtbd_segment.comp.ordinal()]
#' @param seg_value_title Title label for the segment variable
#' @param last_value The segment value to label on the right side of the chart
#' @param save_path Directory to save the plot (default: [tempdir()])
#'
#' @return A ggplot object (also saved as PNG)
#' @export
#'
#' @family visualization
plot_this.graph.abs_score <- function(your_data_frame, seg_value_title, last_value, save_path = tempdir()) {
  p <- your_data_frame %>%
    arrange(seg.value) %>%
    remove_weird_text_formatting.jtbd() %>%
    ggplot(aes(y = score, x = fct_inorder(seg.value), group = objective, color = objective)) +
    geom_line(linewidth = 1.5) +
    geom_point(size = 5) +
    facet_wrap(~job_step) +
    labs(title = paste0("Absolute Opportunity Scores by: ", seg_value_title),
         y = "Opportunity Score", x = "") +
    theme_jtbd() +
    theme(legend.position = "none") +
    scale_x_discrete(expand = c(.05, 0))

  ggsave(filename = paste0("plot-abs_score_by-", seg_value_title, ".png"),
         path = save_path, width = 20, height = 10.68)

  return(p)
}

#' Plot job step scatter
#'
#' Creates an importance vs satisfaction scatter plot for a job step.
#'
#' @param x A data frame containing job step data
#' @param step_title The title for the plot
#' @param ... Additional arguments (unused)
#'
#' @return A ggplot object
#' @export
#'
#' @family visualization
plot.job_step <- function(x, step_title = "", ...) {
  your_plot <- x %>%
    ggplot(aes(x = imp.all, y = sat.all)) +
    geom_point(size = 3) +
    labs(title = step_title, x = "Importance", y = "Satisfaction") +
    theme_jtbd()
  return(your_plot)
}

#' Opportunity matrix plot
#'
#' Creates the classic ODI opportunity matrix: importance on x-axis, satisfaction
#' on y-axis, with high-opportunity objectives highlighted. Optionally shows
#' diagonal zone lines dividing the plot into Under-Served, Appropriately-Served,
#' Over-Served, and Table Stakes regions (from quantjtbd).
#'
#' @param scores A data frame from [get_jtbd_scores()] with imp, sat, and opp columns
#' @param title Plot title (default: "Opportunity Score Matrix")
#' @param subtitle Plot subtitle (default: NULL)
#' @param highlight_threshold Opportunity score threshold for highlighting (default: 10)
#' @param show_zones Show diagonal reference lines and zone labels (default: FALSE)
#'
#' @return A ggplot object
#' @export
#'
#' @family visualization
#'
#' @examples
#' data(jtbd_sample)
#' scores <- get_jtbd_scores(jtbd_sample)
#' # plot_opportunity_matrix(scores)
#' # plot_opportunity_matrix(scores, show_zones = TRUE)
plot_opportunity_matrix <- function(scores, title = "Opportunity Score Matrix",
                                    subtitle = NULL, highlight_threshold = 10,
                                    show_zones = FALSE) {
  # Detect the imp/sat/opp column names (they have segment suffixes)
  imp_col <- grep("^imp\\.", names(scores), value = TRUE)[1]
  sat_col <- grep("^sat\\.", names(scores), value = TRUE)[1]
  opp_col <- grep("^opp\\.", names(scores), value = TRUE)[1]

  if (is.na(imp_col) || is.na(sat_col) || is.na(opp_col)) {
    cli::cli_abort("Could not find imp/sat/opp columns. Run {.fn get_jtbd_scores} first.")
  }

  plot_df <- scores %>%
    mutate(
      .imp = .data[[imp_col]],
      .sat = .data[[sat_col]],
      .opp = .data[[opp_col]],
      .high_opp = if_else(.opp >= highlight_threshold, "High Opportunity", "Other")
    )

  p <- plot_df %>%
    ggplot(aes(x = .imp, y = .sat, color = .high_opp))

  # Add zone annotations if requested
  if (show_zones) {
    zone_color <- "#8E9EAB"
    p <- p +
      # Diagonal: sat = imp (appropriately served line)
      annotate("segment", x = 0, xend = 10, y = 0, yend = 10,
               color = zone_color, linetype = "dashed", linewidth = 0.4) +
      # Diagonal: sat = 2*imp - 10 (under-served boundary)
      annotate("segment", x = 5, xend = 10, y = 0, yend = 10,
               color = zone_color, linetype = "dashed", linewidth = 0.4) +
      # Table stakes line (high sat horizontal)
      annotate("segment", x = 0, xend = 10, y = 7.5, yend = 7.5,
               color = zone_color, linetype = "dotted", linewidth = 0.3) +
      # Zone labels
      annotate("text", x = 8, y = 1.5, label = "Under-Served",
               color = zone_color, size = 3, fontface = "italic") +
      annotate("text", x = 3.5, y = 1.5, label = "Appropriately\nServed",
               color = zone_color, size = 3, fontface = "italic") +
      annotate("text", x = 1, y = 4, label = "Over-\nServed",
               color = zone_color, size = 3, fontface = "italic") +
      annotate("text", x = 1, y = 8, label = "Table\nStakes",
               color = zone_color, size = 2.5, fontface = "italic")
  }

  p <- p +
    geom_point(size = 3, alpha = 0.8) +
    geom_text_repel(
      data = . %>% filter(.high_opp == "High Opportunity"),
      aes(label = stringr::str_wrap(gsub("_", " ", objective), 20)),
      size = 3, nudge_y = 0.2
    ) +
    scale_color_manual(values = c("High Opportunity" = "#E74C3C", "Other" = "#95A5A6")) +
    labs(title = title, subtitle = subtitle,
         x = "Importance", y = "Satisfaction", color = "") +
    coord_cartesian(xlim = c(0, 10), ylim = c(0, 10)) +
    theme_jtbd()

  return(p)
}

#' Cleveland (lollipop) comparison plot
#'
#' Compares two segments side-by-side using a Cleveland dot plot.
#' Ported from the quantjtbd package.
#'
#' @param data A data frame with objective and segment score columns
#' @param objective Unquoted column name for objectives
#' @param group_1 Unquoted column name for first segment scores
#' @param group_2 Unquoted column name for second segment scores
#' @param title_string Plot title
#' @param subtitle_string Plot subtitle
#'
#' @return A ggplot object
#' @export
#'
#' @family visualization
plot_cleveland <- function(data, objective, group_1, group_2, title_string, subtitle_string) {
  objective <- enquo(objective)
  group_1 <- enquo(group_1)
  group_2 <- enquo(group_2)

  plot <- data %>%
    ggplot() +
    geom_segment(aes(x = !!objective, xend = !!objective, y = !!group_1, yend = !!group_2), color = "grey") +
    geom_point(aes(x = !!objective, y = !!group_1), color = "#3498DB", size = 3) +
    geom_point(aes(x = !!objective, y = !!group_2), color = "#E74C3C", size = 3) +
    coord_flip() +
    labs(title = title_string, subtitle = subtitle_string,
         x = "Objectives", y = "Scores") +
    theme_jtbd()

  return(plot)
}
