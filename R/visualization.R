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
    zone_color <- "#B0BEC5"
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
      # Zone labels — placed in corners where data points won't be
      annotate("label", x = 9.5, y = 0.5, label = "UNDER-\nSERVED",
               color = zone_color, size = 2.8, fontface = "bold.italic",
               fill = "white", label.size = 0, alpha = 0.85, hjust = 1, vjust = 0) +
      annotate("label", x = 0.5, y = 0.5, label = "APPROPRIATELY\nSERVED",
               color = zone_color, size = 2.8, fontface = "bold.italic",
               fill = "white", label.size = 0, alpha = 0.85, hjust = 0, vjust = 0) +
      annotate("label", x = 0.5, y = 6.8, label = "OVER-\nSERVED",
               color = zone_color, size = 2.8, fontface = "bold.italic",
               fill = "white", label.size = 0, alpha = 0.85, hjust = 0) +
      annotate("label", x = 0.5, y = 9.5, label = "TABLE STAKES",
               color = zone_color, size = 2.5, fontface = "bold.italic",
               fill = "white", label.size = 0, alpha = 0.85, hjust = 0, vjust = 1)
  }

  p <- p +
    geom_point(size = 3, alpha = 0.8) +
    geom_text_repel(
      data = . %>% filter(.high_opp == "High Opportunity"),
      aes(label = stringr::str_wrap(gsub("_", " ", objective), 20)),
      size = 3, nudge_y = 0.3, box.padding = 0.6, point.padding = 0.4,
      min.segment.length = 0.3, max.overlaps = 20, seed = 42
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

# ============================================================
# Cluster Visualization Functions
# ============================================================

#' PCA scree plot
#'
#' Shows eigenvalues per component with a Kaiser line at 1.0 to help
#' determine how many components to retain.
#'
#' @param pca_result Result from [jtbd_pca()]
#'
#' @return A ggplot object
#' @export
#'
#' @family clustering
#'
#' @examples
#' data(jtbd_sample)
#' pca <- jtbd_pca(jtbd_sample)
#' plot_pca_scree(pca)
plot_pca_scree <- function(pca_result) {
  ve <- pca_result$variance_explained

  ggplot(ve, aes(x = component, y = eigenvalue)) +
    geom_line(linewidth = 1, color = "#2C3E50") +
    geom_point(size = 3, color = "#2C3E50") +
    geom_hline(yintercept = 1, linetype = "dashed", color = "#E74C3C", linewidth = 0.5) +
    annotate("text", x = max(ve$component) - 0.5, y = 1.1, label = "Kaiser Rule (eigenvalue = 1)",
             color = "#E74C3C", size = 3, fontface = "italic", hjust = 1) +
    geom_text(aes(label = paste0(variance_pct, "%")), nudge_y = 0.08, size = 3, color = "#7F8C8D") +
    scale_x_continuous(breaks = ve$component) +
    labs(title = "PCA Scree Plot",
         subtitle = paste0("Retained ", pca_result$n_components, " components (",
                           ve$cumulative_pct[pca_result$n_components], "% variance explained)"),
         x = "Principal Component", y = "Eigenvalue") +
    theme_jtbd()
}

#' PCA loadings bar chart
#'
#' Shows how each objective loads on a given principal component.
#' High-loading objectives define the "theme" of that component.
#'
#' @param pca_result Result from [jtbd_pca()]
#' @param component Which component to plot (default: 1)
#'
#' @return A ggplot object
#' @export
#'
#' @family clustering
plot_pca_loadings <- function(pca_result, component = 1) {
  loadings <- as.data.frame(pca_result$loadings)
  loadings$objective <- rownames(loadings)
  col_name <- colnames(loadings)[component]

  loadings$loading <- loadings[[col_name]]
  loadings$label <- gsub("_", " ", gsub("^[^.]+\\.", "", loadings$objective))
  loadings$label <- tools::toTitleCase(loadings$label)
  loadings <- loadings[order(loadings$loading), ]
  loadings$label <- factor(loadings$label, levels = loadings$label)

  ggplot(loadings, aes(x = label, y = loading, fill = loading > 0)) +
    geom_col(width = 0.7) +
    geom_hline(yintercept = c(-0.3, 0.3), linetype = "dotted", color = "#7F8C8D") +
    coord_flip() +
    scale_fill_manual(values = c("TRUE" = "#2ECC71", "FALSE" = "#E74C3C"), guide = "none") +
    labs(title = paste0("PCA Loadings: Component ", component),
         subtitle = "Objectives that define this component's theme",
         x = "", y = "Loading") +
    theme_jtbd() +
    theme(axis.line.y = element_blank())
}

#' PCA biplot
#'
#' 2D scatter of respondents on PC1 vs PC2, optionally colored by cluster.
#'
#' @param pca_result Result from [jtbd_pca()]
#' @param cluster_labels Optional factor/integer vector of cluster assignments
#'
#' @return A ggplot object
#' @export
#'
#' @family clustering
#'
#' @examples
#' data(jtbd_sample)
#' cl <- jtbd_cluster(jtbd_sample, n_clusters = 3)
#' plot_pca_biplot(cl$pca, cl$cluster)
plot_pca_biplot <- function(pca_result, cluster_labels = NULL) {
  scores_df <- as.data.frame(pca_result$scores[, 1:2])
  colnames(scores_df) <- c("PC1", "PC2")

  if (!is.null(cluster_labels)) {
    scores_df$Cluster <- factor(paste0("Segment ", cluster_labels))
    p <- ggplot(scores_df, aes(x = PC1, y = PC2, color = Cluster)) +
      geom_point(size = 2, alpha = 0.7) +
      stat_ellipse(level = 0.68, linewidth = 0.8, linetype = "dashed")
  } else {
    p <- ggplot(scores_df, aes(x = PC1, y = PC2)) +
      geom_point(size = 2, alpha = 0.5, color = "#2C3E50")
  }

  ve <- pca_result$variance_explained
  p + labs(
    title = "Respondent Clusters in PCA Space",
    subtitle = paste0("PC1 (", ve$variance_pct[1], "%) vs PC2 (", ve$variance_pct[2], "%)"),
    x = paste0("PC1 (", ve$variance_pct[1], "% variance)"),
    y = paste0("PC2 (", ve$variance_pct[2], "% variance)")
  ) +
  theme_jtbd()
}

#' Elbow plot for cluster evaluation
#'
#' Shows within-cluster sum of squares (WCSS) and average silhouette width
#' for different numbers of clusters.
#'
#' @param k_results Result from [jtbd_find_k()]
#'
#' @return A ggplot object
#' @export
#'
#' @family clustering
#'
#' @examples
#' data(jtbd_sample)
#' k_eval <- jtbd_find_k(jtbd_sample, max_k = 5)
#' plot_elbow(k_eval)
plot_elbow <- function(k_results) {
  # Normalize WCSS to 0-1 range for dual axis
  wcss_range <- range(k_results$wcss)
  k_results$wcss_norm <- (k_results$wcss - wcss_range[1]) / (wcss_range[2] - wcss_range[1])

  best_k <- k_results$k[which.max(k_results$avg_silhouette)]

  ggplot(k_results, aes(x = k)) +
    geom_line(aes(y = wcss_norm), color = "#2C3E50", linewidth = 1) +
    geom_point(aes(y = wcss_norm), color = "#2C3E50", size = 3) +
    geom_line(aes(y = avg_silhouette), color = "#E74C3C", linewidth = 1) +
    geom_point(aes(y = avg_silhouette), color = "#E74C3C", size = 3) +
    geom_vline(xintercept = best_k, linetype = "dashed", color = "#7F8C8D") +
    annotate("text", x = best_k + 0.15, y = 0.95, label = paste0("Best k = ", best_k),
             color = "#7F8C8D", size = 3.5, hjust = 0, fontface = "italic") +
    annotate("text", x = max(k_results$k), y = k_results$wcss_norm[nrow(k_results)] + 0.05,
             label = "WCSS", color = "#2C3E50", size = 3.5, fontface = "bold", hjust = 1) +
    annotate("text", x = max(k_results$k), y = k_results$avg_silhouette[nrow(k_results)] + 0.05,
             label = "Silhouette", color = "#E74C3C", size = 3.5, fontface = "bold", hjust = 1) +
    scale_x_continuous(breaks = k_results$k) +
    scale_y_continuous(limits = c(0, 1)) +
    labs(title = "Cluster Evaluation: Elbow + Silhouette",
         subtitle = "Lower WCSS = tighter clusters. Higher silhouette = better separation.",
         x = "Number of Clusters (k)", y = "Normalized Score") +
    theme_jtbd()
}

#' Cluster opportunity heatmap
#'
#' Heatmap of opportunity scores across discovered clusters, showing which
#' objectives are most underserved in each segment.
#'
#' @param profile Result from [jtbd_cluster_profile()] or [jtbd_segment()]$profile
#' @param title Plot title
#'
#' @return A ggplot object
#' @export
#'
#' @family clustering
#'
#' @examples
#' data(jtbd_sample)
#' result <- jtbd_segment(jtbd_sample, n_clusters = 3)
#' plot_cluster_heatmap(result$profile)
plot_cluster_heatmap <- function(profile, title = "Opportunity Heatmap by Discovered Segment") {
  opp_cols <- grep("^opp\\.", names(profile), value = TRUE)

  clean_obj_local <- function(x) {
    x <- gsub("minimize_time_to_", "", x)
    x <- gsub("minimize_likelihood_of_", "Avoid ", x)
    x <- gsub("_", " ", x)
    tools::toTitleCase(x)
  }

  heat_df <- profile %>%
    select(job_step, objective, all_of(opp_cols)) %>%
    mutate(label = clean_obj_local(as.character(objective))) %>%
    arrange(desc(.data[[opp_cols[1]]])) %>%
    mutate(label = factor(label, levels = rev(label))) %>%
    select(label, all_of(opp_cols)) %>%
    pivot_longer(cols = all_of(opp_cols), names_to = "segment", values_to = "opp") %>%
    mutate(
      segment = gsub("^opp\\.", "", segment),
      segment = gsub("_", " ", segment),
      segment = tools::toTitleCase(segment)
    )

  ggplot(heat_df, aes(x = segment, y = label, fill = opp)) +
    geom_tile(color = "white", linewidth = 1.5) +
    geom_text(aes(label = round(opp, 1),
                  color = opp > 13), size = 3.5, fontface = "bold", show.legend = FALSE) +
    scale_color_manual(values = c("TRUE" = "white", "FALSE" = "#2C3E50")) +
    scale_fill_gradient2(low = "#F7F7F7", mid = "#FDEBD0", high = "#C0392B",
                         midpoint = 10, name = "Opportunity\nScore") +
    labs(title = title,
         subtitle = "Darker = bigger unmet need. Compare columns to find segment-specific pain.",
         x = "", y = "") +
    theme_jtbd() +
    theme(panel.grid = element_blank(),
          axis.line = element_blank(),
          axis.text.x = element_text(face = "bold", size = 11))
}
