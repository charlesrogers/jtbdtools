#' Automatically profile discovered segments
#'
#' For each profiling variable (demographics, behaviors, etc.), tests whether
#' the distribution differs significantly across clusters using chi-squared tests.
#' Returns a ranked summary showing which attributes best distinguish each segment.
#'
#' @param df A data frame with a cluster column and profiling columns
#' @param cluster_col Name of the cluster column (default: "jtbd_cluster")
#' @param profile_cols Character vector of column names to profile. If NULL,
#'   auto-detects all non-imp/sat/cluster/caseid columns.
#' @param alpha Significance level (default: 0.05)
#'
#' @return A list with:
#'   - `summary`: tibble with one row per profiling variable — p-value, effect size (Cramer's V), significance flag
#'   - `details`: list of per-variable breakdowns showing distribution per cluster
#'   - `distinguishing`: the top distinguishing attributes ranked by effect size
#' @export
#'
#' @family clustering
#'
#' @examples
#' data(jtbd_sample)
#' cl <- jtbd_cluster(jtbd_sample, n_clusters = 3)
#' profile <- jtbd_profile_segments(cl$data)
#' profile$summary
#' profile$distinguishing
jtbd_profile_segments <- function(df, cluster_col = "jtbd_cluster", profile_cols = NULL, alpha = 0.05) {
  if (!cluster_col %in% names(df)) {
    cli::cli_abort("Column {.val {cluster_col}} not found. Run {.fn jtbd_cluster} first.")
  }

  # Auto-detect profiling columns: everything that's not imp/sat/cluster/caseid
  if (is.null(profile_cols)) {
    skip_patterns <- c("^imp__", "^sat__", "^caseid$", paste0("^", cluster_col, "$"))
    skip_regex <- paste(skip_patterns, collapse = "|")
    profile_cols <- names(df)[!grepl(skip_regex, names(df))]
    # Only keep factor/character columns (demographics, not numeric IDs)
    keep <- sapply(df[profile_cols], function(x) is.factor(x) || is.character(x))
    profile_cols <- profile_cols[keep]
  }

  if (length(profile_cols) == 0) {
    cli::cli_warn("No profiling columns found. Add demographic/behavioral columns to your data.")
    return(list(summary = tibble::tibble(), details = list(), distinguishing = tibble::tibble()))
  }

  clusters <- df[[cluster_col]]
  n_clusters <- length(unique(clusters))
  n_total <- nrow(df)

  summary_rows <- list()
  details_list <- list()

  for (col_name in profile_cols) {
    col_data <- df[[col_name]]
    if (is.character(col_data)) col_data <- factor(col_data)

    # Cross-tabulation
    ct <- table(clusters, col_data)

    # Chi-squared test
    chi_result <- tryCatch(
      suppressWarnings(stats::chisq.test(ct)),
      error = function(e) NULL
    )

    if (is.null(chi_result)) {
      p_val <- NA_real_
      cramers_v <- NA_real_
    } else {
      p_val <- chi_result$p.value
      # Cramer's V effect size
      n_obs <- sum(ct)
      k <- min(nrow(ct), ncol(ct))
      cramers_v <- sqrt(chi_result$statistic / (n_obs * (k - 1)))
      cramers_v <- as.numeric(cramers_v)
    }

    summary_rows[[col_name]] <- data.frame(
      variable = col_name,
      p_value = round(p_val, 4),
      cramers_v = round(cramers_v, 3),
      significant = !is.na(p_val) & p_val < alpha,
      effect = case_when(
        is.na(cramers_v) ~ "unknown",
        cramers_v >= 0.35 ~ "large",
        cramers_v >= 0.20 ~ "medium",
        cramers_v >= 0.10 ~ "small",
        TRUE ~ "negligible"
      ),
      stringsAsFactors = FALSE
    )

    # Per-cluster distribution (percentages)
    pct_table <- prop.table(ct, margin = 1) * 100
    detail_df <- as.data.frame(pct_table)
    colnames(detail_df) <- c("cluster", "value", "pct")
    detail_df$pct <- round(detail_df$pct, 1)
    detail_df$variable <- col_name

    # Find over/under-indexed values per cluster
    overall_pct <- prop.table(table(col_data)) * 100
    detail_df$overall_pct <- round(as.numeric(overall_pct[as.character(detail_df$value)]), 1)
    detail_df$index <- round(detail_df$pct / detail_df$overall_pct * 100)
    detail_df$index[is.nan(detail_df$index) | is.infinite(detail_df$index)] <- NA

    details_list[[col_name]] <- tibble::as_tibble(detail_df)
  }

  # Build summary
  summary_df <- do.call(rbind, summary_rows) %>%
    tibble::as_tibble() %>%
    arrange(p_value)

  # Build distinguishing attributes (significant, ranked by effect size)
  distinguishing <- summary_df %>%
    filter(significant) %>%
    arrange(desc(cramers_v))

  n_sig <- nrow(distinguishing)
  cli::cli_inform(c(
    "v" = "Profiled {length(profile_cols)} variable{?s} across {n_clusters} segments.",
    "i" = "{n_sig} variable{?s} significantly distinguish segments (p < {alpha}).",
    if (n_sig > 0) c("i" = "Top distinguisher{?s}: {paste(distinguishing$variable[1:min(3, n_sig)], collapse = ', ')}")
  ))

  result <- list(
    summary = summary_df,
    details = details_list,
    distinguishing = distinguishing
  )
  class(result) <- c("jtbd_profile", "list")
  return(result)
}

#' Plot segment profiles
#'
#' For each distinguishing profiling variable, shows the distribution across
#' clusters as grouped bar charts with an overall reference line. Highlights
#' where clusters are over- or under-indexed relative to the population.
#'
#' @param profile_result Result from [jtbd_profile_segments()]
#' @param max_vars Maximum number of variables to plot (default: 4, ordered by effect size)
#' @param title Plot title
#'
#' @return A ggplot object
#' @export
#'
#' @family clustering
#'
#' @examples
#' data(jtbd_sample)
#' cl <- jtbd_cluster(jtbd_sample, n_clusters = 3)
#' prof <- jtbd_profile_segments(cl$data)
#' plot_segment_profiles(prof)
plot_segment_profiles <- function(profile_result, max_vars = 4,
                                   title = "Who's In Each Segment?") {
  # Get top distinguishing variables
  top_vars <- profile_result$distinguishing$variable[1:min(max_vars, nrow(profile_result$distinguishing))]

  if (length(top_vars) == 0) {
    # Fall back to all variables if none significant
    top_vars <- profile_result$summary$variable[1:min(max_vars, nrow(profile_result$summary))]
  }

  # Combine detail data for top variables
  plot_data <- do.call(rbind, profile_result$details[top_vars])
  plot_data$variable <- factor(plot_data$variable, levels = top_vars)

  # Clean labels
  plot_data$cluster_label <- gsub("_", " ", as.character(plot_data$cluster))

  ggplot(plot_data, aes(x = value, y = pct, fill = cluster_label)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.7, alpha = 0.85) +
    geom_point(aes(y = overall_pct), shape = 4, size = 2.5, color = "#2C3E50",
               position = position_dodge(width = 0.8), show.legend = FALSE) +
    facet_wrap(~variable, scales = "free_x", ncol = 2) +
    scale_y_continuous(labels = function(x) paste0(x, "%"), expand = expansion(mult = c(0, 0.1))) +
    labs(title = title,
         subtitle = "Bars = segment composition. X marks = overall population average.",
         x = "", y = "% of Segment", fill = "Segment") +
    theme_jtbd() +
    theme(
      axis.text.x = element_text(angle = 30, hjust = 1, size = 9),
      strip.text = element_text(face = "bold", size = 11),
      legend.position = "top"
    )
}

#' Plot segment index heatmap
#'
#' Shows how each segment over- or under-indexes on profiling attributes
#' relative to the overall population. Index of 100 = same as population,
#' >100 = over-represented, <100 = under-represented.
#'
#' @param profile_result Result from [jtbd_profile_segments()]
#' @param max_vars Maximum number of variables to show (default: 4)
#' @param title Plot title
#'
#' @return A ggplot object
#' @export
#'
#' @family clustering
#'
#' @examples
#' data(jtbd_sample)
#' cl <- jtbd_cluster(jtbd_sample, n_clusters = 3)
#' prof <- jtbd_profile_segments(cl$data)
#' plot_segment_index(prof)
plot_segment_index <- function(profile_result, max_vars = 4,
                                title = "Segment Index vs Population") {
  top_vars <- profile_result$distinguishing$variable[1:min(max_vars, nrow(profile_result$distinguishing))]
  if (length(top_vars) == 0) {
    top_vars <- profile_result$summary$variable[1:min(max_vars, nrow(profile_result$summary))]
  }

  plot_data <- do.call(rbind, profile_result$details[top_vars])
  plot_data$variable <- factor(plot_data$variable, levels = top_vars)
  plot_data$label <- paste0(plot_data$variable, ": ", plot_data$value)
  plot_data$cluster_label <- gsub("_", " ", as.character(plot_data$cluster))

  # Filter to interesting rows (not NA index, and not tiny overall %)
  plot_data <- plot_data[!is.na(plot_data$index) & plot_data$overall_pct >= 5, ]

  ggplot(plot_data, aes(x = cluster_label, y = label, fill = index)) +
    geom_tile(color = "white", linewidth = 1.2) +
    geom_text(aes(label = index,
                  color = abs(index - 100) > 20),
              size = 3.5, fontface = "bold", show.legend = FALSE) +
    scale_color_manual(values = c("TRUE" = "white", "FALSE" = "#2C3E50")) +
    scale_fill_gradient2(low = "#3498DB", mid = "#F7F7F7", high = "#E74C3C",
                         midpoint = 100, name = "Index\n(100 = avg)") +
    labs(title = title,
         subtitle = ">100 = over-represented in segment. <100 = under-represented.",
         x = "", y = "") +
    theme_jtbd() +
    theme(panel.grid = element_blank(),
          axis.line = element_blank(),
          axis.text.x = element_text(face = "bold", size = 11))
}
