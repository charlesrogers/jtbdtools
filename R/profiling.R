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
#'   - `summary`: tibble with one row per profiling variable
#'   - `details`: list of per-variable breakdowns showing distribution per cluster
#'   - `distinguishing`: the top distinguishing attributes ranked by effect size
#'   - `personas`: "this not that" persona table per cluster
#' @export
#'
#' @family clustering
#'
#' @examples
#' data(jtbd_sample)
#' cl <- jtbd_cluster(jtbd_sample, n_clusters = 3)
#' profile <- jtbd_profile_segments(cl$data)
#' profile$summary
#' profile$personas
jtbd_profile_segments <- function(df, cluster_col = "jtbd_cluster", profile_cols = NULL, alpha = 0.05) {
  if (!cluster_col %in% names(df)) {
    cli::cli_abort("Column {.val {cluster_col}} not found. Run {.fn jtbd_cluster} first.")
  }

  # Auto-detect profiling columns: everything that's not imp/sat/cluster/caseid
  if (is.null(profile_cols)) {
    skip_patterns <- c("^imp__", "^sat__", "^caseid$", paste0("^", cluster_col, "$"))
    skip_regex <- paste(skip_patterns, collapse = "|")
    profile_cols <- names(df)[!grepl(skip_regex, names(df))]
    keep <- sapply(df[profile_cols], function(x) is.factor(x) || is.character(x))
    profile_cols <- profile_cols[keep]
  }

  if (length(profile_cols) == 0) {
    cli::cli_warn("No profiling columns found. Add demographic/behavioral columns to your data.")
    return(list(summary = tibble::tibble(), details = list(), distinguishing = tibble::tibble(), personas = tibble::tibble()))
  }

  clusters <- df[[cluster_col]]
  n_clusters <- length(unique(clusters))

  summary_rows <- list()
  details_list <- list()

  for (col_name in profile_cols) {
    col_data <- df[[col_name]]
    if (is.character(col_data)) col_data <- factor(col_data)

    ct <- table(clusters, col_data)

    chi_result <- tryCatch(
      suppressWarnings(stats::chisq.test(ct)),
      error = function(e) NULL
    )

    if (is.null(chi_result)) {
      p_val <- NA_real_
      cramers_v <- NA_real_
    } else {
      p_val <- chi_result$p.value
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

    pct_table <- prop.table(ct, margin = 1) * 100
    detail_df <- as.data.frame(pct_table)
    colnames(detail_df) <- c("cluster", "value", "pct")
    detail_df$pct <- round(detail_df$pct, 1)
    detail_df$variable <- col_name

    overall_pct <- prop.table(table(col_data)) * 100
    detail_df$overall_pct <- round(as.numeric(overall_pct[as.character(detail_df$value)]), 1)
    detail_df$index <- round(detail_df$pct / detail_df$overall_pct * 100)
    detail_df$index[is.nan(detail_df$index) | is.infinite(detail_df$index)] <- NA

    details_list[[col_name]] <- tibble::as_tibble(detail_df)
  }

  summary_df <- do.call(rbind, summary_rows) %>%
    tibble::as_tibble() %>%
    arrange(p_value)

  distinguishing <- summary_df %>%
    filter(significant) %>%
    arrange(desc(cramers_v))

  # Build "this not that" personas
  personas <- .build_personas(details_list, distinguishing, summary_df)

  n_sig <- nrow(distinguishing)
  cli::cli_inform(c(
    "v" = "Profiled {length(profile_cols)} variable{?s} across {n_clusters} segments.",
    "i" = "{n_sig} variable{?s} significantly distinguish segments (p < {alpha}).",
    if (n_sig > 0) c("i" = "Top distinguisher{?s}: {paste(distinguishing$variable[1:min(3, n_sig)], collapse = ', ')}")
  ))

  result <- list(
    summary = summary_df,
    details = details_list,
    distinguishing = distinguishing,
    personas = personas
  )
  class(result) <- c("jtbd_profile", "list")
  return(result)
}

#' Build "this not that" persona descriptions (internal)
#' @noRd
.build_personas <- function(details_list, distinguishing, summary_df) {
  all_details <- do.call(rbind, details_list)
  if (nrow(all_details) == 0) return(tibble::tibble())

  cluster_names <- unique(as.character(all_details$cluster))

  persona_rows <- list()
  for (cl in cluster_names) {
    cl_data <- all_details[all_details$cluster == cl & !is.na(all_details$index) & all_details$overall_pct >= 5, ]

    # "More likely" = index >= 120 (20%+ over-represented)
    more <- cl_data[cl_data$index >= 120, ]
    more <- more[order(-more$index), ]
    more_labels <- paste0(more$value, " (", more$index, ")")

    # "Less likely" = index <= 80 (20%+ under-represented)
    less <- cl_data[cl_data$index <= 80, ]
    less <- less[order(less$index), ]
    less_labels <- paste0(less$value, " (", less$index, ")")

    # Size
    cl_n <- sum(all_details$cluster == cl) / length(unique(all_details$variable)) / length(unique(all_details$value))

    persona_rows[[cl]] <- data.frame(
      segment = cl,
      more_likely = paste(utils::head(more_labels, 5), collapse = ", "),
      less_likely = paste(utils::head(less_labels, 5), collapse = ", "),
      stringsAsFactors = FALSE
    )
  }

  do.call(rbind, persona_rows) %>% tibble::as_tibble()
}

#' Plot segment divergence from population
#'
#' For each attribute, shows a diverging bar chart of how much each segment
#' over- or under-indexes relative to the overall population. Only shows
#' attributes with meaningful divergence (index > 115 or < 85).
#'
#' @param profile_result Result from [jtbd_profile_segments()]
#' @param max_vars Maximum number of profiling variables to include (default: 6)
#' @param title Plot title
#' @param n,study Sample size and study label for the plot footer (see [jtbd_footer()]).
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
plot_segment_profiles <- function(profile_result, max_vars = 6,
                                   title = "Segment DNA: Who's In Each Group?",
                                   n = NULL, study = NULL) {
  top_vars <- profile_result$summary$variable[1:min(max_vars, nrow(profile_result$summary))]

  plot_data <- do.call(rbind, profile_result$details[top_vars])
  plot_data <- plot_data[!is.na(plot_data$index) & plot_data$overall_pct >= 5, ]
  plot_data$deviation <- plot_data$index - 100
  plot_data$cluster_label <- gsub("Segment_", "Seg ", as.character(plot_data$cluster))
  plot_data$attr_label <- paste0(tools::toTitleCase(gsub("_", " ", plot_data$variable)), ": ", plot_data$value)

  # Only keep attributes with meaningful divergence in at least one segment
  plot_data <- plot_data %>%
    group_by(attr_label) %>%
    filter(max(abs(deviation)) >= 15) %>%
    ungroup()

  if (nrow(plot_data) == 0) {
    cli::cli_warn("No attributes with meaningful divergence found.")
    return(ggplot() + theme_void())
  }

  # Order by max absolute deviation
  attr_order <- plot_data %>%
    group_by(attr_label) %>%
    summarize(max_dev = max(abs(deviation)), .groups = "drop") %>%
    arrange(max_dev)
  plot_data$attr_label <- factor(plot_data$attr_label, levels = attr_order$attr_label)

  ggplot(plot_data, aes(x = attr_label, y = deviation, fill = cluster_label)) +
    geom_col(position = position_dodge(width = 0.75), width = 0.65, alpha = 0.9) +
    geom_hline(yintercept = 0, linewidth = 0.6, color = "#2C3E50") +
    geom_text(aes(label = ifelse(abs(deviation) >= 15, paste0(ifelse(deviation > 0, "+", ""), deviation), ""),
                  hjust = ifelse(deviation >= 0, -0.1, 1.1)),
              position = position_dodge(width = 0.75), size = 2.8, fontface = "bold") +
    coord_flip() +
    scale_fill_manual(values = c("#E74C3C", "#3498DB", "#2ECC71", "#F39C12", "#9B59B6")[1:length(unique(plot_data$cluster_label))]) +
    scale_y_continuous(labels = function(x) paste0(ifelse(x > 0, "+", ""), x),
                       expand = expansion(mult = c(0.15, 0.15))) +
    labs(title = title,
         subtitle = "How each segment deviates from the population average (index 100 = average)",
         x = "", y = "Deviation from Average (index points)", fill = "",
         caption = jtbd_footer(n = n, study = study)) +
    theme_jtbd() +
    theme(panel.grid.major.y = element_blank(),
          axis.line.y = element_blank(),
          legend.position = "top",
          legend.text = element_text(size = 11, face = "bold"))
}

#' Plot segment index heatmap
#'
#' Shows how each segment over- or under-indexes on profiling attributes
#' relative to the overall population. Index of 100 = same as population.
#'
#' @param profile_result Result from [jtbd_profile_segments()]
#' @param max_vars Maximum number of variables to show (default: 6)
#' @param title Plot title
#' @param n,study Sample size and study label for the plot footer (see [jtbd_footer()]).
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
plot_segment_index <- function(profile_result, max_vars = 6,
                                title = "Segment Index: Over & Under-Represented Traits",
                                n = NULL, study = NULL) {
  top_vars <- profile_result$summary$variable[1:min(max_vars, nrow(profile_result$summary))]

  plot_data <- do.call(rbind, profile_result$details[top_vars])
  plot_data <- plot_data[!is.na(plot_data$index) & plot_data$overall_pct >= 5, ]
  plot_data$cluster_label <- gsub("Segment_", "Seg ", as.character(plot_data$cluster))

  # Clean attribute labels
  plot_data$attr_label <- paste0(
    tools::toTitleCase(gsub("_", " ", plot_data$variable)),
    ": ", plot_data$value
  )

  # Only keep rows with meaningful deviation
  plot_data <- plot_data %>%
    group_by(attr_label) %>%
    filter(max(abs(index - 100)) >= 10) %>%
    ungroup()

  # Order by max deviation
  attr_order <- plot_data %>%
    group_by(attr_label) %>%
    summarize(max_dev = max(abs(index - 100)), .groups = "drop") %>%
    arrange(max_dev)
  plot_data$attr_label <- factor(plot_data$attr_label, levels = attr_order$attr_label)

  ggplot(plot_data, aes(x = cluster_label, y = attr_label, fill = index)) +
    geom_tile(color = "white", linewidth = 2) +
    geom_text(aes(label = index), size = 4.5, fontface = "bold",
              color = ifelse(abs(plot_data$index - 100) > 25, "white", "#2C3E50")) +
    scale_fill_gradient2(
      low = "#2980B9", mid = "#F8F9FA", high = "#C0392B",
      midpoint = 100, limits = c(40, 230),
      oob = scales::squish,
      name = "Index"
    ) +
    labs(title = title,
         subtitle = "100 = population average. Red = over-represented. Blue = under-represented.",
         x = "", y = "",
         caption = jtbd_footer(n = n, study = study)) +
    theme_jtbd() +
    theme(panel.grid = element_blank(),
          axis.line = element_blank(),
          axis.text.x = element_text(face = "bold", size = 12),
          axis.text.y = element_text(size = 10),
          legend.position = "right")
}

#' Create segment persona cards as a gt table
#'
#' Generates a publication-ready "This / Not That" table showing the defining
#' characteristics of each discovered segment. Over-indexed attributes (index
#' >= 120) are listed as "More Likely", under-indexed (index <= 80) as "Less Likely".
#'
#' @param profile_result Result from [jtbd_profile_segments()]
#' @param cluster_profile Optional result from [jtbd_cluster_profile()] to include
#'   top opportunity scores in the persona
#' @param n,study Sample size and study label rendered as a footer (see [jtbd_footer()]).
#'
#' @return A gt table object
#' @export
#'
#' @family clustering
#'
#' @examples
#' data(jtbd_sample)
#' cl <- jtbd_cluster(jtbd_sample, n_clusters = 3)
#' prof <- jtbd_profile_segments(cl$data)
#' opp_profile <- jtbd_cluster_profile(jtbd_sample, cl, test_sig = FALSE)
#' create_persona_table(prof, opp_profile)
create_persona_table <- function(profile_result, cluster_profile = NULL,
                                 n = NULL, study = NULL) {
  all_details <- do.call(rbind, profile_result$details)
  if (nrow(all_details) == 0) {
    cli::cli_warn("No profiling data available.")
    return(gt::gt(data.frame(message = "No data")))
  }

  cluster_names <- sort(unique(as.character(all_details$cluster)))

  clean_attr <- function(variable, value) {
    var_clean <- tools::toTitleCase(gsub("_", " ", variable))
    paste0(var_clean, ": ", value)
  }

  rows <- list()
  for (cl in cluster_names) {
    cl_data <- all_details[all_details$cluster == cl & !is.na(all_details$index) & all_details$overall_pct >= 5, ]
    cl_label <- gsub("_", " ", cl)

    # Size
    n_in_cluster <- nrow(profile_result$details[[1]][profile_result$details[[1]]$cluster == cl, ]) # approximate
    # Actually compute from the data
    size_row <- cl_data[1, ]  # just need cluster name

    # More likely (index >= 120)
    more <- cl_data[cl_data$index >= 115, ]
    more <- more[order(-more$index), ]
    more_text <- if (nrow(more) > 0) {
      paste(sapply(seq_len(min(5, nrow(more))), function(i) {
        paste0(clean_attr(more$variable[i], more$value[i]), " (", more$index[i], ")")
      }), collapse = "\n")
    } else "--"

    # Less likely (index <= 80)
    less <- cl_data[cl_data$index <= 85, ]
    less <- less[order(less$index), ]
    less_text <- if (nrow(less) > 0) {
      paste(sapply(seq_len(min(5, nrow(less))), function(i) {
        paste0(clean_attr(less$variable[i], less$value[i]), " (", less$index[i], ")")
      }), collapse = "\n")
    } else "--"

    # Top unmet needs (from cluster_profile if provided)
    needs_text <- "--"
    if (!is.null(cluster_profile)) {
      opp_col <- paste0("opp.", cl)
      if (opp_col %in% names(cluster_profile)) {
        opp_data <- cluster_profile[order(-cluster_profile[[opp_col]]), ]
        top3 <- utils::head(opp_data, 3)
        obj_clean <- gsub("_", " ", as.character(top3$objective))
        obj_clean <- gsub("minimize time to ", "", obj_clean)
        obj_clean <- gsub("minimize likelihood of ", "Avoid ", obj_clean)
        obj_clean <- tools::toTitleCase(obj_clean)
        needs_text <- paste(paste0(obj_clean, " (", round(top3[[opp_col]], 1), ")"), collapse = "\n")
      }
    }

    rows[[cl]] <- data.frame(
      Segment = cl_label,
      `More Likely` = more_text,
      `Less Likely` = less_text,
      `Top Unmet Needs` = needs_text,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  }

  persona_df <- do.call(rbind, rows)

  tbl <- persona_df %>%
    gt::gt() %>%
    gt::tab_header(
      title = "Outcome-Based Segment Personas",
      subtitle = "\"This, Not That\" -- who they are, what they need"
    ) %>%
    gt::cols_label(
      Segment = "",
      `More Likely` = "More Likely (over-indexed)",
      `Less Likely` = "Less Likely (under-indexed)",
      `Top Unmet Needs` = "Top Unmet Needs"
    ) %>%
    gt::tab_style(
      style = gt::cell_text(weight = "bold", size = gt::px(15)),
      locations = gt::cells_body(columns = "Segment")
    ) %>%
    gt::tab_style(
      style = gt::cell_text(color = "#C0392B"),
      locations = gt::cells_body(columns = "More Likely")
    ) %>%
    gt::tab_style(
      style = gt::cell_text(color = "#2980B9"),
      locations = gt::cells_body(columns = "Less Likely")
    ) %>%
    gt::tab_style(
      style = gt::cell_text(color = "#2C3E50", weight = "bold"),
      locations = gt::cells_body(columns = "Top Unmet Needs")
    ) %>%
    gt::tab_options(
      heading.align = "left",
      column_labels.font.weight = "bold",
      column_labels.border.bottom.width = 2,
      column_labels.border.bottom.color = "#2C3E50",
      data_row.padding = gt::px(10),
      table.font.size = 13,
      table.border.top.style = "none",
      table.border.bottom.style = "none"
    ) %>%
    gt::cols_width(
      Segment ~ gt::px(100),
      `More Likely` ~ gt::px(250),
      `Less Likely` ~ gt::px(250),
      `Top Unmet Needs` ~ gt::px(250)
    )

  footer <- jtbd_footer(n = n, study = study)
  if (!is.null(footer)) {
    tbl <- tbl %>% gt::tab_source_note(source_note = footer)
  }

  return(tbl)
}

# ============================================================
# Internal helper: get top indexed attributes for profile plots
# ============================================================
.get_profile_plot_data <- function(profile_result, max_attrs = 8) {
  all_details <- do.call(rbind, profile_result$details)
  plot_data <- all_details[!is.na(all_details$index) & all_details$overall_pct >= 5, ]
  plot_data$cluster_label <- gsub("Segment_", "Seg ", as.character(plot_data$cluster))
  plot_data$category <- tools::toTitleCase(gsub("_", " ", plot_data$variable))
  plot_data$attr_label <- paste0(plot_data$category, ": ", plot_data$value)

  plot_data <- plot_data %>%
    group_by(attr_label) %>%
    filter(max(abs(index - 100)) >= 15) %>%
    ungroup()

  # Select top attributes
  top_attrs <- plot_data %>%
    group_by(attr_label, category) %>%
    summarize(max_dev = max(abs(index - 100)),
              winner = cluster_label[which.max(abs(index - 100))],
              .groups = "drop") %>%
    arrange(desc(max_dev)) %>%
    utils::head(max_attrs)

  # Order by category first, then by max deviation within category
  # This groups Income values together, Age together, etc.
  top_attrs <- top_attrs %>%
    arrange(category, desc(max_dev))

  plot_data <- plot_data %>%
    filter(attr_label %in% top_attrs$attr_label) %>%
    mutate(attr_label = factor(attr_label, levels = top_attrs$attr_label))

  list(data = plot_data, top_attrs = top_attrs)
}

#' Segment radar chart (overlaid)
#'
#' All segments on one radar chart using index values (100 = population average).
#' Spikes show where a segment is over-represented; dips show under-representation.
#'
#' @param profile_result Result from [jtbd_profile_segments()]
#' @param max_attrs Maximum number of attributes on the radar (default: 8)
#' @param title Plot title
#' @param n,study Sample size and study label for the plot footer (see [jtbd_footer()]).
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
#' plot_segment_radar(prof)
plot_segment_radar <- function(profile_result, max_attrs = 8,
                                title = "Segment Radar: Who Are They?",
                                n = NULL, study = NULL) {
  pd <- .get_profile_plot_data(profile_result, max_attrs)
  radar_data <- pd$data
  attr_levels <- levels(radar_data$attr_label)
  n_attrs <- length(attr_levels)

  radar_data$x_num <- as.numeric(radar_data$attr_label)

  # Close polygons properly: duplicate first row with x wrapped to n+1
  closed <- radar_data %>%
    group_by(cluster_label) %>%
    arrange(x_num) %>%
    bind_rows(slice_head(., n = 1) %>% mutate(x_num = x_num + n_attrs)) %>%
    arrange(x_num) %>%
    ungroup()

  # Smart labels: only label the most extreme segment per spoke
  label_data <- radar_data %>%
    group_by(attr_label) %>%
    filter(abs(index - 100) == max(abs(index - 100))) %>%
    slice_head(n = 1) %>%
    ungroup()

  ggplot(radar_data, aes(x = x_num, y = index, group = cluster_label, color = cluster_label)) +
    geom_hline(yintercept = 100, linetype = "dashed", color = "#BDC3C7", linewidth = 0.4) +
    geom_polygon(data = closed, aes(fill = cluster_label), alpha = 0.1, linewidth = 0) +
    geom_path(data = closed, linewidth = 1.2) +
    geom_point(size = 3) +
    geom_text(data = label_data, aes(label = index),
              size = 3, nudge_y = 16, fontface = "bold", show.legend = FALSE) +
    coord_polar() +
    scale_x_continuous(breaks = seq_len(n_attrs), labels = attr_levels,
                       limits = c(0.5, n_attrs + 1)) +
    scale_color_manual(values = c("#E74C3C", "#3498DB", "#2ECC71", "#F39C12", "#9B59B6")) +
    scale_fill_manual(values = c("#E74C3C", "#3498DB", "#2ECC71", "#F39C12", "#9B59B6")) +
    scale_y_continuous(limits = c(0, 250), breaks = c(50, 100, 150, 200)) +
    labs(title = title,
         subtitle = "Index vs population (100 = average). Further from center = more over-represented.",
         x = NULL, y = NULL, color = "", fill = "",
         caption = jtbd_footer(n = n, study = study)) +
    theme_jtbd() +
    theme(axis.text.y = element_blank(), axis.title = element_blank(),
          axis.line = element_blank(),
          panel.grid.major = element_line(color = "#E8E8E8", linewidth = 0.3),
          legend.position = "top",
          legend.text = element_text(size = 11, face = "bold"),
          axis.text.x = element_text(size = 8))
}

#' Segment radar chart (faceted)
#'
#' One radar panel per segment, making each segment's "personality shape" easy
#' to read without overlap.
#'
#' @param profile_result Result from [jtbd_profile_segments()]
#' @param max_attrs Maximum number of attributes on each radar (default: 8)
#' @param title Plot title
#' @param n,study Sample size and study label for the plot footer (see [jtbd_footer()]).
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
#' plot_segment_radar_facet(prof)
plot_segment_radar_facet <- function(profile_result, max_attrs = 8,
                                      title = "Segment Profiles: Individual Radar Views",
                                      n = NULL, study = NULL) {
  pd <- .get_profile_plot_data(profile_result, max_attrs)
  radar_data <- pd$data
  attr_levels <- levels(radar_data$attr_label)
  n_attrs <- length(attr_levels)

  radar_data$x_num <- as.numeric(radar_data$attr_label)

  closed <- radar_data %>%
    group_by(cluster_label) %>%
    arrange(x_num) %>%
    bind_rows(slice_head(., n = 1) %>% mutate(x_num = x_num + n_attrs)) %>%
    arrange(x_num) %>%
    ungroup()

  ggplot(radar_data, aes(x = x_num, y = index, group = 1)) +
    geom_hline(yintercept = 100, linetype = "dashed", color = "#E74C3C", linewidth = 0.5) +
    geom_polygon(data = closed, fill = "#3498DB", alpha = 0.15) +
    geom_path(data = closed, color = "#2C3E50", linewidth = 1) +
    geom_point(color = "#2C3E50", size = 2.5) +
    geom_text(aes(label = index), size = 2.8, nudge_y = 18, fontface = "bold", color = "#2C3E50") +
    coord_polar() +
    scale_x_continuous(breaks = seq_len(n_attrs), labels = attr_levels,
                       limits = c(0.5, n_attrs + 1)) +
    scale_y_continuous(limits = c(0, 260)) +
    facet_wrap(~cluster_label) +
    labs(title = title, x = NULL, y = NULL,
         subtitle = "Red dashed = population average (100). Shape reveals each segment's personality.",
         caption = jtbd_footer(n = n, study = study)) +
    theme_jtbd() +
    theme(axis.text.y = element_blank(), axis.title = element_blank(),
          axis.line = element_blank(),
          panel.grid.major = element_line(color = "#E8E8E8", linewidth = 0.3),
          strip.text = element_text(face = "bold", size = 13),
          axis.text.x = element_text(size = 7))
}

#' Segment fingerprints (parallel coordinates)
#'
#' Lines connecting each segment's index values across attributes. Where lines
#' diverge = where segments differ most. Where they cross = where rankings flip.
#'
#' @param profile_result Result from [jtbd_profile_segments()]
#' @param max_attrs Maximum number of attributes (default: 8)
#' @param title Plot title
#' @param n,study Sample size and study label for the plot footer (see [jtbd_footer()]).
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
#' plot_segment_fingerprint(prof)
plot_segment_fingerprint <- function(profile_result, max_attrs = 8,
                                      title = "Segment Fingerprints",
                                      n = NULL, study = NULL) {
  pd <- .get_profile_plot_data(profile_result, max_attrs)
  par_data <- pd$data

  ggplot(par_data, aes(x = attr_label, y = index, group = cluster_label, color = cluster_label)) +
    geom_hline(yintercept = 100, linetype = "dashed", color = "#7F8C8D", linewidth = 0.5) +
    geom_line(linewidth = 1.3, alpha = 0.8) +
    geom_point(size = 3.5) +
    geom_text(aes(label = index), nudge_y = 10, size = 3, fontface = "bold", show.legend = FALSE) +
    scale_color_manual(values = c("#E74C3C", "#3498DB", "#2ECC71", "#F39C12", "#9B59B6")) +
    labs(title = title,
         subtitle = "How each segment deviates from the population average (dashed = 100)",
         x = "", y = "Index (100 = average)", color = "",
         caption = jtbd_footer(n = n, study = study)) +
    theme_jtbd() +
    theme(axis.text.x = element_text(angle = 35, hjust = 1, size = 9),
          legend.position = "top",
          legend.text = element_text(size = 11, face = "bold"))
}

#' Segment DNA lollipop chart (faceted)
#'
#' Each segment gets its own panel showing over-indexed (red) and under-indexed
#' (blue) attributes as lollipops extending from the zero line.
#'
#' @param profile_result Result from [jtbd_profile_segments()]
#' @param max_attrs Maximum number of attributes (default: 8)
#' @param title Plot title
#' @param n,study Sample size and study label for the plot footer (see [jtbd_footer()]).
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
#' plot_segment_dna(prof)
plot_segment_dna <- function(profile_result, max_attrs = 8,
                              title = "Segment DNA: What Makes Each Group Unique",
                              n = NULL, study = NULL) {
  pd <- .get_profile_plot_data(profile_result, max_attrs)
  lol_data <- pd$data %>%
    mutate(
      deviation = index - 100,
      direction = ifelse(deviation >= 0, "Over", "Under"),
      attr_label = factor(attr_label, levels = rev(levels(attr_label)))
    )

  ggplot(lol_data, aes(x = attr_label, y = deviation, color = direction)) +
    geom_hline(yintercept = 0, linewidth = 0.5, color = "#2C3E50") +
    geom_segment(aes(xend = attr_label, yend = 0), linewidth = 1.2) +
    geom_point(size = 4) +
    coord_flip() +
    facet_wrap(~cluster_label) +
    scale_color_manual(values = c("Over" = "#C0392B", "Under" = "#2980B9"), guide = "none") +
    labs(title = title,
         subtitle = "Red = over-indexed vs population. Blue = under-indexed.",
         x = "", y = "Index Deviation from Average",
         caption = jtbd_footer(n = n, study = study)) +
    theme_jtbd() +
    theme(panel.grid.major.y = element_blank(),
          axis.line.y = element_blank(),
          strip.text = element_text(face = "bold", size = 13))
}
