#' jtbdtools gt table theme
#'
#' A branded gt theme for JTBD tables. Clean, left-aligned, with heat-mapped
#' numeric columns and professional typography. Inspired by the quantjtbd
#' `gt_theme.yellow` but using the jtbdtools color palette.
#'
#' @param gt_object A gt table object
#' @param n,study Sample size and study label rendered as a `tab_source_note()`
#'   footer (see [jtbd_footer()]).
#' @param ... Additional arguments (unused)
#'
#' @return A styled gt table object
#' @export
#'
#' @family tables
#'
#' @examples
#' library(gt)
#' data(jtbd_sample)
#' scores <- get_jtbd_scores(jtbd_sample)
#' scores[1:5, c("job_step", "objective", "imp.all", "sat.all", "opp.all")] |>
#'   gt() |>
#'   gt_theme_jtbd(n = 250, study = "Pilot")
gt_theme_jtbd <- function(gt_object, n = NULL, study = NULL, ...) {
  styled <- gt_object |>
    data_color(
      columns = where(is.numeric),
      fn = scales::col_numeric(
        palette = c("#F7F7F7", "#FDEBD0", "#E74C3C"),
        domain = NULL
      )
    ) |>
    tab_style(
      locations = cells_column_labels(columns = everything()),
      style = cell_text(
        weight = 650,
        size = px(13),
        transform = "uppercase",
        align = "left"
      )
    ) |>
    tab_style(
      locations = cells_title("title"),
      style = cell_text(weight = 650, size = px(18))
    ) |>
    tab_style(
      locations = cells_title("subtitle"),
      style = cell_text(weight = 400, size = px(13))
    ) |>
    tab_style(
      style = cell_borders(sides = "top", color = "#E0E0E0", weight = px(1), style = "solid"),
      locations = cells_body(rows = everything())
    ) |>
    cols_align(align = "left", columns = everything()) |>
    tab_options(
      table.font.size = 13,
      column_labels.border.bottom.width = 2,
      column_labels.border.bottom.color = "#2C3E50",
      column_labels.border.top.color = "white",
      row_group.border.bottom.color = "#E0E0E0",
      table.border.top.style = "none",
      table.border.bottom.style = "none",
      heading.border.bottom.style = "none",
      heading.align = "left",
      data_row.padding = px(4),
      source_notes.border.lr.style = "none",
      source_notes.font.size = 10
    )

  footer <- jtbd_footer(n = n, study = study)
  if (!is.null(footer)) {
    styled <- styled |> gt::tab_source_note(source_note = footer)
  }
  styled
}

#' Format job step as gt table
#'
#' Creates a publication-ready gt table for a job step's opportunity scores,
#' with color-coded index values and highlighted median row. When `n` is
#' supplied (or `imp_se.all` / `sat_se.all` columns are already present), the
#' Importance and Satisfaction values are rendered as `score ± SE` to make
#' the precision of each estimate visible.
#'
#' @param your.df A data frame from [get_jtbd_scores()] containing scores for one job step
#' @param step_title Title for the table header
#' @param n Sample size for the segment (used to compute Wilson SEs if not present).
#' @param study Study label rendered in the footer.
#'
#' @return A gt table object
#' @export
#'
#' @family tables
theme.job_step <- function(your.df, step_title, n = NULL, study = NULL) {
  has_se <- all(c("imp_se.all", "sat_se.all") %in% names(your.df))
  if (!has_se && !is.null(n)) {
    your.df <- add_score_cis(your.df, n = n)
    has_se <- TRUE
  }

  prepped <- your.df %>%
    arrange(-opp.all) %>%
    relocate(c(opp.all, opp_index.all, imp.all, sat.all), .after = c("objective")) %>%
    select(-job_step) %>%
    remove_weird_text_formatting.jtbd()

  if (has_se) {
    fmt_pm <- function(x, se) ifelse(is.na(x), "", sprintf("%.1f ± %.1f", x, se))
    prepped <- prepped %>%
      mutate(
        Importance = fmt_pm(imp.all, .data[["imp_se.all"]]),
        Satisfaction = fmt_pm(sat.all, .data[["sat_se.all"]])
      )
  } else {
    prepped <- prepped %>%
      rename(Importance = imp.all, Satisfaction = sat.all)
  }

  prepped <- prepped %>%
    select(-c(opp.all, opp_index.all)) %>%
    select(-dplyr::any_of(c(
      "imp.all", "sat.all",
      "imp_lo.all", "imp_hi.all", "imp_se.all",
      "sat_lo.all", "sat_hi.all", "sat_se.all",
      "opp_lo.all", "opp_hi.all", "opp_se.all"
    )))

  output <- prepped %>%
    gt() %>%
    tab_header(title = step_title) %>%
    fmt_percent(columns = "Index", decimals = 0) |>
    gtExtras::gt_theme_nytimes() %>%
    tab_options(data_row.padding = px(1)) %>%
    tab_footnote(
      locations = cells_column_labels(columns = "Index"),
      footnote = "Index is the relative rank of an objective's opportunity score; It's calculated by taking the percentage of the median opportunity"
    ) %>%
    gt_highlight_rows(
      rows = objective == "Median objective",
      fill = "lightgrey",
      bold_target_only = FALSE,
      font_weight = "normal"
    ) %>%
    data_color(columns = c("Index"),
               colors = scales::col_numeric(
                 palette = c("#990099FF", "white", "#066230"),
                 domain = c(.76, 1.26))
    ) %>%
    cols_width(
      objective ~ px(250),
      "Index" ~ px(50),
      "Importance" ~ px(95),
      "Satisfaction" ~ px(95),
    )

  if (has_se) {
    output <- output %>%
      tab_footnote(
        locations = cells_column_labels(columns = c("Importance", "Satisfaction")),
        footnote = "Score ± standard error (Wilson, top-2-box)."
      )
  }

  footer <- jtbd_footer(n = n, study = study)
  if (!is.null(footer)) {
    output <- output %>% gt::tab_source_note(source_note = footer)
  }

  return(output)
}

#' Create and save job step table
#'
#' Filters a scores data frame to a specific job step, formats it with
#' [theme.job_step()], and saves as PNG.
#'
#' @param df A data frame from [get_jtbd_scores()]
#' @param job.string Job name for the filename
#' @param job_step.string Job step to filter to
#' @param path.table Directory to save the PNG (default: [tempdir()])
#' @param n,study Forwarded to [theme.job_step()] — used for `± SE` columns and footer.
#'
#' @return The gt table object (also saved as PNG)
#' @export
#'
#' @family tables
create.job_step.table <- function(df, job.string, job_step.string, path.table = tempdir(),
                                  n = NULL, study = NULL) {
  table_title <- paste0(job.string, " - ", job_step.string)
  table.png <- df %>%
    filter(job_step == job_step.string) %>%
    theme.job_step(., table_title, n = n, study = study)

  file_name.table <- paste("table-opportunities-", job.string, "-", job_step.string, ".png")

  gtsave(table.png, filename = file_name.table, path = path.table, vwidth = 600, vheight = 2500)
  return(table.png)
}

#' Create percentage table
#'
#' Creates a gt table showing frequency percentages with bar charts for
#' categorical data (e.g., tool usage, channel preference).
#'
#' @param df Data frame containing the data
#' @param job.string Job name for the filename
#' @param var_name.string Column name(s) to analyze
#' @param job_step.string Job step label for the header
#' @param path.table Directory to save the PNG (default: [tempdir()])
#' @param n,study Sample size and study label rendered as a footer
#'   (see [jtbd_footer()]).
#'
#' @return The gt table object (also saved as PNG)
#' @export
#'
#' @family tables
create.pct.table <- function(df, job.string, var_name.string, job_step.string, path.table = tempdir(),
                             n = NULL, study = NULL) {
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

  footer <- jtbd_footer(n = n, study = study)
  if (!is.null(footer)) {
    table.png <- table.png %>% gt::tab_source_note(source_note = footer)
  }

  file_name.table <- paste("table-tool_usage-", job.string, "-", job_step.string, ".png")
  gtsave(table.png, filename = file_name.table, path = path.table, vwidth = 1500, vheight = 500)
  return(table.png)
}
