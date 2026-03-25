#' Format job step as gt table
#'
#' Creates a publication-ready gt table for a job step's opportunity scores,
#' with color-coded index values and highlighted median row.
#'
#' @param your.df A data frame from [get_jtbd_scores()] containing scores for one job step
#' @param step_title Title for the table header
#'
#' @return A gt table object
#' @export
#'
#' @family tables
theme.job_step <- function(your.df, step_title) {
  output <- your.df %>%
    arrange(-opp.all) %>%
    relocate(c(opp.all, opp_index.all, imp.all, sat.all), .after = c("objective")) %>%
    select(-job_step) %>%
    remove_weird_text_formatting.jtbd() %>%
    rename(Importance = imp.all,
           Satisfaction = sat.all,
           Opportunity = opp.all) %>%
    select(-c(Opportunity, opp_index.all)) %>%
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
    ) %>%
    gt_highlight_rows(
      rows = objective == "Median objective",
      fill = "lightgrey",
      bold_target_only = FALSE,
      font_weight = "normal"
    ) %>%
    data_color(columns = c(Index),
               colors = scales::col_numeric(
                 palette = c("#990099FF", "white", "#066230"),
                 domain = c(.76, 1.26))
    ) %>%
    cols_width(
      objective ~ px(250),
      Index ~ px(50),
      Importance ~ px(75),
      Satisfaction ~ px(75),
    )

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
#'
#' @return The gt table object (also saved as PNG)
#' @export
#'
#' @family tables
create.job_step.table <- function(df, job.string, job_step.string, path.table = tempdir()) {
  table_title <- paste0(job.string, " - ", job_step.string)
  table.png <- df %>%
    filter(job_step == job_step.string) %>%
    theme.job_step(., table_title)

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
#'
#' @return The gt table object (also saved as PNG)
#' @export
#'
#' @family tables
create.pct.table <- function(df, job.string, var_name.string, job_step.string, path.table = tempdir()) {
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
  return(table.png)
}
