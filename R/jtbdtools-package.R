#' @keywords internal
"_PACKAGE"

#' @import ggplot2
#' @importFrom dplyr mutate group_by count select filter rename %>%
#'   arrange desc left_join bind_rows summarize pull ungroup last_col
#'   starts_with everything relocate across if_else case_when n
#'   mutate_if distinct summarise
#' @importFrom forcats fct_count fct_inorder fct_lump_prop fct_reorder as_factor
#' @importFrom tibble deframe
#' @importFrom tidyr pivot_wider pivot_longer separate separate_wider_delim
#' @importFrom stats median sd setNames
#' @importFrom utils head tail
#' @importFrom sjlabelled label_to_colnames
#' @importFrom janitor clean_names
#' @importFrom haven is.labelled
#' @importFrom stringr str_replace_all str_wrap str_split_fixed str_to_sentence
#' @importFrom scales percent col_numeric
#' @importFrom gt gt tab_header fmt_percent tab_options tab_footnote cols_width
#'   data_color gtsave tab_style cell_text cell_borders cells_column_labels
#'   cells_body cells_row_groups cells_title cols_align px
#'   google_font default_fonts opt_table_font
#' @importFrom gtExtras gt_theme_nytimes gt_highlight_rows gt_plt_bar_pct
#' @importFrom ggrepel geom_text_repel
#' @importFrom labelled to_factor
#' @importFrom rlang .data !! !!! as_name enquo
#' @importFrom cli cli_abort cli_warn cli_inform
#' @importFrom purrr pluck
#' @importFrom tidyselect all_of
NULL
