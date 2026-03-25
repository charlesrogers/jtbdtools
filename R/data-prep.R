#' Prepare data for analysis
#'
#' Runs the full data preparation pipeline: converts SPSS labels to column names,
#' removes prefixes/suffixes, replaces spaces with underscores, and converts
#' labelled data to factors.
#'
#' @param df A data frame (typically imported from SPSS via [haven::read_sav()])
#'
#' @return A processed data frame ready for JTBD scoring
#' @export
#'
#' @family data-prep
#'
#' @examples
#' # Typically used with SPSS data:
#' # df <- haven::read_sav("survey.sav")
#' # df_clean <- prep_data(df)
prep_data <- function(df) {
  df <- convert_labels_to_row_names(df)
  df <- remove_data_prefix(df)
  df <- remove_data_suffix(df)
  df <- replace_spaces_with_underscores(df)
  df <- change_labeles_to_factors(df)
  return(df)
}

#' Build importance column names
#'
#' Renames columns to the required `imp__job_step.objective` format.
#' Runs [prep_data()] first, then prefixes each column with `imp__` and the job section.
#'
#' @param df A data frame to process
#' @param job_section The job section name (e.g., "researching")
#'
#' @return A data frame with columns renamed to `imp__job_section.original_name`
#' @export
#'
#' @family data-prep
build_imp_column_names <- function(df, job_section) {
  df <- prep_data(df)
  new_col_name <- paste0("imp__", job_section, ".", colnames(df))
  df <- setNames(df, new_col_name) %>%
    rename(caseid = 1)
  return(df)
}

#' Build satisfaction column names
#'
#' Renames columns to the required `sat__job_step.objective` format.
#' Runs [prep_data()] first, then prefixes each column with `sat__` and the job section.
#'
#' @param df A data frame to process
#' @param job_section The job section name (e.g., "researching")
#'
#' @return A data frame with columns renamed to `sat__job_section.original_name`
#' @export
#'
#' @family data-prep
build_sat_column_names <- function(df, job_section) {
  df <- prep_data(df)
  new_col_name <- paste0("sat__", job_section, ".", colnames(df))
  df <- setNames(df, new_col_name) %>%
    rename(caseid = 1)
  return(df)
}

#' Convert labels to column names
#'
#' Swaps SPSS variable labels into column names using [sjlabelled::label_to_colnames()].
#'
#' @param df A data frame with SPSS labels
#'
#' @return A data frame with labels as column names
#' @export
#'
#' @family data-prep
convert_labels_to_row_names <- function(df) {
  df <- df %>%
    sjlabelled::label_to_colnames()
}

#' Remove data prefix
#'
#' Removes everything before and including `--` from column names.
#'
#' @param df A data frame to process
#'
#' @return A data frame with prefixes removed from column names
#' @export
#'
#' @family data-prep
remove_data_prefix <- function(df) {
  df <- df %>% setNames(gsub(".*?--", "", names(.)))
}

#' Remove data suffix
#'
#' Removes everything after and including `-` from column names.
#'
#' @param df A data frame to process
#'
#' @return A data frame with suffixes removed from column names
#' @export
#'
#' @family data-prep
remove_data_suffix <- function(df) {
  df <- df %>% setNames(sub("-.*$", "", names(.)))
}

#' Replace spaces with underscores in column names
#'
#' Uses [janitor::clean_names()] to standardize column names.
#'
#' @param df A data frame to process
#'
#' @return A data frame with cleaned column names
#' @export
#'
#' @family data-prep
replace_spaces_with_underscores <- function(df) {
  df <- df %>%
    janitor::clean_names()
}

#' Change labelled data to factors
#'
#' Converts all [haven]-labelled columns to factors.
#'
#' @param df A data frame to process
#'
#' @return A data frame with labelled columns converted to factors
#' @export
#'
#' @family data-prep
change_labeles_to_factors <- function(df) {
  df <- df %>%
    mutate_if(haven::is.labelled, as_factor)
}
