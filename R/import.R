#' Read a Qualtrics CSV export
#'
#' Reads a Qualtrics CSV file, automatically detecting and removing the
#' 2 metadata header rows that Qualtrics adds below the column names.
#' Also strips auto-generated Qualtrics columns (ResponseId, dates, status, etc.).
#'
#' @param file Path to the Qualtrics CSV file
#' @param keep_metadata_cols If TRUE, keep Qualtrics metadata columns like
#'   ResponseId and dates (default: FALSE)
#'
#' @return A data frame with question response columns
#' @export
#'
#' @family import
#'
#' @examples
#' sample_file <- system.file("extdata", "sample_qualtrics.csv", package = "jtbdtools")
#' df <- read_qualtrics(sample_file)
#' head(df)
read_qualtrics <- function(file, keep_metadata_cols = FALSE) {
  # Read all rows including metadata
  raw <- utils::read.csv(file, stringsAsFactors = FALSE, check.names = FALSE)

  # Detect Qualtrics metadata rows

  # Qualtrics CSV has 3 header rows total:
  # Row 0 (header): Column IDs (Q1_1, Q2_1, etc.)
  # Row 1 (data row 1): Full question text
  # Row 2 (data row 2): Import IDs like {"ImportId":"QID1_1"}
  # Then actual responses start at row 3

  first_row <- as.character(raw[1, ])
  second_row <- if (nrow(raw) > 1) as.character(raw[2, ]) else character(0)

  has_import_row1 <- any(grepl("ImportId", first_row, fixed = TRUE))
  has_import_row2 <- any(grepl("ImportId", second_row, fixed = TRUE))

  if (has_import_row2) {
    # Standard Qualtrics: row 1 = question text, row 2 = import IDs
    question_labels <- first_row
    names(question_labels) <- names(raw)
    df <- raw[-(1:2), ]
    rownames(df) <- NULL
    attr(df, "question_labels") <- question_labels
    cli::cli_inform("Detected Qualtrics format: removed 2 metadata rows ({nrow(df)} responses).")
  } else if (has_import_row1) {
    # Only import IDs row, no question text row
    df <- raw[-1, ]
    rownames(df) <- NULL
    cli::cli_inform("Detected Qualtrics format: removed 1 metadata row ({nrow(df)} responses).")
  } else {
    df <- raw
    cli::cli_inform("No Qualtrics metadata rows detected. Using data as-is ({nrow(df)} rows).")
  }

  # Strip Qualtrics auto-generated columns
  if (!keep_metadata_cols) {
    meta_patterns <- c("^StartDate$", "^EndDate$", "^Status$", "^ResponseId$",
                       "^RecipientLastName$", "^RecipientFirstName$", "^RecipientEmail$",
                       "^ExternalReference$", "^LocationLatitude$", "^LocationLongitude$",
                       "^DistributionChannel$", "^UserLanguage$", "^IPAddress$",
                       "^Progress$", "^Duration.*seconds.*$", "^Finished$",
                       "^RecordedDate$", "^ResponseID$")
    meta_regex <- paste(meta_patterns, collapse = "|")
    meta_cols <- grep(meta_regex, names(df), ignore.case = TRUE)
    if (length(meta_cols) > 0) {
      cli::cli_inform("Removed {length(meta_cols)} Qualtrics metadata column{?s}.")
      saved_labels <- attr(df, "question_labels")
      df <- df[, -meta_cols, drop = FALSE]
      # Preserve labels for remaining columns
      if (!is.null(saved_labels)) {
        attr(df, "question_labels") <- saved_labels[names(df)]
      }
    }
  }

  return(df)
}

#' Detect importance and satisfaction columns
#'
#' Scans column names or question labels for text patterns indicating
#' importance or satisfaction questions. Returns the detected groupings.
#'
#' @param df A data frame (optionally with a `question_labels` attribute from [read_qualtrics()])
#' @param imp_pattern Regex pattern to identify importance columns (default: "important|importance")
#' @param sat_pattern Regex pattern to identify satisfaction columns (default: "satisf")
#'
#' @return A list with `$imp` and `$sat` (character vectors of column names)
#' @export
#'
#' @family import
#'
#' @examples
#' sample_file <- system.file("extdata", "sample_qualtrics.csv", package = "jtbdtools")
#' df <- read_qualtrics(sample_file)
#' detected <- detect_imp_sat(df)
#' detected$imp
#' detected$sat
detect_imp_sat <- function(df, imp_pattern = "important|importance", sat_pattern = "satisf") {
  # Use question labels if available (from Qualtrics), otherwise use column names
  labels <- attr(df, "question_labels")
  if (is.null(labels)) {
    search_text <- names(df)
    names(search_text) <- names(df)
  } else {
    search_text <- labels[names(df)]
  }

  imp_mask <- grepl(imp_pattern, search_text, ignore.case = TRUE)
  sat_mask <- grepl(sat_pattern, search_text, ignore.case = TRUE)

  imp_cols <- names(df)[imp_mask]
  sat_cols <- names(df)[sat_mask]

  # Report what was found
  n_imp <- length(imp_cols)
  n_sat <- length(sat_cols)
  n_other <- ncol(df) - n_imp - n_sat

  cli::cli_inform(c(
    "v" = "Detected {n_imp} importance column{?s}",
    "v" = "Detected {n_sat} satisfaction column{?s}",
    "i" = "{n_other} other column{?s} (segmentation, demographics, etc.)"
  ))

  if (n_imp == 0) cli::cli_warn("No importance columns detected. Try adjusting {.arg imp_pattern}.")
  if (n_sat == 0) cli::cli_warn("No satisfaction columns detected. Try adjusting {.arg sat_pattern}.")
  if (n_imp != n_sat) cli::cli_warn("Unequal imp ({n_imp}) and sat ({n_sat}) columns. Check your data.")

  list(imp = imp_cols, sat = sat_cols)
}

#' Prepare survey data for JTBD analysis
#'
#' The universal data prep function. Takes any data frame and renames columns
#' to the required `imp__job_step.objective` / `sat__job_step.objective` format.
#' Works with data from any source (Qualtrics, SurveyMonkey, Google Forms, CSV, etc.).
#'
#' @param df A data frame containing survey responses
#' @param imp_cols Character vector of importance column names
#' @param sat_cols Character vector of satisfaction column names (must be same length and order as `imp_cols`)
#' @param job_steps A named list mapping job step names to column indices or names.
#'   Example: `list(researching = 1:4, purchasing = 5:8)` means the first 4 imp/sat
#'   pairs belong to "researching" and the next 4 to "purchasing".
#'   If NULL, all objectives are assigned to a single job step called "all".
#' @param objective_names Optional character vector of clean objective names. If NULL,
#'   names are auto-generated from column names by cleaning punctuation and spaces.
#' @param segment_col Optional name of a column to keep as segmentation variable.
#'   Can also be a character vector of column names to keep.
#'
#' @return A data frame ready for [get_jtbd_scores()], with properly named
#'   `imp__`/`sat__` columns and factor values (1-5)
#' @export
#'
#' @family import
#'
#' @examples
#' sample_file <- system.file("extdata", "sample_qualtrics.csv", package = "jtbdtools")
#' df <- read_qualtrics(sample_file)
#' detected <- detect_imp_sat(df)
#'
#' ready <- prep_survey(df,
#'   imp_cols = detected$imp,
#'   sat_cols = detected$sat,
#'   job_steps = list(researching = 1:4),
#'   segment_col = "Q3"
#' )
#' head(ready)
prep_survey <- function(df, imp_cols, sat_cols, job_steps = NULL,
                        objective_names = NULL, segment_col = NULL) {
  # Validate inputs

  if (length(imp_cols) != length(sat_cols)) {
    cli::cli_abort(c(
      "Importance and satisfaction column counts must match.",
      "i" = "Got {length(imp_cols)} importance and {length(sat_cols)} satisfaction columns."
    ))
  }

  # Resolve column names if indices were provided
  if (is.numeric(imp_cols)) imp_cols <- names(df)[imp_cols]
  if (is.numeric(sat_cols)) sat_cols <- names(df)[sat_cols]

  # Check columns exist
  missing_imp <- setdiff(imp_cols, names(df))
  missing_sat <- setdiff(sat_cols, names(df))
  if (length(missing_imp) > 0) cli::cli_abort("Importance columns not found: {.val {missing_imp}}")
  if (length(missing_sat) > 0) cli::cli_abort("Satisfaction columns not found: {.val {missing_sat}}")

  n_objectives <- length(imp_cols)

  # Default job_steps: everything in one step called "all"
  if (is.null(job_steps)) {
    job_steps <- list(all = seq_len(n_objectives))
  }

  # Generate clean objective names if not provided
  if (is.null(objective_names)) {
    # Use question labels if available, else column names
    labels <- attr(df, "question_labels")
    if (!is.null(labels)) {
      raw_names <- labels[imp_cols]
    } else {
      raw_names <- imp_cols
    }
    objective_names <- .clean_objective_names(raw_names)
  }

  if (length(objective_names) != n_objectives) {
    cli::cli_abort("objective_names length ({length(objective_names)}) must match column count ({n_objectives}).")
  }

  # Build the new column names
  new_imp_names <- character(n_objectives)
  new_sat_names <- character(n_objectives)

  for (step_name in names(job_steps)) {
    indices <- job_steps[[step_name]]
    # Support both numeric indices and column name references
    if (is.character(indices)) {
      indices <- match(indices, imp_cols)
    }
    for (idx in indices) {
      new_imp_names[idx] <- paste0("imp__", step_name, ".", objective_names[idx])
      new_sat_names[idx] <- paste0("sat__", step_name, ".", objective_names[idx])
    }
  }

  # Build the output data frame
  result <- data.frame(caseid = seq_len(nrow(df)))

  # Add segment columns if specified
  if (!is.null(segment_col)) {
    for (sc in segment_col) {
      if (sc %in% names(df)) {
        result[[sc]] <- factor(df[[sc]])
      } else {
        cli::cli_warn("Segment column {.val {sc}} not found in data.")
      }
    }
  }

  # Add imp/sat columns as factors
  for (i in seq_len(n_objectives)) {
    imp_vals <- as.numeric(df[[imp_cols[i]]])
    sat_vals <- as.numeric(df[[sat_cols[i]]])
    result[[new_imp_names[i]]] <- factor(imp_vals, levels = 1:5)
    result[[new_sat_names[i]]] <- factor(sat_vals, levels = 1:5)
  }

  cli::cli_inform(c(
    "v" = "Prepared {n_objectives} objective{?s} across {length(job_steps)} job step{?s}.",
    "i" = "Data is ready for {.fn get_jtbd_scores}."
  ))

  return(result)
}

#' Prepare Qualtrics survey data for JTBD analysis
#'
#' Convenience wrapper that combines [read_qualtrics()], [detect_imp_sat()],
#' and [prep_survey()] into a single call. Goes from Qualtrics CSV to
#' analysis-ready data frame in one step.
#'
#' @param file Path to the Qualtrics CSV file
#' @param job_steps A named list mapping job step names to objective indices.
#'   Example: `list(researching = 1:4, purchasing = 5:8)`
#' @param imp_pattern Regex pattern to identify importance columns
#' @param sat_pattern Regex pattern to identify satisfaction columns
#' @param imp_cols Explicit importance column names (overrides auto-detection)
#' @param sat_cols Explicit satisfaction column names (overrides auto-detection)
#' @param segment_col Column name(s) to keep as segmentation variable(s)
#'
#' @return A data frame ready for [get_jtbd_scores()]
#' @export
#'
#' @family import
#'
#' @examples
#' sample_file <- system.file("extdata", "sample_qualtrics.csv", package = "jtbdtools")
#' ready <- prep_qualtrics(sample_file,
#'   job_steps = list(researching = 1:4),
#'   segment_col = "Q3"
#' )
#' get_jtbd_scores(ready)
prep_qualtrics <- function(file, job_steps = NULL,
                           imp_pattern = "important|importance",
                           sat_pattern = "satisf",
                           imp_cols = NULL, sat_cols = NULL,
                           segment_col = NULL) {
  df <- read_qualtrics(file)

  if (is.null(imp_cols) || is.null(sat_cols)) {
    detected <- detect_imp_sat(df, imp_pattern = imp_pattern, sat_pattern = sat_pattern)
    if (is.null(imp_cols)) imp_cols <- detected$imp
    if (is.null(sat_cols)) sat_cols <- detected$sat
  }

  prep_survey(df, imp_cols = imp_cols, sat_cols = sat_cols,
              job_steps = job_steps, segment_col = segment_col)
}

#' Validate JTBD data format
#'
#' Checks that a data frame meets the requirements for JTBD scoring:
#' matching imp/sat column pairs, factor format, valid levels.
#' Prints a diagnostic report.
#'
#' @param df A data frame to validate
#'
#' @return Invisible TRUE if valid, FALSE with warnings if issues found
#' @export
#'
#' @family import
#'
#' @examples
#' data(jtbd_sample)
#' validate_jtbd_data(jtbd_sample)
validate_jtbd_data <- function(df) {
  issues <- character(0)
  imp_cols <- grep("^imp__", names(df), value = TRUE)
  sat_cols <- grep("^sat__", names(df), value = TRUE)

  # Check for imp/sat columns

  if (length(imp_cols) == 0) issues <- c(issues, "No columns starting with 'imp__' found.")
  if (length(sat_cols) == 0) issues <- c(issues, "No columns starting with 'sat__' found.")

  # Check matching pairs
  imp_objectives <- sub("^imp__", "", imp_cols)
  sat_objectives <- sub("^sat__", "", sat_cols)
  missing_sat <- setdiff(imp_objectives, sat_objectives)
  missing_imp <- setdiff(sat_objectives, imp_objectives)
  if (length(missing_sat) > 0) {
    issues <- c(issues, paste0("Importance columns without matching satisfaction: ", paste(missing_sat, collapse = ", ")))
  }
  if (length(missing_imp) > 0) {
    issues <- c(issues, paste0("Satisfaction columns without matching importance: ", paste(missing_imp, collapse = ", ")))
  }

  # Check factor format and levels
  for (col in c(imp_cols, sat_cols)) {
    if (!is.factor(df[[col]])) {
      issues <- c(issues, paste0("Column '", col, "' is not a factor (is ", class(df[[col]])[1], ")."))
      break  # Don't spam for every column
    }
  }

  # Check for NAs
  all_cols <- c(imp_cols, sat_cols)
  total_na <- 0
  if (length(all_cols) > 0) {
    na_counts <- sapply(df[, all_cols, drop = FALSE], function(x) sum(is.na(x)))
    total_na <- sum(na_counts)
  }

  # Report
  n_pairs <- length(intersect(imp_objectives, sat_objectives))
  if (length(issues) == 0) {
    cli::cli_inform(c(
      "v" = "Data is valid for JTBD analysis.",
      "i" = "{n_pairs} matched imp/sat objective pair{?s}",
      "i" = "{nrow(df)} respondent{?s}",
      "i" = "{total_na} missing value{?s} across all columns"
    ))
    return(invisible(TRUE))
  } else {
    issue_msgs <- paste0("x ", issues)
    cli::cli_warn(c(
      "!" = "Data has {length(issues)} issue{?s}:",
      issue_msgs
    ))
    return(invisible(FALSE))
  }
}

#' Clean objective names from raw question text (internal)
#' @noRd
.clean_objective_names <- function(raw_names) {
  cleaned <- raw_names
  # Remove common survey prefixes
  cleaned <- gsub("^How (important|satisfied).*?(to |with |is )(.+?)\\??$", "\\3", cleaned, ignore.case = TRUE)
  # If regex didn't match, use the raw name
  unchanged <- cleaned == raw_names
  if (any(unchanged)) {
    # Fallback: just clean up the column name
    cleaned[unchanged] <- gsub("[^a-zA-Z0-9]", "_", raw_names[unchanged])
    cleaned[unchanged] <- gsub("_+", "_", cleaned[unchanged])
    cleaned[unchanged] <- gsub("^_|_$", "", cleaned[unchanged])
  }
  # Final cleanup
  cleaned <- tolower(cleaned)
  cleaned <- gsub("[^a-z0-9]", "_", cleaned)
  cleaned <- gsub("_+", "_", cleaned)
  cleaned <- gsub("^_|_$", "", cleaned)
  # Make unique
  cleaned <- make.unique(cleaned, sep = "_")
  return(cleaned)
}
