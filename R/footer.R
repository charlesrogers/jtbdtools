#' Build a standardized JTBD footer caption
#'
#' Returns a one-line string suitable for use in `labs(caption = ...)` on
#' ggplot2 plots or `gt::tab_source_note()` on gt tables. Combines sample size,
#' study name, and any existing caption text into a single, consistently
#' formatted footer.
#'
#' @param n Sample size. Either a single integer or a named numeric vector
#'   (e.g. `c(all = 200, casual = 80, power = 120)`). When a vector is supplied,
#'   each element is rendered as `name=count`.
#' @param study Study label (e.g. "GradeOptimizer Q1 2026"). Optional.
#' @param extra Extra text to append after the n/study block (e.g. a methodology
#'   note). Optional.
#'
#' @return A single character string. Returns `NULL` if all inputs are NULL/empty.
#' @export
#'
#' @family visualization
#'
#' @examples
#' jtbd_footer(n = 250, study = "GradeOptimizer Q1 2026")
#' jtbd_footer(n = c(all = 250, casual = 80, power = 170))
#' jtbd_footer(n = 250, study = "Pilot", extra = "T2B scoring; 95% Wilson CI")
jtbd_footer <- function(n = NULL, study = NULL, extra = NULL) {
  parts <- character(0)

  if (!is.null(n)) {
    if (is.null(names(n)) || all(names(n) == "")) {
      parts <- c(parts, paste0("n = ", format(sum(n), big.mark = ",")))
    } else {
      labelled <- paste0(names(n), "=", format(n, big.mark = ",", trim = TRUE))
      total <- sum(n)
      parts <- c(parts,
                 paste0("n = ", format(total, big.mark = ","),
                        " (", paste(labelled, collapse = ", "), ")"))
    }
  }

  if (!is.null(study) && nzchar(study)) {
    parts <- c(parts, paste0("Study: ", study))
  }

  if (!is.null(extra) && nzchar(extra)) {
    parts <- c(parts, extra)
  }

  if (length(parts) == 0) return(NULL)
  paste(parts, collapse = "  \u00b7  ")
}

#' Append a JTBD footer to a ggplot caption
#'
#' Internal helper. Combines the plot's existing caption with a `jtbd_footer()`
#' string, joining them on a newline so both stay readable.
#'
#' @param existing The current caption (may be NULL or empty).
#' @param n,study,extra Forwarded to [jtbd_footer()].
#'
#' @return A character string (or NULL if everything is empty).
#' @keywords internal
.merge_caption <- function(existing, n = NULL, study = NULL, extra = NULL) {
  footer <- jtbd_footer(n = n, study = study, extra = extra)
  if (is.null(existing) || !nzchar(existing)) return(footer)
  if (is.null(footer)) return(existing)
  paste0(existing, "\n", footer)
}
