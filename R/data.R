#' Sample JTBD survey data
#'
#' A synthetic dataset containing 200 survey respondents who rated the importance
#' and satisfaction of 12 objectives across 3 job steps on a 1-5 Likert scale.
#' Includes a segmentation column with three user types that have different
#' response patterns.
#'
#' @format A data frame with 200 rows and 31 columns. Key columns include
#' `caseid` (respondent ID), `segment` (user type factor),
#' demographic profiling columns (`gender`, `age_group`, `income`,
#' `education`, `tenure`), and 24 paired importance/satisfaction columns
#' following the `imp__step.objective` / `sat__step.objective` naming convention.
#'
#' @examples
#' data(jtbd_sample)
#' head(jtbd_sample)
#'
#' # Calculate opportunity scores
#' scores <- get_jtbd_scores(jtbd_sample)
#'
#' # Compare segments
#' comparison <- get_jtbd_scores.comparison(jtbd_sample, "segment")
"jtbd_sample"
