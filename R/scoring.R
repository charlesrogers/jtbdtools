#' Calculate JTBD opportunity scores
#'
#' The core scoring function. Takes a data frame with `imp__` and `sat__` columns
#' and calculates importance, satisfaction, and opportunity scores using the
#' Outcome-Driven Innovation formula.
#'
#' @param your_data_frame A data frame with columns following the `imp__job_step.objective`
#'   and `sat__job_step.objective` naming convention. Values should be factors (1-5).
#' @param col_suffix Label for this segment (default: "all")
#'
#' @return A data frame with columns: job_step, objective, imp, sat, opp, rank, opp_index
#'   (suffixed by `col_suffix`)
#' @export
#'
#' @family scoring
#'
#' @examples
#' data(jtbd_sample)
#' scores <- get_jtbd_scores(jtbd_sample)
#' head(scores)
get_jtbd_scores <- function(your_data_frame, col_suffix = "all") {
  # Validate input
  imp_cols <- grep("^imp__", names(your_data_frame), value = TRUE)
  sat_cols <- grep("^sat__", names(your_data_frame), value = TRUE)

  if (length(imp_cols) == 0 || length(sat_cols) == 0) {
    cli::cli_abort(c(
      "No importance/satisfaction columns found.",
      "i" = "Column names must start with {.val imp__} and {.val sat__}.",
      "i" = "Example: {.val imp__researching.minimize_time_to_find_options}",
      "i" = "See {.code ?jtbd_sample} for a working example."
    ))
  }

  opportunity_columns_group_1 <- find_imp_sat_columns(your_data_frame)
  opportunity_score_group_1 <- calculate_pop_pct_score(opportunity_columns_group_1)
  opportunity_score_group_1 <- split_imp_sat_columns(opportunity_score_group_1)
  opportunity_score_group_1 <- calculate_opportunity_score(opportunity_score_group_1)
  opportunity_score_group_1 <- opportunity_score_group_1 %>%
    mutate(opp = if_else(imp < sat, imp, imp + imp - sat)) %>%
    arrange(desc(opp)) %>%
    mutate(segment_name = ifelse(col_suffix == "all", "all", col_suffix),
           rank = rank(desc(opp)))
  opportunity_score_group_1 <- opportunity_score_group_1 %>%
    mutate(opp_index = round(opp / median(opp), 2))

  importance_satisfaction_opportunity <- opportunity_score_group_1 %>%
    pivot_wider(id_cols = objective,
                names_from = c(segment_name),
                values_from = c(imp, sat, opp, rank, opp_index),
                names_sep = ".") %>%
    mutate(objective = as_factor(objective)) %>%
    separate(objective, sep = "([.])", into = c("job_step", "objective")) %>%
    mutate_if(is.numeric, round, 2) %>%
    mutate(objective = factor(objective, levels = objective))

  return(importance_satisfaction_opportunity)
}

#' Find importance and satisfaction columns
#'
#' Extracts all columns starting with `imp_` or `sat_` from a data frame.
#'
#' @param your_data_frame A data frame containing importance and satisfaction columns
#'
#' @return A data frame with only importance and satisfaction columns
#' @export
#'
#' @family scoring
find_imp_sat_columns <- function(your_data_frame) {
  imp_columns <- your_data_frame %>%
    select(starts_with("imp_"))
  sat_columns <- your_data_frame %>%
    select(starts_with("sat_"))
  data_frame_imp_sat <- cbind(imp_columns, sat_columns)
  return(data_frame_imp_sat)
}

#' Calculate population percentage score
#'
#' Converts raw Likert-scale survey responses (1-5) into a 0-10 score by
#' calculating the percentage of respondents who rated 4 or 5 (top-2 box).
#'
#' @param objectives A data frame containing factor columns to be scored
#'
#' @return A data frame with objective names and their calculated scores
#' @export
#'
#' @family scoring
calculate_pop_pct_score <- function(objectives) {
  all_data <- NULL
  for (objective in seq_along(objectives)) {
    namez <- names(objectives)[[objective]]

    objective_score <- fct_count(objectives[[objective]]) %>%
      mutate(objective_name = namez)

    objective_score_tibble <- objective_score %>%
      mutate(user_rating = f) %>%
      filter(user_rating %in% c(1, 2, 3, 4, 5)) %>%
      select(objective_name, user_rating, n)

    individual_data <- objective_score_tibble %>%
      summarize(objective_name = unique(objective_name),
                total_sum = sum(n),
                imp_sat_sum = sum(n[user_rating == 5 | user_rating == 4])) %>%
      mutate(imp_sat_score = ((imp_sat_sum / total_sum) * 10))
    all_data <- rbind(all_data, individual_data)
  }
  return(all_data)
}

#' Split importance and satisfaction columns
#'
#' Separates the `objective_name` column into `imp_sat` (imp/sat indicator) and
#' `objective` (the actual objective name) by splitting on `__`.
#'
#' @param data_frame_imp_sat A data frame with an `objective_name` column
#'
#' @return A data frame with `imp_sat` and `objective` columns
#' @export
#'
#' @family scoring
split_imp_sat_columns <- function(data_frame_imp_sat) {
  data_frame_imp_sat_split <- data_frame_imp_sat %>%
    separate(objective_name, "__", into = c("imp_sat", "objective"), remove = FALSE)
  return(data_frame_imp_sat_split)
}

#' Calculate opportunity score
#'
#' Applies the ODI opportunity formula: `opportunity = importance + max(0, importance - satisfaction)`.
#' When importance >= satisfaction, the gap amplifies opportunity.
#' When satisfaction > importance, opportunity equals importance (the floor).
#'
#' @param data_frame_split A data frame with `imp_sat`, `objective`, and `imp_sat_score` columns
#'
#' @return A data frame with `imp`, `sat`, and `opp` columns per objective
#' @export
#'
#' @family scoring
calculate_opportunity_score <- function(data_frame_split) {
  opportunity_scores <- data_frame_split %>%
    pivot_wider(id_cols = objective,
                names_from = c(imp_sat),
                values_from = imp_sat_score) %>%
    mutate(opp = if_else(imp < sat, imp, imp + imp - sat)) %>%
    arrange(desc(opp))

  return(opportunity_scores)
}

#' Add Wilson confidence intervals to JTBD scores
#'
#' Augments a scores data frame from [get_jtbd_scores()] or
#' [get_jtbd_scores.comparison()] with confidence-interval bounds for each
#' importance, satisfaction, and opportunity column. Because raw scores are
#' top-2-box proportions scaled to 0–10, the imp/sat bounds use the Wilson
#' interval for binomial proportions. Opportunity bounds propagate via the
#' delta method on `opp = imp + max(0, imp - sat)`.
#'
#' @param scores A scores data frame with `imp.<seg>`, `sat.<seg>`, `opp.<seg>` columns.
#' @param n Sample size. Either a single integer applied to every segment, or a
#'   named integer vector keyed by segment (e.g. `c(all = 200, casual = 80)`).
#' @param conf_level Confidence level (default 0.95).
#'
#' @return The input data frame with new `<imp|sat|opp>_lo.<seg>`,
#'   `<imp|sat|opp>_hi.<seg>`, and `<imp|sat|opp>_se.<seg>` columns.
#' @export
#'
#' @family scoring
#'
#' @examples
#' data(jtbd_sample)
#' scores <- get_jtbd_scores(jtbd_sample)
#' add_score_cis(scores, n = get_sample_size(jtbd_sample))
add_score_cis <- function(scores, n, conf_level = 0.95) {
  z <- stats::qnorm(1 - (1 - conf_level) / 2)

  imp_cols <- grep("^imp\\.", names(scores), value = TRUE)
  if (length(imp_cols) == 0) {
    cli::cli_abort("No {.val imp.*} columns found. Run {.fn get_jtbd_scores} first.")
  }

  segments <- sub("^imp\\.", "", imp_cols)

  resolve_n <- function(seg) {
    if (length(n) == 1 && (is.null(names(n)) || identical(names(n), ""))) {
      return(as.numeric(n))
    }
    if (seg %in% names(n)) return(as.numeric(n[[seg]]))
    if ("all" %in% names(n)) return(as.numeric(n[["all"]]))
    cli::cli_abort("No sample size for segment {.val {seg}}; pass {.code n = c({seg} = ...)} or a single integer.")
  }

  wilson_bounds <- function(p, n_seg) {
    # p is on a 0-10 scale (top-2-box proportion * 10)
    p01 <- pmin(pmax(p / 10, 0), 1)
    denom <- 1 + z^2 / n_seg
    centre <- (p01 + z^2 / (2 * n_seg)) / denom
    half <- (z * sqrt(p01 * (1 - p01) / n_seg + z^2 / (4 * n_seg^2))) / denom
    list(lo = pmax(0, (centre - half) * 10),
         hi = pmin(10, (centre + half) * 10),
         se = sqrt(p01 * (1 - p01) / n_seg) * 10)
  }

  for (seg in segments) {
    n_seg <- resolve_n(seg)
    imp <- scores[[paste0("imp.", seg)]]
    sat <- scores[[paste0("sat.", seg)]]
    opp_col <- paste0("opp.", seg)

    imp_ci <- wilson_bounds(imp, n_seg)
    sat_ci <- wilson_bounds(sat, n_seg)

    scores[[paste0("imp_lo.", seg)]] <- round(imp_ci$lo, 2)
    scores[[paste0("imp_hi.", seg)]] <- round(imp_ci$hi, 2)
    scores[[paste0("imp_se.", seg)]] <- round(imp_ci$se, 2)
    scores[[paste0("sat_lo.", seg)]] <- round(sat_ci$lo, 2)
    scores[[paste0("sat_hi.", seg)]] <- round(sat_ci$hi, 2)
    scores[[paste0("sat_se.", seg)]] <- round(sat_ci$se, 2)

    if (opp_col %in% names(scores)) {
      # Delta method on opp = imp + max(0, imp - sat).
      # When imp >= sat: opp = 2*imp - sat   -> Var(opp) ~ 4*Var(imp) + Var(sat)
      # When imp <  sat: opp = imp           -> Var(opp) ~ Var(imp)
      var_opp <- ifelse(imp >= sat,
                        4 * imp_ci$se^2 + sat_ci$se^2,
                        imp_ci$se^2)
      se_opp <- sqrt(var_opp)
      opp <- scores[[opp_col]]
      scores[[paste0("opp_lo.", seg)]] <- round(pmax(0, opp - z * se_opp), 2)
      scores[[paste0("opp_hi.", seg)]] <- round(pmin(20, opp + z * se_opp), 2)
      scores[[paste0("opp_se.", seg)]] <- round(se_opp, 2)
    }
  }

  return(scores)
}

#' Get sample size
#'
#' Returns the number of non-NA respondents based on the last importance column.
#'
#' @param your_data_frame A data frame containing columns starting with "imp__"
#'
#' @return Integer: number of non-NA rows
#' @export
#'
#' @family scoring
#'
#' @examples
#' data(jtbd_sample)
#' get_sample_size(jtbd_sample)
get_sample_size <- function(your_data_frame) {
  last_imp <- your_data_frame %>%
    select(dplyr::starts_with("imp__")) %>%
    select(last_col())
  sample_size <- sum(!is.na(last_imp[[1]]))
  return(sample_size)
}

#' Calculate individual JTBD scores
#'
#' Calculates opportunity scores at the individual respondent level rather than
#' aggregated across the population. Useful for clustering and segmentation.
#' Applies the ODI formula per respondent: `opp = imp + max(0, imp - sat)`.
#'
#' @param df A data frame with `imp__`/`sat__` factor columns (1-5 scale)
#'
#' @return A long-format data frame with columns: caseid, objective, imp, sat, opp
#' @export
#'
#' @family scoring
#'
#' @examples
#' data(jtbd_sample)
#' individual <- get_jtbd_scores.individual(jtbd_sample)
#' head(individual)
get_jtbd_scores.individual <- function(df) {
  imp_cols <- grep("^imp__", names(df), value = TRUE)
  sat_cols <- grep("^sat__", names(df), value = TRUE)

  if (length(imp_cols) == 0) {
    cli::cli_abort("No columns starting with {.val imp__} found.")
  }

  # Build caseid if not present
  if (!"caseid" %in% names(df)) {
    df$caseid <- seq_len(nrow(df))
  }

  # Extract and convert imp columns to numeric
  imp_data <- df[, imp_cols, drop = FALSE]
  imp_data <- as.data.frame(lapply(imp_data, function(x) as.numeric(as.character(x))))
  imp_data$caseid <- df$caseid

  sat_data <- df[, sat_cols, drop = FALSE]
  sat_data <- as.data.frame(lapply(sat_data, function(x) as.numeric(as.character(x))))
  sat_data$caseid <- df$caseid

  # Pivot to long
  imp_long <- imp_data %>%
    pivot_longer(cols = -caseid, names_to = "col", values_to = "imp") %>%
    mutate(objective = sub("^imp__", "", col)) %>%
    select(caseid, objective, imp)

  sat_long <- sat_data %>%
    pivot_longer(cols = -caseid, names_to = "col", values_to = "sat") %>%
    mutate(objective = sub("^sat__", "", col)) %>%
    select(caseid, objective, sat)

  # Join and calculate per-respondent opportunity
  result <- imp_long %>%
    left_join(sat_long, by = c("caseid", "objective")) %>%
    mutate(opp = imp + pmax(0, imp - sat))

  cli::cli_inform("Individual scores calculated: {length(unique(result$caseid))} respondents x {length(unique(result$objective))} objectives.")

  return(tibble::as_tibble(result))
}
