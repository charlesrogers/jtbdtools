# Create sample JTBD survey dataset for package examples
# Run this script to regenerate data/jtbd_sample.rda

set.seed(42)

n <- 200

# Define job steps and objectives
job_steps <- list(
  researching = c(
    "minimize_time_to_find_options",
    "minimize_time_to_evaluate_options",
    "minimize_likelihood_of_missing_relevant_options",
    "minimize_time_to_understand_pricing"
  ),
  purchasing = c(
    "minimize_time_to_complete_transaction",
    "minimize_likelihood_of_unexpected_costs",
    "minimize_time_to_receive_confirmation",
    "minimize_likelihood_of_errors_in_order"
  ),
  onboarding = c(
    "minimize_time_to_get_started",
    "minimize_likelihood_of_confusion_during_setup",
    "minimize_time_to_reach_first_value",
    "minimize_likelihood_of_needing_support"
  )
)

# Generate survey responses (1-5 Likert scale) as factors
generate_responses <- function(n, mean_imp, mean_sat) {
  imp <- pmin(5, pmax(1, round(rnorm(n, mean_imp, 0.9))))
  sat <- pmin(5, pmax(1, round(rnorm(n, mean_sat, 1.0))))
  list(imp = factor(imp, levels = 1:5), sat = factor(sat, levels = 1:5))
}

# Build data frame
df <- data.frame(caseid = 1:n)

# Segmentation column
segments <- sample(c("power_user", "casual", "new_user"),
                   n, replace = TRUE, prob = c(0.3, 0.45, 0.25))
df$segment <- factor(segments)

# ============================================================
# Demographic / profiling columns
# These correlate with segments to make profiling realistic
# ============================================================

# Gender — power users skew male, casuals balanced, new users skew female
df$gender <- factor(ifelse(
  segments == "power_user",
  sample(c("Male", "Female", "Non-binary"), n, replace = TRUE, prob = c(0.55, 0.38, 0.07)),
  ifelse(segments == "new_user",
    sample(c("Male", "Female", "Non-binary"), n, replace = TRUE, prob = c(0.35, 0.55, 0.10)),
    sample(c("Male", "Female", "Non-binary"), n, replace = TRUE, prob = c(0.45, 0.48, 0.07))
  )
))

# Age group — power users older, new users younger
age_probs <- list(
  power_user = c(0.05, 0.15, 0.35, 0.30, 0.15),
  casual     = c(0.15, 0.30, 0.25, 0.20, 0.10),
  new_user   = c(0.30, 0.35, 0.20, 0.10, 0.05)
)
df$age_group <- factor(NA, levels = c("18-24", "25-34", "35-44", "45-54", "55+"))
for (seg in c("power_user", "casual", "new_user")) {
  mask <- segments == seg
  df$age_group[mask] <- sample(c("18-24", "25-34", "35-44", "45-54", "55+"),
                                sum(mask), replace = TRUE, prob = age_probs[[seg]])
}

# Income — power users higher income
income_probs <- list(
  power_user = c(0.05, 0.15, 0.30, 0.35, 0.15),
  casual     = c(0.15, 0.30, 0.30, 0.20, 0.05),
  new_user   = c(0.25, 0.35, 0.25, 0.10, 0.05)
)
df$income <- factor(NA, levels = c("<$30k", "$30-50k", "$50-75k", "$75-100k", "$100k+"))
for (seg in c("power_user", "casual", "new_user")) {
  mask <- segments == seg
  df$income[mask] <- sample(c("<$30k", "$30-50k", "$50-75k", "$75-100k", "$100k+"),
                             sum(mask), replace = TRUE, prob = income_probs[[seg]])
}

# Education
edu_probs <- list(
  power_user = c(0.05, 0.20, 0.40, 0.35),
  casual     = c(0.10, 0.35, 0.35, 0.20),
  new_user   = c(0.20, 0.40, 0.25, 0.15)
)
df$education <- factor(NA, levels = c("High School", "Some College", "Bachelor's", "Graduate"))
for (seg in c("power_user", "casual", "new_user")) {
  mask <- segments == seg
  df$education[mask] <- sample(c("High School", "Some College", "Bachelor's", "Graduate"),
                                sum(mask), replace = TRUE, prob = edu_probs[[seg]])
}

# Tenure with product
tenure_probs <- list(
  power_user = c(0.05, 0.15, 0.35, 0.45),
  casual     = c(0.20, 0.30, 0.30, 0.20),
  new_user   = c(0.45, 0.30, 0.15, 0.10)
)
df$tenure <- factor(NA, levels = c("< 6 months", "6-12 months", "1-2 years", "2+ years"))
for (seg in c("power_user", "casual", "new_user")) {
  mask <- segments == seg
  df$tenure[mask] <- sample(c("< 6 months", "6-12 months", "1-2 years", "2+ years"),
                             sum(mask), replace = TRUE, prob = tenure_probs[[seg]])
}

# ============================================================
# Importance / Satisfaction columns
# ============================================================
base_imp <- runif(12, 3.2, 4.5)
base_sat <- runif(12, 2.5, 4.0)

segment_offsets <- list(
  power_user = list(imp_delta = c(0.3, 0.5, 0.1, 0.2, 0.4, 0.6, 0.2, 0.1, -0.2, 0.3, 0.1, 0.4),
                    sat_delta = c(0.2, -0.3, 0.4, 0.1, -0.2, -0.4, 0.3, 0.2, 0.5, -0.1, 0.3, -0.2)),
  casual     = list(imp_delta = c(-0.1, -0.2, 0.2, 0.1, -0.1, -0.3, 0.1, 0.2, 0.3, -0.1, -0.2, 0.1),
                    sat_delta = c(0.1, 0.3, -0.1, -0.2, 0.2, 0.3, -0.1, -0.3, -0.2, 0.4, 0.1, 0.2)),
  new_user   = list(imp_delta = c(-0.2, -0.1, -0.3, -0.1, -0.2, 0.1, -0.2, -0.3, 0.1, -0.2, 0.2, -0.3),
                    sat_delta = c(-0.4, -0.5, -0.2, -0.3, -0.4, -0.6, -0.3, -0.1, -0.4, -0.5, -0.3, -0.4))
)

i <- 1
for (step_name in names(job_steps)) {
  for (obj_name in job_steps[[step_name]]) {
    imp_col <- paste0("imp__", step_name, ".", obj_name)
    sat_col <- paste0("sat__", step_name, ".", obj_name)

    imp_vals <- numeric(n)
    sat_vals <- numeric(n)

    for (seg in c("power_user", "casual", "new_user")) {
      mask <- segments == seg
      seg_n <- sum(mask)
      seg_imp_mean <- base_imp[i] + segment_offsets[[seg]]$imp_delta[i]
      seg_sat_mean <- base_sat[i] + segment_offsets[[seg]]$sat_delta[i]
      imp_vals[mask] <- pmin(5, pmax(1, round(rnorm(seg_n, seg_imp_mean, 0.9))))
      sat_vals[mask] <- pmin(5, pmax(1, round(rnorm(seg_n, seg_sat_mean, 1.0))))
    }

    df[[imp_col]] <- factor(imp_vals, levels = 1:5)
    df[[sat_col]] <- factor(sat_vals, levels = 1:5)
    i <- i + 1
  }
}

jtbd_sample <- df

usethis::use_data(jtbd_sample, overwrite = TRUE)
