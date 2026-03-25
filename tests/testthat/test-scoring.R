test_that("calculate_opportunity_score applies ODI formula correctly", {
  # When importance > satisfaction: opp = imp + imp - sat
  df <- data.frame(
    objective = c("a", "b", "c"),
    imp_sat = c("imp", "imp", "imp"),
    imp_sat_score = c(8, 6, 5),
    stringsAsFactors = FALSE
  )
  df_sat <- data.frame(
    objective = c("a", "b", "c"),
    imp_sat = c("sat", "sat", "sat"),
    imp_sat_score = c(4, 8, 5),
    stringsAsFactors = FALSE
  )
  combined <- rbind(df, df_sat)

  result <- calculate_opportunity_score(combined)

  # a: imp=8, sat=4, imp > sat -> opp = 8 + 8 - 4 = 12
  expect_equal(result$opp[result$objective == "a"], 12)

  # b: imp=6, sat=8, sat > imp -> opp = imp = 6 (floor)
  expect_equal(result$opp[result$objective == "b"], 6)

  # c: imp=5, sat=5, equal -> opp = 5 + 5 - 5 = 5
  expect_equal(result$opp[result$objective == "c"], 5)
})

test_that("get_jtbd_scores works with sample data", {
  data(jtbd_sample)
  scores <- get_jtbd_scores(jtbd_sample)

  expect_s3_class(scores, "data.frame")
  expect_true("job_step" %in% names(scores))
  expect_true("objective" %in% names(scores))
  expect_true("opp.all" %in% names(scores))
  expect_true("imp.all" %in% names(scores))
  expect_true("sat.all" %in% names(scores))

  # Should have 12 objectives (4 per step x 3 steps)
  expect_equal(nrow(scores), 12)

  # Opportunity scores should be >= importance (the floor)
  expect_true(all(scores$opp.all >= scores$imp.all - 0.01))

  # Scores should be in 0-20 range
  expect_true(all(scores$opp.all >= 0))
  expect_true(all(scores$opp.all <= 20))
})

test_that("get_jtbd_scores rejects data without imp/sat columns", {
  bad_data <- data.frame(x = 1:5, y = 6:10)
  expect_error(get_jtbd_scores(bad_data), "No importance/satisfaction columns found")
})

test_that("get_sample_size returns correct count", {
  data(jtbd_sample)
  n <- get_sample_size(jtbd_sample)
  expect_equal(n, 200)
})
