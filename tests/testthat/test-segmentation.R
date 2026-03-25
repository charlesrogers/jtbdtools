test_that("get_jtbd_scores.comparison produces different scores per segment", {
  data(jtbd_sample)
  result <- get_jtbd_scores.comparison(jtbd_sample, "segment")

  expect_s3_class(result, "data.frame")
  expect_true("opp.all" %in% names(result))
  expect_true("opp.casual" %in% names(result))
  expect_true("opp.new_user" %in% names(result))
  expect_true("opp.power_user" %in% names(result))

  # Scores should actually differ between segments
  expect_false(all(result$opp.casual == result$opp.all))
  expect_false(all(result$opp.new_user == result$opp.all))
})

test_that("get_jtbd_var_values.list finds segments", {
  data(jtbd_sample)
  segments <- get_jtbd_var_values.list(jtbd_sample, "segment", min_n = 30)

  expect_true(length(segments) >= 2)
  expect_true("casual" %in% segments)
})

test_that("get_jtbd_var_values.list errors on missing column", {
  data(jtbd_sample)
  expect_error(
    get_jtbd_var_values.list(jtbd_sample, "nonexistent_column"),
    "not found"
  )
})

test_that("get_jtbd_scores.pairwise compares two segments", {
  data(jtbd_sample)
  result <- get_jtbd_scores.pairwise(jtbd_sample, "segment", "casual", "power_user")

  expect_s3_class(result, "data.frame")
  expect_true("opp.casual" %in% names(result))
  expect_true("opp.power_user" %in% names(result))
  expect_equal(nrow(result), 12)
})
