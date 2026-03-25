test_that("jtbd_profile_segments auto-detects profiling columns", {
  data(jtbd_sample)
  cl <- jtbd_cluster(jtbd_sample, n_clusters = 3)
  prof <- jtbd_profile_segments(cl$data)

  expect_s3_class(prof, "jtbd_profile")
  expect_true(nrow(prof$summary) >= 5)  # gender, age_group, income, education, tenure, segment
  expect_true(all(c("variable", "p_value", "cramers_v", "significant") %in% names(prof$summary)))
  expect_true(all(prof$summary$p_value >= 0 & prof$summary$p_value <= 1, na.rm = TRUE))
})

test_that("profiling finds distinguishing variables", {
  data(jtbd_sample)
  cl <- jtbd_cluster(jtbd_sample, n_clusters = 3)
  prof <- jtbd_profile_segments(cl$data)

  # At least segment should be significant (it correlates with the data generation)
  expect_true(any(prof$summary$significant))
  expect_true(nrow(prof$distinguishing) >= 1)
})

test_that("profiling details contain index values", {
  data(jtbd_sample)
  cl <- jtbd_cluster(jtbd_sample, n_clusters = 3)
  prof <- jtbd_profile_segments(cl$data)

  # Check details structure
  expect_true(length(prof$details) > 0)
  first_detail <- prof$details[[1]]
  expect_true(all(c("cluster", "value", "pct", "overall_pct", "index") %in% names(first_detail)))
})

test_that("profiling errors without cluster column", {
  data(jtbd_sample)
  expect_error(jtbd_profile_segments(jtbd_sample), "not found")
})

test_that("profiling with explicit columns works", {
  data(jtbd_sample)
  cl <- jtbd_cluster(jtbd_sample, n_clusters = 3)
  prof <- jtbd_profile_segments(cl$data, profile_cols = c("income", "tenure"))

  expect_equal(nrow(prof$summary), 2)
})
