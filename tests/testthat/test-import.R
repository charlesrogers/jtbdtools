test_that("read_qualtrics handles Qualtrics CSV format", {
  sample_file <- system.file("extdata", "sample_qualtrics.csv", package = "jtbdtools")
  df <- read_qualtrics(sample_file)

  # Should have removed metadata rows
  expect_equal(nrow(df), 10)

  # Should have removed auto-generated columns
  expect_false("ResponseId" %in% names(df))
  expect_false("StartDate" %in% names(df))

  # Should have question labels attribute
  labels <- attr(df, "question_labels")
  expect_false(is.null(labels))
  expect_true(any(grepl("important", labels, ignore.case = TRUE)))
})

test_that("detect_imp_sat finds importance and satisfaction columns", {
  sample_file <- system.file("extdata", "sample_qualtrics.csv", package = "jtbdtools")
  df <- read_qualtrics(sample_file)
  detected <- detect_imp_sat(df)

  expect_equal(length(detected$imp), 4)
  expect_equal(length(detected$sat), 4)
  expect_true("Q1_1" %in% detected$imp)
  expect_true("Q2_1" %in% detected$sat)
})

test_that("prep_survey creates valid JTBD data", {
  sample_file <- system.file("extdata", "sample_qualtrics.csv", package = "jtbdtools")
  df <- read_qualtrics(sample_file)
  detected <- detect_imp_sat(df)

  ready <- prep_survey(df, detected$imp, detected$sat,
                       job_steps = list(researching = 1:4),
                       segment_col = "Q3")

  # Should have imp__ and sat__ columns
  imp_cols <- grep("^imp__", names(ready), value = TRUE)
  sat_cols <- grep("^sat__", names(ready), value = TRUE)
  expect_equal(length(imp_cols), 4)
  expect_equal(length(sat_cols), 4)

  # Should be factors

  expect_true(is.factor(ready[[imp_cols[1]]]))

  # Should have segment column
  expect_true("Q3" %in% names(ready))

  # Should work with get_jtbd_scores
  scores <- get_jtbd_scores(ready)
  expect_equal(nrow(scores), 4)
})

test_that("prep_qualtrics works end-to-end", {
  sample_file <- system.file("extdata", "sample_qualtrics.csv", package = "jtbdtools")
  ready <- prep_qualtrics(sample_file,
                          job_steps = list(researching = 1:4),
                          segment_col = "Q3")

  scores <- get_jtbd_scores(ready)
  expect_equal(nrow(scores), 4)
  expect_true(all(scores$opp.all >= 0))
})

test_that("prep_survey errors on mismatched column counts", {
  df <- data.frame(a = 1:5, b = 1:5, c = 1:5)
  expect_error(prep_survey(df, c("a", "b"), c("c")), "must match")
})

test_that("validate_jtbd_data validates correct data", {
  data(jtbd_sample)
  expect_true(validate_jtbd_data(jtbd_sample))
})

test_that("validate_jtbd_data catches bad data", {
  bad_data <- data.frame(x = 1:5, y = 6:10)
  expect_warning(validate_jtbd_data(bad_data))
  expect_false(suppressWarnings(validate_jtbd_data(bad_data)))
})
