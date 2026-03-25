test_that("remove_data_prefix removes prefix before --", {
  df <- data.frame(a = 1, `Q1--How important` = 2, check.names = FALSE)
  result <- remove_data_prefix(df)
  expect_equal(names(result), c("a", "How important"))
})

test_that("remove_data_suffix removes suffix after -", {
  df <- data.frame(`How important - scale` = 1, check.names = FALSE)
  result <- remove_data_suffix(df)
  expect_equal(names(result), "How important ")
})

test_that("replace_spaces_with_underscores cleans names", {
  df <- data.frame(`my column` = 1, check.names = FALSE)
  result <- replace_spaces_with_underscores(df)
  expect_true(!grepl(" ", names(result)))
})
