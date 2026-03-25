test_that("test_segment_significance returns p-values for all objectives", {
  data(jtbd_sample)
  sig <- test_segment_significance(jtbd_sample, "segment", "casual", "power_user")

  expect_s3_class(sig, "tbl_df")
  expect_true("p.imp" %in% names(sig))
  expect_true("p.sat" %in% names(sig))
  expect_true("sig.imp" %in% names(sig))
  expect_true("sig.sat" %in% names(sig))

  # Should have 12 objectives

  expect_equal(nrow(sig), 12)

  # P-values should be between 0 and 1
  expect_true(all(sig$p.imp >= 0 & sig$p.imp <= 1, na.rm = TRUE))
  expect_true(all(sig$p.sat >= 0 & sig$p.sat <= 1, na.rm = TRUE))
})

test_that("segments with clear differences produce significant p-values", {
  data(jtbd_sample)
  sig <- test_segment_significance(jtbd_sample, "segment", "casual", "power_user")

  # At least some objectives should be significantly different
  expect_true(any(sig$sig.imp))
  expect_true(any(sig$sig.sat))
})

test_that("get_jtbd_scores.comparison with test_sig adds p-value columns", {
  data(jtbd_sample)
  comp <- get_jtbd_scores.comparison(jtbd_sample, "segment", test_sig = TRUE)

  # Should have p-value columns for each segment
  expect_true("p.imp.casual" %in% names(comp))
  expect_true("p.sat.casual" %in% names(comp))
  expect_true("sig.imp.power_user" %in% names(comp))

  # Without test_sig, no p-value columns
  comp_no_sig <- get_jtbd_scores.comparison(jtbd_sample, "segment", test_sig = FALSE)
  expect_false("p.imp.casual" %in% names(comp_no_sig))
})

test_that("test_segment_significance warns on small samples", {
  data(jtbd_sample)
  # Create a tiny segment
  tiny <- jtbd_sample[1:3, ]
  tiny$segment <- factor("tiny")
  combined <- rbind(jtbd_sample[1:50, ], tiny)

  expect_warning(
    test_segment_significance(combined, "segment", "tiny", "casual"),
    "very small"
  )
})
