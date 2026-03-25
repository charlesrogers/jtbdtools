test_that("jtbd_feature_matrix returns correct dimensions", {
  data(jtbd_sample)
  fm <- jtbd_feature_matrix(jtbd_sample)

  expect_true(is.matrix(fm))
  expect_equal(nrow(fm), 200)
  expect_equal(ncol(fm), 12)

  # Opportunity scores should be in 1-9 range (imp 1-5, opp = imp + max(0, imp-sat))
  expect_true(all(fm >= 1, na.rm = TRUE))
  expect_true(all(fm <= 9, na.rm = TRUE))
})

test_that("jtbd_pca returns correct structure", {
  data(jtbd_sample)
  pca <- jtbd_pca(jtbd_sample)

  expect_s3_class(pca, "jtbd_pca")
  expect_true(pca$n_components >= 2)
  expect_equal(nrow(pca$scores), 200)
  expect_equal(ncol(pca$loadings), pca$n_components)
  expect_equal(nrow(pca$loadings), 12)

  # Variance explained should sum to 100
  expect_equal(tail(pca$variance_explained$cumulative_pct, 1), 100)
})

test_that("jtbd_cluster returns valid clusters", {
  data(jtbd_sample)
  cl <- jtbd_cluster(jtbd_sample, n_clusters = 3)

  expect_s3_class(cl, "jtbd_cluster")
  expect_equal(length(cl$cluster), 200)
  expect_equal(cl$n_clusters, 3)
  expect_equal(length(unique(cl$cluster)), 3)
  expect_equal(sum(cl$size), 200)

  # Data should have jtbd_cluster column
  expect_true("jtbd_cluster" %in% names(cl$data))
  expect_true(is.factor(cl$data$jtbd_cluster))
})

test_that("jtbd_cluster without PCA works", {
  data(jtbd_sample)
  cl <- jtbd_cluster(jtbd_sample, n_clusters = 2, use_pca = FALSE)

  expect_equal(length(unique(cl$cluster)), 2)
  expect_null(cl$pca)
})

test_that("jtbd_find_k returns evaluation data", {
  data(jtbd_sample)
  k_eval <- jtbd_find_k(jtbd_sample, max_k = 4)

  expect_s3_class(k_eval, "data.frame")
  expect_equal(nrow(k_eval), 3)  # k = 2, 3, 4
  expect_true(all(c("k", "wcss", "avg_silhouette") %in% names(k_eval)))
  expect_true(all(k_eval$avg_silhouette > 0))
})

test_that("jtbd_cluster_profile produces comparison with cluster columns", {
  data(jtbd_sample)
  cl <- jtbd_cluster(jtbd_sample, n_clusters = 3)
  profile <- jtbd_cluster_profile(jtbd_sample, cl, test_sig = FALSE)

  # Should have opp columns for each cluster
  opp_cols <- grep("^opp\\.", names(profile), value = TRUE)
  expect_true(length(opp_cols) >= 4)  # all + 3 clusters
  expect_equal(nrow(profile), 12)
})

test_that("jtbd_segment runs the full pipeline", {
  data(jtbd_sample)
  result <- jtbd_segment(jtbd_sample, n_clusters = 3, test_sig = FALSE)

  expect_s3_class(result, "jtbd_segment")
  expect_true("profile" %in% names(result))
  expect_true("data" %in% names(result))
  expect_true("pca" %in% names(result))
  expect_true("jtbd_cluster" %in% names(result$data))
})

test_that("discovered clusters differ from known segments", {
  data(jtbd_sample)
  cl <- jtbd_cluster(jtbd_sample, n_clusters = 3)

  # Cluster assignments should NOT perfectly match the known segments
  # (they're discovering structure, not reproducing demographics)
  known <- as.integer(jtbd_sample$segment)
  discovered <- cl$cluster

  # Check that it's not a perfect permutation match
  # (there might be some correlation, but shouldn't be identical)
  ct <- table(known, discovered)
  # At least some off-diagonal entries should be non-zero
  diag_sum <- sum(apply(ct, 1, max))
  expect_true(diag_sum < 200)  # Not a perfect match
})

test_that("get_jtbd_scores.individual works", {
  data(jtbd_sample)
  individual <- get_jtbd_scores.individual(jtbd_sample)

  expect_s3_class(individual, "tbl_df")
  expect_true(all(c("caseid", "objective", "imp", "sat", "opp") %in% names(individual)))
  expect_equal(length(unique(individual$caseid)), 200)
  expect_equal(length(unique(individual$objective)), 12)

  # Opp should follow ODI formula
  expect_true(all(individual$opp >= individual$imp, na.rm = TRUE))
})
