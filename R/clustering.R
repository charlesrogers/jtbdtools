#' Build respondent x objective feature matrix
#'
#' Converts raw survey data into a numeric matrix suitable for PCA and clustering.
#' For each respondent and objective, calculates the per-respondent opportunity score
#' using the ODI formula: `opp = imp + max(0, imp - sat)`.
#'
#' @param df A data frame with `imp__`/`sat__` factor columns (1-5 scale)
#'
#' @return A numeric matrix with respondents as rows and objectives as columns.
#'   Column names are the objective identifiers (e.g., "researching.minimize_time_to_find_options").
#' @export
#'
#' @family clustering
#'
#' @examples
#' data(jtbd_sample)
#' fm <- jtbd_feature_matrix(jtbd_sample)
#' dim(fm)  # 200 x 12
jtbd_feature_matrix <- function(df) {
  imp_cols <- grep("^imp__", names(df), value = TRUE)
  sat_cols <- grep("^sat__", names(df), value = TRUE)

  if (length(imp_cols) == 0 || length(sat_cols) == 0) {
    cli::cli_abort("No imp__/sat__ columns found. See {.code ?jtbd_sample} for expected format.")
  }

  objectives <- sub("^imp__", "", imp_cols)
  n <- nrow(df)
  m <- length(objectives)
  mat <- matrix(NA_real_, nrow = n, ncol = m)
  colnames(mat) <- objectives

  for (i in seq_along(imp_cols)) {
    imp_vals <- as.numeric(as.character(df[[imp_cols[i]]]))
    sat_col <- paste0("sat__", objectives[i])
    if (sat_col %in% names(df)) {
      sat_vals <- as.numeric(as.character(df[[sat_col]]))
    } else {
      sat_vals <- rep(NA_real_, n)
    }
    mat[, i] <- imp_vals + pmax(0, imp_vals - sat_vals)
  }

  cli::cli_inform("Feature matrix: {n} respondents x {m} objectives (opportunity scores 1-9).")
  return(mat)
}

#' Run PCA on JTBD opportunity data
#'
#' Performs Principal Component Analysis on the respondent-level opportunity
#' feature matrix. Uses the Kaiser rule (eigenvalues > 1) to select the number
#' of components if not specified.
#'
#' @param df A data frame with `imp__`/`sat__` columns, or a pre-computed feature matrix
#' @param n_components Number of components to retain. If NULL, uses Kaiser rule.
#'
#' @return A list with:
#'   - `pca`: the `prcomp` object
#'   - `n_components`: number of components retained
#'   - `loadings`: loading matrix (objectives x components)
#'   - `scores`: respondent PC scores matrix
#'   - `variance_explained`: data frame with per-component and cumulative variance
#'   - `eigenvalues`: eigenvalues for each component
#' @export
#'
#' @family clustering
#'
#' @examples
#' data(jtbd_sample)
#' pca_result <- jtbd_pca(jtbd_sample)
#' pca_result$n_components
#' pca_result$variance_explained
jtbd_pca <- function(df, n_components = NULL) {
  # Accept either a data frame or a pre-computed matrix

  if (is.matrix(df)) {
    mat <- df
  } else {
    mat <- jtbd_feature_matrix(df)
  }

  # Remove rows with NAs
  complete <- stats::complete.cases(mat)
  if (sum(!complete) > 0) {
    cli::cli_warn("Removed {sum(!complete)} rows with missing values.")
    mat <- mat[complete, ]
  }

  # Run PCA (centered and scaled)
  pca <- stats::prcomp(mat, center = TRUE, scale. = TRUE)

  # Eigenvalues
  eigenvalues <- pca$sdev^2

  # Kaiser rule: retain components with eigenvalue > 1
  if (is.null(n_components)) {
    n_components <- sum(eigenvalues > 1)
    n_components <- max(n_components, 2)  # Keep at least 2 for biplot
    cli::cli_inform("Kaiser rule: retaining {n_components} components (eigenvalue > 1).")
  }

  n_components <- min(n_components, length(eigenvalues))

  # Variance explained
  var_explained <- data.frame(
    component = seq_along(eigenvalues),
    eigenvalue = round(eigenvalues, 3),
    variance_pct = round(eigenvalues / sum(eigenvalues) * 100, 1),
    cumulative_pct = round(cumsum(eigenvalues / sum(eigenvalues) * 100), 1)
  )

  # Loading matrix (objectives x retained components)
  loadings <- pca$rotation[, 1:n_components, drop = FALSE]

  # Scores (respondents x retained components)
  scores <- pca$x[, 1:n_components, drop = FALSE]

  result <- list(
    pca = pca,
    n_components = n_components,
    loadings = loadings,
    scores = scores,
    variance_explained = var_explained,
    eigenvalues = eigenvalues
  )
  class(result) <- c("jtbd_pca", "list")
  return(result)
}

#' K-Means clustering on JTBD opportunity data
#'
#' Clusters respondents into groups with shared unmet needs using K-Means.
#' Optionally performs PCA first to reduce dimensionality (recommended for
#' surveys with many objectives).
#'
#' @param df A data frame with `imp__`/`sat__` columns
#' @param n_clusters Number of clusters (default: 3)
#' @param use_pca If TRUE, cluster on PCA scores instead of raw features (default: TRUE)
#' @param n_components Number of PCA components to use. If NULL, uses Kaiser rule.
#' @param seed Random seed for reproducibility (default: 42)
#'
#' @return A list with:
#'   - `cluster`: integer vector of cluster assignments
#'   - `centers`: cluster centroids
#'   - `n_clusters`: number of clusters
#'   - `size`: cluster sizes
#'   - `pca`: PCA result (if `use_pca = TRUE`)
#'   - `feature_matrix`: the opportunity feature matrix
#'   - `data`: original data with `jtbd_cluster` column appended
#'   - `kmeans`: the raw kmeans object
#' @export
#'
#' @family clustering
#'
#' @examples
#' data(jtbd_sample)
#' cl <- jtbd_cluster(jtbd_sample, n_clusters = 3)
#' table(cl$cluster)
#'
#' # Use with existing comparison functions
#' comparison <- get_jtbd_scores.comparison(cl$data, "jtbd_cluster")
jtbd_cluster <- function(df, n_clusters = 3, use_pca = TRUE, n_components = NULL, seed = 42) {
  mat <- jtbd_feature_matrix(df)

  pca_result <- NULL
  if (use_pca) {
    pca_result <- jtbd_pca(mat, n_components = n_components)
    cluster_input <- pca_result$scores
  } else {
    cluster_input <- scale(mat)
  }

  set.seed(seed)
  km <- stats::kmeans(cluster_input, centers = n_clusters, nstart = 25, iter.max = 100)

  # Add cluster labels to original data
  data_with_clusters <- df
  data_with_clusters$jtbd_cluster <- factor(paste0("Segment_", km$cluster))

  sizes <- table(km$cluster)
  cli::cli_inform(c(
    "v" = "Clustered {nrow(df)} respondents into {n_clusters} segments.",
    "i" = "Sizes: {paste(paste0('Segment_', names(sizes), ' (n=', sizes, ')'), collapse = ', ')}"
  ))

  result <- list(
    cluster = km$cluster,
    centers = km$centers,
    n_clusters = n_clusters,
    size = as.integer(sizes),
    pca = pca_result,
    feature_matrix = mat,
    data = data_with_clusters,
    kmeans = km
  )
  class(result) <- c("jtbd_cluster", "list")
  return(result)
}

#' Evaluate multiple cluster solutions
#'
#' Runs K-Means for k = 2 through `max_k` and returns diagnostics
#' to help choose the optimal number of clusters (elbow method + silhouette).
#'
#' @param df A data frame with `imp__`/`sat__` columns
#' @param max_k Maximum number of clusters to evaluate (default: 6)
#' @param use_pca If TRUE, cluster on PCA scores (default: TRUE)
#' @param seed Random seed (default: 42)
#'
#' @return A data frame with columns: k, wcss, avg_silhouette
#' @export
#'
#' @family clustering
#'
#' @examples
#' data(jtbd_sample)
#' k_eval <- jtbd_find_k(jtbd_sample, max_k = 5)
#' k_eval
jtbd_find_k <- function(df, max_k = 6, use_pca = TRUE, seed = 42) {
  mat <- jtbd_feature_matrix(df)

  if (use_pca) {
    pca_result <- jtbd_pca(mat)
    cluster_input <- pca_result$scores
  } else {
    cluster_input <- scale(mat)
  }

  results <- data.frame(k = integer(0), wcss = numeric(0), avg_silhouette = numeric(0))

  for (k in 2:max_k) {
    set.seed(seed)
    km <- stats::kmeans(cluster_input, centers = k, nstart = 25, iter.max = 100)

    # Silhouette
    sil <- cluster::silhouette(km$cluster, stats::dist(cluster_input))
    avg_sil <- mean(sil[, "sil_width"])

    results <- rbind(results, data.frame(
      k = k,
      wcss = round(km$tot.withinss, 1),
      avg_silhouette = round(avg_sil, 3)
    ))
  }

  best_k <- results$k[which.max(results$avg_silhouette)]
  cli::cli_inform("Best k by silhouette: {best_k} (avg silhouette = {max(results$avg_silhouette)})")

  return(results)
}

#' Profile discovered clusters using T2B scoring
#'
#' Calculates top-2-box opportunity scores per cluster using the existing
#' JTBD scoring pipeline, with statistical significance testing.
#'
#' @param df The original data frame
#' @param cluster_result Result from [jtbd_cluster()]
#' @param test_sig Run significance tests (default: TRUE)
#'
#' @return A data frame from [get_jtbd_scores.comparison()] with scores per cluster
#' @export
#'
#' @family clustering
#'
#' @examples
#' data(jtbd_sample)
#' cl <- jtbd_cluster(jtbd_sample, n_clusters = 3)
#' profile <- jtbd_cluster_profile(jtbd_sample, cl)
#' head(profile)
jtbd_cluster_profile <- function(df, cluster_result, test_sig = TRUE) {
  data_with_clusters <- cluster_result$data
  get_jtbd_scores.comparison(data_with_clusters, "jtbd_cluster", test_sig = test_sig)
}

#' Run the full ODI segmentation pipeline
#'
#' One-call convenience function that runs: feature matrix → PCA → K-Means →
#' T2B profiling with significance testing. Returns everything needed for
#' analysis and visualization.
#'
#' @param df A data frame with `imp__`/`sat__` columns
#' @param n_clusters Number of clusters (default: 3)
#' @param use_pca Use PCA before clustering (default: TRUE)
#' @param test_sig Run significance tests on cluster profiles (default: TRUE)
#' @param seed Random seed (default: 42)
#'
#' @return A list with:
#'   - `cluster_result`: full [jtbd_cluster()] output
#'   - `pca`: PCA result
#'   - `profile`: T2B scores per cluster with p-values
#'   - `data`: original data with `jtbd_cluster` column
#' @export
#'
#' @family clustering
#'
#' @examples
#' data(jtbd_sample)
#' result <- jtbd_segment(jtbd_sample, n_clusters = 3)
#' names(result)
jtbd_segment <- function(df, n_clusters = 3, use_pca = TRUE, test_sig = TRUE, seed = 42) {
  cl <- jtbd_cluster(df, n_clusters = n_clusters, use_pca = use_pca, seed = seed)
  profile <- jtbd_cluster_profile(df, cl, test_sig = test_sig)

  result <- list(
    cluster_result = cl,
    pca = cl$pca,
    profile = profile,
    data = cl$data
  )
  class(result) <- c("jtbd_segment", "list")

  cli::cli_inform(c(
    "v" = "ODI segmentation complete.",
    "i" = "Use {.fn plot_cluster_heatmap} or {.fn plot_pca_biplot} to visualize."
  ))

  return(result)
}
