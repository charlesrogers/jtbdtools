# Run the full ODI segmentation pipeline

One-call convenience function that runs: feature matrix → PCA → K-Means
→ T2B profiling with significance testing. Returns everything needed for
analysis and visualization.

## Usage

``` r
jtbd_segment(df, n_clusters = 3, use_pca = TRUE, test_sig = TRUE, seed = 42)
```

## Arguments

- df:

  A data frame with `imp__`/`sat__` columns

- n_clusters:

  Number of clusters (default: 3)

- use_pca:

  Use PCA before clustering (default: TRUE)

- test_sig:

  Run significance tests on cluster profiles (default: TRUE)

- seed:

  Random seed (default: 42)

## Value

A list with:

- `cluster_result`: full
  [`jtbd_cluster()`](https://charlesrogers.github.io/jtbdtools/reference/jtbd_cluster.md)
  output

- `pca`: PCA result

- `profile`: T2B scores per cluster with p-values

- `data`: original data with `jtbd_cluster` column

## See also

Other clustering:
[`jtbd_cluster()`](https://charlesrogers.github.io/jtbdtools/reference/jtbd_cluster.md),
[`jtbd_cluster_profile()`](https://charlesrogers.github.io/jtbdtools/reference/jtbd_cluster_profile.md),
[`jtbd_feature_matrix()`](https://charlesrogers.github.io/jtbdtools/reference/jtbd_feature_matrix.md),
[`jtbd_find_k()`](https://charlesrogers.github.io/jtbdtools/reference/jtbd_find_k.md),
[`jtbd_pca()`](https://charlesrogers.github.io/jtbdtools/reference/jtbd_pca.md),
[`jtbd_profile_segments()`](https://charlesrogers.github.io/jtbdtools/reference/jtbd_profile_segments.md),
[`plot_cluster_heatmap()`](https://charlesrogers.github.io/jtbdtools/reference/plot_cluster_heatmap.md),
[`plot_elbow()`](https://charlesrogers.github.io/jtbdtools/reference/plot_elbow.md),
[`plot_pca_biplot()`](https://charlesrogers.github.io/jtbdtools/reference/plot_pca_biplot.md),
[`plot_pca_loadings()`](https://charlesrogers.github.io/jtbdtools/reference/plot_pca_loadings.md),
[`plot_pca_scree()`](https://charlesrogers.github.io/jtbdtools/reference/plot_pca_scree.md),
[`plot_segment_index()`](https://charlesrogers.github.io/jtbdtools/reference/plot_segment_index.md),
[`plot_segment_profiles()`](https://charlesrogers.github.io/jtbdtools/reference/plot_segment_profiles.md)

## Examples

``` r
data(jtbd_sample)
result <- jtbd_segment(jtbd_sample, n_clusters = 3)
#> Feature matrix: 200 respondents x 12 objectives (opportunity scores 1-9).
#> Kaiser rule: retaining 5 components (eigenvalue > 1).
#> ✔ Clustered 200 respondents into 3 segments.
#> ℹ Sizes: Segment_1 (n=60), Segment_2 (n=76), Segment_3 (n=64)
#> Found 3 segments with n > 30.
#> ✔ ODI segmentation complete.
#> ℹ Use `plot_cluster_heatmap()` or `plot_pca_biplot()` to visualize.
names(result)
#> [1] "cluster_result" "pca"            "profile"        "data"          
```
