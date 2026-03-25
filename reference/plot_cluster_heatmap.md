# Cluster opportunity heatmap

Heatmap of opportunity scores across discovered clusters, showing which
objectives are most underserved in each segment.

## Usage

``` r
plot_cluster_heatmap(
  profile,
  title = "Opportunity Heatmap by Discovered Segment"
)
```

## Arguments

- profile:

  Result from
  [`jtbd_cluster_profile()`](https://charlesrogers.github.io/jtbdtools/reference/jtbd_cluster_profile.md)
  or
  [`jtbd_segment()`](https://charlesrogers.github.io/jtbdtools/reference/jtbd_segment.md)\$profile

- title:

  Plot title

## Value

A ggplot object

## See also

Other clustering:
[`jtbd_cluster()`](https://charlesrogers.github.io/jtbdtools/reference/jtbd_cluster.md),
[`jtbd_cluster_profile()`](https://charlesrogers.github.io/jtbdtools/reference/jtbd_cluster_profile.md),
[`jtbd_feature_matrix()`](https://charlesrogers.github.io/jtbdtools/reference/jtbd_feature_matrix.md),
[`jtbd_find_k()`](https://charlesrogers.github.io/jtbdtools/reference/jtbd_find_k.md),
[`jtbd_pca()`](https://charlesrogers.github.io/jtbdtools/reference/jtbd_pca.md),
[`jtbd_segment()`](https://charlesrogers.github.io/jtbdtools/reference/jtbd_segment.md),
[`plot_elbow()`](https://charlesrogers.github.io/jtbdtools/reference/plot_elbow.md),
[`plot_pca_biplot()`](https://charlesrogers.github.io/jtbdtools/reference/plot_pca_biplot.md),
[`plot_pca_loadings()`](https://charlesrogers.github.io/jtbdtools/reference/plot_pca_loadings.md),
[`plot_pca_scree()`](https://charlesrogers.github.io/jtbdtools/reference/plot_pca_scree.md)

## Examples

``` r
data(jtbd_sample)
result <- jtbd_segment(jtbd_sample, n_clusters = 3)
#> Feature matrix: 200 respondents x 12 objectives (opportunity scores 1-9).
#> Kaiser rule: retaining 5 components (eigenvalue > 1).
#> ✔ Clustered 200 respondents into 3 segments.
#> ℹ Sizes: Segment_1 (n=65), Segment_2 (n=63), Segment_3 (n=72)
#> Found 3 segments with n > 30.
#> ✔ ODI segmentation complete.
#> ℹ Use `plot_cluster_heatmap()` or `plot_pca_biplot()` to visualize.
plot_cluster_heatmap(result$profile)
```
