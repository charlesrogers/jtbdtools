# Evaluate multiple cluster solutions

Runs K-Means for k = 2 through `max_k` and returns diagnostics to help
choose the optimal number of clusters (elbow method + silhouette).

## Usage

``` r
jtbd_find_k(df, max_k = 6, use_pca = TRUE, seed = 42)
```

## Arguments

- df:

  A data frame with `imp__`/`sat__` columns

- max_k:

  Maximum number of clusters to evaluate (default: 6)

- use_pca:

  If TRUE, cluster on PCA scores (default: TRUE)

- seed:

  Random seed (default: 42)

## Value

A data frame with columns: k, wcss, avg_silhouette

## See also

Other clustering:
[`create_persona_table()`](https://charlesrogers.github.io/jtbdtools/reference/create_persona_table.md),
[`jtbd_cluster()`](https://charlesrogers.github.io/jtbdtools/reference/jtbd_cluster.md),
[`jtbd_cluster_profile()`](https://charlesrogers.github.io/jtbdtools/reference/jtbd_cluster_profile.md),
[`jtbd_feature_matrix()`](https://charlesrogers.github.io/jtbdtools/reference/jtbd_feature_matrix.md),
[`jtbd_pca()`](https://charlesrogers.github.io/jtbdtools/reference/jtbd_pca.md),
[`jtbd_profile_segments()`](https://charlesrogers.github.io/jtbdtools/reference/jtbd_profile_segments.md),
[`jtbd_segment()`](https://charlesrogers.github.io/jtbdtools/reference/jtbd_segment.md),
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
k_eval <- jtbd_find_k(jtbd_sample, max_k = 5)
#> Feature matrix: 200 respondents x 12 objectives (opportunity scores 1-9).
#> Kaiser rule: retaining 5 components (eigenvalue > 1).
#> Best k by silhouette: 2 (avg silhouette = 0.162)
k_eval
#>   k   wcss avg_silhouette
#> 1 2 1060.8          0.162
#> 2 3  924.9          0.157
#> 3 4  832.7          0.157
#> 4 5  756.1          0.160
```
