# K-Means clustering on JTBD opportunity data

Clusters respondents into groups with shared unmet needs using K-Means.
Optionally performs PCA first to reduce dimensionality (recommended for
surveys with many objectives).

## Usage

``` r
jtbd_cluster(
  df,
  n_clusters = 3,
  use_pca = TRUE,
  n_components = NULL,
  seed = 42
)
```

## Arguments

- df:

  A data frame with `imp__`/`sat__` columns

- n_clusters:

  Number of clusters (default: 3)

- use_pca:

  If TRUE, cluster on PCA scores instead of raw features (default: TRUE)

- n_components:

  Number of PCA components to use. If NULL, uses Kaiser rule.

- seed:

  Random seed for reproducibility (default: 42)

## Value

A list with:

- `cluster`: integer vector of cluster assignments

- `centers`: cluster centroids

- `n_clusters`: number of clusters

- `size`: cluster sizes

- `pca`: PCA result (if `use_pca = TRUE`)

- `feature_matrix`: the opportunity feature matrix

- `data`: original data with `jtbd_cluster` column appended

- `kmeans`: the raw kmeans object

## See also

Other clustering:
[`jtbd_cluster_profile()`](https://charlesrogers.github.io/jtbdtools/reference/jtbd_cluster_profile.md),
[`jtbd_feature_matrix()`](https://charlesrogers.github.io/jtbdtools/reference/jtbd_feature_matrix.md),
[`jtbd_find_k()`](https://charlesrogers.github.io/jtbdtools/reference/jtbd_find_k.md),
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
cl <- jtbd_cluster(jtbd_sample, n_clusters = 3)
#> Feature matrix: 200 respondents x 12 objectives (opportunity scores 1-9).
#> Kaiser rule: retaining 5 components (eigenvalue > 1).
#> ✔ Clustered 200 respondents into 3 segments.
#> ℹ Sizes: Segment_1 (n=60), Segment_2 (n=76), Segment_3 (n=64)
table(cl$cluster)
#> 
#>  1  2  3 
#> 60 76 64 

# Use with existing comparison functions
comparison <- get_jtbd_scores.comparison(cl$data, "jtbd_cluster")
#> Found 3 segments with n > 30.
```
