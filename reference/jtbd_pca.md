# Run PCA on JTBD opportunity data

Performs Principal Component Analysis on the respondent-level
opportunity feature matrix. Uses the Kaiser rule (eigenvalues \> 1) to
select the number of components if not specified.

## Usage

``` r
jtbd_pca(df, n_components = NULL)
```

## Arguments

- df:

  A data frame with `imp__`/`sat__` columns, or a pre-computed feature
  matrix

- n_components:

  Number of components to retain. If NULL, uses Kaiser rule.

## Value

A list with:

- `pca`: the `prcomp` object

- `n_components`: number of components retained

- `loadings`: loading matrix (objectives x components)

- `scores`: respondent PC scores matrix

- `variance_explained`: data frame with per-component and cumulative
  variance

- `eigenvalues`: eigenvalues for each component

## See also

Other clustering:
[`jtbd_cluster()`](https://charlesrogers.github.io/jtbdtools/reference/jtbd_cluster.md),
[`jtbd_cluster_profile()`](https://charlesrogers.github.io/jtbdtools/reference/jtbd_cluster_profile.md),
[`jtbd_feature_matrix()`](https://charlesrogers.github.io/jtbdtools/reference/jtbd_feature_matrix.md),
[`jtbd_find_k()`](https://charlesrogers.github.io/jtbdtools/reference/jtbd_find_k.md),
[`jtbd_segment()`](https://charlesrogers.github.io/jtbdtools/reference/jtbd_segment.md),
[`plot_cluster_heatmap()`](https://charlesrogers.github.io/jtbdtools/reference/plot_cluster_heatmap.md),
[`plot_elbow()`](https://charlesrogers.github.io/jtbdtools/reference/plot_elbow.md),
[`plot_pca_biplot()`](https://charlesrogers.github.io/jtbdtools/reference/plot_pca_biplot.md),
[`plot_pca_loadings()`](https://charlesrogers.github.io/jtbdtools/reference/plot_pca_loadings.md),
[`plot_pca_scree()`](https://charlesrogers.github.io/jtbdtools/reference/plot_pca_scree.md)

## Examples

``` r
data(jtbd_sample)
pca_result <- jtbd_pca(jtbd_sample)
#> Feature matrix: 200 respondents x 12 objectives (opportunity scores 1-9).
#> Kaiser rule: retaining 5 components (eigenvalue > 1).
pca_result$n_components
#> [1] 5
pca_result$variance_explained
#>    component eigenvalue variance_pct cumulative_pct
#> 1          1      1.754         14.6           14.6
#> 2          2      1.320         11.0           25.6
#> 3          3      1.200         10.0           35.6
#> 4          4      1.108          9.2           44.8
#> 5          5      1.081          9.0           53.9
#> 6          6      0.982          8.2           62.0
#> 7          7      0.927          7.7           69.8
#> 8          8      0.884          7.4           77.1
#> 9          9      0.792          6.6           83.7
#> 10        10      0.725          6.0           89.8
#> 11        11      0.663          5.5           95.3
#> 12        12      0.564          4.7          100.0
```
