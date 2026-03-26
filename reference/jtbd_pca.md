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
[`create_persona_table()`](https://charlesrogers.github.io/jtbdtools/reference/create_persona_table.md),
[`jtbd_cluster()`](https://charlesrogers.github.io/jtbdtools/reference/jtbd_cluster.md),
[`jtbd_cluster_profile()`](https://charlesrogers.github.io/jtbdtools/reference/jtbd_cluster_profile.md),
[`jtbd_feature_matrix()`](https://charlesrogers.github.io/jtbdtools/reference/jtbd_feature_matrix.md),
[`jtbd_find_k()`](https://charlesrogers.github.io/jtbdtools/reference/jtbd_find_k.md),
[`jtbd_profile_segments()`](https://charlesrogers.github.io/jtbdtools/reference/jtbd_profile_segments.md),
[`jtbd_segment()`](https://charlesrogers.github.io/jtbdtools/reference/jtbd_segment.md),
[`plot_cluster_heatmap()`](https://charlesrogers.github.io/jtbdtools/reference/plot_cluster_heatmap.md),
[`plot_elbow()`](https://charlesrogers.github.io/jtbdtools/reference/plot_elbow.md),
[`plot_pca_biplot()`](https://charlesrogers.github.io/jtbdtools/reference/plot_pca_biplot.md),
[`plot_pca_loadings()`](https://charlesrogers.github.io/jtbdtools/reference/plot_pca_loadings.md),
[`plot_pca_scree()`](https://charlesrogers.github.io/jtbdtools/reference/plot_pca_scree.md),
[`plot_segment_dna()`](https://charlesrogers.github.io/jtbdtools/reference/plot_segment_dna.md),
[`plot_segment_fingerprint()`](https://charlesrogers.github.io/jtbdtools/reference/plot_segment_fingerprint.md),
[`plot_segment_index()`](https://charlesrogers.github.io/jtbdtools/reference/plot_segment_index.md),
[`plot_segment_profiles()`](https://charlesrogers.github.io/jtbdtools/reference/plot_segment_profiles.md),
[`plot_segment_radar()`](https://charlesrogers.github.io/jtbdtools/reference/plot_segment_radar.md),
[`plot_segment_radar_facet()`](https://charlesrogers.github.io/jtbdtools/reference/plot_segment_radar_facet.md)

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
#> 1          1      1.621         13.5           13.5
#> 2          2      1.336         11.1           24.6
#> 3          3      1.231         10.3           34.9
#> 4          4      1.163          9.7           44.6
#> 5          5      1.101          9.2           53.8
#> 6          6      0.991          8.3           62.0
#> 7          7      0.910          7.6           69.6
#> 8          8      0.880          7.3           76.9
#> 9          9      0.799          6.7           83.6
#> 10        10      0.716          6.0           89.6
#> 11        11      0.666          5.6           95.1
#> 12        12      0.586          4.9          100.0
```
