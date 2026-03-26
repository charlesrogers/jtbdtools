# Segment radar chart (faceted)

One radar panel per segment, making each segment's "personality shape"
easy to read without overlap.

## Usage

``` r
plot_segment_radar_facet(
  profile_result,
  max_attrs = 8,
  title = "Segment Profiles: Individual Radar Views"
)
```

## Arguments

- profile_result:

  Result from
  [`jtbd_profile_segments()`](https://charlesrogers.github.io/jtbdtools/reference/jtbd_profile_segments.md)

- max_attrs:

  Maximum number of attributes on each radar (default: 8)

- title:

  Plot title

## Value

A ggplot object

## See also

Other clustering:
[`create_persona_table()`](https://charlesrogers.github.io/jtbdtools/reference/create_persona_table.md),
[`jtbd_cluster()`](https://charlesrogers.github.io/jtbdtools/reference/jtbd_cluster.md),
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
[`plot_segment_dna()`](https://charlesrogers.github.io/jtbdtools/reference/plot_segment_dna.md),
[`plot_segment_fingerprint()`](https://charlesrogers.github.io/jtbdtools/reference/plot_segment_fingerprint.md),
[`plot_segment_index()`](https://charlesrogers.github.io/jtbdtools/reference/plot_segment_index.md),
[`plot_segment_profiles()`](https://charlesrogers.github.io/jtbdtools/reference/plot_segment_profiles.md),
[`plot_segment_radar()`](https://charlesrogers.github.io/jtbdtools/reference/plot_segment_radar.md)

## Examples

``` r
data(jtbd_sample)
cl <- jtbd_cluster(jtbd_sample, n_clusters = 3)
#> Feature matrix: 200 respondents x 12 objectives (opportunity scores 1-9).
#> Kaiser rule: retaining 5 components (eigenvalue > 1).
#> ✔ Clustered 200 respondents into 3 segments.
#> ℹ Sizes: Segment_1 (n=60), Segment_2 (n=76), Segment_3 (n=64)
prof <- jtbd_profile_segments(cl$data)
#> ✔ Profiled 6 variables across 3 segments.
#> ℹ 2 variables significantly distinguish segments (p < 0.05).
#> ℹ Top distinguisher: segment, income
plot_segment_radar_facet(prof)
#> Error in slice_head(., n = 1): could not find function "slice_head"
```
