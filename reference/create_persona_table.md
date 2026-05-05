# Create segment persona cards as a gt table

Generates a publication-ready "This / Not That" table showing the
defining characteristics of each discovered segment. Over-indexed
attributes (index= 120) are listed as "More Likely", under-indexed
(index \<= 80) as "Less Likely".

## Usage

``` r
create_persona_table(
  profile_result,
  cluster_profile = NULL,
  n = NULL,
  study = NULL
)
```

## Arguments

- profile_result:

  Result from
  [`jtbd_profile_segments()`](https://charlesrogers.github.io/jtbdtools/reference/jtbd_profile_segments.md)

- cluster_profile:

  Optional result from
  [`jtbd_cluster_profile()`](https://charlesrogers.github.io/jtbdtools/reference/jtbd_cluster_profile.md)
  to include top opportunity scores in the persona

- n, study:

  Sample size and study label rendered as a footer (see
  [`jtbd_footer()`](https://charlesrogers.github.io/jtbdtools/reference/jtbd_footer.md)).

## Value

A gt table object

## See also

Other clustering:
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
[`plot_segment_radar()`](https://charlesrogers.github.io/jtbdtools/reference/plot_segment_radar.md),
[`plot_segment_radar_facet()`](https://charlesrogers.github.io/jtbdtools/reference/plot_segment_radar_facet.md)

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
opp_profile <- jtbd_cluster_profile(jtbd_sample, cl, test_sig = FALSE)
#> Found 3 segments with n > 30.
create_persona_table(prof, opp_profile)


  








Outcome-Based Segment Personas
```
