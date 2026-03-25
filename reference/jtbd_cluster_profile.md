# Profile discovered clusters using T2B scoring

Calculates top-2-box opportunity scores per cluster using the existing
JTBD scoring pipeline, with statistical significance testing.

## Usage

``` r
jtbd_cluster_profile(df, cluster_result, test_sig = TRUE)
```

## Arguments

- df:

  The original data frame

- cluster_result:

  Result from
  [`jtbd_cluster()`](https://charlesrogers.github.io/jtbdtools/reference/jtbd_cluster.md)

- test_sig:

  Run significance tests (default: TRUE)

## Value

A data frame from
[`get_jtbd_scores.comparison()`](https://charlesrogers.github.io/jtbdtools/reference/get_jtbd_scores.comparison.md)
with scores per cluster

## See also

Other clustering:
[`jtbd_cluster()`](https://charlesrogers.github.io/jtbdtools/reference/jtbd_cluster.md),
[`jtbd_feature_matrix()`](https://charlesrogers.github.io/jtbdtools/reference/jtbd_feature_matrix.md),
[`jtbd_find_k()`](https://charlesrogers.github.io/jtbdtools/reference/jtbd_find_k.md),
[`jtbd_pca()`](https://charlesrogers.github.io/jtbdtools/reference/jtbd_pca.md),
[`jtbd_segment()`](https://charlesrogers.github.io/jtbdtools/reference/jtbd_segment.md),
[`plot_cluster_heatmap()`](https://charlesrogers.github.io/jtbdtools/reference/plot_cluster_heatmap.md),
[`plot_elbow()`](https://charlesrogers.github.io/jtbdtools/reference/plot_elbow.md),
[`plot_pca_biplot()`](https://charlesrogers.github.io/jtbdtools/reference/plot_pca_biplot.md),
[`plot_pca_loadings()`](https://charlesrogers.github.io/jtbdtools/reference/plot_pca_loadings.md),
[`plot_pca_scree()`](https://charlesrogers.github.io/jtbdtools/reference/plot_pca_scree.md)

## Examples

``` r
data(jtbd_sample)
cl <- jtbd_cluster(jtbd_sample, n_clusters = 3)
#> Feature matrix: 200 respondents x 12 objectives (opportunity scores 1-9).
#> Kaiser rule: retaining 5 components (eigenvalue > 1).
#> ✔ Clustered 200 respondents into 3 segments.
#> ℹ Sizes: Segment_1 (n=65), Segment_2 (n=63), Segment_3 (n=72)
profile <- jtbd_cluster_profile(jtbd_sample, cl)
#> Found 3 segments with n > 30.
head(profile)
#> # A tibble: 6 × 34
#>   job_step    objective           imp.all sat.all opp.all rank.all opp_index.all
#>   <chr>       <chr>                 <dbl>   <dbl>   <dbl>    <dbl>         <dbl>
#> 1 researching minimize_time_to_e…    8.5     2.2     14.8        1          1.64
#> 2 purchasing  minimize_likelihoo…    7.6     1.5     13.7        2          1.51
#> 3 purchasing  minimize_time_to_r…    8.4     4.2     12.6        3          1.39
#> 4 onboarding  minimize_likelihoo…    7.35    2.1     12.6        4          1.39
#> 5 researching minimize_time_to_u…    8.3     6.05    10.6        5          1.17
#> 6 researching minimize_time_to_f…    7.9     6.7      9.1        6          1.01
#> # ℹ 27 more variables: imp.Segment_1 <dbl>, sat.Segment_1 <dbl>,
#> #   opp.Segment_1 <dbl>, rank.Segment_1 <dbl>, opp_index.Segment_1 <dbl>,
#> #   p.imp.Segment_1 <dbl>, p.sat.Segment_1 <dbl>, sig.imp.Segment_1 <lgl>,
#> #   sig.sat.Segment_1 <lgl>, imp.Segment_2 <dbl>, sat.Segment_2 <dbl>,
#> #   opp.Segment_2 <dbl>, rank.Segment_2 <dbl>, opp_index.Segment_2 <dbl>,
#> #   p.imp.Segment_2 <dbl>, p.sat.Segment_2 <dbl>, sig.imp.Segment_2 <lgl>,
#> #   sig.sat.Segment_2 <lgl>, imp.Segment_3 <dbl>, sat.Segment_3 <dbl>, …
```
