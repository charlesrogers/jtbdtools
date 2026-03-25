# Automatically profile discovered segments

For each profiling variable (demographics, behaviors, etc.), tests
whether the distribution differs significantly across clusters using
chi-squared tests. Returns a ranked summary showing which attributes
best distinguish each segment.

## Usage

``` r
jtbd_profile_segments(
  df,
  cluster_col = "jtbd_cluster",
  profile_cols = NULL,
  alpha = 0.05
)
```

## Arguments

- df:

  A data frame with a cluster column and profiling columns

- cluster_col:

  Name of the cluster column (default: "jtbd_cluster")

- profile_cols:

  Character vector of column names to profile. If NULL, auto-detects all
  non-imp/sat/cluster/caseid columns.

- alpha:

  Significance level (default: 0.05)

## Value

A list with:

- `summary`: tibble with one row per profiling variable — p-value,
  effect size (Cramer's V), significance flag

- `details`: list of per-variable breakdowns showing distribution per
  cluster

- `distinguishing`: the top distinguishing attributes ranked by effect
  size

## See also

Other clustering:
[`jtbd_cluster()`](https://charlesrogers.github.io/jtbdtools/reference/jtbd_cluster.md),
[`jtbd_cluster_profile()`](https://charlesrogers.github.io/jtbdtools/reference/jtbd_cluster_profile.md),
[`jtbd_feature_matrix()`](https://charlesrogers.github.io/jtbdtools/reference/jtbd_feature_matrix.md),
[`jtbd_find_k()`](https://charlesrogers.github.io/jtbdtools/reference/jtbd_find_k.md),
[`jtbd_pca()`](https://charlesrogers.github.io/jtbdtools/reference/jtbd_pca.md),
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
profile <- jtbd_profile_segments(cl$data)
#> ✔ Profiled 6 variables across 3 segments.
#> ℹ 2 variables significantly distinguish segments (p < 0.05).
#> ℹ Top distinguisher: segment, income
profile$summary
#> # A tibble: 6 × 5
#>   variable  p_value cramers_v significant effect    
#>   <chr>       <dbl>     <dbl> <lgl>       <chr>     
#> 1 segment    0          0.328 TRUE        medium    
#> 2 income     0.0186     0.214 TRUE        medium    
#> 3 tenure     0.0855     0.167 FALSE       small     
#> 4 education  0.143      0.155 FALSE       small     
#> 5 age_group  0.257      0.159 FALSE       small     
#> 6 gender     0.739      0.07  FALSE       negligible
profile$distinguishing
#> # A tibble: 2 × 5
#>   variable p_value cramers_v significant effect
#>   <chr>      <dbl>     <dbl> <lgl>       <chr> 
#> 1 segment   0          0.328 TRUE        medium
#> 2 income    0.0186     0.214 TRUE        medium
```
