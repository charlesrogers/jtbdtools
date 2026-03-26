# Declare global variables used in NSE/tidyverse pipelines
# This avoids R CMD check NOTEs about "no visible binding for global variable"
utils::globalVariables(c(
  # Column names used in tidyverse pipelines
  "objective", "objective_name", "imp_sat", "imp_sat_score", "imp", "sat", "opp",
  "segment_name", "rank", "opp_index", "job_step", "f", "n", "user_rating",
  "imp_sat_sum", "total_sum", "col_suffix", "caseid", "imp_sat_string",
  "score", "measure", "segment", "seg.value", "opp.all", "rank.all",
  "rank.seg", "rank.delta", "rank.max", "range", "sig_level",
  "vs.max_all", "pct_max.all", "pct_max.seg", "pct_max.seg_value",
  "vs.ave_seg.obj", "rank.delta.abs", "pct_max.delta",
  "max.all", "max.segment", "max.seg_value",
  "max_value", "min_value", "max_value.pct_of_seg_val", "min_value.pct_of_seg_val",
  "range.pct_of_seg_val", "binary.linear", "p.value",
  "measure.max", "measure.min", "measure.diff", "diff.total",
  "seg.max", "seg.min", "seg.range", "score.normd",
  "frequency", "value", "name", "Index",
  "Importance", "Satisfaction", "Opportunity",
  "imp.all", "sat.all", "opp_index.all",
  "objective_string", "score_show",
  ".imp", ".sat", ".opp", ".high_opp",
  "count.linear.1", "count.linear.05", "count.outlier.sal",
  "sd", "count",
  ".", "opp_sum", "total_count", "reorder",
  "p.imp", "p.sat", "sig.imp", "sig.sat",
  "component", "eigenvalue", "variance_pct", "loading", "label",
  "PC1", "PC2", "Cluster", "wcss_norm", "avg_silhouette", "k",
  "variable", "p_value", "cramers_v", "significant", "effect",
  "pct", "overall_pct", "index", "cluster_label",
  "deviation", "attr_label", "max_dev", "direction"
))
