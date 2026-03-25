# Changelog

## jtbdtools 0.2.0

### Statistical Significance

- New
  [`test_segment_significance()`](https://charlesrogers.github.io/jtbdtools/reference/test_segment_significance.md):
  Wilcoxon rank-sum (Mann-Whitney U) test between segments on raw Likert
  responses
- [`get_jtbd_scores.comparison()`](https://charlesrogers.github.io/jtbdtools/reference/get_jtbd_scores.comparison.md)
  gains `test_sig` parameter: adds p-value columns per segment when TRUE
- Warns on small sample sizes (n \< 5)

### Qualtrics Import & Data Prep

- New
  [`prep_qualtrics()`](https://charlesrogers.github.io/jtbdtools/reference/prep_qualtrics.md):
  one-line import from Qualtrics CSV to analysis-ready data frame
- New
  [`read_qualtrics()`](https://charlesrogers.github.io/jtbdtools/reference/read_qualtrics.md):
  handles Qualtrics 3-row header format, strips metadata columns
- New
  [`detect_imp_sat()`](https://charlesrogers.github.io/jtbdtools/reference/detect_imp_sat.md):
  auto-detects importance/satisfaction columns from question text
  patterns
- New
  [`prep_survey()`](https://charlesrogers.github.io/jtbdtools/reference/prep_survey.md):
  universal data prep for any survey source (CSV, Google Forms, etc.)
- New
  [`validate_jtbd_data()`](https://charlesrogers.github.io/jtbdtools/reference/validate_jtbd_data.md):
  checks format before scoring with diagnostic messages
- Sample Qualtrics CSV included at `inst/extdata/sample_qualtrics.csv`

------------------------------------------------------------------------

## jtbdtools 0.1.0

Initial public release.

### Features

- Core ODI opportunity scoring:
  [`get_jtbd_scores()`](https://charlesrogers.github.io/jtbdtools/reference/get_jtbd_scores.md),
  [`calculate_opportunity_score()`](https://charlesrogers.github.io/jtbdtools/reference/calculate_opportunity_score.md)
- Segment comparison:
  [`get_jtbd_scores.comparison()`](https://charlesrogers.github.io/jtbdtools/reference/get_jtbd_scores.comparison.md),
  [`get_jtbd_scores.pairwise()`](https://charlesrogers.github.io/jtbdtools/reference/get_jtbd_scores.pairwise.md)
- Opportunity matrix visualization:
  [`plot_opportunity_matrix()`](https://charlesrogers.github.io/jtbdtools/reference/plot_opportunity_matrix.md)
- Cleveland (lollipop) comparison chart:
  [`plot_cleveland()`](https://charlesrogers.github.io/jtbdtools/reference/plot_cleveland.md)
- Publication-ready gt tables:
  [`theme.job_step()`](https://charlesrogers.github.io/jtbdtools/reference/theme.job_step.md),
  [`create.job_step.table()`](https://charlesrogers.github.io/jtbdtools/reference/create.job_step.table.md)
- SPSS data preparation pipeline:
  [`prep_data()`](https://charlesrogers.github.io/jtbdtools/reference/prep_data.md),
  [`build_imp_column_names()`](https://charlesrogers.github.io/jtbdtools/reference/build_imp_column_names.md)
- Bundled sample dataset: `jtbd_sample` (200 respondents, 12 objectives,
  3 segments)
- Custom ggplot2 theme:
  [`theme_jtbd()`](https://charlesrogers.github.io/jtbdtools/reference/theme_jtbd.md)

### Bug Fixes

- Fixed segment comparison returning identical scores for all segments
  (variable shadowing in filter)
- Removed hardcoded file paths from visualization functions
- Removed hardcoded filter thresholds (now parameterized)
