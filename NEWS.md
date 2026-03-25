# jtbdtools 0.2.0

## Statistical Significance

- New `test_segment_significance()`: Wilcoxon rank-sum (Mann-Whitney U) test between segments on raw Likert responses
- `get_jtbd_scores.comparison()` gains `test_sig` parameter: adds p-value columns per segment when TRUE
- Warns on small sample sizes (n < 5)

## Qualtrics Import & Data Prep

- New `prep_qualtrics()`: one-line import from Qualtrics CSV to analysis-ready data frame
- New `read_qualtrics()`: handles Qualtrics 3-row header format, strips metadata columns
- New `detect_imp_sat()`: auto-detects importance/satisfaction columns from question text patterns
- New `prep_survey()`: universal data prep for any survey source (CSV, Google Forms, etc.)
- New `validate_jtbd_data()`: checks format before scoring with diagnostic messages
- Sample Qualtrics CSV included at `inst/extdata/sample_qualtrics.csv`

---

# jtbdtools 0.1.0

Initial public release.

## Features

- Core ODI opportunity scoring: `get_jtbd_scores()`, `calculate_opportunity_score()`
- Segment comparison: `get_jtbd_scores.comparison()`, `get_jtbd_scores.pairwise()`
- Opportunity matrix visualization: `plot_opportunity_matrix()`
- Cleveland (lollipop) comparison chart: `plot_cleveland()`
- Publication-ready gt tables: `theme.job_step()`, `create.job_step.table()`
- SPSS data preparation pipeline: `prep_data()`, `build_imp_column_names()`
- Bundled sample dataset: `jtbd_sample` (200 respondents, 12 objectives, 3 segments)
- Custom ggplot2 theme: `theme_jtbd()`

## Bug Fixes

- Fixed segment comparison returning identical scores for all segments (variable shadowing in filter)
- Removed hardcoded file paths from visualization functions
- Removed hardcoded filter thresholds (now parameterized)
