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
