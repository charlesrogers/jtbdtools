# Changelog

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
