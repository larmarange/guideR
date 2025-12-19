# Changelog

## guideR (development version)

**Improvements**

- [`style_grouped_tbl()`](https://larmarange.github.io/guideR/dev/reference/grouped_tbl_pivot_wider.md)
  and
  [`grouped_tbl_pivot_wider()`](https://larmarange.github.io/guideR/dev/reference/grouped_tbl_pivot_wider.md)
  now accepts tables generated with
  [`gtsummary::tbl_stack()`](https://www.danieldsjoberg.com/gtsummary/reference/tbl_stack.html)

**Bug fix**

- bug fix in
  [`theme_gtsummary_unweighted_n()`](https://larmarange.github.io/guideR/dev/reference/gtsummary_themes.md)

## guideR 0.8.1

CRAN release: 2025-12-19

**Bug fix**

- bug fix in `theme_gtsummary_prop_n(mean_sd = FALSE)`
- bug fix in
  [`combine_answers()`](https://larmarange.github.io/guideR/dev/reference/combine_answers.md)
  when `data` is a `survey` object
  ([\#55](https://github.com/larmarange/guideR/issues/55))

**Improvements**

- [`plot_multiple_answers_dodge()`](https://larmarange.github.io/guideR/dev/reference/plot_multiple_answers.md)
  now uses
  [`safe_pal()`](https://larmarange.github.io/guideR/dev/reference/safe_pal.md)
  by default ([\#55](https://github.com/larmarange/guideR/issues/55))
- [`plot_trajectories()`](https://larmarange.github.io/guideR/dev/reference/plot_trajectories.md)
  now uses
  [`safe_pal()`](https://larmarange.github.io/guideR/dev/reference/safe_pal.md)
  by default ([\#56](https://github.com/larmarange/guideR/issues/56))

## guideR 0.8.0

CRAN release: 2025-12-10

**Minor breaking change**

- [`theme_gtsummary_prop_n()`](https://larmarange.github.io/guideR/dev/reference/gtsummary_themes.md)
  and
  [`theme_gtsummary_unweighted_n()`](https://larmarange.github.io/guideR/dev/reference/gtsummary_themes.md)
  have been updated following version 2.5.0 of `gtsummary`
  ([\#44](https://github.com/larmarange/guideR/issues/44))
- [`plot_proportions()`](https://larmarange.github.io/guideR/dev/reference/plot_proportions.md)
  has been reorganized (some column names have changed when calling
  `return_data = TRUE`)

**New features**

- new function
  [`plot_categorical()`](https://larmarange.github.io/guideR/dev/reference/plot_categorical.md)
  to plot a categorical variable by sub-groups using bar plots
  ([\#47](https://github.com/larmarange/guideR/issues/47))
- new function
  [`plot_continuous()`](https://larmarange.github.io/guideR/dev/reference/plot_continuous.md)
  to plot a continuous variable by sub-groups using bar boxplots
  ([\#47](https://github.com/larmarange/guideR/issues/47))
- new function
  [`plot_means()`](https://larmarange.github.io/guideR/dev/reference/plot_means.md)
  to compare means by sub-groups
- new method
  [`mean_sd()`](https://larmarange.github.io/guideR/dev/reference/mean_sd.md)
  to compute means, standard deviation and confidence intervals by
  sub-group
- new method
  [`median_iqr()`](https://larmarange.github.io/guideR/dev/reference/median_iqr.md)
  to compute medians, quartiles and interquartile ranges by sub-group
- new function
  [`svyoneway()`](https://larmarange.github.io/guideR/dev/reference/svyoneway.md)
  to test for equal means for survey design object
- new
  [`svyttest_oneway()`](https://larmarange.github.io/guideR/dev/reference/gtsummary_test.md)
  test for
  [`gtsummary::add_p.tbl_svysummary`](https://www.danieldsjoberg.com/gtsummary/reference/add_p.tbl_svysummary.html)
  using
  [`survey::svyttest()`](https://rdrr.io/pkg/survey/man/svyttest.html)
  for comparing 2 means and
  [`svyoneway()`](https://larmarange.github.io/guideR/dev/reference/svyoneway.md)
  for comparing 3 means or more
- new colour palette
  [`safe_pal()`](https://larmarange.github.io/guideR/dev/reference/safe_pal.md)
  and corresponding scales
  [`scale_fill_safe()`](https://larmarange.github.io/guideR/dev/reference/safe_pal.md)
  and
  [`scale_colour_safe()`](https://larmarange.github.io/guideR/dev/reference/safe_pal.md)
  for `ggplot2` ([\#49](https://github.com/larmarange/guideR/issues/49))
- new argument `minimal` for
  [`plot_proportions()`](https://larmarange.github.io/guideR/dev/reference/plot_proportions.md)
  ([\#51](https://github.com/larmarange/guideR/issues/51))
- new argument `drop_ba_by` for
  [`plot_multiple_answers()`](https://larmarange.github.io/guideR/dev/reference/plot_multiple_answers.md)
  and
  [`plot_multiple_answers_dodge()`](https://larmarange.github.io/guideR/dev/reference/plot_multiple_answers.md)

**Fix**

- bug fix in [`proportions()`](https://rdrr.io/r/base/proportions.html)
  when `.na.rm = TRUE`
  ([\#46](https://github.com/larmarange/guideR/issues/46))

## guideR 0.7.0

CRAN release: 2025-11-26

**New features**

- additional themes for `gtsummary`:
  [`theme_gtsummary_prop_n()`](https://larmarange.github.io/guideR/dev/reference/gtsummary_themes.md),
  [`theme_gtsummary_unweighted_n()`](https://larmarange.github.io/guideR/dev/reference/gtsummary_themes.md),
  [`theme_gtsummary_fisher_simulate_p()`](https://larmarange.github.io/guideR/dev/reference/gtsummary_themes.md)
  and
  [`theme_gtsummary_bold_labels()`](https://larmarange.github.io/guideR/dev/reference/gtsummary_themes.md)
  ([\#42](https://github.com/larmarange/guideR/issues/42))
- [`fisher.simulate.p()`](https://larmarange.github.io/guideR/dev/reference/gtsummary_test.md),
  an implementation of the Fisher test, with computation of p-values by
  Monte Carlo simulation in larger than 2×2 tables, to be used in
  [`gtsummary::add_p()`](https://www.danieldsjoberg.com/gtsummary/reference/add_p.html)
  ([\#41](https://github.com/larmarange/guideR/issues/41))

## guideR 0.6.0

CRAN release: 2025-11-08

**New features**

- new functions
  [`plot_trajectories()`](https://larmarange.github.io/guideR/dev/reference/plot_trajectories.md)
  and
  [`plot_periods()`](https://larmarange.github.io/guideR/dev/reference/plot_trajectories.md)
  to plot individual trajectories (similar to sequence index plots)
  ([\#35](https://github.com/larmarange/guideR/issues/35))
- new functions
  [`view_dictionary()`](https://larmarange.github.io/guideR/dev/reference/view_dictionary.md)
  and
  [`view_detailed_dictionary()`](https://larmarange.github.io/guideR/dev/reference/view_dictionary.md)
  to display the variable dictionary of a data frame, a tibble or a
  survey object ([\#33](https://github.com/larmarange/guideR/issues/33))
- [`view_dictionary()`](https://larmarange.github.io/guideR/dev/reference/view_dictionary.md)
  and
  [`view_detailed_dictionary()`](https://larmarange.github.io/guideR/dev/reference/view_dictionary.md)
  are also accessible through dedicated addins
  ([\#33](https://github.com/larmarange/guideR/issues/33))
- new utility
  [`to_DT()`](https://larmarange.github.io/guideR/dev/reference/view_dictionary.md)
  to convert the result of
  [`labelled::look_for()`](https://larmarange.github.io/labelled/reference/look_for.html)
  into a [`DT::datatable()`](https://rdrr.io/pkg/DT/man/datatable.html)
  ([\#33](https://github.com/larmarange/guideR/issues/33))
- new utilities for tables generated with `gtsummary`:
  [`bold_variable_group_headers()`](https://larmarange.github.io/guideR/dev/reference/gtsummary_utilities.md),
  [`italicize_variable_group_headers()`](https://larmarange.github.io/guideR/dev/reference/gtsummary_utilities.md),
  [`indent_labels()`](https://larmarange.github.io/guideR/dev/reference/gtsummary_utilities.md)
  and
  [`indent_levels()`](https://larmarange.github.io/guideR/dev/reference/gtsummary_utilities.md)
  ([\#34](https://github.com/larmarange/guideR/issues/34))

## guideR 0.5.1

CRAN release: 2025-10-21

**New feature**

- new function
  [`plot_multiple_answers_dodge()`](https://larmarange.github.io/guideR/dev/reference/plot_multiple_answers.md)
  ([\#31](https://github.com/larmarange/guideR/issues/31))

**Fix**

- fix in
  [`plot_multiple_answers()`](https://larmarange.github.io/guideR/dev/reference/plot_multiple_answers.md)
  with `NA` values when `drop_na = FALSE`
  ([\#31](https://github.com/larmarange/guideR/issues/31))

## guideR 0.5.0

CRAN release: 2025-10-19

**New features**

- new function
  [`plot_multiple_answers()`](https://larmarange.github.io/guideR/dev/reference/plot_multiple_answers.md)
  to plot a multiple answers question coded as several binary variables
  ([\#29](https://github.com/larmarange/guideR/issues/29))
- new helper
  [`combine_answers()`](https://larmarange.github.io/guideR/dev/reference/combine_answers.md)
  for multiple answers questions
  ([\#29](https://github.com/larmarange/guideR/issues/29))

## guideR 0.4.1

CRAN release: 2025-09-16

**New feature**

- new argument `dependencies` for
  [`install_dependencies()`](https://larmarange.github.io/guideR/dev/reference/install_dependencies.md)
  ([\#27](https://github.com/larmarange/guideR/issues/27))

## guideR 0.4.0

CRAN release: 2025-04-22

**New features**

- new helpers
  ([`grouped_tbl_pivot_wider()`](https://larmarange.github.io/guideR/dev/reference/grouped_tbl_pivot_wider.md),
  [`multinom_add_global_p_pivot_wider()`](https://larmarange.github.io/guideR/dev/reference/grouped_tbl_pivot_wider.md)
  and
  [`style_grouped_tbl()`](https://larmarange.github.io/guideR/dev/reference/grouped_tbl_pivot_wider.md))
  for grouped tables generated by
  [`gtsummary::tbl_regression()`](https://www.danieldsjoberg.com/gtsummary/reference/tbl_regression.html)
  for multinomial models, multi-components models or other grouped
  models ([\#3](https://github.com/larmarange/guideR/issues/3))
- [`dummy_proportions()`](https://larmarange.github.io/guideR/dev/reference/plot_proportions.md)
  helper for
  [`plot_proportions()`](https://larmarange.github.io/guideR/dev/reference/plot_proportions.md)
  ([\#21](https://github.com/larmarange/guideR/issues/21))
- new argument `free_scale` for
  [`plot_proportions()`](https://larmarange.github.io/guideR/dev/reference/plot_proportions.md)
  ([\#22](https://github.com/larmarange/guideR/issues/22))
- [`install_dependencies()`](https://larmarange.github.io/guideR/dev/reference/install_dependencies.md)
  now checks if dependencies are on CRAN/BIOC, installs those available
  and returns the list of packages not installed/updated.

## guideR 0.3.0

CRAN release: 2025-03-29

**New features**

- [`plot_proportions()`](https://larmarange.github.io/guideR/dev/reference/plot_proportions.md)
  now accepts several conditions
  ([\#18](https://github.com/larmarange/guideR/issues/18))
- [`stratified_by()`](https://larmarange.github.io/guideR/dev/reference/plot_proportions.md)
  helper for
  [`plot_proportions()`](https://larmarange.github.io/guideR/dev/reference/plot_proportions.md)
  ([\#19](https://github.com/larmarange/guideR/issues/19))
- new function
  [`cut_quartiles()`](https://larmarange.github.io/guideR/dev/reference/cut_quartiles.md)
  ([\#13](https://github.com/larmarange/guideR/issues/13))
- new argument `convert_continuous` for
  [`plot_proportions()`](https://larmarange.github.io/guideR/dev/reference/plot_proportions.md)
  ([\#14](https://github.com/larmarange/guideR/issues/14))
- new argument `.drop_by_na` for
  [`proportion()`](https://larmarange.github.io/guideR/dev/reference/proportion.md)
  ([\#12](https://github.com/larmarange/guideR/issues/12))
- new argument `drop_by_na` for
  [`plot_proportions()`](https://larmarange.github.io/guideR/dev/reference/plot_proportions.md)
  ([\#16](https://github.com/larmarange/guideR/issues/16))

**Bug fixes**

- fix in
  [`proportion()`](https://larmarange.github.io/guideR/dev/reference/proportion.md)
  when `.conf.int = TRUE` and some rows have no observation (N = 0)
  ([\#15](https://github.com/larmarange/guideR/issues/15))

## guideR 0.2.0

CRAN release: 2025-03-16

**New features**

- [`proportion()`](https://larmarange.github.io/guideR/dev/reference/proportion.md)
  could be applied to atomic vectors
  ([\#4](https://github.com/larmarange/guideR/issues/4))
- new function
  [`periods_to_long()`](https://larmarange.github.io/guideR/dev/reference/periods_to_long.md)
  ([\#7](https://github.com/larmarange/guideR/issues/7))
- new function
  [`plot_proportions()`](https://larmarange.github.io/guideR/dev/reference/plot_proportions.md)
  ([\#9](https://github.com/larmarange/guideR/issues/9))

## guideR 0.1.0

CRAN release: 2025-02-14

- initial CRAN submission.
