# Package index

## Data manipulation

- [`combine_answers()`](https://larmarange.github.io/guideR/dev/reference/combine_answers.md)
  : Combine answers of a multiple answers question
- [`cut_quartiles()`](https://larmarange.github.io/guideR/dev/reference/cut_quartiles.md)
  : Cut a continuous variable in quartiles
- [`long_to_periods()`](https://larmarange.github.io/guideR/dev/reference/long_to_periods.md)
  : Transform a data frame from long format to period format
- [`long_to_seq()`](https://larmarange.github.io/guideR/dev/reference/long_to_seq.md)
  : Transform a data frame from long format to a sequence obect
- [`periods_to_long()`](https://larmarange.github.io/guideR/dev/reference/periods_to_long.md)
  : Transform a data frame from period format to long format
- [`unrowwise()`](https://larmarange.github.io/guideR/dev/reference/unrowwise.md)
  : Remove row-wise grouping

## Descriptive statistics

- [`mean_sd()`](https://larmarange.github.io/guideR/dev/reference/mean_sd.md)
  : Compute means, standard deviations and confidence intervals by
  sub-groups
- [`median_iqr()`](https://larmarange.github.io/guideR/dev/reference/median_iqr.md)
  : Compute median, quartiles and interquartile range by sub-groups
- [`proportion()`](https://larmarange.github.io/guideR/dev/reference/proportion.md)
  : Compute proportions
- [`round_preserve_sum()`](https://larmarange.github.io/guideR/dev/reference/round_preserve_sum.md)
  : Round values while preserve their rounded sum in R

## Statistical tests

- [`fisher.simulate.p()`](https://larmarange.github.io/guideR/dev/reference/gtsummary_test.md)
  [`svyttest_oneway()`](https://larmarange.github.io/guideR/dev/reference/gtsummary_test.md)
  :

  Additional tests for `gtsummary`

- [`svyoneway()`](https://larmarange.github.io/guideR/dev/reference/svyoneway.md)
  : Test for Equal Means for survey design object

## Statistical plots

- [`plot_categorical()`](https://larmarange.github.io/guideR/dev/reference/plot_categorical.md)
  : Plot a categorical variable by sub-groups
- [`plot_continuous()`](https://larmarange.github.io/guideR/dev/reference/plot_continuous.md)
  : Plot a continuous variable by sub-groups
- [`plot_means()`](https://larmarange.github.io/guideR/dev/reference/plot_means.md)
  : Plot means by sub-groups
- [`plot_multiple_answers()`](https://larmarange.github.io/guideR/dev/reference/plot_multiple_answers.md)
  [`plot_multiple_answers_dodge()`](https://larmarange.github.io/guideR/dev/reference/plot_multiple_answers.md)
  : Plot a multiple answers question
- [`plot_proportions()`](https://larmarange.github.io/guideR/dev/reference/plot_proportions.md)
  [`stratified_by()`](https://larmarange.github.io/guideR/dev/reference/plot_proportions.md)
  [`dummy_proportions()`](https://larmarange.github.io/guideR/dev/reference/plot_proportions.md)
  : Plot proportions by sub-groups
- [`plot_trajectories()`](https://larmarange.github.io/guideR/dev/reference/plot_trajectories.md)
  [`plot_periods()`](https://larmarange.github.io/guideR/dev/reference/plot_trajectories.md)
  : Plot trajectories
- [`safe_pal()`](https://larmarange.github.io/guideR/dev/reference/safe_pal.md)
  [`scale_fill_safe()`](https://larmarange.github.io/guideR/dev/reference/safe_pal.md)
  [`scale_colour_safe()`](https://larmarange.github.io/guideR/dev/reference/safe_pal.md)
  [`scale_color_safe()`](https://larmarange.github.io/guideR/dev/reference/safe_pal.md)
  : A safe discrete colour palette

## Models

- [`add_interactions_by_step()`](https://larmarange.github.io/guideR/dev/reference/add_interactions_by_step.md)
  **\[experimental\]** :

  Add potential relevant interactions using
  [`step()`](https://rdrr.io/r/stats/step.html)

- [`grouped_tbl_pivot_wider()`](https://larmarange.github.io/guideR/dev/reference/grouped_tbl_pivot_wider.md)
  [`multinom_add_global_p_pivot_wider()`](https://larmarange.github.io/guideR/dev/reference/grouped_tbl_pivot_wider.md)
  [`style_grouped_tbl()`](https://larmarange.github.io/guideR/dev/reference/grouped_tbl_pivot_wider.md)
  :

  Helpers for grouped tables generated with `gtsummary`

- [`observed_vs_theoretical()`](https://larmarange.github.io/guideR/dev/reference/observed_vs_theoretical.md)
  : Plot observed vs predicted distribution of a fitted model

- [`step_with_na()`](https://larmarange.github.io/guideR/dev/reference/step_with_na.md)
  :

  Apply [`step()`](https://rdrr.io/r/stats/step.html), taking into
  account missing values

## Classification tree

- [`plot_inertia_from_tree()`](https://larmarange.github.io/guideR/dev/reference/plot_inertia_from_tree.md)
  [`get_inertia_from_tree()`](https://larmarange.github.io/guideR/dev/reference/plot_inertia_from_tree.md)
  : Plot inertia, absolute loss and relative loss from a classification
  tree

## Logical operators

- [`is_different()`](https://larmarange.github.io/guideR/dev/reference/is_different.md)
  [`is_equal()`](https://larmarange.github.io/guideR/dev/reference/is_different.md)
  [`cumdifferent()`](https://larmarange.github.io/guideR/dev/reference/is_different.md)
  [`num_cycle()`](https://larmarange.github.io/guideR/dev/reference/is_different.md)
  :

  Comparison tests considering `NA` as values to be compared

## Utilities

- [`theme_gtsummary_prop_n()`](https://larmarange.github.io/guideR/dev/reference/gtsummary_themes.md)
  [`theme_gtsummary_fisher_simulate_p()`](https://larmarange.github.io/guideR/dev/reference/gtsummary_themes.md)
  [`theme_gtsummary_unweighted_n()`](https://larmarange.github.io/guideR/dev/reference/gtsummary_themes.md)
  [`theme_gtsummary_bold_labels()`](https://larmarange.github.io/guideR/dev/reference/gtsummary_themes.md)
  :

  Themes for `gtsummary`

- [`bold_variable_group_headers()`](https://larmarange.github.io/guideR/dev/reference/gtsummary_utilities.md)
  [`italicize_variable_group_headers()`](https://larmarange.github.io/guideR/dev/reference/gtsummary_utilities.md)
  [`indent_levels()`](https://larmarange.github.io/guideR/dev/reference/gtsummary_utilities.md)
  [`indent_labels()`](https://larmarange.github.io/guideR/dev/reference/gtsummary_utilities.md)
  :

  Utilities for `gtsummary`

- [`install_dependencies()`](https://larmarange.github.io/guideR/dev/reference/install_dependencies.md)
  : Install / Update project dependencies

- [`leading_zeros()`](https://larmarange.github.io/guideR/dev/reference/leading_zeros.md)
  : Add leading zeros

- [`view_dictionary()`](https://larmarange.github.io/guideR/dev/reference/view_dictionary.md)
  [`view_detailed_dictionary()`](https://larmarange.github.io/guideR/dev/reference/view_dictionary.md)
  [`to_DT()`](https://larmarange.github.io/guideR/dev/reference/view_dictionary.md)
  : Display the variable dictionary of a data frame in the RStudio
  viewer

## Datasets

- [`titanic`](https://larmarange.github.io/guideR/dev/reference/titanic.md)
  : Titanic data set in long format
