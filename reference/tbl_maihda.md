# Table summary of for MAIHDA analysis

**\[experimental\]**  
Helpers to generate formatted tables of a MAIHDA analysis as proposed by
Evans et al. (*SSM - Population Health* 2024,
[doi:10.1016/j.ssmph.2024.101664](https://doi.org/10.1016/j.ssmph.2024.101664)
). It relies on the
[MAIHDA](https://hdbt.github.io/MAIHDA/reference/MAIHDA-package.html)
package. This package being under active development, the proposed
functions here are experimental.

## Usage

``` r
tbl_maihda(
  x,
  ...,
  global_p = FALSE,
  twomodels_labels = c("Null model", "Adjusted model"),
  statistics_header = "Summary statistics",
  statistics_labels = list(bsv = "Between-stratum variance", bssd =
    "Between-stratum standard deviation", vpc = "Variance Partition Coefficient (VPC)",
    pcv = "Proportional Change in Variance (PCV)", auc =
    "Area Under Receiver Operating Characteristic Curve (AUC)", mor =
    "Median Odds Ratio (MOR)", csvpc = "Context share (VPC)"),
  statistics_include = -dplyr::any_of("bssd"),
  notes = TRUE,
  notes_labels = list(n_strata = "Strata:", nobs = "Observations:", engine = "Engine:",
    family = "Family:", context = "Variable(s) in context:")
)

tbl_partially_adjusted_maihda(
  x,
  ...,
  global_p = FALSE,
  twomodels_labels = c("Null model", "Fully adjusted model"),
  statistics_header = "Summary statistics",
  statistics_labels = list(bsv = "Between-stratum variance", bssd =
    "Between-stratum standard deviation", vpc = "Variance Partition Coefficient (VPC)",
    pcv = "Proportional Change in Variance (PCV)", auc =
    "Area Under Receiver Operating Characteristic Curve (AUC)", mor =
    "Median Odds Ratio (MOR)", csvpc = "Context share (VPC)"),
  statistics_include = -dplyr::any_of("bssd"),
  notes = TRUE,
  notes_labels = list(n_strata = "Strata:", nobs = "Observations:", engine = "Engine:",
    family = "Family:", context = "Variable(s) in context:"),
  return_data = FALSE
)

tbl_strata_info(
  x,
  breaks = c(10, 20, 30, 50, 100),
  column_labels = list(size = "Sample size per stratum", n = "Number of strata", prop =
    "Proportion of strata"),
  total_label = "Total number of strata:"
)

tbl_strata_predictions(
  x,
  n_strata = 5L,
  scale = c("response", "link"),
  which = c("null", "adjusted"),
  column_labels = list(rank = "Rank", n = "n", predicted = "Predicted", ci = "95% CI"),
  group_labels = list("highest", "lowest"),
  digits = 1L,
  return_data = FALSE
)

plot_maihda_predictions_by(
  x,
  by,
  scale = c("response", "link"),
  which = c("null", "adjusted"),
  sort = TRUE
)

glance_maihda_model(x)
```

## Arguments

- x:

  a MAIHDA object (`maihda_analysis` or `maihda_model`); for
  `tbl_maihda()` it could also be a list of `maihda_model` objects; for
  `tbl_partially_adjusted_maihda()`, only a `maihda_analysis` computed
  with `MAIHDA::maihda(decomposition = "two-model")` is allowed; for
  `tbl_strata_info()`, the result of
  [`MAIHDA::make_strata()`](https://hdbt.github.io/MAIHDA/reference/make_strata.html)
  is also accepted

- ...:

  additional parameters passed to
  [`gtsummary::tbl_regression()`](https://www.danieldsjoberg.com/gtsummary/reference/tbl_regression.html)

- global_p:

  display global p-value instead of terms p-value (see
  [`gtsummary::add_global_p()`](https://www.danieldsjoberg.com/gtsummary/reference/add_global_p.html)),
  not available if `engine = "wemix"`.

- twomodels_labels:

  for a two-model MAIHDA analysis, labels for the two models

- statistics_header:

  string header of the summary statistics

- statistics_labels:

  name list of labels for the summary statistics

- statistics_include:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>  
  names of summary statistics to be included: must be column names of
  the tibble returned by `glance_maihda_model()`

- notes:

  display some notes (number of strata, of observations, engine, model
  family) about the analysis?

- notes_labels:

  name list of labels for the notes

- return_data:

  return a data frame instead of a table

- breaks:

  breaks for sample size per stratum

- column_labels:

  named list of column labels

- total_label:

  string of the total label in the notes

- n_strata:

  number of strata to show at each end (top and bottom), use `Inf` or
  `NULL` to show all strata

- scale:

  Scale for the predicted stratum values: `"response"` (default) or
  `"link"`. For a cumulative (ordinal) model the response scale is the
  expected category score.

- which:

  For a two-model analysis, which model's predictions to rank the strata
  by: `"null"` (default) or `"adjusted"`. Ignored for a
  crossed-dimensions analysis or a single model.

- group_labels:

  labels for group names

- digits:

  number of decimals for predictions

- by:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>  
  list of variables to compare by

- sort:

  should the plot be sorted?

## Details

`tbl_maihda()` is intended to replicate Table 3 of Evans et al. 2024,
with fixed effects, between-stratum variance and model summary
statistics including VPC (variance partition coefficient) and PCV
(proportional change in variance). It accepts a `maihda_analysis` object
created with
[`MAIHDA::maihda()`](https://hdbt.github.io/MAIHDA/reference/maihda.html),
a single `maihda_model` created with
[`MAIHDA::fit_maihda()`](https://hdbt.github.io/MAIHDA/reference/fit_maihda.html)
or a list of several `maihfda_model` objects. For this last case, PCV
should be manually added to the models to be displayed (see examples).

`tbl_partially_adjusted_maihda()` is an helper allowing to compute and
display all partially adjusted models (see examples).

`tbl_strata_info()` is intended to replicate Table 2, showing the number
of strata having a certain sample size.

`tbl_strata_predictions()` is intended to replicate Table, showing the
strata with the highest and the lowest predicted value. If a
`maihda_analysis` object is passed to `tbl_strata_predictions()`, the
null model is taken into account by default for computing the predicted
values, following the behavior of
[`MAIHDA::maihda_table()`](https://hdbt.github.io/MAIHDA/reference/maihda_table.html).
It should be noted that in Evans et al. 2024, the authors used the
adjusted model, which could be done with the argument
`which = "adjusted"`.

`plot_maihda_predictions_by()` allows to visually compare predicted
values by strata according to one or several specific variable defining
the strata.

To be noted, themes from the
[gtsummary](https://www.danieldsjoberg.com/gtsummary/reference/theme_gtsummary.html)
package are taken into account for formatting the different values.

## Examples

``` r
# \donttest{
theme_gtsummary_bold_labels()

# gaussian model

data("maihda_health_data", package = "MAIHDA")
a <- MAIHDA::maihda(
  BMI ~ Age + Gender + Race + (1 | Gender:Race),
  data = maihda_health_data
)

a |> tbl_strata_info(breaks = c(50, 100, 150))


  

Sample size per stratum
```
