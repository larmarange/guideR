# Deprecated functions

Deprecated functions

## Usage

``` r
plot_maihda_predictions_by(
  x,
  by = NULL,
  scale = c("response", "link"),
  which = c("null", "adjusted"),
  sort = TRUE
)
```

## Arguments

- x:

  a MAIHDA object (`maihda_analysis` or `maihda_model`); for
  [`tbl_maihda()`](https://larmarange.github.io/guideR/reference/tbl_maihda.md)
  it could also be a list of `maihda_model` objects; for
  [`tbl_partially_adjusted_maihda()`](https://larmarange.github.io/guideR/reference/tbl_maihda.md),
  only a `maihda_analysis` computed with
  `MAIHDA::maihda(decomposition = "two-model")` is allowed; for
  [`tbl_strata_info()`](https://larmarange.github.io/guideR/reference/tbl_maihda.md),
  the result of
  [`MAIHDA::make_strata()`](https://hdbt.github.io/MAIHDA/reference/make_strata.html)
  is also accepted

- by:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>  
  list of variables to compare by

- scale:

  scale for the predicted stratum values: "response" (default), "link",
  or "random_effect" (random effect only on the link scale); for a
  cumulative (ordinal) model the response scale is the expected category
  score.

- which:

  For a two-model analysis, which model's predictions to rank the strata
  by: `"null"` (default) or `"adjusted"`. Ignored for a
  crossed-dimensions analysis or a single model.

- sort:

  should the plot be sorted?
