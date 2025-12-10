# Plot a continuous variable by sub-groups

Plot one or several continuous variables by sub-groups. See
[`median_iqr()`](https://larmarange.github.io/guideR/reference/median_iqr.md)
for more details on the way statistics are computed. Return a box plot
(see examples).

## Usage

``` r
plot_continuous(
  data,
  outcome,
  by = NULL,
  drop_na_by = FALSE,
  convert_continuous = TRUE,
  ...,
  show_overall = TRUE,
  overall_label = "Overall",
  show_pvalues = TRUE,
  pvalues_labeller = scales::label_pvalue(add_p = TRUE),
  pvalues_size = 3.5,
  facet_labeller = ggplot2::label_wrap_gen(width = 50, multi_line = TRUE),
  flip = FALSE,
  minimal = FALSE,
  free_scale = FALSE,
  return_data = FALSE
)
```

## Arguments

- data:

  A data frame, data frame extension (e.g. a tibble), or a survey design
  object.

- outcome:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  List of continuous variables to be plotted.

- by:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  List of variables to group by (comparison is done separately for each
  variable).

- drop_na_by:

  Remove `NA` values in `by` variables?

- convert_continuous:

  Should continuous by variables (with 5 unique values or more) be
  converted to quartiles (using
  [`cut_quartiles()`](https://larmarange.github.io/guideR/reference/cut_quartiles.md))?

- ...:

  Additional arguments passed to
  [`ggplot2::geom_boxplot()`](https://ggplot2.tidyverse.org/reference/geom_boxplot.html).

- show_overall:

  Display "Overall" column?

- overall_label:

  Label for the overall column.

- show_pvalues:

  Display p-values in the top-left corner? p-values are computed with
  [`stats::kruskal.test()`](https://rdrr.io/r/stats/kruskal.test.html)
  for data frames, and with
  [`survey::svyranktest()`](https://rdrr.io/pkg/survey/man/svyranktest.html)
  for survey objects.

- pvalues_labeller:

  Labeller function for p-values.

- pvalues_size:

  Text size for p-values.

- facet_labeller:

  Labeller function for strip labels.

- flip:

  Flip x and y axis?

- minimal:

  Should a minimal theme be applied? (no y-axis, no grid)

- free_scale:

  Allow y axis to vary between conditions?

- return_data:

  Return computed data instead of the plot?

## Examples

``` r
iris |>
  plot_continuous(Petal.Length, by = Species)


iris |>
  plot_continuous(
    dplyr::starts_with("Petal"),
    by = Species,
    free_scale = TRUE,
    fill = "lightblue",
    outlier.color = "red"
  )


# \donttest{

mtcars |>
  plot_continuous(
    mpg,
    by = c(cyl, gear),
    flip = TRUE,
    mapping = ggplot2::aes(fill = by)
  )


# works with continuous by variables
mtcars |>
  plot_continuous(
    mpg,
    by = c(disp, drat),
    flip = TRUE,
    minimal = TRUE
  )


# works with survey object
iris |>
  srvyr::as_survey() |>
  plot_continuous(
    Petal.Length,
    by = c(Species, Petal.Width),
    flip = TRUE
  )

# }
```
