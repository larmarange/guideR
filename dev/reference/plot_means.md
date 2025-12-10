# Plot means by sub-groups

Plot one or several means by sub-groups. See
[`mean_sd()`](https://larmarange.github.io/guideR/dev/reference/mean_sd.md)
for more details on the way means and confidence intervals are computed.
By default, return a point plot, but other geometries could be used (see
examples).

## Usage

``` r
plot_means(
  data,
  outcome,
  by = NULL,
  drop_na_by = FALSE,
  convert_continuous = TRUE,
  geom = "point",
  ...,
  show_overall = TRUE,
  overall_label = "Overall",
  show_ci = TRUE,
  conf_level = 0.95,
  ci_color = "black",
  show_pvalues = TRUE,
  pvalues_labeller = scales::label_pvalue(add_p = TRUE),
  pvalues_size = 3.5,
  show_labels = TRUE,
  label_y = NULL,
  labels_labeller = scales::label_number(0.1),
  labels_size = 3.5,
  labels_color = "black",
  show_overall_line = FALSE,
  overall_line_type = "dashed",
  overall_line_color = "black",
  overall_line_width = 0.5,
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
  [`cut_quartiles()`](https://larmarange.github.io/guideR/dev/reference/cut_quartiles.md))?

- geom:

  Geometry to use for plotting means (`"point"` by default).

- ...:

  Additional arguments passed to the geom defined by `geom`.

- show_overall:

  Display "Overall" column?

- overall_label:

  Label for the overall column.

- show_ci:

  Display confidence intervals?

- conf_level:

  Confidence level for the confidence intervals.

- ci_color:

  Color of the error bars representing confidence intervals.

- show_pvalues:

  Display p-values in the top-left corner? p-values are computed with
  [`stats::oneway.test()`](https://rdrr.io/r/stats/oneway.test.html) for
  data frames, and with
  [`survey::svyttest()`](https://rdrr.io/pkg/survey/man/svyttest.html)
  (2 groups) or
  [`svyoneway()`](https://larmarange.github.io/guideR/dev/reference/svyoneway.md)
  (3 groups or more) for survey objects.

- pvalues_labeller:

  Labeller function for p-values.

- pvalues_size:

  Text size for p-values.

- show_labels:

  Display mean labels?

- label_y:

  Y position of labels. If `NULL`, will be auto-determined.

- labels_labeller:

  Labeller function for labels.

- labels_size:

  Size of labels.

- labels_color:

  Color of labels.

- show_overall_line:

  Add an overall line?

- overall_line_type:

  Line type of the overall line.

- overall_line_color:

  Color of the overall line.

- overall_line_width:

  Line width of the overall line.

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
  plot_means(Petal.Length, by = Species)


iris |>
  plot_means(
    dplyr::starts_with("Petal"),
    by = Species,
    geom = "bar",
    fill = "lightblue",
    show_overall_line = TRUE
  )


# \donttest{

mtcars |>
  plot_means(
    mpg,
    by = c(cyl, gear),
    size = 3,
    colour = "plum",
    flip = TRUE
  )


# works with continuous by variables
mtcars |>
  plot_means(
    mpg,
    by = c(disp, drat),
    fill = "plum",
    geom = "bar",
    flip = TRUE,
    minimal = TRUE
  )


# works with survey object
iris |>
  srvyr::as_survey() |>
  plot_means(
    Petal.Length,
    by = c(Species, Petal.Width),
    label_y = -1,
    size = 3,
    mapping = ggplot2::aes(colour = by),
    flip = TRUE
  )

# }
```
