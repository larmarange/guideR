# Utilities for `gtsummary`

Utilities for tables generated with
[gtsummary](https://www.danieldsjoberg.com/gtsummary/reference/gtsummary-package.html).

## Usage

``` r
bold_variable_group_headers(x)

italicize_variable_group_headers(x)

indent_levels(x, indent = 8L)

indent_labels(x, indent = 4L)
```

## Arguments

- x:

  A `gtsummary` object.

- indent:

  An integer indicating how many space to indent text.

## See also

[`gtsummary::modify_bold()`](https://www.danieldsjoberg.com/gtsummary/reference/modify_bold_italic.html),
[`gtsummary::modify_italic()`](https://www.danieldsjoberg.com/gtsummary/reference/modify_bold_italic.html),
[`gtsummary::modify_indent()`](https://www.danieldsjoberg.com/gtsummary/reference/modify_indent.html)

## Examples

``` r
# \donttest{
library(gtsummary)
tbl <-
  trial |>
  tbl_summary(
    include = c(stage, grade, age, trt, response, death)
  ) |>
  add_variable_group_header(
    header = "Clinical situation at diagnosis",
    variables = c(stage, grade, age)
  ) |>
  add_variable_group_header(
    header = "Treatment and outcome",
    variables = c(trt, response, death)
  )
tbl


  

Characteristic
```
