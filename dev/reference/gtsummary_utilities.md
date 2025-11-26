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

**N = 200**¹

Clinical situation at diagnosis

  

    T Stage

  

    T1

53 (27%)

    T2

54 (27%)

    T3

43 (22%)

    T4

50 (25%)

    Grade

  

    I

68 (34%)

    II

68 (34%)

    III

64 (32%)

    Age

47 (38, 57)

    Unknown

11

Treatment and outcome

  

    Chemotherapy Treatment

  

    Drug A

98 (49%)

    Drug B

102 (51%)

    Tumor Response

61 (32%)

    Unknown

7

    Patient Died

112 (56%)

¹ n (%); Median (Q1, Q3)

tbl \|\> bold_variable_group_headers() \|\>
[italicize_labels](https://www.danieldsjoberg.com/gtsummary/reference/bold_italicize_labels_levels.html)()
\|\> indent_levels(indent = 8L)

[TABLE]

\# }
