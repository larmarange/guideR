# Themes for `gtsummary`

Additional themes for tables generated with `gtsummary`.

## Usage

``` r
theme_gtsummary_prop_n(
  prop_stat = "{p}% ({n})",
  prop_digits = 1,
  mean_sd = FALSE,
  cont_digits = 1,
  set_theme = TRUE
)

theme_gtsummary_fisher_simulate_p(set_theme = TRUE)

theme_gtsummary_unweighted_n(
  n_unweighted_prefix = "",
  n_unweighted_suffix = " obs.",
  prop_digits = 1,
  mean_sd = FALSE,
  cont_digits = 1,
  overall_string = NULL,
  set_theme = TRUE
)

theme_gtsummary_bold_labels(set_theme = TRUE)
```

## Arguments

- prop_stat:

  (`character`)  
  Statistics to display for categorical variables (see
  [`gtsummary::tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/reference/tbl_summary.html)).

- prop_digits:

  (non-negative `integer`)  
  Define the number of decimals to display for proportions.

- mean_sd:

  (scalar `logical`)  
  Also, set default summary statistics to mean and standard deviation in
  [`gtsummary::tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/reference/tbl_summary.html).
  Default is `FALSE`.

- cont_digits:

  (non-negative `integer`)  
  Define the number of decimals to display for continuous variables.

- set_theme:

  (scalar `logical`)  
  Logical indicating whether to set the theme. Default is `TRUE`. When
  `FALSE` the named list of theme elements is returned invisibly

- n_unweighted_prefix, n_unweighted_suffix:

  (`character`)  
  Prefix and suffix displayed before and after the unweighted number of
  observations.

- overall_string:

  (`character`)  
  Optional string to name the *overall* column.

## Details

`theme_gtsummary_prop_n()` displays, by default, proportions before the
number of observations (between brackets). This function cannot be used
simultaneously with
[`gtsummary::theme_gtsummary_mean_sd()`](https://www.danieldsjoberg.com/gtsummary/reference/theme_gtsummary.html),
but you can use the `mean_sd = TRUE` option of
`theme_gtsummary_prop_n()`.

`theme_gtsummary_fisher_simulate_p()` modify the default test used for
categorical variables by Fisher test, with computation of p-values by
Monte Carlo simulation in larger than 2×2 tables.

`theme_gtsummary_unweighted_n()` modifies default values of tables
returned by
[`gtsummary::tbl_svysummary()`](https://www.danieldsjoberg.com/gtsummary/reference/tbl_svysummary.html)
and displays the unweighted number of observations instead of the
weighted n.

`theme_gtsummary_bold_labels()` applies automatically
[`gtsummary::bold_labels()`](https://www.danieldsjoberg.com/gtsummary/reference/bold_italicize_labels_levels.html)
to all tables generated with `gtsummary`.

## Examples

``` r
library(gtsummary)
trial |>
  tbl_summary(include = c(grade, age), by = trt) |>
  add_p()


  

Characteristic
```

**Drug A**  
N = 98¹

**Drug B**  
N = 102¹

**p-value**²

Grade

  

  

0.9

    I

35 (36%)

33 (32%)

  

    II

32 (33%)

36 (35%)

  

    III

31 (32%)

33 (32%)

  

Age

46 (37, 60)

48 (39, 56)

0.7

    Unknown

7

4

  

¹ n (%); Median (Q1, Q3)

² Pearson’s Chi-squared test; Wilcoxon rank sum test

theme_gtsummary_prop_n(mean_sd = TRUE)
theme_gtsummary_fisher_simulate_p() theme_gtsummary_bold_labels() trial
\|\>
[tbl_summary](https://www.danieldsjoberg.com/gtsummary/reference/tbl_summary.html)(include
= [c](https://rdrr.io/r/base/c.html)(grade, age), by = trt) \|\>
[add_p](https://www.danieldsjoberg.com/gtsummary/reference/add_p.html)()

[TABLE]

[data](https://rdrr.io/r/utils/data.html)("api", package = "survey")
apistrat\$both\[1:5\] \<- NA apistrat \|\>
srvyr::[as_survey](http://gdfe.co/srvyr/reference/as_survey.md)(strata =
stype, weights = pw) \|\>
[tbl_svysummary](https://www.danieldsjoberg.com/gtsummary/reference/tbl_svysummary.html)(include
= [c](https://rdrr.io/r/base/c.html)(stype, both), by = awards) \|\>
[add_overall](https://www.danieldsjoberg.com/gtsummary/reference/add_overall.html)()

[TABLE]

theme_gtsummary_unweighted_n() apistrat \|\>
srvyr::[as_survey](http://gdfe.co/srvyr/reference/as_survey.md)(strata =
stype, weights = pw) \|\>
[tbl_svysummary](https://www.danieldsjoberg.com/gtsummary/reference/tbl_svysummary.html)(include
= [c](https://rdrr.io/r/base/c.html)(stype, both), by = awards) \|\>
[add_overall](https://www.danieldsjoberg.com/gtsummary/reference/add_overall.html)()

[TABLE]

gtsummary::[reset_gtsummary_theme](https://www.danieldsjoberg.com/gtsummary/reference/set_gtsummary_theme.html)()
