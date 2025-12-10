# Additional tests for `gtsummary`

See
[gtsummary::tests](https://www.danieldsjoberg.com/gtsummary/reference/tests.html)
for more details on how defining custom tests. `fisher.simulate.p()`
implements Fisher test with computation of p-values by Monte Carlo
simulation in larger than 2×2 tables (see
[`stats::fisher.test()`](https://rdrr.io/r/stats/fisher.test.html)).
`svyttest_oneway()` is designed to compare means between sub-groups for
survey objects. It is based on
[`survey::svyttest()`](https://rdrr.io/pkg/survey/man/svyttest.html) for
comparing 2 means, and on
[`svyoneway()`](https://larmarange.github.io/guideR/dev/reference/svyoneway.md)
for comparing 3 means or more.

## Usage

``` r
fisher.simulate.p(data, variable, by, ...)

svyttest_oneway(data, variable, by, ...)
```

## Arguments

- data:

  A data set.

- variable:

  Name of the variable to test.

- by:

  Name of the by variable.

- ...:

  Unused.

## Examples

``` r
library(gtsummary)
trial |>
  tbl_summary(include = grade, by = trt) |>
  add_p(test = all_categorical() ~ "fisher.simulate.p")


  

Characteristic
```

**Drug A**  
N = 98¹

**Drug B**  
N = 102¹

**p-value**²

Grade

  

  

\>0.9

    I

35 (36%)

33 (32%)

  

    II

32 (33%)

36 (35%)

  

    III

31 (32%)

33 (32%)

  

¹ n (%)

² Fisher’s Exact Test for Count Data with simulated p-value (based on
2000 replicates)

\# \donttest{ iris \|\>
srvyr::[as_survey](http://gdfe.co/srvyr/reference/as_survey.md)() \|\>
[tbl_svysummary](https://www.danieldsjoberg.com/gtsummary/reference/tbl_svysummary.html)(
include = Petal.Length, by = Species ) \|\>
[add_p](https://www.danieldsjoberg.com/gtsummary/reference/add_p.html)(test
=
[all_continuous](https://www.danieldsjoberg.com/gtsummary/reference/select_helpers.html)()
~ svyttest_oneway)

[TABLE]

\# }
