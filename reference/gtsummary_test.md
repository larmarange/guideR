# Additional tests for `gtsummary`

See
[gtsummary::tests](https://www.danieldsjoberg.com/gtsummary/reference/tests.html)
for more details on how defining custom tests. `fisher.simulate.p()`
implements Fisher test with computation of p-values by Monte Carlo
simulation in larger than 2×2 tables (see
[`stats::fisher.test()`](https://rdrr.io/r/stats/fisher.test.html)).

## Usage

``` r
fisher.simulate.p(data, variable, by, ...)
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
