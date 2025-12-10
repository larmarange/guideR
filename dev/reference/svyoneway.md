# Test for Equal Means for survey design object

This function allows to compare several means using
[`survey::svyglm()`](https://rdrr.io/pkg/survey/man/svyglm.html). More
precisely, this is a wrapper for `survey::regTermTest(m, "group")` where
`m <- survey::svyglm(x ~ group, design)`.

## Usage

``` r
svyoneway(formula, design, ...)
```

## Arguments

- formula:

  a formula of the form `lhs ~ rhs` where `lhs` gives the sample values
  and `rhs` the corresponding groups

- design:

  a survey design object

- ...:

  additional parameters passed to
  [`survey::regTermTest()`](https://rdrr.io/pkg/survey/man/regTermTest.html)

## Value

an object of class `"htest"`

## See also

[`stats::oneway.test()`](https://rdrr.io/r/stats/oneway.test.html) for
classic data frames

## Examples

``` r
svyoneway(
  Petal.Length ~ Species,
  design = srvyr::as_survey(iris)
)
#> 
#>  Design-based one-way analysis of means
#> 
#> data:  Petal.Length and Species
#> F = 1868.8, df = 2, ddf = 147, p-value < 2.2e-16
#> 
```
