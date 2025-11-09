# Plot observed vs predicted distribution of a fitted model

Plot observed vs predicted distribution of a fitted model

## Usage

``` r
observed_vs_theoretical(model)
```

## Arguments

- model:

  A statistical model.

## Value

A `ggplot2` plot.

## Details

Has been tested with [`stats::lm()`](https://rdrr.io/r/stats/lm.html)
and [`stats::glm()`](https://rdrr.io/r/stats/glm.html) models. It may
work with other types of models, but without any warranty.

## Examples

``` r
# a linear model
mod <- lm(Sepal.Length ~ Sepal.Width + Species, data = iris)
mod |> observed_vs_theoretical()


# a logistic regression
mod <- glm(
  as.factor(Survived) ~ Class + Sex,
  data = titanic,
  family = binomial()
)
mod |> observed_vs_theoretical()
```
