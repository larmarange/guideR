# Apply `step()`, taking into account missing values

When your data contains missing values, concerned observations are
removed from a model. However, then at a later stage, you try to apply a
descending stepwise approach to reduce your model by minimization of
AIC, you may encounter an error because the number of rows has changed.

## Usage

``` r
step_with_na(model, ...)

# Default S3 method
step_with_na(model, ..., full_data = eval(model$call$data))

# S3 method for class 'svyglm'
step_with_na(model, ..., design)
```

## Arguments

- model:

  A model object.

- ...:

  Additional parameters passed to
  [`stats::step()`](https://rdrr.io/r/stats/step.html).

- full_data:

  Full data frame used for the model, including missing data.

- design:

  Survey design previously passed to
  [`survey::svyglm()`](https://rdrr.io/pkg/survey/man/svyglm.html).

## Value

The stepwise-selected model.

## Details

`step_with_na()` applies the following strategy:

- recomputes the models using only complete cases;

- applies [`stats::step()`](https://rdrr.io/r/stats/step.html);

- recomputes the reduced model using the full original dataset.

`step_with_na()` has been tested with
[`stats::lm()`](https://rdrr.io/r/stats/lm.html),
[`stats::glm()`](https://rdrr.io/r/stats/glm.html),
[`nnet::multinom()`](https://rdrr.io/pkg/nnet/man/multinom.html),
[`survey::svyglm()`](https://rdrr.io/pkg/survey/man/svyglm.html) and
[`survival::coxph()`](https://rdrr.io/pkg/survival/man/coxph.html). It
may be working with other types of models, but with no warranty.

In some cases, it may be necessary to provide the full dataset initially
used to estimate the model.

`step_with_na()` may not work inside other functions. In that case, you
may try to pass `full_data` to the function.

## Examples

``` r
set.seed(42)
d <- titanic |>
  dplyr::mutate(
    Group = sample(
      c("a", "b", NA),
      dplyr::n(),
      replace = TRUE
    )
  )
mod <- glm(as.factor(Survived) ~ ., data = d, family = binomial())
# step(mod) should produce an error
mod2 <- step_with_na(mod, full_data = d)
#> Start:  AIC=712.54
#> as.factor(Survived) ~ Class + Sex + Age + n + Group
#> 
#>         Df Deviance     AIC
#> - Group  1   697.72  711.72
#> <none>       696.54  712.54
#> - Age    1   735.85  749.85
#> - Sex    1   765.88  779.88
#> - Class  3  1069.25 1079.25
#> - n      1  1459.08 1473.08
#> 
#> Step:  AIC=711.72
#> as.factor(Survived) ~ Class + Sex + Age + n
#> 
#>         Df Deviance     AIC
#> <none>       697.72  711.72
#> - Age    1   736.66  748.66
#> - Sex    1   767.24  779.24
#> - Class  3  1069.62 1077.62
#> - n      1  1459.42 1471.42
mod2
#> 
#> Call:  glm(formula = as.factor(Survived) ~ Class + Sex + Age + n, family = binomial(), 
#>     data = d)
#> 
#> Coefficients:
#> (Intercept)     Class2nd     Class3rd    ClassCrew      SexMale     AgeChild  
#>     4.78600     -1.08991     -1.55871      6.48346     -1.52340     -1.81226  
#>           n  
#>    -0.02924  
#> 
#> Degrees of Freedom: 2200 Total (i.e. Null);  2194 Residual
#> Null Deviance:       2769 
#> Residual Deviance: 1043  AIC: 1057

# \donttest{
## WITH SURVEY ---------------------------------------

library(survey)
ds <- d |>
  dplyr::mutate(Survived = as.factor(Survived)) |>
  srvyr::as_survey()
mods <- survey::svyglm(
  Survived ~ Class + Group + Sex,
  design = ds,
  family = quasibinomial()
)
mod2s <- step_with_na(mods, design = ds)
#> Start:  AIC=1471.56
#> Survived ~ Class + Group + Sex
#> 
#>         Df Deviance    AIC
#> - Group  1   1462.6 1469.9
#> <none>       1462.2 1471.6
#> - Class  3   1527.9 1530.3
#> - Sex    1   1712.6 1716.5
#> 
#> Step:  AIC=1469.94
#> Survived ~ Class + Sex
#> 
#>         Df Deviance    AIC
#> <none>       1462.6 1469.9
#> - Class  3   1528.4 1528.8
#> - Sex    1   1713.1 1714.8
mod2s
#> Independent Sampling design (with replacement)
#> Called via srvyr
#> Sampling variables:
#>   - ids: `1` 
#> 
#> Call:  svyglm(formula = Survived ~ Class + Sex, design = ds, family = quasibinomial())
#> 
#> Coefficients:
#> (Intercept)     Class2nd     Class3rd    ClassCrew      SexMale  
#>      2.0682      -0.9526      -1.6582      -0.8808      -2.4213  
#> 
#> Degrees of Freedom: 2200 Total (i.e. Null);  2196 Residual
#> Null Deviance:       2769 
#> Residual Deviance: 2229  AIC: NA
# }
```
