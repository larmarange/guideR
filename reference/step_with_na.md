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
#> Start:  AIC=1473.08
#> as.factor(Survived) ~ Class + Sex + Age + Group
#> 
#>         Df Deviance    AIC
#> - Group  1   1459.4 1471.4
#> <none>       1459.1 1473.1
#> - Age    1   1462.2 1474.2
#> - Class  3   1527.8 1535.8
#> - Sex    1   1704.4 1716.4
#> 
#> Step:  AIC=1471.42
#> as.factor(Survived) ~ Class + Sex + Age
#> 
#>         Df Deviance    AIC
#> <none>       1459.4 1471.4
#> - Age    1   1462.6 1472.6
#> - Class  3   1528.3 1534.3
#> - Sex    1   1704.8 1714.8
mod2
#> 
#> Call:  glm(formula = as.factor(Survived) ~ Class + Sex + Age, family = binomial(), 
#>     data = d)
#> 
#> Coefficients:
#> (Intercept)     Class2nd     Class3rd    ClassCrew      SexMale     AgeChild  
#>      2.0438      -1.0181      -1.7778      -0.8577      -2.4201       1.0615  
#> 
#> Degrees of Freedom: 2200 Total (i.e. Null);  2195 Residual
#> Null Deviance:       2769 
#> Residual Deviance: 2210  AIC: 2222

# \donttest{
## WITH SURVEY ---------------------------------------

library(survey)
#> Loading required package: grid
#> Loading required package: Matrix
#> Loading required package: survival
#> 
#> Attaching package: ‘survey’
#> The following object is masked from ‘package:graphics’:
#> 
#>     dotchart
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
