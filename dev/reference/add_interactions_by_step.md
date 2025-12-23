# Add potential relevant interactions using `step()`

**\[experimental\]** Add potential relevant interactions to a model
usind [`stats::step()`](https://rdrr.io/r/stats/step.html). The function
extract the formula of the model, identifies all potential interactions
and pass them as the **upper** component of the `scope` argument to
[`stats::step()`](https://rdrr.io/r/stats/step.html). The current model
formula is passed as the **lower** component of `scope`.

## Usage

``` r
add_interactions_by_step(model, ...)

# Default S3 method
add_interactions_by_step(model, ...)
```

## Arguments

- model:

  A model object.

- ...:

  Additional parameters passed to
  [`stats::step()`](https://rdrr.io/r/stats/step.html).

## Value

The stepwise-selected model.

## Examples

``` r
mod <- glm(as.factor(Survived) ~ ., data = titanic, family = binomial())
mod |> add_interactions_by_step()
#> Start:  AIC=2222.06
#> as.factor(Survived) ~ Class + Sex + Age
#> 
#>             Df Deviance    AIC
#> + Class:Sex  3   2143.4 2161.4
#> + Class:Age  2   2174.4 2190.4
#> + Sex:Age    1   2192.0 2206.0
#> <none>           2210.1 2222.1
#> 
#> Step:  AIC=2161.39
#> as.factor(Survived) ~ Class + Sex + Age + Class:Sex
#> 
#>             Df Deviance    AIC
#> + Class:Age  2   2099.2 2121.2
#> + Sex:Age    1   2134.8 2154.8
#> <none>           2143.4 2161.4
#> - Class:Sex  3   2210.1 2222.1
#> 
#> Step:  AIC=2121.18
#> as.factor(Survived) ~ Class + Sex + Age + Class:Sex + Class:Age
#> 
#>             Df Deviance    AIC
#> <none>           2099.2 2121.2
#> + Sex:Age    1   2097.5 2121.5
#> - Class:Age  2   2143.4 2161.4
#> - Class:Sex  3   2174.4 2190.4
#> 
#> Call:  glm(formula = as.factor(Survived) ~ Class + Sex + Age + Class:Sex + 
#>     Class:Age, family = binomial(), data = titanic)
#> 
#> Coefficients:
#>        (Intercept)            Class2nd            Class3rd           ClassCrew  
#>            3.55535            -1.73827            -3.77275            -1.65823  
#>            SexMale            AgeChild    Class2nd:SexMale    Class3rd:SexMale  
#>           -4.28298            16.85008             0.06801             2.89768  
#>  ClassCrew:SexMale   Class2nd:AgeChild   Class3rd:AgeChild  ClassCrew:AgeChild  
#>            1.13608             0.77411           -16.51217                  NA  
#> 
#> Degrees of Freedom: 2200 Total (i.e. Null);  2190 Residual
#> Null Deviance:       2769 
#> Residual Deviance: 2099  AIC: 2121
```
