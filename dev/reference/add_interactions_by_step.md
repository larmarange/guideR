# Add potential relevant interactions using `step()`

**\[experimental\]** Add potential relevant interactions to a model
using [`stats::step()`](https://rdrr.io/r/stats/step.html). The function
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
#> Start:  AIC=1057.36
#> as.factor(Survived) ~ Class + Sex + Age + n
#> 
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#>             Df Deviance     AIC
#> + Class:Sex  3    656.0   676.0
#> + Age:n      1    918.9   934.9
#> + Class:n    3    985.9  1005.9
#> + Class:Age  2   1011.1  1029.1
#> + Sex:Age    1   1035.7  1051.7
#> <none>           1043.4  1057.4
#> + Sex:n      1  11173.5 11189.5
#> 
#> Step:  AIC=676.04
#> as.factor(Survived) ~ Class + Sex + Age + n + Class:Sex
#> 
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#>             Df Deviance     AIC
#> + Age:n      1   519.87  541.87
#> + Class:Age  2   625.89  649.89
#> + Sex:n      1   628.29  650.29
#> + Sex:Age    1   649.10  671.10
#> + Class:n    3   645.19  671.19
#> <none>           656.04  676.04
#> - Class:Sex  3  1043.36 1057.36
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> 
#> Step:  AIC=541.87
#> as.factor(Survived) ~ Class + Sex + Age + n + Class:Sex + Age:n
#> 
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#>             Df Deviance    AIC
#> + Class:n    3   313.61 341.61
#> + Sex:n      1   321.56 345.56
#> <none>           519.87 541.87
#> + Sex:Age    1   519.87 543.87
#> + Class:Age  2   519.87 545.87
#> - Age:n      1   656.04 676.04
#> - Class:Sex  3   918.87 934.87
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> 
#> Step:  AIC=341.61
#> as.factor(Survived) ~ Class + Sex + Age + n + Class:Sex + Age:n + 
#>     Class:n
#> 
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#>             Df Deviance    AIC
#> + Sex:n      1     0.00  30.00
#> <none>           313.61 341.61
#> + Sex:Age    1   313.61 343.61
#> + Class:Age  2   313.61 345.61
#> - Class:Sex  3   412.88 434.88
#> - Class:n    3   519.87 541.87
#> - Age:n      1   645.19 671.19
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> 
#> Step:  AIC=30
#> as.factor(Survived) ~ Class + Sex + Age + n + Class:Sex + Age:n + 
#>     Class:n + Sex:n
#> 
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#>               Df Deviance     AIC
#> <none>                0.0    30.0
#> + Sex:Age      1      0.0    32.0
#> + Class:Sex:n  3      0.0    36.0
#> - Sex:n        1    313.6   341.6
#> - Class:n      3    321.6   345.6
#> - Age:n        1    597.2   625.2
#> + Class:Age    2   6704.1  6738.1
#> - Class:Sex    3  16003.4 16027.4
#> 
#> Call:  glm(formula = as.factor(Survived) ~ Class + Sex + Age + n + Class:Sex + 
#>     Age:n + Class:n + Sex:n, family = binomial(), data = titanic)
#> 
#> Coefficients:
#>       (Intercept)           Class2nd           Class3rd          ClassCrew  
#>         -181.2250           140.1750           454.0838           156.7432  
#>           SexMale           AgeChild                  n   Class2nd:SexMale  
#>          253.6527          1245.4309             1.4833           -74.8003  
#>  Class3rd:SexMale  ClassCrew:SexMale         AgeChild:n         Class2nd:n  
#>         1620.2928          -183.5547           -92.0696            -0.2506  
#>        Class3rd:n        ClassCrew:n          SexMale:n  
#>           -4.7910             0.7243            -2.3143  
#> 
#> Degrees of Freedom: 2200 Total (i.e. Null);  2186 Residual
#> Null Deviance:       2769 
#> Residual Deviance: 3.9e-07   AIC: 30
```
