# Estimate the contribution of each variable of a model

**\[experimental\]**  
Use an analysis of variance or an analysis of deviance to estimate the
contribution of each variable in reducing variance or deviance. This
contribution could be explained as a proportion of the total deviance
(*total contribution*), of the residual deviance (*partial
contribution*) or of the explained deviance (*relative contribution*),
see details for more information. `contributions()` computes the
different contributions. `tbl_contributions()` displays the results as a
formatted [`gt`](https://gt.rstudio.com/reference/gt.html) table, taking
into account variable labels.

## Usage

``` r
contributions(mod, type = c("II", "III", "I", 1, 2, 3, "drop1", "add1"), ...)

tbl_contributions(
  mod,
  type = c("II", "III", "I", 1, 2, 3, "drop1", "add1"),
  ...,
  show = c("Total", "Partial", "Relative"),
  notes = TRUE
)

total_deviance(mod)
```

## Arguments

- mod:

  a statistical model

- type:

  type of Anova, Roman numerals being equivalent to the corresponding
  Arabic numerals:
  [`stats::anova()`](https://rdrr.io/r/stats/anova.html) will be used
  for type-I, Anova,
  [`car::Anova()`](https://rdrr.io/pkg/car/man/Anova.html) for types II
  and III; an alternative method, `"drop1"` uses
  [`stats::drop1()`](https://rdrr.io/r/stats/add1.html) to calculate the
  reduction of deviance associated with each predictor (in the absence
  of interactions, is equivalent to a type-II Anova); `"add1"` uses
  [`stats::add1()`](https://rdrr.io/r/stats/add1.html) to calculate the
  reduction of deviance associated with each predictor in an univariable
  model (model with just this predictor compared to the null model), in
  this scenario, partial and relative contribution are not defined.

- ...:

  additional parameters passed to
  [`stats::anova()`](https://rdrr.io/r/stats/anova.html),
  [`car::Anova()`](https://rdrr.io/pkg/car/man/Anova.html),
  [`stats::drop1()`](https://rdrr.io/r/stats/add1.html) or
  [`stats::add1()`](https://rdrr.io/r/stats/add1.html)

- show:

  list of contributions to display

- notes:

  should table notes be added?

## Details

In linear regression, the squared multiple correlation, R² is used to
assess goodness of fit as it represents the proportion of variance in
the criterion that is explained by the predictors. It could be expressed
as `1 - RSS / TSS` where `TSS` represents the total sum of squares (the
overall observed variance) and `RSS` represents the residual sum of
squares (i.e. the variance not explained by the full model).

For generalized linear models (GLM), model fitting does not rely on
ordinary least squares (OLS). It is achieved by maximum likelihood.
Therefore, sum of squares is not available and R-squared cannot be
computed. An alternative goodness of fit measure used in such case is
pseudo R². The McFadden pseudo R² (sometimes called likelihood ratio
index) could be expressed using deviance rather than likelihood. In such
case, it is equal to `1 - Dr / Dt` where `Dt` represents the full
deviance (i.e. the deviance of the null model, a model with no
predictor) and `Dr` the residual deviance (i.e. the deviance of the full
model). The McFadden pseudo R² corresponds the proportion of deviance
reduced by the model. Deviance is a generalization of the idea of using
the sum of squares of residuals in the cases where model-fitting is
achieved by maximum likelihood.

In **R**, the residual deviance (`Dr`) of a GLM could be obtain with
[`stats::deviance()`](https://rdrr.io/r/stats/deviance.html). For a
linear model,
[`stats::deviance()`](https://rdrr.io/r/stats/deviance.html) reports
RSS. The function `total_deviance()` could be used to get the total
deviance (`Dt`), i.e. the deviance of the null model. For a linear
model, it provides TSS.

*Analysis of variance* (Anova) is common to identify of the different
predictors have a significant effect on a model. Anova approaches have
been extended to also covers *analysis of deviance*. In the context of
Anova-like tests, it is common to report effect sizes indicators
representing the amount of variance or deviance explained by each
variable included in the model. These indicators are based on `Dpred` or
the delta of deviance/variance reduced by the inclusion of a specific
predictor in the model.

A first measure, known as *Eta-squared* (η²) in the context of linear
models, expresses this delta of deviance/variance as a proportion of the
total deviance (`Dpred / Dt`). This indicator represents the **total
contribution** of a predictor in the reduction of deviance. In the
context of a linear model, it represents the proportion of variance
explained by this predictor.

An alternative, known as *partial Eta-squared* (η_(p)²), could be
expressed as `Dpred / (Dr + Dpred)`, where `Dr` represents the residual
deviance of the full model. This **partial contribution** is the
proportion of partial variance/deviance uniquely explained by the
associated effect. That is, the variance/deviance uniquely explained by
the effect expressed as the proportion of variance/deviance not
explained by the other effects. Here the variance/deviance explained by
the other effects in the model is completely partialed out.

Finally, it is possible to express a **relative contribution** as the
proportion of the variance/deviance explained by the model, that could
be be expressed as `Dt - Dr`. Therefore, relative contribution is equal
to `Dpred / (Dt - Dr)` and represents the relative reduction of deviance
of the predictor compared to the total reduction of the deviance by the
full model.

It is crucial to understand the different types of Anova.

In a type-I Anova, as performed by
[`stats::anova()`](https://rdrr.io/r/stats/anova.html), the different
predictors are included sequentially and in-order into the model. Such
analysis is therefore order-dependant. The effect of a predictor is
computed once taken into account the previous predictors (but not the
other one introduced later). The first factor is tested without
adjustment. The second factor is tested after removing the effect of the
first. The third is tested after removing the effect of the first and
the second.

Type II-Anova (default of
[`car::Anova()`](https://rdrr.io/pkg/car/man/Anova.html)) tests each
main effect adjusted for all other effects of the same order or lower,
but not for interactions involving that factor. Each main effect is
tested as if it were the last main effect entered. Type II are
order-independent for main effects and are generally preferred when
there is no interaction. They have higher power than Type III for
testing main effects because they do not adjust for the interaction
term.

Type III-Anova (also done with
[`car::Anova()`](https://rdrr.io/pkg/car/man/Anova.html)) tests each
effect adjusted for all other effects in the model, including
higher-order interactions. Each effect is tested as if it were the last
one entered into a model containing all other effects. Type III are
order-independent. They require a specific contrast coding (typically
sum-to-zero or Helmert) to be interpretable.

[`car::Anova()`](https://rdrr.io/pkg/car/man/Anova.html) calculates
type-II or type-III Anova tables indicating for each predictor the
variance or deviance explained by adding this predictor in the model
compared to a model without this predictor (but keeping all other
predictors in the model).

`contributions()` also includes two alternatives: `"drop1"` and
`"add1"`.

`type = "drop1"` uses
[`stats::drop1()`](https://rdrr.io/r/stats/add1.html) which is
equivalent, in the absence, of interaction terms to a type-II Anova. For
some models (such as
[`survey::svyglm()`](https://rdrr.io/pkg/survey/man/svyglm.html)
models), it provides better estimates than
[`car::Anova()`](https://rdrr.io/pkg/car/man/Anova.html).

`type = "add1"` uses
[`stats::add1()`](https://rdrr.io/r/stats/add1.html) to calculate the
reduction of deviance associated with each predictor in an univariable
model (a model with just this predictor compared to the null model). In
this scenario, partial and relative contribution are not defined. It
provides an estimate of the contribution of a variable in the absence of
all other predictors.

Regarding η² indicators, more details are provided in a [dedicated
vignette](https://easystats.github.io/effectsize/articles/anovaES.html)
of the `effectsize` package. This vignette also presents variations of
these indicators. Some explanations are also available in the
[documentation of the
`GAMLj`](https://gamlj.github.io/details_glm_effectsize.html) package
for **Jamovi**.

To be noted, `GAMLj` highlights an potential issue regarding the
computation of total variance in
`effectsize::eta_squared(partial = FALSE)`. The `effectsize` package sum
the values displayed in the Anova object instead of performing a null
model. Here, we rely on `total_deviance()` and therefore estimates for
η² are not equal to those performed by
`effectsize::eta_squared(partial = FALSE)`. To be noted,
`effectsize::eta_squared(partial = TRUE)` (partial η²), is not impacted
by this difference in approaches.

In a type-II or type-III Anova, the sum of relative contributions would
equal 100% only if all predictors are perfectly independent. In
practice, the sum is never equal to 100% due to some correlation between
predictors.

For [`survey::svyglm()`](https://rdrr.io/pkg/survey/man/svyglm.html)
object, only type II and III are supported. The `type = "drop1"` is
recommended, in the absence of interactions, for such models.

`contributions()` and `tbl_contributions()` have been tested so far with
[`stats::lm()`](https://rdrr.io/r/stats/lm.html),
[`stats::glm()`](https://rdrr.io/r/stats/glm.html),
[`MASS::glm.nb()`](https://rdrr.io/pkg/MASS/man/glm.nb.html),
[`survey::svyglm()`](https://rdrr.io/pkg/survey/man/svyglm.html) and
[`survival::coxph()`](https://rdrr.io/pkg/survival/man/coxph.html)
models.

In the field of linear models, several authors have been working on
decomposing the coefficient of determination (R²) into individual
contributions of predictors in regression models, accounting for
correlations between predictors. It addresses a key challenge in
relative importance analysis: how to allocate shared variance when
predictors are collinear. It includes Shapley value-based approaches,
Genizi method, Proportional Marginal Variance Decomposition (PMVD),
relative weight analysis (RWA) or dominance analysis.

## Examples

``` r
# Linear model
i <- iris |>
  labelled::set_variable_labels(
    Sepal.Width = "Sepal's width",
    Petal.Length = "Petal's length"
  )
m <- lm(Sepal.Length ~ Sepal.Width + Species + Petal.Length, data = i)
m |> contributions()
#> Error in eval(mf, parent.frame()): object 'i' not found
m |> tbl_contributions()
#> Error in eval(mf, parent.frame()): object 'i' not found

# \donttest{
m |> tbl_contributions(type = 1)
#> Error in eval(mf, parent.frame()): object 'i' not found

# GLM
m2 <- glm(Survived == "Yes" ~ ., data = titanic, family = binomial)
m2 |> contributions()
#> Analysis of Deviance Table (Type II tests)
#> 
#> Response: Survived == "Yes"
#>       LR Chisq    Total  Partial Relative Df Pr(>Chisq)    
#> Class   119.03 0.042981 0.051107  0.21279  3  < 2.2e-16 ***
#> Sex     352.91 0.127430 0.137696  0.63088  1  < 2.2e-16 ***
#> Age      18.85 0.006807 0.008458  0.03370  1  1.413e-05 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
m2 |> tbl_contributions()


  

Predictor
```
