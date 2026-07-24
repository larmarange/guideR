# Variable contribution and Dominance analysis

**\[experimental\]**  
Use an analysis of variance or an analysis of deviance to estimate the
contribution of each variable in reducing variance or deviance (sort of
R² decomposition). This contribution could be explained as a proportion
of the total deviance (*total contribution*), of the residual deviance
(*partial contribution*) or of the explained deviance (*relative
contribution*), see details for more information. `contributions()`
computes the different contributions. `tbl_contributions()` displays the
results as a formatted [`gt`](https://gt.rstudio.com/reference/gt.html)
table, taking into account variable labels. Alternatively,
`tbl_domimance()` could be used to perform a dominance analysis with
[`dominanceanalysis::dominanceAnalysis()`](https://rdrr.io/pkg/dominanceanalysis/man/dominanceAnalysis.html)
and display the results as a formatted
[`gt`](https://gt.rstudio.com/reference/gt.html) table.

## Usage

``` r
contributions(mod, type = c("II", "III", "I", 1, 2, 3, "drop1", "add1"), ...)

tbl_contributions(
  mod,
  type = c("II", "III", "I", 1, 2, 3, "drop1", "add1"),
  ...,
  show = c("Total", "Partial", "Relative"),
  decimals = 1,
  notes = TRUE
)

total_deviance(mod)

tbl_dominance(mod, indice = NULL, decimals = 1, totals = TRUE, notes = TRUE)
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

- decimals:

  number of decimals for deviance and contributions

- notes:

  should table notes be added?

- indice:

  fit indice name, see
  [`dominanceanalysis::dominanceAnalysis()`](https://rdrr.io/pkg/dominanceanalysis/man/dominanceAnalysis.html)

- totals:

  add a summary row with totals

## Details

In linear regression, the squared multiple correlation, R² is used to
assess goodness of fit as it represents the proportion of variance in
the criterion that is explained by the predictors. It could be expressed
as `1 - RSS / TSS` where `TSS` represents the total sum of squares (the
overall observed variance) and `RSS` represents the residual sum of
squares (i.e. the variance not explained by the full model).

For generalized linear models (GLM), model fitting does not rely on
ordinary least squares (OLS). It is achieved by maximum likelihood.
Therefore, sum of squares is not available and R² cannot be computed. An
alternative goodness of fit measure used in such case is pseudo R². The'
McFadden pseudo R² (sometimes called likelihood ratio index) could be
expressed using deviance rather than likelihood. In such case, it is
equal to `1 - Dr / Dt` where `Dt` represents the full deviance (i.e. the
deviance of the null model, a model with no predictor) and `Dr` the
residual deviance (i.e. the deviance of the full model). The McFadden
pseudo R² corresponds the proportion of deviance reduced by the model.
Deviance is a generalization of the idea of using the sum of squares of
residuals in the cases where model-fitting is achieved by maximum
likelihood.

In **R**, the residual deviance (`Dr`) of a GLM could be obtain with
[`stats::deviance()`](https://rdrr.io/r/stats/deviance.html). For a
linear model,
[`stats::deviance()`](https://rdrr.io/r/stats/deviance.html) reports
RSS. The function `total_deviance()` could be used to get the total
deviance (`Dt`), i.e. the deviance of the null model. For a linear
model, it provides TSS.

*Analysis of variance* (Anova) is common to identify if the different
predictors have a significant effect on a model. Anova approaches have
been extended to also covers *analysis of deviance*. In the context of
Anova-like tests, it is common to report effect sizes indicators
representing the amount of variance or deviance explained by each
variable included in the model. Such approach is also known as *R²*
decomposition. These indicators are based on `Dpred` (the delta of
deviance) or `SSpred` (the delta of the sum of squares, i.e. the delta
of variance) reduced by the inclusion of a specific predictor in the
model.

A first measure, known as *Eta-squared* (η²) in the context of linear
models, expresses this delta of sum of squares (variance) as a
proportion of the total variance (`SSpred / TSS`). In the context of
GLMs, the delta of deviance could similarly expressed as a proportion of
the total deviance (`Dpred / Dt`), also known as the *semi-partial
pseudo-R²*. This indicator represents the **total contribution** of a
predictor in the reduction of deviance.

An alternative, known as *partial Eta-squared* (η_(p)²), in the context
of linear models, could be expressed as `SSpred / (RSS + SSpred)`, the
variance uniquely explained by the effect expressed as the proportion of
variance not explained by the other effects. Here the variance explained
by the other effects in the model is completely partialed out.
Similarly, for GLMs, *partial pseudo-R²* is expressed as
`Dpred / (Dr + Dpred)`, where `Dr` represents the residual deviance of
the full model. This **partial contribution** is the proportion of
partial variance/deviance uniquely explained by the associated effect.

Finally, it is possible to express a **relative contribution** as the
proportion of the variance/deviance explained by the model, that could
be be expressed as `TSS - RSS` or `Dt - Dr`. Therefore, relative
contribution is equal to `SSpred / (TSS - RSS)` or `Dpred / (Dt - Dr)`
and represents the relative reduction of variance/deviance of the
predictor compared to the total reduction of the variance / deviance by
the full model.

It is crucial to understand the different types of Anova.

In a type-I Anova, as performed by
[`stats::anova()`](https://rdrr.io/r/stats/anova.html), the different
predictors are included sequentially and in-order into the model. Such
analysis is therefore order-dependant. The effect of a predictor is
computed once taken into account the previous predictors (but not the
other one introduced later). The first factor is tested without
adjustment. The second factor is tested after removing the effect of the
first. The third is tested after removing the effect of the first and
the second. The advantage of such approach is that the sum of relative
contributions is equal to 100%. However, the contribution of each
variable changes if the variables are entered in a different order.

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

For [`survey::svyglm()`](https://rdrr.io/pkg/survey/man/svyglm.html)
object, only type II and III are supported. The `type = "drop1"` is
recommended, in the absence of interactions, for such models.

In a type-II or type-III Anova, the sum of relative contributions would
equal 100% only if all predictors are perfectly independent. In
practice, the sum is never equal to 100% due to some correlation between
predictors.

Some approaches, such as dominance analysis, have been developed for
decomposing the coefficient of determination (R²) into individual
contributions of predictors in regression models, accounting for
correlations between predictors. They address a key challenge in
relative importance analysis: how to allocate shared variance when
predictors are collinear.

Dominance analysis follows a systematic, computationally intensive
approach based on all-subsets regression. Models of every possible
combination of predictors are generated, each pair of predictors are
compared by examining their incremental contributions within matching
subsets. Finally, the dominance relationship is determined based on
three levels: complete dominance (variable A always contributes more),
conditional dominance (higher average contribution), and general
dominance (weighted average across subset sizes).

The last dimension (the average contribution of each variable) provides
a metric similar to semi-partial R², and could be expressed as a
proportion of the global R² (relative contribution). The sum of these
relative contributions is equal 100%.

`tbl_dominance()` is a quick helper to perform a dominance analysis with
[`dominanceanalysis::dominanceAnalysis()`](https://rdrr.io/pkg/dominanceanalysis/man/dominanceAnalysis.html)
and to display the average total contribution variable in a nicely
formatted [`gt::gt()`](https://gt.rstudio.com/reference/gt.html) table.
The function also displays the relative contribution (i.e. the
contribution of each variable expressed as a proportion of the R²).

To be noted, alternative implementations of dominance analysis in **R**
include
[`parameters::dominance_analysis()`](https://easystats.github.io/parameters/reference/dominance_analysis.html)
or `domir::domin()`.

## Note

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

## Examples

``` r
# Linear model
m <- lm(Sepal.Length ~ Sepal.Width + Species + Petal.Length, data = iris)
m |> contributions()
#> Anova Table (Type II tests)
#> 
#> Response: Sepal.Length
#>               Sum Sq    Total Partial Relative Df F value    Pr(>F)    
#> Sepal.Width   2.7161 0.026585 0.16282 0.030794  1  28.201 4.026e-07 ***
#> Species       2.3632 0.023131 0.14473 0.026793  2  12.268 1.195e-05 ***
#> Petal.Length 14.0382 0.137402 0.50130 0.159158  1 145.754 < 2.2e-16 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
m |> tbl_contributions()


  
```
