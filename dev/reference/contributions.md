# Estimate the contribution of each variable of a model

**\[experimental\]**  
Use [`car::Anova()`](https://rdrr.io/pkg/car/man/Anova.html) to perform
an analysis of variance (linear models) or an analysis of deviance (GLM
models), and then estimate the contribution of each variable in reducing
variance or deviance. See details for an explanation of the differences
between *total contribution*, *partial contribution* and *relative
contribution*. `contributions()` computes the different contributions.
`tbl_contributions()` displays the results as a formatted `{gt}` table.

## Usage

``` r
contributions(mod, ...)

tbl_contributions(
  mod,
  ...,
  show = c("Total", "Partial", "Relative"),
  notes = TRUE
)
```

## Arguments

- mod:

  a statistical model

- ...:

  additional parameters passed to
  [`car::Anova()`](https://rdrr.io/pkg/car/man/Anova.html)

- show:

  list of contributions to display

- notes:

  should table notes be added?

## Details

**Linear models**

In linear regression, the squared multiple correlation, R-squared is
used to assess goodness of fit as it represents the proportion of
variance in the criterion that is explained by the predictors. It could
be expressed as `1 - RSS / TSS` where TSS represents the total sum of
squares (the overall observed variance) and RSS represents the residual
sum of squares (i.e. the variance not explained by the full model).

In the context of ANOVA-like tests, it is common to report effect sizes
indicators representing the amount of variance explained by each
variable included in the model.

[`car::Anova()`](https://rdrr.io/pkg/car/man/Anova.html) calculates
type-II or type-III analysis-of-variance tables indicating for each
predictor the variance (or sum of squares SS) explained by adding this
predictor in the model compared to a model without this predictor (but
keeping all other predictors in the model).

A first measure, known as *Eta-squared*, report the SS explained by the
predictor to the total sum of squares (TSS). This *total contribution*
of the variable represents the proportion of the total variance
explained by this predictor.

An alternative, known as *partial Eta-squared*, which is the percent of
the partial variance (after accounting for other predictors in the
model). For the *partial contribution*, SS is divided by (SS + RSS)
(Residual Sum of Squares, i.e. the variance not explained by the full
model). This is the proportion of partial variance uniquely explained by
the associated effect. That is, the variance uniquely explained by the
effect expressed as the proportion of variance not explained by the
other effects. Here the variance explained by the other effects in the
model is completely partialed out.

Finally, it is possible to express a *relative contribution* as the
proportion of the variance explained by the model attributable to this
predictor. In this case, SS is divided by (TSS - RSS), corresponding to
the variance explained by the full model.

Note: for more details about effect size measures for ANOVA, see the
[dedicated
vignette](https://easystats.github.io/effectsize/articles/anovaES.html)
of the `effectsize` package. Some explanations are also available in the
[documentation of the
`GAMLj`](https://gamlj.github.io/details_glm_effectsize.html) package
for **Jamovi**.

To be noted, `GAMLj` highlights an potential issue regarding the
computation of TSS in `effectsize::eta_squared(partial = FALSE)`. For
now, `comparisons()` follows `effectsize`. An optional `correct_tss`
argument may be added in the future.

**GLM**

For generalized linear models (GLM), model fitting does not rely on
ordinary least squares (OLS). It is achieved by maximum likelihood.
Therefore, sum of squares is not available and R-squared cannot be
computed. An alternative goodness of fit measure used in such case is
pseudo R-squared. The McFadden pseudo R-squared (sometimes called
likelihood ratio index) could be expressed using deviance rather than
likelihood. In such case, it is equal to `1 - DF / D0` where D0
represents the deviance of a null model (i.e. a model with no predictor)
and DF the deviance of the full model. The McFadden pseudo R2 could be
seen as the proportion of deviance reduced by the model.

For such models,
[`car::Anova()`](https://rdrr.io/pkg/car/man/Anova.html) provides an
analysis of deviance with the reduction of deviance (called "LR Chisq"
in the output) attributable to each predictor (comparing the deviance of
a model without this predictor to the deviance of the full model).

Therefore, the *total contribution* of a predictor is the reduction of
deviance attributable to this predictor (DP) divided by D0, the deviance
of the null model (representing the total deviance).

A *partial contribution* expressed as DP / (DP + DF), DF representing
the deviance still not reduced by the model (deviance of the full model,
i.e. residual deviance).

Finally, a *relative contribution* would be expressed as DP / (D0 -DF)
and representing the relative reduction of deviance of the predictor
compared to the total reduction of the deviance by the full model. To be
noted, the sum of all relative contributions would equal 100% only if
all predictors are perfectly independent. In practice, the sum is never
equal to 100% due to some correlation between predictors.

An alternative approach consists of using `test.statistic = "F"` to
generate estimates based on Pearson residuals where deviance is somehow
interpreted as sums of squares. The F test is usually not recommended
for GLM models.

## Examples

``` r
# Linear model
m <- lm(Sepal.Length ~ Sepal.Width + Species + Petal.Length, data = iris)
m |> contributions()
#> Anova Table (Type II tests)
#> 
#> Response: Sepal.Length
#>               Sum Sq   Total Partial Relative Df F value    Pr(>F)    
#> Sepal.Width   2.7161 0.08210 0.16282  0.14208  1  28.201 4.026e-07 ***
#> Species       2.3632 0.07143 0.14473  0.12362  2  12.268 1.195e-05 ***
#> Petal.Length 14.0382 0.42433 0.50130  0.73431  1 145.754 < 2.2e-16 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
m |> tbl_contributions()


  

Predictor
```
