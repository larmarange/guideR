#' Estimate the contribution of each variable of a model
#'
#' `r lifecycle::badge("experimental")`<br />
#' Use [car::Anova()] to perform an analysis of variance (linear models) or
#' an analysis of deviance (GLM models), and then estimate the contribution
#' of each variable in reducing variance or deviance. See details for an
#' explanation of the differences between *total contribution*,
#' *partial contribution* and *relative contribution*. `contributions()`
#' computes the different contributions. `tbl_contributions()` displays
#' the results as a formatted `{gt}` table.
#'
#' @param mod a statistical model
#' @param ... additional parameters passed to [car::Anova()]
#' @details
#' **Linear models**
#'
#' In linear regression, the squared multiple correlation, R² is used to assess
#' goodness of fit as it represents the proportion of variance in the criterion
#' that is explained by the predictors. It could be expressed as `1 - RSS / TSS`
#' where TSS represents the total sum of squares (the overall observed variance)
#' and RSS represents the residual sum of squares (i.e. the variance not
#' explained by the full model).
#'
#' In the context of ANOVA-like tests, it is common to report effect sizes
#' indicators representing the amount of variance explained by each variable
#' included in the model.
#'
#' [car::Anova()] calculates type-II or type-III analysis-of-variance tables
#' indicating for each predictor the variance (or sum of squares SS) explained
#' by adding this predictor in the model compared to a model without this
#' predictor (but keeping all other predictors in the model).
#'
#' A first measure, known as *Eta-squared*, report the SS explained by the
#' predictor to the total sum of squares (TSS). This *total contribution*
#' of the variable represents the proportion of the total variance explained by
#' this predictor.
#'
#' An alternative, known as *partial Eta-squared*, which is the percent of the
#' partial variance (after accounting for other predictors in the model). For
#' the *partial contribution*, SS is divided by (SS + RSS) (Residual Sum of
#' Squares, i.e. the variance not explained by the full model). This is the
#' proportion of partial variance uniquely explained by the associated effect.
#' That is, the variance uniquely explained by the effect expressed as the
#' proportion of variance not explained by the other effects. Here the variance
#' explained by the other effects in the model is completely partialed out.
#'
#' Finally, it is possible to express a *relative contribution* as the
#' proportion of the variance explained by the model attributable to this
#' predictor. In this case, SS is divided by (TSS - RSS), corresponding to
#' the variance explained by the full model.
#'
#' Note: for more details about effect size measures for ANOVA, see the
#' [dedicated vignette](https://easystats.github.io/effectsize/articles/anovaES.html)
#' of the `effectsize` package. Some explanations are also available in the
#' [documentation of the `GAMLj`](https://gamlj.github.io/details_glm_effectsize.html)
#' package for **Jamovi**.
#'
#' **GLM**
#'
#' For generalized linear models (GLM), model fitting does not rely on ordinary
#' least squares (OLS). It is achieved by maximum likelihood. Therefore, sum
#' of squares is not available and R² cannot be computed. An alternative
#' goodness of fit measure used in such case is pseudo R². The McFadden pseudo
#' R² (sometimes called likelihood ratio index) could be expressed using
#' deviance rather than likelihood. In such case, it is equal to `1 - DF / D0`
#' where D0 represents the deviance of a null model (i.e. a model with no
#' predictor) and DF the deviance of the full model. The McFadden pseudo R2
#' could be seen as the proportion of deviance reduced by the model.
#'
#' For such models, [car::Anova()] provides an analysis of deviance with
#' the reduction of deviance (called "LR Chisq" in the output) attributable to
#' each predictor (comparing the deviance of a model without this predictor
#' to the deviance of the full model).
#'
#' Therefore, the *total contribution* of a predictor is the reduction of
#' deviance attributable to this predictor (DP) divided by D0, the deviance
#' of the null model (representing the total deviance).
#'
#' A *partial contribution* by dividing DP by (RD + DF), DF representing the
#' deviance still not reduced by the model.
#'
#' Finally, a *relative contribution* would be expressed as DP / (D0 -DF) and
#' representing the relative reduction of deviance of the predictor compared
#' to the total reduction of the deviance by the full model. To be noted,
#' the sum of all relative contributions would equal 100% only if all
#' predictors are perfectly independent. In practice, the sum is never equal
#' to 100% due to some correlation between predictors.
#'
#' An alternative approach consists of using `test.statistic = "F"` to generate
#' estimates based on Pearson residuals allowing to simulate sums of squares.
#' The F test is usually not recommended for GLM models to estimate global
#' p-values. However, it has the advantage that the sum of relative
#' contributions will be equal to 100%.
#' @export
#' @examples
#' # Linear model
#' m <- lm(Sepal.Length ~ Sepal.Width + Species + Petal.Length, data = iris)
#' m |> contributions()
#' m |> tbl_contributions()
#'
#' # GLM
#' m2 <- glm(Survived == "Yes" ~ ., data = titanic, family = binomial)
#' m2 |> contributions()
#' m2 |> tbl_contributions()
#' m2 |> tbl_contributions(show = "Relative", notes = FALSE)
contributions <- function(mod, ...) {
  rlang::check_installed("car")
  a <- mod |> car::Anova(...)
  heading <- attr(a, "heading")

  # linear case
  if ("Sum Sq" %in% names(a) && "Residuals" %in% row.names(a)) {
    tss <- sum(a$`Sum Sq`) # total sum of squares
    rss <- a["Residuals", "Sum Sq"] # residual sum of squares
    a <- a[row.names(a) != "Residuals", ]
    a$Total <- a$`Sum Sq` / tss
    a$Partial <- a$`Sum Sq` / (a$`Sum Sq` + rss)
    a$Relative <- a$`Sum Sq` / (tss - rss)
    a <-
      a |>
      dplyr::relocate(
        "Total", "Partial", "Relative",
        .after = "Sum Sq"
      )
    attr(a, "heading") <- heading
    attr(a, "rss") <- rss
    attr(a, "tss") <- tss
    return(a)
  }

  # GLM case
  if (
    "LR Chisq" %in% names(a) && !is.null(mod$deviance) &&
    !is.null(mod$null.deviance)
  ) {
    df <- mod$deviance # full model
    d0 <- mod$null.deviance # null model
    a$Total <- a$`LR Chisq` / d0
    a$Partial <- a$`LR Chisq` / (a$`LR Chisq` + df)
    a$Relative <- a$`LR Chisq` / (d0 - df)
    a <-
      a |>
      dplyr::relocate(
        "Total", "Partial", "Relative",
        .after = "LR Chisq"
      )
    attr(a, "heading") <- heading
    attr(a, "d0") <- d0
    attr(a, "df") <- df
    return(a)
  }

  cli::cli_abort("Model/Case not covered.")
}

#' @rdname contributions
#' @param show list of contributions to display
#' @param notes should table notes be added?
#' @export
tbl_contributions <- function(
    mod,
    ...,
    show = c("Total", "Partial", "Relative"),
    notes = TRUE
) {
  rlang::check_installed("gt")
  rlang::check_installed("broom.helpers")
  rlang::check_installed("gtsummary")

  cc <- mod |> contributions(...)
  lv <-
    mod |>
    broom.helpers::model_list_variables() |>
    dplyr::select(variable, var_label)

  newnames <- c(
    "Deviance" = "LR Chisq",
    "Sum of Squares" = "Sum Sq",
    "Predictor" = "var_label",
    "Total contribution" = "Total",
    "Partial contribution" = "Partial",
    "Relative contribution" = "Relative",
    "p-value" = "Pr(>Chisq)",
    "p-value" = "Pr(>F)"
  )
  res <-
    cc |>
    dplyr::as_tibble(rownames = "variable") |>
    dplyr::left_join(lv, by = "variable") |>
    dplyr::select(
      "var_label",
      dplyr::any_of(c("LR Chisq", "Sum Sq")),
      dplyr::all_of(show),
      dplyr::any_of(c("Pr(>F)", "Pr(>Chisq)"))
    ) |>
    dplyr::rename(dplyr::any_of(newnames)) |>
    gt::gt() |>
    gt::tab_style(
      style = gt::cell_text(weight = "bold"),
      locations = gt::cells_column_labels()
    ) |>
    gt::fmt_number(
      columns = gt::any_of(
        c("Deviance", "Sum of Squares")
      ),
      decimals = 1
    ) |>
    gt::fmt_percent(
      columns = gt::any_of(
        c("Total contribution", "Partial contribution", "Relative contribution")
      ),
      decimals = 1
    ) |>
    gt::fmt(
      columns = "p-value",
      fns = gtsummary::label_style_pvalue(digits = 1)
    )

  if (notes) {
    if (!is.null(attr(cc, "d0")))
      res <-
        res |>
        gt::tab_source_note(
          paste(
            "Deviance of the null model:",
            scales::number(attr(cc, "d0"), accuracy = .1)
          )
        )
    if (!is.null(attr(cc, "df")))
      res <-
        res |>
        gt::tab_source_note(
          paste(
            "Deviance of the full model:",
            scales::number(attr(cc, "df"), accuracy = .1)
          )
        )
    if (!is.null(attr(cc, "df")) && !is.null(attr(cc, "d0")))
      res <-
        res |>
        gt::tab_source_note(
          paste(
            "McFadden pseudo R²:",
            scales::percent(
              1 - (attr(cc, "df") / attr(cc, "d0")),
              accuracy = .1
            )
          )
        )
    if (!is.null(attr(cc, "tss")))
      res <-
        res |>
        gt::tab_source_note(
          paste(
            "Total sum of squares:",
            scales::number(attr(cc, "tss"), accuracy = .1)
          )
        )
    if (!is.null(attr(cc, "rss")))
      res <-
      res |>
      gt::tab_source_note(
        paste(
          "Residual sum of squares:",
          scales::number(attr(cc, "rss"), accuracy = .1)
        )
      )
  }

  res
}
