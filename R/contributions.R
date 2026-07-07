#' Estimate the contribution of each variable of a model
#'
#' `r lifecycle::badge("experimental")`<br />
#' Use an analysis of variance or an analysis of deviance to estimate the
#' contribution of each variable in reducing variance or deviance. This
#' contribution could be explained as a proportion of the total deviance
#' (*total contribution*), of the residual deviance (*partial contribution*)
#' or of the explained deviance (*relative contribution*), see details for
#' more information. `contributions()` computes the different contributions.
#' `tbl_contributions()` displays the results as a formatted [`gt`][gt::gt()]
#' table, taking into account variable labels.
#' @param mod a statistical model
#' @param type type of Anova, Roman numerals being equivalent to the
#' corresponding Arabic numerals: [stats::anova()] will be used for type-I,
#' Anova, [car::Anova()] for types II and III; an alternative method, `"drop1"`
#' uses [stats::drop1()] to calculate the reduction of deviance associated with
#' each predictor (in the absence of interactions, is equivalent to a type-II
#' Anova); `"add1"` uses [stats::add1()] to calculate the reduction of deviance
#' associated with each predictor in an univariable model (model with just this
#' predictor compared to the null model), in this scenario, partial and relative
#' contribution are not defined.
#' @param ... additional parameters passed to [stats::anova()], [car::Anova()],
#' [stats::drop1()] or [stats::add1()]
#' @details
#' In linear regression, the squared multiple correlation, R<sup>2</sup> is used
#' to assess goodness of fit as it represents the proportion of variance in the
#' criterion that is explained by the predictors. It could be expressed as
#' `1 - RSS / TSS` where `TSS` represents the total sum of squares (the overall
#' observed variance) and `RSS` represents the residual sum of squares (i.e.
#' the variance not explained by the full model).
#'
#' For generalized linear models (GLM), model fitting does not rely on ordinary
#' least squares (OLS). It is achieved by maximum likelihood. Therefore, sum
#' of squares is not available and R-squared cannot be computed. An alternative
#' goodness of fit measure used in such case is pseudo R<sup>2</sup>. The
#' McFadden pseudo R<sup>2</sup> (sometimes called likelihood ratio index) could
#' be expressed using deviance rather than likelihood. In such case, it is equal
#' to `1 - Dr / Dt` where `Dt` represents the full deviance
#' (i.e. the deviance of the null model, a model with no
#' predictor) and `Dr` the residual deviance (i.e. the deviance of
#' the full model). The McFadden pseudo R<sup>2</sup> corresponds the
#' proportion of deviance reduced by the model. Deviance is a generalization of
#' the idea of using the sum of squares of residuals in the cases where
#' model-fitting is achieved by maximum likelihood.
#'
#' In **R**, the residual deviance (`Dr`) of a GLM could be obtain with
#' [stats::deviance()]. For a linear model, [stats::deviance()] reports RSS.
#' The function `total_deviance()` could be used to get the total deviance
#' (`Dt`), i.e. the deviance of the null model. For a linear model, it provides
#' TSS.
#'
#' *Analysis of variance* (Anova) is common to identify of the different
#' predictors have a significant effect on a model. Anova approaches have been
#' extended to also covers *analysis of deviance*. In the context of Anova-like
#' tests, it is common to report effect sizes indicators representing the amount
#' of variance or deviance explained by each variable included in the model.
#' These indicators are based on `Dpred` or the delta of deviance/variance
#' reduced by the inclusion of a specific predictor in the model.
#'
#' A first measure, known as *Eta-squared* (&eta;<sup>2</sup>) in the context of
#' linear models, expresses this delta of deviance/variance as a
#' proportion of the total deviance (`Dpred / Dt`). This indicator represents
#' the **total contribution** of a predictor in the reduction of deviance. In
#' the context of a linear model, it represents the proportion of variance
#' explained by this predictor.
#'
#' An alternative, known as *partial Eta-squared*
#' (&eta;<sub>p</sub><sup>2</sup>), could be expressed as
#' `Dpred / (Dr + Dpred)`, where `Dr` represents the residual deviance of the
#' full model. This **partial contribution** is the proportion of partial
#' variance/deviance uniquely explained by the associated effect.
#' That is, the variance/deviance uniquely explained by the effect expressed
#' as the proportion of variance/deviance not explained by the other effects.
#' Here the variance/deviance explained by the other effects in the model is
#' completely partialed out.
#'
#' Finally, it is possible to express a **relative contribution** as the
#' proportion of the variance/deviance explained by the model, that could be
#' be expressed as `Dt - Dr`. Therefore, relative contribution is equal to
#' `Dpred / (Dt - Dr)` and represents the relative reduction of deviance of the
#' predictor compared to the total reduction of the deviance by the full model.
#'
#' It is crucial to understand the different types of Anova.
#'
#' In a type-I Anova,
#' as performed by [stats::anova()], the different predictors are included
#' sequentially and in-order into the model. Such analysis is therefore
#' order-dependant. The effect of a predictor is computed once taken into
#' account the previous predictors (but not the other one introduced later).
#' The first factor is tested without adjustment. The second factor is tested
#' after removing the effect of the first. The third is tested after removing
#' the effect of the first and the second.
#'
#' Type II-Anova (default of [car::Anova()]) tests each main effect adjusted
#' for all other effects of the same order or lower, but not for interactions
#' involving that factor. Each main effect is tested as if it were the last
#' main effect entered. Type II are order-independent for main effects and are
#' generally preferred when there is no interaction. They have higher power
#' than Type III for testing main effects because they do not adjust for the
#' interaction term.
#'
#' Type III-Anova (also done with [car::Anova()]) tests each effect adjusted
#' for all other effects in the model, including higher-order interactions.
#' Each effect is tested as if it were the last one entered into a model
#' containing all other effects. Type III are order-independent.
#' They require a specific contrast coding (typically sum-to-zero or Helmert)
#' to be interpretable.
#'
#' [car::Anova()] calculates type-II or type-III Anova tables
#' indicating for each predictor the variance or deviance explained
#' by adding this predictor in the model compared to a model without this
#' predictor (but keeping all other predictors in the model).

#'
#' `contributions()` also includes two alternatives: `"drop1"` and `"add1"`.
#'
#' `type = "drop1"` uses [stats::drop1()] which is equivalent, in the absence,
#' of interaction terms to a type-II Anova. For some models (such as
#' [survey::svyglm()] models), it provides better estimates than [car::Anova()].
#'
#' `type = "add1"` uses [stats::add1()] to calculate the reduction of deviance
#' associated with each predictor in an univariable model (a model with just
#' this predictor compared to the null model). In this scenario, partial and
#' relative contribution are not defined. It provides an estimate of the
#' contribution of a variable in the absence of all other predictors.
#'
#' Regarding &eta;<sup>2</sup> indicators, more details are provided in a
#' [dedicated vignette](https://easystats.github.io/effectsize/articles/anovaES.html)
#' of the `effectsize` package. This vignette also presents variations of these
#' indicators. Some explanations are also available in the
#' [documentation of the `GAMLj`](https://gamlj.github.io/details_glm_effectsize.html)
#' package for **Jamovi**.
#'
#' To be noted, `GAMLj` highlights an potential issue regarding the computation
#' of total variance in `effectsize::eta_squared(partial = FALSE)`. The
#' `effectsize` package sum the values displayed in the Anova object instead
#' of performing a null model. Here, we rely on `total_deviance()` and therefore
#' estimates for &eta;<sup>2</sup> are not equal to those performed by
#' `effectsize::eta_squared(partial = FALSE)`. To be noted,
#' `effectsize::eta_squared(partial = TRUE)` (partial &eta;<sup>2</sup>), is
#' not impacted by this difference in approaches.
#'
#'
#' In a type-II or type-III Anova, the sum of relative contributions would
#' equal 100% only if all predictors are perfectly independent. In practice,
#' the sum is never equal to 100% due to some correlation between predictors.
#'
#'
#' For [survey::svyglm()] object, only type II and III are supported. The
#' `type = "drop1"` is recommended, in the absence of interactions, for
#' such models.
#'
#' `contributions()` and `tbl_contributions()` have been tested so far with
#' [stats::lm()], [stats::glm()], [MASS::glm.nb()], [survey::svyglm()] and
#' [survival::coxph()] models.
#'
#' In the field of linear models, several authors have been working on
#' decomposing the coefficient of determination (R<sup>2</sup>) into individual
#' contributions of predictors in regression models, accounting for correlations
#' between predictors. It addresses a key challenge in relative importance
#' analysis: how to allocate shared variance when predictors are collinear.
#' It includes Shapley value-based approaches, Genizi method, Proportional
#' Marginal Variance Decomposition (PMVD), relative weight
#' analysis (RWA) or dominance analysis.
#' @export
#' @keywords models
#' @examples
#' # Linear model
#' i <- iris |>
#'   labelled::set_variable_labels(
#'     Sepal.Width = "Sepal's width",
#'     Petal.Length = "Petal's length"
#'   )
#' m <- lm(Sepal.Length ~ Sepal.Width + Species + Petal.Length, data = i)
#' m |> contributions()
#' m |> tbl_contributions()
#'
#' \donttest{
#' m |> tbl_contributions(type = 1)
#'
#' # GLM
#' m2 <- glm(Survived == "Yes" ~ ., data = titanic, family = binomial)
#' m2 |> contributions()
#' m2 |> tbl_contributions()
#' m2 |> tbl_contributions(show = "Relative", notes = FALSE)
#'
#' # Survey-weighted GLM
#' library(survey)
#' m3 <- survey::svyglm(
#'   Survived == "Yes" ~ Class + Sex + Age,
#'   design = srvyr::as_survey(titanic),
#'   family = quasibinomial
#' )
#' m3 |> tbl_contributions(type = "drop1")
#' }
contributions <- function(
  mod,
  type = c("II", "III", "I", 1, 2, 3, "drop1", "add1"),
  ...
) {
  type <- as.character(type)
  type <- match.arg(type)

  if (inherits(mod, "svyglm") && type %in% c("1", "I"))
    cli::cli_abort("Type I not covered for {.class svyglm}.")

  if (type %in% c("1", "I")) {
    a <- stats::anova(mod, ...)
  } else if (type == "drop1") {
    a <- drop1_to_anova(mod, ...)
  } else if (type == "add1") {
    a <- add1_to_anova(mod, ...)
  } else {
    rlang::check_installed("car")
    a <- car::Anova(mod, type = type, ...)
  }

  dev_names <- c("Sum Sq", "LR Chisq", "Deviance", "Chisq")
  dev_col <- colnames(a)[colnames(a) %in% dev_names]
  if (length(dev_col) == 0)
    cli::cli_abort("Deviance not found in ANOVA results.")
  if (length(dev_col) > 1)
    dev_col <- dev_col[1]

  Dr <- stats::deviance(mod)
  Dt <- total_deviance(mod)

  heading <- attr(a, "heading")
  a <- a[row.names(a) != "Residuals" & row.names(a) != "NULL", ]

  a$Total <- a[[dev_col]] / Dt
  a$Partial <- a[[dev_col]] / (Dr + a[[dev_col]])
  a$Relative <- a[[dev_col]] / (Dt - Dr)

  if (type == "add1") {
    a$Partial <- NA_real_
    a$Relative <- NA_real_
  }

  a <-
    a |>
    dplyr::relocate(
      "Total", "Partial", "Relative",
      .after = dplyr::all_of(dev_col)
    )
  attr(a, "heading") <- heading
  attr(a, "total_deviance") <- Dt
  attr(a, "residual_deviance") <- Dr
  attr(a, "deviance_column") <- dev_col
  a
}

#' @rdname contributions
#' @param show list of contributions to display
#' @param notes should table notes be added?
#' @export
tbl_contributions <- function(
  mod,
  type = c("II", "III", "I", 1, 2, 3, "drop1", "add1"),
  ...,
  show = c("Total", "Partial", "Relative"),
  notes = TRUE
) {
  rlang::check_installed("gt")
  rlang::check_installed("broom.helpers")
  rlang::check_installed("gtsummary")

  cc <- mod |> contributions(type = type, ...)
  lv <-
    mod |>
    broom.helpers::model_list_variables() |>
    dplyr::select("variable", "var_label")

  newnames <- c(
    "Deviance" = "LR Chisq",
    "Deviance" = "Chisq",
    "Sum of Squares" = "Sum Sq",
    "Predictor" = "var_label",
    "p-value" = "Pr(>Chisq)",
    "p-value" = "Pr(>Chi)",
    "p-value" = "Pr(>F)"
  )
  p_names <- c("Pr(>F)", "Pr(>Chisq)", "Pr(>Chi)")
  contrib_col_names <- list(
    Total = gt::html("Total contribution (&eta;<sup>2</sup>)"),
    Partial = gt::html("Partial contribution  (&eta;<sub>p</sub><sup>2</sup>)"), #nolint
    Relative = "Relative contribution"
  )

  res <-
    cc |>
    dplyr::as_tibble(rownames = "variable") |>
    dplyr::left_join(lv, by = "variable") |>
    dplyr::select(
      "var_label",
      dplyr::any_of(attr(cc, "deviance_column")),
      dplyr::all_of(show),
      dplyr::any_of(p_names)
    ) |>
    dplyr::rename(dplyr::any_of(newnames)) |>
    gt::gt() |>
    gt::tab_style(
      style = gt::cell_text(weight = "bold"),
      locations = gt::cells_column_labels()
    ) |>
    gt::fmt_number(
      columns = gt::any_of(c("Deviance", "Sum of Squares")),
      decimals = 1
    ) |>
    gt::fmt_percent(
      columns = gt::any_of(
        c("Total", "Partial", "Relative")
      ),
      decimals = 1
    ) |>
    gt::fmt(
      columns = gt::any_of("p-value"),
      fns = gtsummary::label_style_pvalue(digits = 1)
    ) |>
    gt::cols_label(.list = contrib_col_names[show])

  if (notes) {
    res <-
      res |>
      gt::tab_source_note(
        paste(
          "Total deviance (null model):",
          scales::number(attr(cc, "total_deviance"), accuracy = .1)
        )
      ) |>
      gt::tab_source_note(
        paste(
          "Residual deviance (full model):",
          scales::number(attr(cc, "residual_deviance"), accuracy = .1)
        )
      ) |>
      gt::tab_source_note(
        gt::html(
          paste(
            "McFadden pseudo R<sup>2</sup>:",
            scales::percent(
              1 - (attr(cc, "residual_deviance") / attr(cc, "total_deviance")),
              accuracy = .1
            )
          )
        )
      )
  }

  res
}

#' @rdname contributions
#' @export
total_deviance <- function(mod) {
  if (!is.null(mod$null.deviance)) return(mod$null.deviance)

  null_mod <- mod |> stats::update(. ~ 1)
  stats::deviance(null_mod)
}

drop1_to_anova <- function(mod, ...) {
  args <- list(...)
  if (is.null(args$test)) args$test <- "Chisq"
  args$object <- mod
  d1 <- do.call(stats::drop1, args)

  if (!"Deviance" %in% colnames(d1))
    cli::cli_abort("Deviance not reported by {.fn stats::drop1}.")

  residual_deviance <- d1["<none>", "Deviance"]
  d1$Deviance <- d1$Deviance - residual_deviance

  d1[rownames(d1) != "<none>", ]
}

add1_to_anova <- function(mod, ...) {
  args <- list(...)
  if (is.null(args$test)) args$test <- "Chisq"
  args$object <- mod |> stats::update(. ~ 1)
  args$scope <- stats::terms(mod)
  d1 <- do.call(stats::add1, args)

  if (!"Deviance" %in% colnames(d1))
    cli::cli_abort("Deviance not reported by {.fn stats::add1}.")

  total_deviance <- d1["<none>", "Deviance"]
  d1$Deviance <- total_deviance - d1$Deviance

  d1[rownames(d1) != "<none>", ]
}
