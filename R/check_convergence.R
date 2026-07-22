#' Check Convergence for a Regression Model
#'
#' Assesses model convergence and provides diagnostics
#' for each exposure (in univariate mode) or
#' for the full model (in multivariable mode),
#' depending on the regression approach used.
#'
#' @param data A data frame containing the dataset.
#' @param exposures A character vector of predictor variable names. Quoted names
#'   are recommended in scripts, and bare names are also accepted.
#'   If \code{multivariate = FALSE}, each exposure is assessed separately.
#'   If \code{multivariate = TRUE}, exposures are included together.
#' @param outcome A character string specifying the outcome variable. Quoted and
#'   bare names are accepted.
#' @param approach A character string specifying the regression approach.
#' One of:
#'   \code{"logit"}, \code{"logbinomial"}, \code{"poisson"},
#'   \code{"robpoisson"}, or \code{"negbin"}.
#' @param multivariate Logical. If \code{TRUE},
#' checks convergence for a multivariable model;
#'   otherwise, performs checks for each univariate model.
#' @param format Output format. One of \code{"flextable"} (default),
#'   \code{"gt"}, or \code{"tibble"}. Use \code{format = "tibble"} for the
#'   original data-frame style output.
#'
#' @return A data frame, \code{gt_tbl}, or \code{flextable} summarizing
#' convergence diagnostics, including:
#' \describe{
#'   \item{\code{Exposure}}{Name of the exposure variable.}
#'   \item{\code{Model}}{The regression approach used.}
#'   \item{\code{Converged}}{\code{TRUE} if the model converged successfully;
#'    \code{FALSE} otherwise.}
#'   \item{\code{Max.prob.}}{Maximum predicted probability or
#'   fitted value in the dataset.}
#' }
#'
#' @details
#' For \code{robpoisson}, predicted probabilities (fitted values) may exceed 1,
#' which is acceptable when estimating risk ratios but should not be interpreted
#' as actual probabilities.
#'
#' This function is useful for identifying convergence issues, especially for
#' \code{"logbinomial"} models, which often fail to converge.
#'
#' @seealso [identify_confounder()],  [interaction_models()]
#'
#' @examples
#' if (requireNamespace("gtregression", quietly = TRUE)) {
#'   data(data_PimaIndiansDiabetes, package = "gtregression")
#'
#'   check_convergence(
#'     data = data_PimaIndiansDiabetes,
#'     exposures = c("age", "mass"),
#'     outcome = "diabetes",
#'     approach = "logit"
#'   )
#'
#'   check_convergence(
#'     data = data_PimaIndiansDiabetes,
#'     exposures = c("age", "mass"),
#'     outcome = "diabetes",
#'     approach = "logit",
#'     multivariate = TRUE
#'   )
#' }
#' @export
check_convergence <- function(data,
                              exposures,
                              outcome,
                              approach = "logit",
                              multivariate = FALSE,
                              format = c("flextable", "gt", "tibble")) {

  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }
  exposures <- .vars_arg(substitute(exposures), env = parent.frame())
  outcome <- .vars_arg(substitute(outcome), env = parent.frame())
  if (!is.character(exposures) || length(exposures) < 1L ||
      anyNA(exposures) || any(!nzchar(exposures))) {
    stop("`exposures` must be a non-empty character vector.", call. = FALSE)
  }
  if (!is.character(outcome) || length(outcome) != 1L ||
      is.na(outcome) || !nzchar(outcome)) {
    stop("`outcome` must be a single character variable name.", call. = FALSE)
  }
  if (!is.logical(multivariate) || length(multivariate) != 1L ||
      is.na(multivariate)) {
    stop("`multivariate` must be TRUE or FALSE.", call. = FALSE)
  }
  format <- .choice_arg(
    substitute(format),
    env = parent.frame(),
    choices = c("flextable", "gt", "tibble")
  )
  format <- match.arg(format, c("flextable", "gt", "tibble"))

  missing_vars <- setdiff(c(outcome, exposures), names(data))
  if (length(missing_vars)) {
    stop("Variables not found: ", paste(missing_vars, collapse = ", "),
         call. = FALSE)
  }

  approach <- .choice_arg(
    substitute(approach),
    env = parent.frame(),
    choices = c("logit","logbinomial","poisson","robpoisson","negbin")
  )
  approach <- .normalize_approach(approach)

  .validate_approach(approach, context = "check_convergence")

  # Return early if dataset is empty

  if (nrow(data) == 0) {
    empty <- data.frame(
      Exposure = character(0),
      Model = character(0),
      Converged = logical(0),
      Max.prob. = numeric(0)
    )
    if (format == "tibble") {
      return(empty)
    }
    return(.build_check_convergence_table(empty, format = format))
  }

  .validate_outcome_by_approach(data[[outcome]], approach)

  fit_model <- function(fmla) {
    switch(approach,
      "logit" = stats::glm(fmla, data = data, family = stats::binomial("logit")),
      "logbinomial" = stats::glm(fmla, data = data, family = stats::binomial("log")),
      "poisson" = stats::glm(fmla, data = data, family = stats::poisson("log")),
      "negbin" = MASS::glm.nb(fmla, data = data),
      risks::riskratio(formula = fmla, data = data, approach = approach)
    )
  }

  extract_convergence <- function(fit) {
    if ("converged" %in% names(fit)) {
      return(isTRUE(fit$converged))
    }
    if (!is.null(fit$conv)) {
      return(isTRUE(fit$conv == 0))
    }
    NA
  }

  extract_max_prob <- function(fit) {
    max_prob <- if ("maxprob" %in% names(fit)) {
      fit$maxprob
    } else {
      max(stats::predict(fit, type = "response"), na.rm = TRUE)
    }
    as.numeric(max_prob)
  }

  warn_if_large_robpoisson <- function(max_prob) {
    if (approach == "robpoisson" && is.finite(max_prob) && max_prob > 1) {
      warning(
        "robpoisson: predicted fitted value exceeds 1; interpret estimates with caution.",
        call. = FALSE
      )
    }
  }

  results_list <- list()

  if (!multivariate) {
    # Loop through exposures and fit separate models
    for (exposure in exposures) {
      fmla <- stats::reformulate(exposure, response = outcome)
      result <- tryCatch({
          fit <- fit_model(fmla)
          # Extract convergence status and predicted max
          converged <- extract_convergence(fit)
          max_prob <- extract_max_prob(fit)
          # Warning if robpoisson max_prob > 1
          warn_if_large_robpoisson(max_prob)
          data.frame(Exposure = exposure, Model = approach,
                     Converged = converged, Max.prob. = max_prob)
        },
        error = function(e) {
          data.frame(Exposure = exposure, Model = approach,
                     Converged = FALSE, Max.prob. = NA)
        }
      )
      results_list[[exposure]] <- result
    }
  } else {
    # Build multivariable model
    fmla <- stats::reformulate(exposures, response = outcome)
    result <- tryCatch({
        fit <- fit_model(fmla)
        converged <- extract_convergence(fit)
        max_prob <- extract_max_prob(fit)

        # Custom warning if predicted prob > 1
        warn_if_large_robpoisson(max_prob)
        data.frame(Exposure = paste(exposures, collapse = " + "),
                   Model = approach, Converged = converged,
                   Max.prob. = max_prob)
      },
      error = function(e) {
        warning("Model fitting failed for the selected approach", call. = FALSE)
        data.frame(Exposure = NA,
                   Model = approach,
                   Converged = FALSE,
                   Max.prob. = NA)
      }
    )


    results_list[["multivariable"]] <- result
  }

  out <- do.call(rbind, results_list)
  if (format == "tibble") {
    return(out)
  }
  .build_check_convergence_table(out, format = format)
}

#' Build formatted convergence table
#' @keywords internal
#' @noRd
.build_check_convergence_table <- function(result,
                                           format = c("flextable", "gt")) {
  format <- match.arg(format, c("flextable", "gt"))
  note <- paste(
    "Screening aid only; inspect non-convergence, impossible fitted values,",
    "and model specification before interpreting estimates."
  )

  display <- result |>
    dplyr::mutate(
      Converged = dplyr::case_when(
        is.na(.data$Converged) ~ "",
        .data$Converged ~ "Yes",
        TRUE ~ "No"
      ),
      Max.prob. = dplyr::if_else(
        is.na(.data$Max.prob.),
        "",
        formatC(.data$Max.prob., digits = 3, format = "f")
      )
    )

  if (format == "gt") {
    return(
      gt::gt(display) |>
        gt::tab_header(title = "Convergence check") |>
        gt::cols_label(Max.prob. = "Max fitted value") |>
        gt::cols_align(align = "left", columns = c("Exposure", "Model")) |>
        gt::cols_align(align = "center", columns = c("Converged", "Max.prob.")) |>
        gt::tab_style(
          style = gt::cell_text(weight = "bold"),
          locations = gt::cells_column_labels()
        ) |>
        gt::tab_style(
          style = gt::cell_fill(color = "#e7f5ec"),
          locations = gt::cells_body(rows = .data$Converged == "Yes")
        ) |>
        gt::tab_style(
          style = gt::cell_fill(color = "#fde2e2"),
          locations = gt::cells_body(rows = .data$Converged == "No")
        ) |>
        gt::tab_source_note(gt::md(note))
    )
  }

  ft <- flextable::flextable(display)
  ft <- flextable::set_caption(ft, caption = "Convergence check")
  ft <- flextable::set_header_labels(ft, Max.prob. = "Max fitted value")
  ft <- flextable::align(ft, j = c("Exposure", "Model"), align = "left", part = "all")
  ft <- flextable::align(ft, j = c("Converged", "Max.prob."), align = "center", part = "all")
  ft <- flextable::bold(ft, part = "header", bold = TRUE)
  ft <- flextable::bg(ft, i = which(display$Converged == "Yes"), bg = "#e7f5ec", part = "body")
  ft <- flextable::bg(ft, i = which(display$Converged == "No"), bg = "#fde2e2", part = "body")
  ft <- flextable::add_footer_lines(ft, values = note)
  ft <- flextable::fontsize(ft, size = 8, part = "footer")
  ft <- flextable::italic(ft, italic = TRUE, part = "footer")
  flextable::autofit(ft)
}
