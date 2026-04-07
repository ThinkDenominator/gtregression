#' Check collinearity using VIF for fitted models
#'
#' Computes Variance Inflation Factors (VIF) for fitted multivariable models
#' returned by \code{multi_reg()} or related functions.
#'
#' @param model A fitted model object returned by \code{uni_reg()},
#'   \code{multi_reg()}, \code{uni_reg_nbin()}, \code{multi_reg_nbin()},
#'   or compatible \code{gtregression} functions.
#'
#' @return
#' For multivariable models, a tibble if a single fitted model is present,
#' or a named list of tibbles if multiple fitted models are present.
#'
#' For univariate models, an error is raised indicating that VIF is not applicable.
#'
#' @importFrom stats terms
#' @export
check_collinearity <- function(model) {
  if (!requireNamespace("car", quietly = TRUE)) {
    stop("Package 'car' is required.", call. = FALSE)
  }

  valid_sources <- c(
    "uni_reg", "multi_reg", "uni_reg_nbin", "multi_reg_nbin",
    "stratified_multi_reg"
  )

  if (!inherits(model, "gtregression")) {
    stop("Input must be a gtregression model object.", call. = FALSE)
  }

  model_source <- model$source
  model_list <- model$models

  if (is.null(model_source) || !(model_source %in% valid_sources)) {
    stop(
      "Input must be a fitted model from uni_reg, multi_reg, uni_reg_nbin, ",
      "multi_reg_nbin, or stratified_multi_reg.",
      call. = FALSE
    )
  }

  if (is.null(model_list) || !length(model_list)) {
    stop("Model list not found in object. Cannot compute VIF.", call. = FALSE)
  }

  if (model_source %in% c("uni_reg", "uni_reg_nbin")) {
    stop(
      "VIF is not applicable for univariate models. ",
      "Use multi_reg() to check collinearity among predictors.",
      call. = FALSE
    )
  }

  .make_vif_table <- function(fit_model, model_name = NULL) {
    if (!inherits(fit_model, c("glm", "lm", "negbin"))) {
      return(NULL)
    }

    term_labels <- attr(stats::terms(fit_model), "term.labels")

    if (length(term_labels) < 2) {
      return(
        tibble::tibble(
          Variable = character(0),
          VIF = numeric(0),
          Interpretation = character(0)
        )
      )
    }

    vif_vals <- tryCatch(
      car::vif(fit_model),
      error = function(e) NULL
    )

    if (is.null(vif_vals)) {
      return(
        tibble::tibble(
          Variable = NA_character_,
          VIF = NA_real_,
          Interpretation = "VIF could not be computed"
        )
      )
    }

    if (is.matrix(vif_vals)) {
      vif_vals <- vif_vals[, 1]
    }

    tibble::tibble(
      Variable = names(vif_vals),
      VIF = round(as.numeric(vif_vals), 2),
      Interpretation = dplyr::case_when(
        as.numeric(vif_vals) < 2 ~ "No collinearity",
        as.numeric(vif_vals) >= 2 & as.numeric(vif_vals) < 5 ~ "Moderate",
        as.numeric(vif_vals) >= 5 ~ "High",
        TRUE ~ NA_character_
      )
    )
  }

  # multi_reg default mode: usually list(multivariable_model = fit)
  # multi_reg adjusted mode: named list of fits
  # stratified_multi_reg: nested list by stratum -> list(multivariable_model=fit) or more

  if (model_source == "multi_reg") {
    out <- lapply(names(model_list), function(nm) {
      .make_vif_table(model_list[[nm]], nm)
    })
    names(out) <- names(model_list)

    if (length(out) == 1) {
      return(out[[1]])
    }
    return(out)
  }

  if (model_source == "multi_reg_nbin") {
    out <- lapply(names(model_list), function(nm) {
      .make_vif_table(model_list[[nm]], nm)
    })
    names(out) <- names(model_list)

    if (length(out) == 1) {
      return(out[[1]])
    }
    return(out)
  }

  if (model_source == "stratified_multi_reg") {
    out <- lapply(names(model_list), function(stratum_name) {
      stratum_models <- model_list[[stratum_name]]

      vif_stratum <- lapply(names(stratum_models), function(nm) {
        .make_vif_table(stratum_models[[nm]], nm)
      })
      names(vif_stratum) <- names(stratum_models)

      if (length(vif_stratum) == 1) {
        vif_stratum[[1]]
      } else {
        vif_stratum
      }
    })
    names(out) <- names(model_list)
    return(out)
  }

  stop("Unsupported model source for VIF calculation.", call. = FALSE)
}
