#' Run core multivariable regression workflow
#'
#' @keywords internal
#' @noRd
.run_multi_core <- function(data,
                            outcome,
                            exposures,
                            approach,
                            adjust_for = NULL,
                            interaction = NULL) {

  adjusted_mode <- !is.null(adjust_for) && length(adjust_for) > 0

  data_clean <- .validate_multi_inputs(
    data = data,
    outcome = outcome,
    exposures = exposures,
    approach = approach,
    adjust_for = adjust_for,
    interaction = interaction
  )

  if (!adjusted_mode) {
    fit <- .fit_multi_model(
      data = data_clean,
      outcome = outcome,
      exposures = exposures,
      approach = approach,
      adjust_for = NULL,
      interaction = interaction
    )

    if (is.null(fit)) {
      stop("Model fitting failed.", call. = FALSE)
    }

    td <- .tidy_multi(fit, exposures, approach)

    if (is.null(td) || !nrow(td)) {
      stop("No estimable coefficients for supplied exposures.", call. = FALSE)
    }

    model_list <- list(multivariable_model = fit)
    model_summaries <- list(multivariable_model = summary(fit))
    reg_diagnostics <- list(
      multivariable_model = if (approach == "linear") {
        .reg_check_linear(fit, exposure = "multivariable_model")
      } else {
        "Regression diagnostics available only for 'linear' models."
      }
    )

    n_used <- tryCatch(stats::nobs(fit), error = function(e) NA_integer_)

  } else {
    fits <- vector("list", length(exposures))
    names(fits) <- exposures

    tds <- vector("list", length(exposures))
    names(tds) <- exposures

    for (i in seq_along(exposures)) {
      exp_i <- exposures[i]

      fit_i <- .fit_multi_model(
        data = data_clean,
        outcome = outcome,
        exposures = exp_i,
        approach = approach,
        adjust_for = adjust_for,
        interaction = interaction
      )

      if (is.null(fit_i)) {
        stop(
          paste0("Model fitting failed for exposure '", exp_i, "'."),
          call. = FALSE
        )
      }

      td_i <- .tidy_multi(fit_i, exp_i, approach)

      if (is.null(td_i) || !nrow(td_i)) {
        stop(
          paste0("No estimable coefficients for exposure '", exp_i, "'."),
          call. = FALSE
        )
      }

      fits[[i]] <- fit_i
      tds[[i]] <- td_i
    }

    td <- do.call(rbind, tds)

    model_list <- fits
    model_summaries <- lapply(fits, summary)
    reg_diagnostics <- lapply(
      seq_along(fits),
      function(i) {
        fit_i <- fits[[i]]
        exp_i <- names(fits)[i]
        if (approach == "linear") {
          .reg_check_linear(fit_i, exposure = exp_i)
        } else {
          "Regression diagnostics available only for 'linear' models."
        }
      }
    )
    names(reg_diagnostics) <- names(fits)

    n_used <- nrow(data_clean)
  }

  list(
    data_clean = data_clean,
    table_body = td,
    models = model_list,
    model_summaries = model_summaries,
    reg_check = reg_diagnostics,
    n_used = n_used,
    adjusted_mode = adjusted_mode
  )
}
