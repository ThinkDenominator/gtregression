#' Check collinearity using VIF for fitted models
#'
#' Computes Variance Inflation Factors (VIF) for fitted multivariable models
#' returned by \code{multi_reg()} or related functions.
#'
#' @param model A fitted model object returned by \code{multi_reg()},
#'   \code{stratified_multi_reg()}, or compatible \code{gtregression}
#'   functions. Univariable model objects are rejected because VIF is not
#'   applicable.
#' @param format Output format. One of \code{"flextable"} (default),
#'   \code{"gt"}, or \code{"tibble"}. Use \code{format = "tibble"} to preserve
#'   the original tibble or nested-list output.
#'
#' @return For multivariable models, a tibble if a single fitted model is
#' present, or a named list of tibbles if multiple fitted models are present.
#' With \code{format = "gt"} or \code{format = "flextable"}, leaf tibbles are
#' converted to formatted tables while preserving any list nesting. The tibble
#' contains:
#' \describe{
#'   \item{\code{Variable}}{Model term.}
#'   \item{\code{VIF}}{Variance inflation factor. For multi-degree-of-freedom
#'   terms, this is the adjusted GVIF: \code{GVIF^(1 / (2 * Df))}.}
#'   \item{\code{Interpretation}}{Simple interpretation based on common cut
#'   points: no collinearity, moderate, or high.}
#' }
#'
#' For univariate models, an error is raised indicating that VIF is not applicable.
#'
#' @details If the \pkg{car} package is installed, \code{check_collinearity()}
#' uses \code{car::vif()}. Otherwise, it computes VIF/GVIF from the fitted model
#' matrix so that diagnostics remain available without an additional dependency.
#'
#' @importFrom stats complete.cases cor model.matrix terms
#' @export
check_collinearity <- function(model,
                               format = c("flextable", "gt", "tibble")) {
  format <- .choice_arg(
    substitute(format),
    env = parent.frame(),
    choices = c("flextable", "gt", "tibble")
  )
  format <- match.arg(format, c("flextable", "gt", "tibble"))

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

  .empty_vif_table <- function() {
    tibble::tibble(
      Variable = character(0),
      VIF = numeric(0),
      Interpretation = character(0)
    )
  }

  .interpret_vif <- function(vif) {
    dplyr::case_when(
      is.na(vif) ~ NA_character_,
      vif < 2 ~ "No collinearity",
      vif >= 2 & vif < 5 ~ "Moderate",
      vif >= 5 ~ "High"
    )
  }

  .from_car_vif <- function(vif_vals) {
    if (is.matrix(vif_vals) || is.data.frame(vif_vals)) {
      vif_df <- as.data.frame(vif_vals)
      if (all(c("GVIF", "Df") %in% names(vif_df))) {
        vals <- vif_df$GVIF^(1 / (2 * vif_df$Df))
      } else {
        vals <- vif_df[[1]]
      }
      names(vals) <- rownames(vif_df)
      return(vals)
    }
    vif_vals
  }

  .manual_vif <- function(fit_model) {
    term_labels <- attr(stats::terms(fit_model), "term.labels")
    if (length(term_labels) < 2) return(stats::setNames(numeric(0), character(0)))

    mm <- stats::model.matrix(fit_model)
    assign <- attr(mm, "assign")
    keep <- assign > 0
    mm <- mm[, keep, drop = FALSE]
    assign <- assign[keep]

    if (ncol(mm) < 2) return(stats::setNames(numeric(0), character(0)))

    complete <- stats::complete.cases(mm)
    mm <- mm[complete, , drop = FALSE]
    non_constant <- apply(mm, 2, function(x) stats::var(x, na.rm = TRUE) > 0)
    mm <- mm[, non_constant, drop = FALSE]
    assign <- assign[non_constant]

    if (ncol(mm) < 2) return(stats::setNames(numeric(0), character(0)))

    cmat <- stats::cor(mm)
    if (anyNA(cmat)) return(NULL)

    out <- vapply(seq_along(term_labels), function(i) {
      cols <- which(assign == i)
      other <- which(assign != i)
      if (!length(cols) || !length(other)) return(NA_real_)

      if (length(cols) == 1L) {
        fit <- stats::lm.fit(x = cbind(1, mm[, other, drop = FALSE]),
                             y = mm[, cols])
        rss <- sum(fit$residuals^2)
        y <- mm[, cols]
        tss <- sum((y - mean(y))^2)
        if (!is.finite(tss) || tss <= 0) return(NA_real_)
        r_squared <- max(0, min(1, 1 - rss / tss))
        if (isTRUE(all.equal(r_squared, 1))) return(Inf)
        return(1 / (1 - r_squared))
      }

      det_term <- det(cmat[cols, cols, drop = FALSE])
      det_other <- det(cmat[other, other, drop = FALSE])
      det_all <- det(cmat)
      if (!is.finite(det_term) || !is.finite(det_other) ||
          !is.finite(det_all) || det_term <= 0 || det_other <= 0 ||
          det_all <= 0) {
        return(NA_real_)
      }
      gvif <- (det_term * det_other) / det_all
      gvif^(1 / (2 * length(cols)))
    }, numeric(1))

    stats::setNames(out, term_labels)
  }

  .make_vif_table <- function(fit_model, model_name = NULL) {
    if (!inherits(fit_model, c("glm", "lm", "negbin"))) {
      stop("Unsupported model type for VIF calculation.", call. = FALSE)
    }

    term_labels <- attr(stats::terms(fit_model), "term.labels")

    if (length(term_labels) < 2) {
      return(.empty_vif_table())
    }

    vif_vals <- NULL
    vif_fun <- getOption("gtregression.vif_fun", NULL)
    if (is.function(vif_fun)) {
      vif_vals <- tryCatch(
        .from_car_vif(vif_fun(fit_model)),
        error = function(e) NULL
      )
    } else if (requireNamespace("car", quietly = TRUE)) {
      vif_vals <- tryCatch(
        .from_car_vif(car::vif(fit_model)),
        error = function(e) NULL
      )
    }
    if (is.null(vif_vals)) {
      vif_vals <- tryCatch(
        .manual_vif(fit_model),
        error = function(e) NULL
      )
    }

    if (is.null(vif_vals)) {
      return(
        tibble::tibble(
          Variable = NA_character_,
          VIF = NA_real_,
          Interpretation = "VIF could not be computed"
        )
      )
    }

    vif_names <- names(vif_vals)
    vif_vals <- as.numeric(vif_vals)
    names(vif_vals) <- vif_names

    tibble::tibble(
      Variable = names(vif_vals),
      VIF = round(as.numeric(vif_vals), 2),
      Interpretation = .interpret_vif(as.numeric(vif_vals))
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
      out <- out[[1]]
      if (format == "tibble") {
        return(out)
      }
      return(.format_vif_output(out, format = format))
    }
    if (format == "tibble") {
      return(out)
    }
    return(.format_vif_output(out, format = format))
  }

  if (model_source == "multi_reg_nbin") {
    out <- lapply(names(model_list), function(nm) {
      .make_vif_table(model_list[[nm]], nm)
    })
    names(out) <- names(model_list)

    if (length(out) == 1) {
      out <- out[[1]]
      if (format == "tibble") {
        return(out)
      }
      return(.format_vif_output(out, format = format))
    }
    if (format == "tibble") {
      return(out)
    }
    return(.format_vif_output(out, format = format))
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
    if (format == "tibble") {
      return(out)
    }
    return(.format_vif_output(out, format = format))
  }

  stop("Unsupported model source for VIF calculation.", call. = FALSE)
}

#' Format VIF output recursively
#' @keywords internal
#' @noRd
.format_vif_output <- function(x, format = c("flextable", "gt")) {
  format <- match.arg(format, c("flextable", "gt"))

  if (is.data.frame(x)) {
    return(.build_check_collinearity_table(x, format = format))
  }

  if (is.list(x)) {
    return(lapply(x, .format_vif_output, format = format))
  }

  x
}

#' Build formatted VIF table
#' @keywords internal
#' @noRd
.build_check_collinearity_table <- function(result,
                                            format = c("flextable", "gt")) {
  format <- match.arg(format, c("flextable", "gt"))
  note <- paste(
    "Screening aid only; interpret VIF with model purpose, coding choices,",
    "sample size, and subject-matter knowledge."
  )

  display <- result |>
    dplyr::mutate(
      VIF = dplyr::if_else(
        is.na(.data$VIF),
        "",
        formatC(.data$VIF, digits = 2, format = "f")
      )
    )

  if (format == "gt") {
    return(
      gt::gt(display) |>
        gt::tab_header(title = "Collinearity check") |>
        gt::cols_align(align = "left", columns = c("Variable", "Interpretation")) |>
        gt::cols_align(align = "center", columns = "VIF") |>
        gt::tab_style(
          style = gt::cell_text(weight = "bold"),
          locations = gt::cells_column_labels()
        ) |>
        gt::tab_style(
          style = gt::cell_fill(color = "#e7f5ec"),
          locations = gt::cells_body(rows = .data$Interpretation == "No collinearity")
        ) |>
        gt::tab_style(
          style = gt::cell_fill(color = "#fff4d6"),
          locations = gt::cells_body(rows = .data$Interpretation == "Moderate")
        ) |>
        gt::tab_style(
          style = gt::cell_fill(color = "#fde2e2"),
          locations = gt::cells_body(rows = .data$Interpretation == "High")
        ) |>
        gt::tab_source_note(gt::md(note))
    )
  }

  ft <- flextable::flextable(display)
  ft <- flextable::set_caption(ft, caption = "Collinearity check")
  ft <- flextable::align(ft, j = c("Variable", "Interpretation"), align = "left", part = "all")
  ft <- flextable::align(ft, j = "VIF", align = "center", part = "all")
  ft <- flextable::bold(ft, part = "header", bold = TRUE)
  ft <- flextable::bg(
    ft,
    i = which(display$Interpretation == "No collinearity"),
    bg = "#e7f5ec",
    part = "body"
  )
  ft <- flextable::bg(
    ft,
    i = which(display$Interpretation == "Moderate"),
    bg = "#fff4d6",
    part = "body"
  )
  ft <- flextable::bg(
    ft,
    i = which(display$Interpretation == "High"),
    bg = "#fde2e2",
    part = "body"
  )
  ft <- flextable::add_footer_lines(ft, values = note)
  ft <- flextable::fontsize(ft, size = 8, part = "footer")
  ft <- flextable::italic(ft, italic = TRUE, part = "footer")
  flextable::autofit(ft)
}
