#' Plot adjusted survival curves from a Cox model
#'
#' Plot model-based survival curves from a fitted Cox proportional hazards
#' model at user-supplied or representative covariate profiles.
#'
#' @param model A fitted \code{coxph} model or a \code{cox_reg()} result
#'   fitted without \code{stratifier}. Models with a stratified baseline hazard
#'   (\code{strata()} terms, or \code{cox_reg(baseline_strata = )}) are
#'   supported; see Details.
#' @param model_name Model name or position when a \code{cox_reg()} result
#'   contains more than one fitted model.
#' @param newdata Optional data frame containing one covariate profile per
#'   curve. Row names are used as curve labels when available.
#' @param by Optional model variable used to generate one curve per observed
#'   level. Other model variables are held at representative values. Ignored
#'   when \code{newdata} is supplied.
#' @param conf.int Logical; display pointwise confidence bands.
#' @param break_time_by Optional numeric interval for x-axis breaks.
#' @param xlim Optional numeric vector of length 2 specifying x-axis limits.
#' @param xlab,ylab Axis labels.
#' @param title Optional plot title.
#' @param legend_title Legend title.
#' @param palette Optional character vector of curve colours.
#' @param base_size Base font size.
#'
#' @details
#' For a model with a stratified baseline hazard, each covariate profile has a
#' different survival curve in every baseline stratum. One curve is therefore
#' drawn per profile and stratum: with a single profile the strata are
#' distinguished by colour; with several profiles, colour identifies the
#' profile and line type identifies the stratum. The stratum variables are not
#' covariates and cannot be used in \code{by}. If \code{newdata} contains the
#' stratum variables, each row is drawn only for its own stratum.
#'
#' @return A \code{ggplot2} object with attributes \code{survfit},
#'   \code{plot_data}, and \code{prediction_data}. \code{plot_data} has one
#'   row per time point and curve, with columns \code{profile} and
#'   \code{stratum} identifying the curve.
#'
#' @examples
#' lung_data <- data_lungcancer
#' lung_data$trt <- factor(lung_data$trt, levels = c(1, 2),
#'                         labels = c("Standard", "Test"))
#' fit <- cox_reg(
#'   lung_data, time, status,
#'   exposures = c(trt, age, karno),
#'   multivariable = TRUE
#' )
#' plot_cox_survival(fit, by = trt)
#'
#' @export
plot_cox_survival <- function(model,
                              model_name = NULL,
                              newdata = NULL,
                              by = NULL,
                              conf.int = TRUE,
                              break_time_by = NULL,
                              xlim = NULL,
                              xlab = "Time",
                              ylab = "Adjusted survival probability",
                              title = "Adjusted Cox survival curves",
                              legend_title = "Profile",
                              palette = NULL,
                              base_size = 13) {
  fit <- .cox_curve_model(model, model_name)
  by_expr <- substitute(by)
  by <- if (is.null(by_expr) || identical(by_expr, quote(NULL))) {
    NULL
  } else if (is.symbol(by_expr)) {
    as.character(by_expr)
  } else {
    value <- eval(by_expr, envir = parent.frame())
    if (!is.character(value) || length(value) != 1L) {
      stop("`by` must be a single model variable.", call. = FALSE)
    }
    value
  }

  .validate_cox_curve_options(conf.int, break_time_by, xlim, base_size)
  prediction_data <- if (is.null(newdata)) {
    .cox_curve_profiles(fit, by = by)
  } else {
    if (!is.data.frame(newdata) || !nrow(newdata)) {
      stop("`newdata` must be a non-empty data frame.", call. = FALSE)
    }
    newdata
  }
  curve_labels <- rownames(prediction_data)
  if (is.null(curve_labels) || any(!nzchar(curve_labels)) ||
      identical(curve_labels, as.character(seq_len(nrow(prediction_data))))) {
    curve_labels <- if (is.null(by) || !by %in% names(prediction_data)) {
      paste("Profile", seq_len(nrow(prediction_data)))
    } else {
      as.character(prediction_data[[by]])
    }
  }
  rownames(prediction_data) <- make.unique(curve_labels)

  sf <- tryCatch(
    survival::survfit(fit, newdata = prediction_data, se.fit = conf.int),
    error = function(e) {
      stop("Could not calculate adjusted Cox curves: ", conditionMessage(e),
           call. = FALSE)
    }
  )
  strata_vars <- .cox_curve_strata_vars(fit)
  plot_data <- .tidy_cox_survfit(
    sf,
    labels = rownames(prediction_data),
    strata_in_newdata = length(strata_vars) > 0L &&
      all(strata_vars %in% names(prediction_data))
  )
  breaks <- .km_time_breaks(plot_data$time, break_time_by, xlim)
  # With several profiles in several baseline strata, colour shows the profile
  # and line type the stratum; otherwise every curve has its own colour.
  crossed <- nlevels(plot_data$profile) > 1L &&
    length(unique(stats::na.omit(plot_data$stratum))) > 1L &&
    nlevels(plot_data$curve) > nlevels(plot_data$profile)
  plot_data$colour_group <- if (crossed) plot_data$profile else plot_data$curve
  curve_levels <- levels(plot_data$colour_group)
  if (is.null(palette)) palette <- .km_default_palette(length(curve_levels))

  p <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(
      x = .data$time, y = .data$survival, color = .data$colour_group,
      group = .data$curve
    )
  )
  if (isTRUE(conf.int)) {
    ci <- plot_data[
      is.finite(plot_data$conf.low) & is.finite(plot_data$conf.high),
      , drop = FALSE
    ]
    p <- p + ggplot2::geom_ribbon(
      data = ci,
      ggplot2::aes(
        ymin = .data$conf.low, ymax = .data$conf.high,
        fill = .data$colour_group
      ),
      alpha = 0.16,
      colour = NA
    )
  }
  p <- p +
    (if (crossed) {
      ggplot2::geom_step(ggplot2::aes(linetype = .data$stratum), linewidth = 0.9)
    } else {
      ggplot2::geom_step(linewidth = 0.9)
    }) +
    ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    ggplot2::scale_x_continuous(breaks = breaks) +
    ggplot2::scale_color_manual(
      values = stats::setNames(palette[seq_along(curve_levels)], curve_levels)
    ) +
    ggplot2::scale_fill_manual(
      values = stats::setNames(palette[seq_along(curve_levels)], curve_levels),
      guide = "none"
    ) +
    ggplot2::labs(
      title = title, x = xlab, y = ylab,
      color = legend_title
    ) +
    ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      legend.position = "bottom",
      plot.title = ggplot2::element_text(face = "bold")
    ) +
    ggplot2::coord_cartesian(xlim = xlim, ylim = c(0, 1))
  if (crossed) {
    p <- p + ggplot2::labs(linetype = paste(strata_vars, collapse = ", "))
  }
  plot_data$colour_group <- NULL

  attr(p, "survfit") <- sf
  attr(p, "plot_data") <- plot_data
  attr(p, "prediction_data") <- prediction_data
  p
}

#' @keywords internal
#' @noRd
.cox_curve_model <- function(model, model_name = NULL) {
  if (inherits(model, "coxph")) return(model)
  if (!inherits(model, "gtregression") || !identical(model$source, "cox_reg")) {
    stop("`model` must be a fitted coxph model or non-stratified cox_reg() object.",
         call. = FALSE)
  }
  fits <- model$models
  if (!length(fits) || any(vapply(fits, is.list, logical(1)) &
                           !vapply(fits, inherits, logical(1), what = "coxph"))) {
    stop("Stratified cox_reg() results require a directly selected coxph model.",
         call. = FALSE)
  }
  index <- .check_ph_plot_index(
    model_name, names(fits), "model", required = length(fits) > 1L
  )
  fits[[index]]
}

#' @keywords internal
#' @noRd
.cox_curve_profiles <- function(fit, by = NULL) {
  response_vars <- all.vars(stats::formula(fit)[[2]])
  strata_vars <- .cox_curve_strata_vars(fit)
  # Baseline-hazard strata are not covariates: leaving them out of the profile
  # makes survfit() return the curve of each profile in every stratum.
  predictor_vars <- setdiff(
    all.vars(stats::formula(fit)), c(response_vars, strata_vars)
  )
  if (!length(predictor_vars)) {
    stop("The Cox model has no predictors for adjusted curves.", call. = FALSE)
  }
  if (!is.null(by) && by %in% strata_vars) {
    stop(
      "`by` cannot be a baseline-hazard stratum ('", by, "'): curves are ",
      "already drawn separately for each stratum. Use a model covariate or ",
      "omit `by`.",
      call. = FALSE
    )
  }
  if (!is.null(by) && !by %in% predictor_vars) {
    stop("`by` must be a variable in the fitted Cox model.", call. = FALSE)
  }

  mf <- fit$model
  if (is.null(mf)) {
    stop(
      "Automatic profiles need the model frame. Refit the Cox model with ",
      "`model = TRUE` or supply `newdata` explicitly.",
      call. = FALSE
    )
  }
  model_column <- function(var) {
    if (var %in% names(mf)) return(mf[[var]])
    hit <- grep(paste0("\\(", var, "\\)$"), names(mf))
    if (length(hit) == 1L) return(mf[[hit]])
    stop(
      "Automatic profiles are unavailable for transformed model term '", var,
      "'. Supply `newdata` explicitly.", call. = FALSE
    )
  }
  typical <- function(x) {
    if (is.numeric(x)) return(stats::median(x, na.rm = TRUE))
    if (is.logical(x)) return(names(which.max(table(x))) == "TRUE")
    xf <- factor(x)
    value <- names(which.max(table(xf)))
    factor(value, levels = levels(xf), ordered = is.ordered(x))
  }

  if (is.null(by)) {
    out <- data.frame(.profile = "Adjusted profile", stringsAsFactors = FALSE)
  } else {
    by_data <- model_column(by)
    values <- if (is.factor(by_data) || is.character(by_data) || is.logical(by_data)) {
      unique(stats::na.omit(by_data))
    } else {
      stop("Automatic `by` curves require a categorical variable.", call. = FALSE)
    }
    out <- data.frame(.profile = as.character(values), stringsAsFactors = FALSE)
  }

  for (var in predictor_vars) {
    source <- model_column(var)
    if (!is.null(by) && identical(var, by)) {
      values <- out$.profile
      out[[var]] <- if (is.factor(source)) {
        factor(values, levels = levels(source), ordered = is.ordered(source))
      } else if (is.logical(source)) {
        values == "TRUE"
      } else {
        values
      }
    } else {
      out[[var]] <- rep(typical(source), nrow(out))
    }
  }
  rownames(out) <- out$.profile
  out$.profile <- NULL
  out
}

#' @keywords internal
#' @noRd
.tidy_cox_survfit <- function(fit, labels, strata_in_newdata = FALSE) {
  survival <- as.matrix(fit$surv)
  lower <- if (is.null(fit$lower)) matrix(NA_real_, nrow(survival), ncol(survival)) else as.matrix(fit$lower)
  upper <- if (is.null(fit$upper)) matrix(NA_real_, nrow(survival), ncol(survival)) else as.matrix(fit$upper)
  one_curve <- function(rows, column, curve, profile, stratum) {
    data.frame(
      time = c(0, fit$time[rows]),
      survival = c(1, survival[rows, column]),
      conf.low = c(1, lower[rows, column]),
      conf.high = c(1, upper[rows, column]),
      curve = curve,
      profile = profile,
      stratum = stratum,
      stringsAsFactors = FALSE
    )
  }

  if (is.null(fit$strata)) {
    if (ncol(survival) != length(labels)) {
      labels <- colnames(survival) %||% paste("Profile", seq_len(ncol(survival)))
    }
    rows <- seq_len(nrow(survival))
    pieces <- lapply(seq_len(ncol(survival)), function(i) {
      one_curve(rows, i, labels[i], labels[i], NA_character_)
    })
    out <- do.call(rbind, pieces)
    out$curve <- factor(out$curve, levels = labels)
    out$profile <- factor(out$profile, levels = labels)
    return(out)
  }

  # Stratified baseline hazard: survfit() stacks the strata in the rows, so
  # each stratum block has to be cut out before a curve is built.
  block_names <- names(fit$strata)
  block_sizes <- as.integer(fit$strata)
  block_end <- cumsum(block_sizes)
  block_start <- block_end - block_sizes + 1L
  block_rows <- lapply(seq_along(block_sizes), function(k) {
    if (block_sizes[k] > 0L) seq.int(block_start[k], block_end[k]) else integer(0)
  })

  if (isTRUE(strata_in_newdata) && ncol(survival) == 1L) {
    # `newdata` named the stratum of every profile: one block per profile.
    curve_labels <- if (length(block_names) == length(labels)) labels else block_names
    pieces <- lapply(seq_along(block_rows), function(k) {
      one_curve(block_rows[[k]], 1L, curve_labels[k], curve_labels[k], NA_character_)
    })
    out <- do.call(rbind, pieces)
    out$curve <- factor(out$curve, levels = unique(curve_labels))
    out$profile <- factor(out$profile, levels = unique(curve_labels))
    return(out)
  }

  if (ncol(survival) != length(labels)) {
    labels <- colnames(survival) %||% paste("Profile", seq_len(ncol(survival)))
  }
  single_profile <- length(labels) == 1L
  curve_name <- function(i, k) {
    if (single_profile) block_names[k] else paste(labels[i], block_names[k], sep = ", ")
  }
  index <- expand.grid(
    k = seq_along(block_rows), i = seq_along(labels), KEEP.OUT.ATTRS = FALSE
  )
  pieces <- lapply(seq_len(nrow(index)), function(j) {
    i <- index$i[j]
    k <- index$k[j]
    one_curve(block_rows[[k]], i, curve_name(i, k), labels[i], block_names[k])
  })
  out <- do.call(rbind, pieces)
  out$curve <- factor(
    out$curve,
    levels = unique(vapply(
      seq_len(nrow(index)), function(j) curve_name(index$i[j], index$k[j]),
      character(1)
    ))
  )
  out$profile <- factor(out$profile, levels = labels)
  out$stratum <- factor(out$stratum, levels = block_names)
  out
}

#' Variables used in strata() terms of a Cox model
#' @keywords internal
#' @noRd
.cox_curve_strata_vars <- function(fit) {
  tt <- tryCatch(stats::terms(fit), error = function(e) NULL)
  if (is.null(tt)) return(character(0))
  special <- attr(tt, "specials")$strata
  if (!length(special)) return(character(0))
  variables <- attr(tt, "variables")
  unique(unlist(lapply(special, function(i) all.vars(variables[[i + 1L]]))))
}

#' @keywords internal
#' @noRd
.validate_cox_curve_options <- function(conf.int, break_time_by, xlim, base_size) {
  if (!is.logical(conf.int) || length(conf.int) != 1L || is.na(conf.int)) {
    stop("`conf.int` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.null(break_time_by) &&
      (!is.numeric(break_time_by) || length(break_time_by) != 1L ||
       is.na(break_time_by) || break_time_by <= 0)) {
    stop("`break_time_by` must be a positive number or NULL.", call. = FALSE)
  }
  if (!is.null(xlim) &&
      (!is.numeric(xlim) || length(xlim) != 2L || anyNA(xlim) ||
       xlim[1] >= xlim[2])) {
    stop("`xlim` must contain two increasing numbers or be NULL.", call. = FALSE)
  }
  if (!is.numeric(base_size) || length(base_size) != 1L || is.na(base_size) ||
      base_size <= 0) {
    stop("`base_size` must be a positive number.", call. = FALSE)
  }
  invisible(NULL)
}
