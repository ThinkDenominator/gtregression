# Plain-language checks used by the gtregression app (app and web modes).
#
# None of these change an analysis. They describe the data the user has chosen
# before a model is run, translate model warnings after it is run, and explain
# what happened on import.

# ---- Model warnings ----------------------------------------------------------

gtx_friendly_warning <- function(msg) {
  msg <- unique(trimws(gsub("[[:space:]]+", " ", as.character(msg))))
  msg <- msg[nzchar(msg)]
  if (!length(msg)) return(character(0))
  rules <- list(
    list("fitted probabilities numerically 0 or 1",
         "Some categories have all or no events (separation), so their estimates may be extreme or unreliable. Firth logistic regression, or merging sparse categories, usually helps."),
    list("algorithm did not converge|failed to converge|did not converge|convergence failure|iteration limit reached|Model failed to converge",
         "The model did not fully converge, so its estimates may be unreliable. Try fewer variables, merge sparse categories, or use a simpler model."),
    list("Loglik converged before variable|coefficient may be infinite",
         "At least one estimate is effectively infinite (a group with no events, or only events). Merge sparse categories or report the group descriptively."),
    list("fitted rates numerically 0",
         "Some groups have no events, so their rate estimates may be extreme."),
    list("singular fit|boundary \\(singular\\) fit",
         "The random-effect variance was estimated as zero (singular fit). The clustering may not add information; interpret the multilevel part with care."),
    list("Hessian is singular|non-positive definite|NaNs produced",
         "Some standard errors could not be estimated reliably. Check for sparse categories or variables that duplicate each other."),
    list("prediction from rank-deficient fit|not defined because of singularities|rank deficient",
         "Some variables carry the same information as others (collinearity), so not every estimate could be calculated.")
  )
  out <- vapply(msg, function(m) {
    for (rule in rules) if (grepl(rule[[1]], m, ignore.case = TRUE)) return(rule[[2]])
    m
  }, character(1), USE.NAMES = FALSE)
  unique(out)
}

# Evaluate an analysis call and keep its warnings and messages with the result
# (attribute "gtx_notes") instead of printing them to the console.
gtx_collect <- function(expr) {
  warnings <- character(0)
  notes <- character(0)
  value <- withCallingHandlers(
    expr,
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    },
    message = function(m) {
      text <- trimws(conditionMessage(m))
      if (nzchar(text) && !grepl("^(Running |>|Stratum)", text)) notes <<- c(notes, text)
      invokeRestart("muffleMessage")
    }
  )
  if (!is.null(value) && (length(warnings) || length(notes))) {
    attr(value, "gtx_notes") <- list(
      warnings = gtx_friendly_warning(warnings),
      messages = unique(notes)
    )
  }
  value
}

# ---- Data checks before a model is run --------------------------------------

gtx_binary_approaches <- c("logit", "firth", "logbinomial", "robpoisson")

gtx_is_binary <- function(x) {
  values <- unique(x[!is.na(x)])
  if (is.logical(x)) return(TRUE)
  if (is.numeric(x)) return(length(values) <= 2 && (all(values %in% c(0, 1)) || all(values %in% c(1, 2))))
  length(values) == 2
}

gtx_event_indicator <- function(x) {
  if (is.logical(x)) return(x)
  if (is.numeric(x)) {
    values <- sort(unique(x[!is.na(x)]))
    return(x == max(values))
  }
  levels <- if (is.factor(x)) levels(droplevels(x)) else sort(unique(as.character(x[!is.na(x)])))
  as.character(x) == levels[length(levels)]
}

gtx_looks_like_codes <- function(x) {
  if (!is.numeric(x)) return(FALSE)
  values <- unique(x[!is.na(x)])
  length(values) >= 3 && length(values) <= 6 && all(values == floor(values)) &&
    all(values >= 0) && max(values) <= 9
}

# Character vector of advisories (empty when nothing needs attention).
gtx_model_checks <- function(data, outcome = NULL, exposures = character(0),
                             adjust_for = character(0), approach = NULL,
                             time = NULL, event = NULL, multivariable = FALSE) {
  if (!is.data.frame(data)) return(character(0))
  out <- character(0)
  predictors <- unique(c(exposures, adjust_for))
  predictors <- predictors[predictors %in% names(data)]
  roles <- c(outcome, time, event)
  roles <- roles[!is.null(roles) & nzchar(roles) & roles %in% names(data)]
  predictors <- setdiff(predictors, roles)
  vars <- unique(c(roles, predictors))
  if (!length(vars)) return(character(0))

  complete <- stats::complete.cases(data[, vars, drop = FALSE])
  n_complete <- sum(complete)
  if (n_complete == 0) {
    return("No row has a value for every selected variable, so the model cannot be fitted. Remove variables with many missing values.")
  }
  dropped <- nrow(data) - n_complete
  if (dropped > 0) {
    share <- dropped / nrow(data)
    out <- c(out, sprintf(
      "%s of %s rows have a missing value in the selected variables and will be left out%s.",
      format(dropped, big.mark = ","), format(nrow(data), big.mark = ","),
      if (share >= 0.2) " (a large share; consider whether the missing data could bias the results)" else ""
    ))
  }
  d <- data[complete, , drop = FALSE]

  event_flag <- NULL
  if (!is.null(outcome) && nzchar(outcome) && outcome %in% names(d) && !is.null(approach)) {
    y <- d[[outcome]]
    n_values <- length(unique(y[!is.na(y)]))
    if (approach %in% gtx_binary_approaches) {
      if (!gtx_is_binary(y)) {
        out <- c(out, sprintf(
          "The outcome '%s' has %d different values; this approach needs a two-category outcome (such as yes/no or 0/1). Choose a binary outcome, or Linear for a continuous one.",
          outcome, n_values
        ))
      } else {
        event_flag <- gtx_event_indicator(y)
      }
    } else if (approach %in% c("poisson", "negbin")) {
      if (!is.numeric(y) || any(y < 0 | y != floor(y))) {
        out <- c(out, sprintf("The outcome '%s' is not a count (whole numbers of zero or more), which this approach needs.", outcome))
      }
    } else if (identical(approach, "linear")) {
      if (!is.numeric(y)) {
        out <- c(out, sprintf("The outcome '%s' is not numeric; linear regression needs a numeric outcome.", outcome))
      } else if (n_values <= 2) {
        out <- c(out, sprintf("The outcome '%s' has only %d values; for a yes/no outcome, logistic or robust Poisson regression is usually more suitable.", outcome, n_values))
      }
    }
  }
  if (!is.null(event) && nzchar(event) && event %in% names(d)) {
    e <- d[[event]]
    if (gtx_is_binary(e)) {
      event_flag <- gtx_event_indicator(e)
    } else {
      out <- c(out, sprintf("The event variable '%s' should have two values (event and censored), for example 1 and 0.", event))
    }
  }
  if (!is.null(time) && nzchar(time) && time %in% names(d)) {
    tt <- d[[time]]
    if (!is.numeric(tt)) {
      out <- c(out, sprintf("The time variable '%s' is not numeric.", time))
    } else if (any(tt < 0)) {
      out <- c(out, sprintf("The time variable '%s' has negative values.", time))
    }
  }

  for (v in predictors) {
    x <- d[[v]]
    values <- unique(x[!is.na(x)])
    if (length(values) < 2) {
      out <- c(out, sprintf("'%s' has the same value in every analysed row, so it cannot be estimated. Remove it.", v))
      next
    }
    categorical <- is.factor(x) || is.character(x) || is.logical(x)
    if (categorical && length(values) > 10) {
      out <- c(out, sprintf("'%s' has %d categories; estimates will be imprecise. Consider grouping categories in Data Prep.", v, length(values)))
    }
    if (gtx_looks_like_codes(x) && !v %in% c(time)) {
      out <- c(out, sprintf(
        "'%s' is stored as numbers (%s) and will be treated as continuous. If these are categories, change its type to categorical in Data Prep.",
        v, paste(sort(values), collapse = ", ")
      ))
    }
    if (categorical && !is.null(event_flag)) {
      groups <- as.character(x)
      events <- tapply(event_flag, groups, sum)
      totals <- tapply(event_flag, groups, length)
      empty <- names(events)[events == 0 | events == totals]
      if (length(empty)) {
        out <- c(out, sprintf(
          "In '%s', the categor%s %s %s all or no events, so %s cannot be estimated reliably. Merge sparse categories%s.",
          v, if (length(empty) == 1) "y" else "ies", paste(sprintf("'%s'", empty), collapse = ", "),
          if (length(empty) == 1) "has" else "have", if (length(empty) == 1) "its estimate" else "their estimates",
          if (!is.null(approach) && approach %in% c("logit", "logbinomial", "robpoisson")) " or use Firth logistic regression" else ""
        ))
      }
    }
  }

  if (isTRUE(multivariable) && !is.null(event_flag) && length(predictors)) {
    terms <- sum(vapply(predictors, function(v) {
      x <- d[[v]]
      if (is.factor(x) || is.character(x) || is.logical(x)) max(length(unique(x[!is.na(x)])) - 1, 1) else 1
    }, numeric(1)))
    events <- min(sum(event_flag), sum(!event_flag))
    if (!is.null(event) && nzchar(event)) events <- sum(event_flag)
    if (terms > 0 && events / terms < 10) {
      out <- c(out, sprintf(
        "There are %d events for %d model terms (fewer than 10 per term); the multivariable estimates may be unstable. Consider fewer variables.",
        events, terms
      ))
    }
  }
  unique(out)
}

# ---- Import ------------------------------------------------------------------

gtx_import_notes <- function(data, original_names = NULL, ext = NULL, path = NULL, sheet = NULL) {
  out <- character(0)
  if (!is.data.frame(data)) return(out)
  ext <- tolower(ext %||% "")
  if (nrow(data) == 0) {
    out <- c(out, "The file was read but contains no data rows. Check that the right file or sheet was chosen.")
  }
  if (ncol(data) == 1 && ext %in% c("csv", "tsv", "txt") && grepl("[;,\t|]", names(data)[1])) {
    out <- c(out, "Only one column was found, and its name contains separators. Choose the correct separator (for example semicolon) and load the file again.")
  }
  empty <- names(data)[vapply(data, function(x) all(is.na(x) | (is.character(x) & !nzchar(trimws(x)))), logical(1))]
  if (length(empty)) {
    out <- c(out, sprintf(
      "%d column%s %s completely empty: %s.",
      length(empty), if (length(empty) == 1) "" else "s", if (length(empty) == 1) "is" else "are",
      paste(utils::head(empty, 5), collapse = ", ")
    ))
  }
  if (!is.null(original_names) && length(original_names) == ncol(data)) {
    changed <- which(as.character(original_names) != names(data))
    if (length(changed)) {
      examples <- sprintf("'%s' is now '%s'", original_names[changed], names(data)[changed])
      out <- c(out, sprintf(
        "Some column names were adjusted so they can be used in R code: %s%s.",
        paste(utils::head(examples, 3), collapse = "; "),
        if (length(changed) > 3) sprintf(" and %d more", length(changed) - 3) else ""
      ))
    }
  }
  if (ext %in% c("xls", "xlsx") && is.null(sheet) && !is.null(path) &&
      requireNamespace("readxl", quietly = TRUE)) {
    sheets <- tryCatch(readxl::excel_sheets(path), error = function(e) character(0))
    if (length(sheets) > 1) {
      out <- c(out, sprintf(
        "This workbook has %d sheets; the first sheet ('%s') was imported. Enter a sheet number or name to import another.",
        length(sheets), sheets[1]
      ))
    }
  }
  out
}

# ---- Saved sessions ----------------------------------------------------------

# Inputs worth saving: values a user set, not buttons, uploads or internals.
gtx_session_inputs <- function(inputs) {
  keep <- vapply(names(inputs), function(id) {
    value <- inputs[[id]]
    if (inherits(value, "shinyActionButtonValue")) return(FALSE)
    if (is.data.frame(value) || is.list(value)) return(FALSE)
    if (grepl("^\\.|^data_prep-|^gtx_|_rows_|_cell_|_search$|_state$|_columns_selected$|^plotly", id)) return(FALSE)
    if (id %in% c("data_file", "restore_session_file", "environment_dataset_name", "dataset_name")) return(FALSE)
    TRUE
  }, logical(1))
  inputs[keep]
}

# ---- Results kept in a session -------------------------------------------------

# Approximate memory held by a result, not counting the analysis data that
# fitted models only point to.
gtx_result_bytes <- function(x) {
  drop_data <- function(obj, depth = 0L) {
    if (depth > 6L || !is.list(obj) || is.data.frame(obj)) return(obj)
    if (!is.null(oldClass(obj)) && !inherits(obj, "gtregression") && is.data.frame(obj[["data"]])) {
      obj[["data"]] <- NULL
    }
    for (i in seq_along(obj)) {
      if (is.list(obj[[i]]) && !is.data.frame(obj[[i]])) obj[[i]] <- drop_data(obj[[i]], depth + 1L)
    }
    obj
  }
  tryCatch(as.numeric(utils::object.size(drop_data(x))), error = function(e) 0)
}

# A result without its fitted models: still a complete table for merging,
# modifying, plotting and export, but not for model diagnostics.
gtx_compact_result <- function(x) {
  strip <- function(obj) {
    if (!is.list(obj) || is.data.frame(obj)) return(obj)
    obj$models <- NULL
    obj$model_summaries <- NULL
    for (part in c("per_stratum", "per_outcome")) {
      if (is.list(obj[[part]])) obj[[part]] <- lapply(obj[[part]], strip)
    }
    obj
  }
  out <- strip(x)
  attr(out, "gtx_compact") <- TRUE
  out
}
