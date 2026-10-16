#' @importFrom stats AIC BIC anova as.formula binomial coef cooks.distance
#' @importFrom stats deviance glm glm.control lm logLik na.omit nobs poisson
#' @importFrom stats predict residuals shapiro.test
#' @importFrom utils globalVariables
#' @importFrom dplyr bind_rows case_when filter mutate
#' @importFrom tibble tibble
#' @importFrom broom.helpers tidy_plus_plus tidy_add_reference_rows
#' @importFrom utils head
#' @importFrom rlang %||%

# Prevent global variable warnings
utils::globalVariables(c(
  ".data",
  "reference_row", "variable", "label_clean", "header_order", "row_id", "label",
  "estimate", "conf.low", "conf.high", "approach", "stat_n", "row_type", "level",
  "var_type", "p.value", "type", "ref"
))

#' Normalize quoted or bare option arguments
#'
#' @keywords internal
#' @noRd
.choice_arg <- function(expr,
                        env = parent.frame(),
                        choices = NULL,
                        aliases = NULL,
                        lower = TRUE) {
  normalize_one <- function(x) {
    if (is.character(x)) {
      out <- if (lower) tolower(x) else x
      if (!is.null(aliases)) {
        idx <- match(out, names(aliases))
        out[!is.na(idx)] <- unname(aliases[idx[!is.na(idx)]])
      }
      return(out)
    }

    if (is.symbol(x)) {
      name <- as.character(x)
      key <- if (lower) tolower(name) else name

      if (!is.null(aliases) && key %in% names(aliases)) {
        return(unname(aliases[[key]]))
      }

      choice_keys <- if (lower) tolower(choices) else choices
      if (!is.null(choices) && key %in% choice_keys) {
        return(if (lower) key else name)
      }

      if (exists(name, envir = env, inherits = TRUE)) {
        return(get(name, envir = env, inherits = TRUE))
      }

      return(if (lower) key else name)
    }

    if (is.call(x) && identical(x[[1L]], as.name("c"))) {
      return(unlist(lapply(as.list(x)[-1L], normalize_one), use.names = FALSE))
    }

    eval(x, envir = env)
  }

  out <- normalize_one(expr)
  if (is.character(out) && lower) {
    out <- tolower(out)
  }
  out
}

#' Value passed through a wrapper function for a bare variable-name symbol
#'
#' Bare names resolve to data columns first. The one exception is a wrapper
#' function that forwards its own argument or local variable, for example
#' `wrap <- function(dat, by) descriptive_table(dat, "x", by = by)`: there the
#' symbol `by` is bound, in the frame of a calling function, to the name of
#' another column, and the caller clearly means that value.
#'
#' Returns the character value when all of the following hold, otherwise
#' `NULL` (meaning "use the column called `name`"):
#' * `name` is bound in the frame of a function that is currently being
#'   evaluated (a formal argument or local variable of a calling function).
#'   Objects in the global environment (the workspace), in package namespaces
#'   or attached packages, and in environments used by `eval()`/`local()`
#'   never count;
#' * its value is a non-missing character vector (normally one string);
#' * every element is itself a column name of `data` and the value differs
#'   from `name`.
#'
#' @keywords internal
#' @noRd
.wrapper_passed_name <- function(name, env, data_names) {
  if (is.null(data_names) || !is.environment(env)) {
    return(NULL)
  }

  # First environment in the lexical chain that binds `name`.
  holder <- NULL
  current <- env
  repeat {
    if (identical(current, emptyenv()) || identical(current, globalenv()) ||
        identical(current, baseenv()) || isNamespace(current)) {
      break
    }
    env_name <- attr(current, "name", exact = TRUE)
    if (is.character(env_name) &&
        (startsWith(env_name, "package:") || startsWith(env_name, "imports:"))) {
      break
    }
    if (exists(name, envir = current, inherits = FALSE)) {
      holder <- current
      break
    }
    current <- parent.env(current)
  }
  if (is.null(holder)) {
    return(NULL)
  }

  # `holder` must be the evaluation frame of a calling function (a closure
  # call), not an environment handed to eval()/local().
  frames <- sys.frames()
  is_function_frame <- FALSE
  for (i in seq_along(frames)) {
    if (!identical(frames[[i]], holder)) next
    fun <- sys.function(i)
    if (is.function(fun) && !is.primitive(fun) &&
        identical(environment(fun), parent.env(holder))) {
      is_function_frame <- TRUE
      break
    }
  }
  if (!is_function_frame) {
    return(NULL)
  }

  value <- tryCatch(
    get(name, envir = holder, inherits = FALSE),
    error = function(e) NULL
  )
  if (is.character(value) && length(value) >= 1L && !anyNA(value) &&
      !identical(value, name) && all(value %in% data_names)) {
    return(value)
  }
  NULL
}

#' Normalize quoted or bare variable-name arguments
#'
#' Resolution rule for a bare symbol: if it is a column name in `data`
#' (`data_names`), the column is used, even when an unrelated object of the
#' same name exists in the workspace or in a package (for example `rank`,
#' `class`, `by`, `time`, `df`, `sample`). The only exception is a value
#' forwarded by a wrapper function; see `.wrapper_passed_name()`.
#'
#' @keywords internal
#' @noRd
.vars_arg <- function(expr,
                      env = parent.frame(),
                      allow_null = FALSE,
                      data_names = NULL) {
  if (identical(expr, quote(NULL))) {
    if (allow_null) {
      return(NULL)
    }
    stop("Variable names cannot be NULL.", call. = FALSE)
  }
  normalize_variable <- function(x) {
    if (is.symbol(x)) {
      name <- as.character(x)
      # A data-column name takes precedence over an unrelated object such as
      # the built-in `pressure` dataset.
      if (!is.null(data_names) && name %in% data_names) {
        passed <- .wrapper_passed_name(name, env, data_names)
        if (!is.null(passed)) {
          return(passed)
        }
        return(name)
      }
    }

    if (is.call(x) && identical(x[[1L]], as.name("c"))) {
      return(unlist(
        lapply(as.list(x)[-1L], normalize_variable),
        use.names = FALSE
      ))
    }

    .choice_arg(x, env = env, lower = FALSE)
  }

  out <- normalize_variable(expr)
  if (is.null(out) && allow_null) {
    return(NULL)
  }
  out
}

#' Single variable-name argument (time, event) with the wrapper rule
#'
#' Same rule as `.vars_arg()`: the data column wins unless a calling function
#' forwards the name of another column through a same-named argument.
#' @keywords internal
#' @noRd
.single_var_arg <- function(expr, data, env = parent.frame()) {
  if (is.symbol(expr) && as.character(expr) %in% names(data)) {
    passed <- .wrapper_passed_name(as.character(expr), env, names(data))
    if (!is.null(passed) && length(passed) == 1L) {
      return(passed)
    }
  }
  .cox_single_var_arg(expr, data = data, env = env)
}

#' Normalize quoted or bare interaction arguments
#'
#' @keywords internal
#' @noRd
.interaction_arg <- function(expr, env = parent.frame(), allow_null = TRUE) {
  if (identical(expr, quote(NULL))) {
    if (allow_null) {
      return(NULL)
    }
    stop("`interaction` cannot be NULL.", call. = FALSE)
  }

  if (is.character(expr)) {
    out <- expr
  } else if (is.symbol(expr)) {
    nm <- as.character(expr)
    if (exists(nm, envir = env, inherits = TRUE)) {
      out <- get(nm, envir = env, inherits = TRUE)
    } else {
      out <- nm
    }
  } else if (is.call(expr)) {
    if (as.character(expr[[1L]]) %in% c("*", ":")) {
      out <- paste(gsub("`", "", vapply(as.list(expr)[-1L], deparse, character(1)), fixed = TRUE),
                   collapse = as.character(expr[[1L]]))
    } else {
      out <- eval(expr, envir = env)
    }
  } else {
    out <- eval(expr, envir = env)
  }

  if (is.null(out) && allow_null) {
    return(NULL)
  }
  if (!is.character(out) || length(out) != 1L || is.na(out) || !nzchar(out)) {
    stop("`interaction` must be a single character string such as 'bmi*sex'.", call. = FALSE)
  }
  gsub("\\s+", "", out)
}

#' Normalize regression approach spellings
#'
#' @keywords internal
#' @noRd
.normalize_approach <- function(approach) {
  approach <- tolower(as.character(approach))
  approach[approach == "log-binomial"] <- "logbinomial"
  approach[approach %in% c("surv", "survival", "surv_reg")] <- "survreg"
  approach
}

#' Normalize parametric survival distribution arguments
#'
#' @keywords internal
#' @noRd
.surv_distribution_arg <- function(expr,
                                   env = parent.frame(),
                                   multiple = FALSE,
                                   arg = "distribution") {
  choices <- c("weibull", "exponential", "lognormal", "loglogistic")
  aliases <- c(
    "exp" = "exponential",
    "log-normal" = "lognormal",
    "log normal" = "lognormal",
    "log_normal" = "lognormal",
    "log-logistic" = "loglogistic",
    "log logistic" = "loglogistic",
    "log_logistic" = "loglogistic"
  )

  out <- .choice_arg(
    expr,
    env = env,
    choices = choices,
    aliases = aliases
  )

  if (!is.character(out) || anyNA(out) || any(!out %in% choices) ||
      length(out) < 1L || (!multiple && length(out) != 1L)) {
    if (identical(arg, "distributions")) {
      stop(
        "`distributions` must contain one or more of: ",
        paste(choices, collapse = ", "),
        call. = FALSE
      )
    }

    stop(
      "Invalid distribution. Choose one of: ",
      paste(choices, collapse = ", "),
      call. = FALSE
    )
  }

  out
}
