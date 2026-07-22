#' @importFrom stats AIC BIC anova as.formula binomial coef cooks.distance
#' @importFrom stats deviance glm glm.control lm logLik na.omit nobs poisson
#' @importFrom stats predict residuals shapiro.test
#' @importFrom utils globalVariables
#' @importFrom dplyr bind_rows case_when filter mutate
#' @importFrom tibble tibble
#' @importFrom broom.helpers tidy_plus_plus tidy_add_reference_rows
#' @importFrom utils head

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

#' Normalize quoted or bare variable-name arguments
#'
#' @keywords internal
#' @noRd
.vars_arg <- function(expr, env = parent.frame(), allow_null = FALSE) {
  if (identical(expr, quote(NULL))) {
    if (allow_null) {
      return(NULL)
    }
    stop("Variable names cannot be NULL.", call. = FALSE)
  }
  out <- .choice_arg(expr, env = env, lower = FALSE)
  if (is.null(out) && allow_null) {
    return(NULL)
  }
  out
}

#' Normalize regression approach spellings
#'
#' @keywords internal
#' @noRd
.normalize_approach <- function(approach) {
  approach <- tolower(as.character(approach))
  approach[approach == "log-binomial"] <- "logbinomial"
  approach
}
