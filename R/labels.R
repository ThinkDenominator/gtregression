#' Get a display label from a variable label attribute
#'
#' @keywords internal
#' @noRd
.var_label_one <- function(data, var) {
  if (is.null(data) || !var %in% names(data)) {
    return(var)
  }
  lab <- attr(data[[var]], "label", exact = TRUE)
  if (is.null(lab) || length(lab) < 1L || is.na(lab[1]) || !nzchar(as.character(lab[1]))) {
    return(var)
  }
  as.character(lab[1])
}

#' Build a named variable-label map
#'
#' @keywords internal
#' @noRd
.var_label_map <- function(data, vars) {
  vars <- unique(as.character(vars))
  stats::setNames(vapply(vars, .var_label_one, character(1), data = data), vars)
}

#' Look up a display label from a named map
#'
#' @keywords internal
#' @noRd
.label_var <- function(var, label_map = NULL) {
  if (is.null(label_map) || !length(label_map) || is.na(match(var, names(label_map)))) {
    return(var)
  }
  out <- unname(label_map[[var]])
  if (is.null(out) || is.na(out) || !nzchar(out)) var else out
}

#' Attach raw row metadata to display data frames
#'
#' @keywords internal
#' @noRd
.attach_display_metadata <- function(display_df, row_exposure = NULL, variable_labels = NULL) {
  if (!is.null(row_exposure) && length(row_exposure) == nrow(display_df)) {
    attr(display_df, "row_exposure") <- as.character(row_exposure)
  }
  if (!is.null(variable_labels)) {
    attr(display_df, "variable_labels") <- variable_labels
  }
  display_df
}
