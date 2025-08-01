#' Dissect a Dataset Before Regression
#'
#' Returns a tidy summary of each variable's structure, missingness, uniqueness,
#' and suitability for use in regression models.
#'
#' @param data A data frame.
#'
#' @return A tibble with columns: Variable, Type, Missing (%), Unique, Levels,
#' and Regression Hint.
#' @export
#'
#' @examples
#' dissect(data_birthwt)
dissect <- function(data) {
  if (!is.data.frame(data)) {
    stop("The input to `dissect()` must be a data frame. ",
         "You provided an object of class: ", class(data)[1], call. = FALSE)
  }


  summarize_column <- function(x, name) {
    x_non_na <- x[!is.na(x)]
    type <- class(x)[1]
    unique_vals <- length(unique(x_non_na))
    pct_missing <- round(mean(is.na(x)) * 100, 1)

    # Levels or sample values
    if (is.factor(x)) {
      lvls <- levels(x)
      levels_out <- paste(head(lvls, 5), collapse = ", ")
      if (length(lvls) > 5) levels_out <- paste0(levels_out, ", ...")
    } else if (is.character(x) || is.logical(x)) {
      lvls <- unique(x_non_na)
      levels_out <- paste(head(lvls, 5), collapse = ", ")
      if (length(lvls) > 5) levels_out <- paste0(levels_out, ", ...")
    } else {
      levels_out <- "-"
    }

    # Compatibility tag only
    if (all(is.na(x))) {
      comp <- "incompatible"
    } else if (unique_vals == 1) {
      comp <- "incompatible"
    } else if (is.numeric(x)) {
      comp <- if (unique_vals > 2) "compatible" else "maybe"
    } else if (is.factor(x)) {
      comp <- if (unique_vals > 5) "maybe" else "compatible"
    } else if (is.character(x)) {
      comp <- if (unique_vals >5) "maybe" else "compatible"
    } else if (inherits(x, "Date")) {
      comp <- "maybe"
    } else if (is.logical(x)) {
      comp <- "compatible"
    } else {
      comp <- "incompatible"
    }

    tibble::tibble(
      Variable = name,
      Type = type,
      `Missing (%)` = paste0(pct_missing, "%"),
      Unique = unique_vals,
      Levels = levels_out,
      Compatibility = comp
    )
  }

  result <- purrr::map_dfr(names(data), ~ summarize_column(data[[.x]], .x))

  print(result)

  message("\nInterpretation notes:\n",
      "- compatible: ready to use in regression\n",
      "- maybe: require transformation to factor or check no of levels\n",
      "- incompatible: not usable as-is (e.g., all NA, <2 levels)\n")

  invisible(result)
}
