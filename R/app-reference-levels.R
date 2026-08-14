# Internal helpers shared by the gtregression app and its tests.

gtx_reference_candidates <- function(data, predictors, outcome = NULL) {
  predictors <- setdiff(intersect(as.character(predictors), names(data)), outcome)
  keep <- vapply(data[predictors], function(x) {
    is_categorical <- is.factor(x) || is.character(x) || is.logical(x)
    is_categorical && length(unique(as.character(x[!is.na(x)]))) >= 2L
  }, logical(1))
  predictors <- predictors[keep]
  stats::setNames(lapply(predictors, function(variable) {
    x <- data[[variable]]
    values <- as.character(x[!is.na(x)])
    if (is.factor(x)) {
      levels(x)[levels(x) %in% values]
    } else {
      unique(values)
    }
  }), predictors)
}

gtx_relevel_predictors <- function(data, references) {
  if (!length(references)) return(data)
  for (variable in names(references)) {
    if (!variable %in% names(data)) stop("Unknown reference variable: ", variable, call. = FALSE)
    x <- data[[variable]]
    if (!(is.factor(x) || is.character(x) || is.logical(x))) {
      stop("Reference categories can only be set for categorical predictors: ", variable, call. = FALSE)
    }
    observed <- as.character(x[!is.na(x)])
    levels <- if (is.factor(x)) levels(x)[levels(x) %in% observed] else unique(observed)
    reference <- as.character(references[[variable]])
    if (!reference %in% levels) stop("Reference level is not observed for ", variable, ": ", reference, call. = FALSE)
    data[[variable]] <- stats::relevel(factor(as.character(x), levels = levels), ref = reference)
  }
  data
}

gtx_reference_code <- function(references, candidates, data_name = "analysis_data") {
  if (!length(references)) return(character(0))
  vapply(names(references), function(variable) {
    levels <- candidates[[variable]]
    sprintf(
      '%s[[%s]] <- stats::relevel(factor(as.character(%s[[%s]]), levels = %s), ref = %s)',
      data_name, shQuote(variable), data_name, shQuote(variable),
      paste0("c(", paste(shQuote(levels), collapse = ", "), ")"),
      shQuote(as.character(references[[variable]]))
    )
  }, character(1))
}
