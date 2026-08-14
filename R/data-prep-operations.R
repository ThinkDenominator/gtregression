# Internal data-preparation operations used by the gtregression Shiny app.

gt_dp_rename <- function(data, variable, new_name) {
  if (!variable %in% names(data)) stop("Selected variable was not found.", call. = FALSE)
  new_name <- trimws(new_name)
  if (!nzchar(new_name) || !make.names(new_name) == new_name) {
    stop("New variable names must be non-empty valid R names.", call. = FALSE)
  }
  if (!identical(variable, new_name) && new_name %in% names(data)) {
    stop("A variable with that name already exists.", call. = FALSE)
  }
  names(data)[match(variable, names(data))] <- new_name
  data
}

gt_dp_recode <- function(data, variable, from, to, preserve = TRUE) {
  if (!variable %in% names(data)) stop("Selected variable was not found.", call. = FALSE)
  from <- as.character(from)
  to <- as.character(to)
  keep <- nzchar(from)
  from <- from[keep]; to <- to[keep]
  if (!length(from)) stop("Add at least one value to recode.", call. = FALSE)
  if (anyDuplicated(from)) stop("Each source value can be mapped only once.", call. = FALSE)
  original <- as.character(data[[variable]])
  changed <- original %in% from
  replacement <- to[match(original, from)]
  if (isTRUE(preserve)) replacement[!changed] <- original[!changed]
  data[[variable]] <- replacement
  attr(data, "gt_dp_affected") <- sum(changed & !is.na(replacement) & original != replacement, na.rm = TRUE)
  data
}

gt_dp_define_missing <- function(data, variable, codes) {
  if (!variable %in% names(data)) stop("Selected variable was not found.", call. = FALSE)
  codes <- trimws(as.character(codes))
  codes <- codes[nzchar(codes)]
  if (!length(codes)) stop("Enter at least one missing-value code.", call. = FALSE)
  values <- as.character(data[[variable]])
  affected <- !is.na(values) & values %in% codes
  data[[variable]][affected] <- NA
  attr(data, "gt_dp_affected") <- sum(affected)
  data
}

gt_dp_show_missing <- function(data, variable, label = "Missing") {
  if (!variable %in% names(data)) stop("Selected variable was not found.", call. = FALSE)
  label <- trimws(as.character(label))
  if (!nzchar(label)) stop("Enter a label for missing values.", call. = FALSE)
  values <- data[[variable]]
  affected <- is.na(values)
  if (!any(affected)) stop("This variable has no missing values to show as a category.", call. = FALSE)
  existing <- unique(as.character(values[!is.na(values)]))
  if (label %in% existing) {
    stop(
      "The missing-value label is already a recorded value. Choose a different label.",
      call. = FALSE
    )
  }
  output <- as.character(values)
  output[affected] <- label
  data[[variable]] <- factor(output, levels = c(existing, label))
  attr(data, "gt_dp_affected") <- sum(affected)
  data
}

gt_dp_set_type <- function(data, variable, type, levels = character()) {
  if (!variable %in% names(data)) stop("Selected variable was not found.", call. = FALSE)
  type <- match.arg(type, c("factor", "ordered", "numeric", "text"))
  values <- data[[variable]]
  if (identical(type, "numeric")) {
    converted <- suppressWarnings(as.numeric(as.character(values)))
    invalid <- !is.na(values) & is.na(converted)
    if (any(invalid)) {
      stop(
        "This variable contains non-numeric values and cannot safely be converted to numeric.",
        call. = FALSE
      )
    }
    data[[variable]] <- converted
  } else if (identical(type, "text")) {
    data[[variable]] <- as.character(values)
  } else {
    levels <- trimws(as.character(levels))
    levels <- levels[nzchar(levels)]
    observed <- unique(as.character(values[!is.na(values)]))
    if (!length(levels)) levels <- observed
    omitted <- setdiff(observed, levels)
    if (length(omitted)) {
      stop(paste0("Level order is missing observed value(s): ", paste(omitted, collapse = ", "), "."), call. = FALSE)
    }
    data[[variable]] <- factor(as.character(values), levels = levels, ordered = identical(type, "ordered"))
  }
  attr(data, "gt_dp_affected") <- sum(!is.na(values))
  attr(data, "gt_dp_type") <- type
  data
}

gt_dp_keep_variables <- function(data, variables) {
  variables <- unique(as.character(variables))
  variables <- variables[nzchar(variables)]
  if (!length(variables)) stop("Select at least one variable to keep.", call. = FALSE)
  missing <- setdiff(variables, names(data))
  if (length(missing)) stop("One or more selected variables were not found.", call. = FALSE)
  result <- data[, variables, drop = FALSE]
  attr(result, "gt_dp_affected") <- ncol(data) - ncol(result)
  result
}

gt_dp_condition <- function(data, variable, operator, value, value2 = NULL) {
  if (!variable %in% names(data)) stop("Selected variable was not found.", call. = FALSE)
  x <- data[[variable]]
  value_num <- suppressWarnings(as.numeric(value))
  compare_value <- if (is.numeric(x) && !is.na(value_num)) value_num else as.character(value)
  x_compare <- if (is.numeric(x) && !is.na(value_num)) x else as.character(x)
  if (operator %in% c("between", "outside")) {
    if (!is.numeric(x)) stop("Between and outside-range conditions require a numeric variable.", call. = FALSE)
    lower <- suppressWarnings(as.numeric(value))
    upper <- suppressWarnings(as.numeric(value2))
    if (!is.finite(lower) || !is.finite(upper)) stop("Enter numeric lower and upper bounds.", call. = FALSE)
    if (lower > upper) stop("The lower bound must not exceed the upper bound.", call. = FALSE)
    if (identical(operator, "between")) return(x >= lower & x <= upper)
    return(x < lower | x > upper)
  }
  switch(operator,
    "==" = x_compare == compare_value, "!=" = x_compare != compare_value,
    ">" = x_compare > compare_value, ">=" = x_compare >= compare_value,
    "<" = x_compare < compare_value, "<=" = x_compare <= compare_value,
    stop("Choose a valid filter operator.", call. = FALSE)
  )
}

gt_dp_transform_arithmetic <- function(data, source, new_name, operator, number) {
  new_name <- trimws(new_name)
  if (!nzchar(new_name) || !make.names(new_name) == new_name || new_name %in% names(data)) {
    stop("New variable name must be a unique valid R name.", call. = FALSE)
  }
  if (!source %in% names(data) || !is.numeric(data[[source]])) {
    stop("Arithmetic and comparison transforms require a numeric source variable.", call. = FALSE)
  }
  number <- suppressWarnings(as.numeric(number))
  if (!is.finite(number)) stop("Enter a finite numeric value.", call. = FALSE)
  data[[new_name]] <- switch(operator,
    "+" = data[[source]] + number, "-" = data[[source]] - number,
    "*" = data[[source]] * number, "/" = data[[source]] / number,
    "^" = data[[source]] ^ number,
    ">" = data[[source]] > number, ">=" = data[[source]] >= number,
    "<" = data[[source]] < number, "<=" = data[[source]] <= number,
    "==" = data[[source]] == number, "!=" = data[[source]] != number,
    stop("Choose a valid arithmetic operator.", call. = FALSE)
  )
  attr(data, "gt_dp_affected") <- sum(!is.na(data[[source]]))
  data
}

gt_dp_calculate <- function(data, source, new_name, mode = c("single", "two_variables", "ratio_power"),
                            operator = "+", number = 1, second = NULL) {
  mode <- match.arg(mode)
  if (identical(mode, "single")) {
    return(gt_dp_transform_arithmetic(data, source, new_name, operator, number))
  }
  new_name <- trimws(new_name)
  if (!nzchar(new_name) || !make.names(new_name) == new_name || new_name %in% names(data)) {
    stop("New variable name must be a unique valid R name.", call. = FALSE)
  }
  if (
    !source %in% names(data) || !second %in% names(data) ||
      !is.numeric(data[[source]]) || !is.numeric(data[[second]])
  ) {
    stop("Choose two numeric source variables.", call. = FALSE)
  }
  if (identical(mode, "two_variables")) {
    data[[new_name]] <- switch(operator,
      "+" = data[[source]] + data[[second]], "-" = data[[source]] - data[[second]],
      "*" = data[[source]] * data[[second]], "/" = data[[source]] / data[[second]],
      stop("Choose a valid calculation.", call. = FALSE)
    )
  } else {
    power <- suppressWarnings(as.numeric(number))
    if (!is.finite(power)) stop("Enter a finite denominator power.", call. = FALSE)
    data[[new_name]] <- data[[source]] / (data[[second]] ^ power)
  }
  attr(data, "gt_dp_affected") <- sum(!is.na(data[[source]]) & !is.na(data[[second]]))
  data
}

gt_dp_group_values <- function(data, variables, operators, values, results, default,
                               values2 = rep("", length(values))) {
  keep <- nzchar(variables) & nzchar(values) & nzchar(results)
  if (!any(keep)) stop("Add at least one condition and result.", call. = FALSE)
  labels <- trimws(c(results[keep], default))
  if (any(!nzchar(labels))) stop("Enter a label for every group.", call. = FALSE)
  if (anyDuplicated(labels)) stop("Each group needs a unique label.", call. = FALSE)
  output <- rep(as.character(default), nrow(data))
  unmatched <- rep(TRUE, nrow(data))
  matched_count <- 0L
  for (index in which(keep)) {
    matched <- gt_dp_condition(
      data, variables[[index]], operators[[index]], values[[index]], values2[[index]]
    ) %in% TRUE
    assign <- unmatched & matched
    output[assign] <- results[[index]]
    unmatched[assign] <- FALSE
    matched_count <- matched_count + sum(assign)
  }
  counts <- stats::setNames(
    vapply(labels, function(label) sum(output == label, na.rm = TRUE), integer(1)),
    labels
  )
  attr(output, "gt_dp_group_counts") <- counts
  attr(output, "gt_dp_affected") <- matched_count
  output
}

gt_dp_empty_group_message <- function(data, variables, counts) {
  empty <- names(counts)[counts == 0L]
  if (!length(empty)) return(NULL)
  source <- unique(variables[nzchar(variables)])
  range_note <- ""
  if (length(source) == 1L && source %in% names(data) && is.numeric(data[[source]])) {
    observed <- data[[source]][is.finite(data[[source]])]
    if (length(observed)) {
      range_note <- paste0(
        " Observed range of `", source, "`: ",
        format(min(observed), trim = TRUE), " to ", format(max(observed), trim = TRUE), "."
      )
    }
  }
  paste0(
    "Grouping would create empty group(s): ", paste(empty, collapse = ", "), ".",
    range_note, " Revise the conditions before applying."
  )
}

gt_dp_transform_case_when <- function(data, new_name, variables, operators, values, results, default,
                                      values2 = rep("", length(values))) {
  new_name <- trimws(new_name)
  if (!nzchar(new_name) || !make.names(new_name) == new_name || new_name %in% names(data)) {
    stop("New variable name must be a unique valid R name.", call. = FALSE)
  }
  output <- gt_dp_group_values(data, variables, operators, values, results, default, values2)
  empty_message <- gt_dp_empty_group_message(
    data, variables, attr(output, "gt_dp_group_counts")
  )
  if (!is.null(empty_message)) stop(empty_message, call. = FALSE)
  affected <- attr(output, "gt_dp_affected")
  attributes(output) <- NULL
  data[[new_name]] <- output
  attr(data, "gt_dp_affected") <- affected
  data
}

gt_dp_filter <- function(data, variable1, operator1, value1,
                         connector = "", variable2 = "", operator2 = "", value2 = "") {
  first <- gt_dp_condition(data, variable1, operator1, value1)
  condition <- first
  if (nzchar(variable2) && nzchar(value2)) {
    second <- gt_dp_condition(data, variable2, operator2, value2)
    condition <- if (identical(connector, "OR")) first | second else first & second
  }
  attr(data, "gt_dp_filter_counts") <- c(
    before = nrow(data), included = sum(condition %in% TRUE),
    excluded = sum(condition %in% FALSE), unknown = sum(is.na(condition))
  )
  data[which(condition %in% TRUE), , drop = FALSE]
}

gt_dp_code_line <- function(operation, ...) {
  args <- list(...)
  quote_value <- function(x) encodeString(as.character(x), quote = '"')
  switch(operation,
    rename = paste0("data <- dplyr::rename(data, ", args$new_name, " = ", args$variable, ")"),
    recode = paste0("data$", args$variable, " <- dplyr::recode(as.character(data$", args$variable,
      "), ", paste(paste0(quote_value(args$from), " = ", quote_value(args$to)), collapse = ", "), ")"),
    missing = paste0("data$", args$variable, "[as.character(data$", args$variable, ") %in% ",
      "c(", paste(quote_value(args$codes), collapse = ", "), ")] <- NA"),
    missing_category = paste0("data$", args$variable, " <- factor(ifelse(is.na(data$", args$variable, "), ",
      quote_value(args$label), ", as.character(data$", args$variable, ")))") ,
    type = switch(args$type,
      factor = paste0(
        "data$", args$variable, " <- factor(data$", args$variable,
        ", levels = c(", paste(quote_value(args$levels), collapse = ", "), "))"
      ),
      ordered = paste0(
        "data$", args$variable, " <- ordered(data$", args$variable,
        ", levels = c(", paste(quote_value(args$levels), collapse = ", "), "))"
      ),
      numeric = paste0("data$", args$variable, " <- as.numeric(as.character(data$", args$variable, "))"),
      text = paste0("data$", args$variable, " <- as.character(data$", args$variable, ")")
    ),
    keep = paste0("data <- dplyr::select(data, ", paste(args$variables, collapse = ", "), ")"),
    filter = paste0("data <- dplyr::filter(data, ", args$expression, ")"),
    transform = args$code,
    ""
  )
}
