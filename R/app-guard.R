# Error safety for the gtregression app (app and web modes).
#
# An error that escapes a Shiny observer ends the session, and in RStudio with
# "break on error" it opens the debugger. Every observer in the app runs its
# handler through gtx_guard(), which turns any error into a short, plain
# notification and keeps the session alive. Statistical code is not touched.

# Plain-language version of an error message.
gtx_friendly_error <- function(e) {
  msg <- if (inherits(e, "condition")) conditionMessage(e) else as.character(e)
  msg <- paste(msg[!is.na(msg)], collapse = " ")
  msg <- gsub("\033\\[[0-9;]*m", "", msg)
  msg <- trimws(gsub("[[:space:]]+", " ", msg))
  msg <- sub("^Error( in [^:]*)?: ", "", msg)
  msg <- sub("^(✖|ℹ|!|x) ", "", msg)

  rules <- list(
    list("Package '(readxl|haven)' is required",
         "Opening this type of file needs the '\\1' package. Install it with install.packages(\"\\1\") and try again."),
    list("invalid multibyte|utf8towcs|invalid UTF-8|embedded nul",
         "The file contains characters that could not be read. Save it as UTF-8 (for example 'CSV UTF-8' in Excel) and load it again."),
    list("did not have [0-9]+ elements|more columns than column names|duplicate 'row.names'",
         "The rows of the file do not line up into columns. Check the separator (comma, semicolon or tab) and whether the first row holds column names."),
    list("no lines available in input",
         "The file is empty."),
    list("Can't retrieve sheet|Sheet .* not found|sheet.*out of range|cannot find sheet",
         "That sheet was not found in the workbook. Check the sheet number or name."),
    list("zip file|invalid zip|Failed to open|cannot open file|cannot open the connection|not an excel file|is not a valid (zip|xlsx|excel)",
         "The file could not be opened. It may be damaged, password-protected, or still open in another program."),
    list("([A-Za-z]+) +is not a valid approach for [A-Za-z_.]+\\.? *Valid options: ([A-Za-z, ]+)",
         "The '\\1' approach is not available for this tool. Choose one of: \\2."),
    list("'arg' should be one of",
         "One of the options has a value that is not available for this analysis. Choose the option again and try once more."),
    list("EXPR must be a length 1 vector",
         "A required choice is missing. Complete the selections and try again."),
    list("This approach requires either a factor variable or numeric variable coded as 0 and 1",
         "The outcome must have two categories (such as yes/no or 0/1) for this approach. Choose a binary outcome, or the Linear approach for a continuous outcome."),
    list("Poisson regression requires a count outcome|Negative binomial requires a count outcome",
         "This approach needs a count outcome (whole numbers of zero or more). Choose a count outcome or another approach."),
    list("Linear regression requires a continuous outcome",
         "Linear regression needs a numeric outcome with more than two values. For a yes/no outcome, choose logistic or robust Poisson regression."),
    list("No valid models across strata",
         "The model could not be fitted in any stratum. The strata may be too small or have no events; try fewer variables or a different stratifier."),
    list("subscript out of bounds",
         "Some of the selected variables could not be used together. Check that each variable has only one role (outcome, exposure, adjustment, stratifier) and enough data, then try again."),
    list("object '([^']+)' not found",
         "The variable '\\1' was not found in the active data. It may have been renamed or removed in Data Prep; reselect the variables and try again."),
    list("contrasts can be applied only to factors with 2 or more levels",
         "A selected categorical variable has only one category in the analysed rows. Remove it, or check its missing values, and try again."),
    list("Right censored data only",
         "The event variable must be coded 0/1 (or have two categories) for this analysis. Check the Event selection."),
    list("NA/NaN/Inf in|non-finite|missing value where TRUE/FALSE needed",
         "The model could not be fitted with these data (missing or infinite values, or a variable with no variation). Try fewer variables or check the data in Data Prep."),
    list("0 \\(non-NA\\) cases|no \\(non-missing\\) observations|no complete",
         "No complete rows remain for the selected variables after missing values are removed."),
    list("cannot allocate vector|vector memory",
         "This analysis needs more memory than is available. Try fewer variables or a smaller dataset."),
    list("non-numeric argument|requires numeric|is not numeric",
         "One of the selected variables needs to be numeric for this analysis. Check its type in Data Prep.")
  )
  for (rule in rules) {
    if (grepl(rule[[1]], msg, perl = TRUE)) {
      friendly <- rule[[2]]
      if (grepl("\\\\1", friendly)) {
        friendly <- sub(paste0("^.*?", rule[[1]], ".*$"), friendly, msg, perl = TRUE)
      }
      return(friendly)
    }
  }
  if (!nzchar(msg)) {
    return("This step could not be completed. Check the selected variables and try again.")
  }
  msg <- gsub("`([^`]*)`", "'\\1'", msg)
  msg <- gsub(" ?\\(version 1 limit\\)", "", msg)
  msg <- gsub("risk_difference\\(\\) ", "The risk difference ", msg)
  if (nchar(msg) > 400) msg <- paste0(substr(msg, 1, 397), "...")
  msg
}

# Show an error as a notification instead of letting it escape.
gtx_notify_error <- function(e, prefix = NULL) {
  msg <- gtx_friendly_error(e)
  if (!is.null(prefix) && nzchar(prefix)) msg <- paste0(prefix, ": ", msg)
  # Keep the original message in the console for anyone debugging.
  if (inherits(e, "condition")) {
    message("gtregression app: ", conditionMessage(e))
  }
  if (!is.null(shiny::getDefaultReactiveDomain())) {
    notify <- getOption("gtregression.app_notify", shiny::showNotification)
    notify(msg, type = "error", duration = 12)
  }
  invisible(msg)
}

# Evaluate an observer handler; errors become notifications, req() stays silent.
gtx_guard <- function(expr) {
  tryCatch(
    expr,
    shiny.silent.error = function(e) invisible(NULL),
    error = function(e) {
      gtx_notify_error(e)
      invisible(NULL)
    }
  )
}

# Drop-in replacements for shiny::observeEvent() and shiny::observe() whose
# handlers run inside gtx_guard().
gtx_safe_observe_event <- function(eventExpr, handlerExpr,
                                   event.env = parent.frame(), event.quoted = FALSE,
                                   handler.env = parent.frame(), handler.quoted = FALSE,
                                   ..., label = NULL) {
  if (!event.quoted) eventExpr <- substitute(eventExpr)
  if (!handler.quoted) handlerExpr <- substitute(handlerExpr)
  if (is.null(label)) {
    label <- sprintf("observeEvent(%s)", paste(deparse(eventExpr), collapse = " "))
  }
  guarded <- as.call(list(gtx_guard, handlerExpr))
  shiny::observeEvent(
    eventExpr, guarded,
    event.env = event.env, event.quoted = TRUE,
    handler.env = handler.env, handler.quoted = TRUE,
    ..., label = label
  )
}

gtx_safe_observe <- function(x, env = parent.frame(), quoted = FALSE, ..., label = NULL) {
  if (!quoted) x <- substitute(x)
  if (is.null(label)) label <- sprintf("observe(%s)", paste(utils::head(deparse(x), 1), collapse = ""))
  guarded <- as.call(list(gtx_guard, x))
  shiny::observe(guarded, env = env, quoted = TRUE, ..., label = label)
}

# downloadHandler() whose failures are explained in a notification. The error
# is raised again so the browser does not save a broken file.
gtx_safe_download_handler <- function(filename, content, ...) {
  safe_content <- function(file) {
    tryCatch(
      content(file),
      error = function(e) {
        gtx_notify_error(e, prefix = "The file could not be created")
        stop(e)
      }
    )
  }
  shiny::downloadHandler(filename = filename, content = safe_content, ...)
}

# Variables chosen for two roles at once.
#
# `roles` are single-variable roles (for example list(Outcome = "low",
# Stratifier = "race")). `lists` are multi-variable inputs, each
# list(id = <input id>, label = <shown name>, value = <selected variables>).
# Returns one row per variable that must be removed from a list: a variable in
# a list that is also a single role, or a variable in two lists (the later list
# loses it, so exposures are kept over adjustment variables).
gtx_role_conflicts <- function(roles = list(), lists = list()) {
  roles <- roles[vapply(roles, function(x) length(x) == 1 && !is.na(x) && nzchar(x), logical(1))]
  out <- list()
  seen <- character(0)
  seen_label <- character(0)
  for (item in lists) {
    value <- unique(item$value[!is.na(item$value) & nzchar(item$value)])
    for (v in value) {
      role <- names(roles)[vapply(roles, identical, logical(1), v)]
      other <- if (v %in% seen) seen_label[match(v, seen)] else character(0)
      if (length(role) || length(other)) {
        out[[length(out) + 1L]] <- data.frame(
          variable = v,
          role = c(role, other)[1],
          type = if (length(role)) "role" else "list",
          id = item$id,
          label = item$label,
          stringsAsFactors = FALSE
        )
      }
    }
    seen <- c(seen, value)
    seen_label <- c(seen_label, rep(item$label, length(value)))
  }
  if (!length(out)) {
    return(data.frame(variable = character(0), role = character(0), type = character(0), id = character(0),
                      label = character(0), stringsAsFactors = FALSE))
  }
  do.call(rbind, out)
}

# Single roles that point at the same variable (for example Outcome and
# Stratifier). These cannot be fixed by removing a variable from a list.
gtx_role_clashes <- function(roles = list()) {
  roles <- roles[vapply(roles, function(x) length(x) == 1 && !is.na(x) && nzchar(x), logical(1))]
  if (length(roles) < 2) return(character(0))
  values <- unlist(roles, use.names = FALSE)
  dup <- unique(values[duplicated(values)])
  vapply(dup, function(v) {
    paste0("'", v, "' is selected as both ", paste(names(roles)[values == v], collapse = " and "), ".")
  }, character(1), USE.NAMES = FALSE)
}

# Sentence for one conflict row.
gtx_role_conflict_text <- function(conflicts) {
  if (!nrow(conflicts)) return(character(0))
  ifelse(
    conflicts$type == "role",
    sprintf("'%s' is the %s and is also selected in %s.",
            conflicts$variable, tolower(conflicts$role), conflicts$label),
    sprintf("'%s' is selected in both %s and %s.",
            conflicts$variable, conflicts$role, conflicts$label)
  )
}
