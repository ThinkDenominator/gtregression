# Internal helpers for discovering and loading Addin data sources.

gtx_discover_data_frames <- function(env) {
  if (!is.environment(env)) {
    stop("'env' must be an environment.", call. = FALSE)
  }

  object_names <- ls(envir = env, all.names = TRUE)
  records <- lapply(object_names, function(name) {
    if (bindingIsActive(name, env)) return(NULL)

    object <- tryCatch(
      get(name, envir = env, inherits = FALSE),
      error = function(e) NULL
    )
    if (!inherits(object, "data.frame")) return(NULL)

    data.frame(
      name = name,
      rows = as.integer(nrow(object)),
      columns = as.integer(ncol(object)),
      class = paste(class(object), collapse = ", "),
      stringsAsFactors = FALSE
    )
  })

  records <- Filter(Negate(is.null), records)
  if (!length(records)) {
    return(data.frame(
      name = character(0),
      rows = integer(0),
      columns = integer(0),
      class = character(0),
      stringsAsFactors = FALSE
    ))
  }

  out <- do.call(rbind, records)
  rownames(out) <- NULL
  out
}

gtx_get_data_frame <- function(env, name) {
  if (!is.environment(env)) {
    stop("'env' must be an environment.", call. = FALSE)
  }
  if (!is.character(name) || length(name) != 1L || is.na(name) || !nzchar(name)) {
    stop("Choose an R-environment dataset to load.", call. = FALSE)
  }
  if (!exists(name, envir = env, inherits = FALSE)) {
    stop("The selected R-environment object is no longer available: ", name, call. = FALSE)
  }
  if (bindingIsActive(name, env)) {
    stop("Active bindings cannot be loaded as datasets: ", name, call. = FALSE)
  }

  object <- tryCatch(
    get(name, envir = env, inherits = FALSE),
    error = function(e) {
      stop("The selected R-environment object could not be retrieved: ", name, call. = FALSE)
    }
  )
  if (!inherits(object, "data.frame")) {
    stop("The selected R-environment object is not a data frame: ", name, call. = FALSE)
  }
  object
}

gtx_object_reference <- function(name) {
  paste(deparse(as.name(name), backtick = TRUE), collapse = "")
}
