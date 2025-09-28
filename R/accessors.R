# R/accessors.R

#' Access fields on gtregression objects with `$`
#'
#' Works for any object from this package, since they all carry class
#' `"gtregression"`. Returns NULL (quietly) if the field isn't present.
#'
#' Common fields:
#' - table, table_display, table_body
#' - models, model_summaries, reg_check
#' - approach, format (or engine), source
#' - parts, spanners (for merged tables)
#' - by, levels (for descriptive tables)
#'
#' @export
`$.gtregression` <- function(x, name) {
  # prefer known aliases
  if (identical(name, "engine") && !is.null(x$format)) return(x$format)
  if (identical(name, "format") && !is.null(x$engine)) return(x$engine)

  # return if present; otherwise NULL (quietly)
  x[[name]]
}
