# R/accessors.R

#' Access fields on gtregression objects with `$`
#'
#' Works for any object from this package, since they all carry class
#' `"gtregression"`. Returns NULL (quietly) if the field is not present.
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
  # avoid recursive calls by using [[ ]] only
  fmt <- x[["format"]]
  eng <- x[["engine"]]

  # aliases
  if (identical(name, "engine") && !is.null(fmt)) {
    return(fmt)
  }
  if (identical(name, "format") && !is.null(eng)) {
    return(eng)
  }

  # ordinary field access
  x[[name]]
}
