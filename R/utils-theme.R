#' @keywords internal
#' @noRd
.theme_presets <- list(
  minimal  = c("plain","lines","labels_bold"),
  clinical = c("plain","labels_bold","compact"),
  striped  = c("zebra","labels_bold","compact"),
  shaded   = c("header_shaded","labels_bold","lines"),
  # aliases (beginner-friendly; “inspired by”, not replicas)
  jama     = c("plain","lines","labels_bold","compact")
)
#' @keywords internal
#' @noRd
.resolve_theme <- function(theme) {
  if (length(theme) == 1 && !is.na(theme) && theme %in% names(.theme_presets))
    return(.theme_presets[[theme]])
  unique(tolower(theme))
}
