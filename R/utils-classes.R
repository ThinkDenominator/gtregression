#' @keywords internal
#' @noRd
is_gt <- function(x) inherits(x, "gt_uni") || inherits(x$table, "gt_tbl")

#' @keywords internal
#' @noRd
is_flextable <- function(x) inherits(x, "ft_uni") || inherits(x$table, "flextable")

#' @keywords internal
#' @noRd
is_uni <- function(x) inherits(x, "uni_reg")
