.is_adjusted_reg_output <- function(x) {
  isTRUE(x$adjusted_mode) || isTRUE(x$multivariable)
}
