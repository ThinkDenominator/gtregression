#' Launch the gtregression app
#'
#' Open a menu-driven Shiny app for common \pkg{gtregression} workflows:
#' data import, descriptive tables, regression tables, survival analysis,
#' diagnostics, causal mediation, plots, and exports.
#'
#' @param ... Additional arguments passed to \code{shiny::runApp()}.
#' @param launch.browser Logical; passed to \code{shiny::runApp()}. The default
#'   uses the RStudio Viewer when available, otherwise opens a browser only in
#'   interactive sessions.
#'
#' @return Invisibly returns the result of \code{shiny::runApp()}.
#'
#' @details
#' The app is intentionally kept out of the core package startup path. Shiny and
#' other interface packages are suggested dependencies and are loaded only when
#' \code{gtregression_app()} is called.
#'
#' The Advanced tab includes a guided candidate-model builder. Users can name
#' two to six models, choose model-specific exposures and adjustment variables,
#' optionally add an interaction, and track a primary exposure. The app fits
#' compatible \code{multi_reg()}, \code{cox_reg()}, or \code{surv_reg()} objects
#' before passing them to \code{compare_models()}. Generated code records every
#' fitting call and the final comparison for reproducible use outside the app.
#'
#' @examples
#' if (interactive()) {
#'   gtregression_app()
#' }
#'
#' @seealso \code{compare_models()}, \code{multi_reg()}, \code{cox_reg()},
#'   \code{surv_reg()}
#'
#' @export
gtregression_app <- function(..., launch.browser = NULL) {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop(
      "The gtregression app requires the 'shiny' package.\n",
      "Install it with install.packages('shiny') and run gtregression_app() again.",
      call. = FALSE
    )
  }

  app_dir <- system.file("shiny", package = "gtregression")
  if (!nzchar(app_dir) || !dir.exists(app_dir)) {
    stop(
      "The gtregression Shiny app was not found in the installed package.",
      call. = FALSE
    )
  }

  if (is.null(launch.browser)) {
    launch.browser <- if (
      interactive() &&
        requireNamespace("rstudioapi", quietly = TRUE) &&
        rstudioapi::isAvailable()
    ) {
      rstudioapi::viewer
    } else {
      interactive()
    }
  }

  shiny::runApp(app_dir, launch.browser = launch.browser, ...)
}
