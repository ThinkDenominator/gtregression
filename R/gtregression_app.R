gtx_shiny_app_dir <- function() {
  app_dir <- system.file("shiny", package = "gtregression")
  if (!nzchar(app_dir) || !dir.exists(app_dir)) {
    stop(
      "The gtregression Shiny app was not found in the installed package.",
      call. = FALSE
    )
  }
  app_dir
}

# Build the GT Regression Shiny application.
#
# mode = "app" is the canonical package GUI (the D3 workspace) running in the
# user's local R session; `data_env` is where R Environment datasets are
# discovered (a capability, not a different interface). "addin" is accepted as
# a legacy alias for "app". mode = "web" is the hosted/Shiny Server behaviour
# (inst/shiny/app.R sourced without a local session) and is unchanged.
gtx_shiny_app <- function(mode = c("web", "app", "addin"), data_env = NULL) {
  mode <- match.arg(mode)
  if (identical(mode, "addin")) mode <- "app"
  if (identical(mode, "app") && !is.environment(data_env)) {
    stop("App mode requires an explicit data environment.", call. = FALSE)
  }

  app_env <- new.env(parent = globalenv())
  app_env$gtx_launch_context <- list(mode = mode, data_env = data_env)
  sys.source(
    file.path(gtx_shiny_app_dir(), "app.R"),
    envir = app_env
  )
  shiny::shinyApp(app_env$ui, app_env$server)
}

# The one canonical GT Regression application (single UI and server). Every
# launcher builds exactly this; launchers differ only in the viewer/container.
gtx_gui_app <- function() {
  gtx_shiny_app(mode = "app", data_env = globalenv())
}

#' Launch the gtregression app
#'
#' Open GT Regression, the graphical interface for \pkg{gtregression}: data
#' import and preparation, descriptive tables, regression tables, survival
#' analysis, diagnostics, causal mediation, plots, exports and reproducible R
#' code.
#'
#' @param ... Additional arguments passed to \code{shiny::runApp()}, for
#'   example \code{port}.
#' @param launch.browser Passed to \code{shiny::runApp()}. The default opens
#'   the app in your web browser in interactive sessions. Supply a function
#'   such as \code{rstudioapi::viewer} to choose another viewer.
#'
#' @return Invisibly returns the result of \code{shiny::runApp()}.
#'
#' @details
#' \code{gtregression_app()} opens GT Regression in your web browser. The
#' RStudio Addin (\emph{Addins > GT Regression app}) opens exactly the same
#' application in an RStudio dialog window through
#' \code{\link{gtregression_addin}()}; only the window differs.
#'
#' The app runs in your local R session, so data frames in your global
#' environment can be chosen from the Data workspace (\emph{R Environment}).
#' Selected data are copied into the app as read-only snapshots; the source
#' objects are not modified. Files can also be imported, and example data are
#' included.
#'
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
#' @seealso \code{\link{gtregression_addin}}, \code{compare_models()},
#'   \code{multi_reg()}, \code{cox_reg()}, \code{surv_reg()}
#'
#' @export
gtregression_app <- function(..., launch.browser = NULL) {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop(
      "GT Regression requires the 'shiny' package.\n",
      "Install it with install.packages('shiny') and run gtregression_app() again.",
      call. = FALSE
    )
  }

  gtx_shiny_app_dir()
  app <- gtx_gui_app()

  if (is.null(launch.browser)) {
    launch.browser <- interactive()
  }

  shiny::runApp(app, launch.browser = launch.browser, ...)
}
