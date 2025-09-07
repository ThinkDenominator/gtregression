#' Launch the **gtregression** Shiny GUI
#'
#' Opens the Shiny-based graphical interface for running regression analyses,
#' generating tables/plots, and viewing diagnostics built on
#' \pkg{gtregression}.
#'
#' @details
#' This function requires \pkg{shiny}. If it is not installed, an informative
#' error is thrown. The app directory is located via
#' [base::system.file()] under `inst/shiny` in the installed package.
#' A quick sanity check ensures either `app.R` or `ui.R` + `server.R` exist
#' before launching with [shiny::runApp()].
#'
#' @return No return value; called for its side effects (launches the GUI).
#'
#' @seealso [shiny::runApp()], [base::system.file()]
#'
#' @export
#'
#' @examples
#' if (interactive()) {
#'   gtregression_app()
#' }
gtregression_app <- function() {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop(
      "Package 'shiny' is required to run the GUI. ",
      "Install it with install.packages('shiny')."
    )
  }

  app_dir <- system.file("shiny", package = "gtregression")
  if (!nzchar(app_dir)) {
    stop(
      "GUI directory not found in the installed package. ",
      "Try re-installing 'gtregression'."
    )
  }

  # sanity check: must have app.R or ui.R + server.R
  has_app <- file.exists(file.path(app_dir, "app.R")) ||
    (file.exists(file.path(app_dir, "ui.R")) &&
       file.exists(file.path(app_dir, "server.R")))
  if (!has_app) {
    stop(
      "No Shiny app found in ", app_dir,
      " (missing app.R or ui.R + server.R)."
    )
  }

  shiny::runApp(
    appDir = app_dir,
    display.mode = "normal",
    quiet = TRUE,
    launch.browser = interactive()
  )
}
