#' Launch the gtregression app from the RStudio Addins menu
#'
#' The RStudio Addin (\emph{Addins > GT Regression app}) opens GT Regression in
#' an RStudio dialog window. It is the same application as
#' \code{\link{gtregression_app}()}, which opens it in your web browser.
#'
#' @return Invisibly returns the result of \code{shiny::runGadget()}.
#'
#' @details
#' There is no separate Addin interface: the interface, analyses, generated R
#' code and access to data frames in your global environment are identical to
#' \code{gtregression_app()}. Only the container differs (a 1200 x 800 RStudio
#' dialog instead of a browser window). The \pkg{shiny} and \pkg{rstudioapi}
#' packages are optional and are checked only when this function is called.
#' Selected data frames are copied into the app as read-only snapshots; the
#' source objects are not modified.
#'
#' @examples
#' if (
#'   interactive() &&
#'     requireNamespace("rstudioapi", quietly = TRUE) &&
#'     rstudioapi::isAvailable()
#' ) {
#'   gtregression_addin()
#' }
#'
#' @seealso \code{\link{gtregression_app}}
#'
#' @export
gtregression_addin <- function() {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop(
      "The GT Regression Addin requires the 'shiny' package.\n",
      "Install it with install.packages('shiny') and try again.",
      call. = FALSE
    )
  }

  if (!requireNamespace("rstudioapi", quietly = TRUE)) {
    stop(
      "The GT Regression Addin requires the optional 'rstudioapi' package.\n",
      "Install it with install.packages('rstudioapi') and try again from RStudio.",
      call. = FALSE
    )
  }

  if (!rstudioapi::isAvailable()) {
    stop(
      "The GT Regression Addin must be run from an available RStudio session.\n",
      "Open RStudio and choose Addins > GT Regression app, or run gtregression_app().",
      call. = FALSE
    )
  }

  # Same canonical application as gtregression_app(); only the container
  # (an RStudio dialog window) differs.
  app <- gtx_gui_app()
  viewer <- shiny::dialogViewer(
    "GT Regression",
    width = 1200,
    height = 800
  )

  shiny::runGadget(
    app,
    viewer = viewer,
    stopOnCancel = TRUE
  )
}
