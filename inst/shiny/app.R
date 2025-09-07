# Minimal app wrapper that wires modules together
suppressPackageStartupMessages({
  library(shiny)
  library(shinythemes)
  library(shinyjs)
  library(DT)
  library(gt)
  library(ggplot2)
  library(readxl)
  # gtregression is used but users can run this app in a minimal env
})
options(shiny.maxRequestSize = 50 * 1024^2)
# source helpers + modules
# ---------- robust app dir finder ----------
`%||%` <- function(a, b) if (!is.null(a) && nzchar(a)) a else b

find_app_dir <- function() {
  # 1) installed package path (CHANGE pkg name if different)
  d <- system.file("shiny", package = "gtregression")
  if (nzchar(d)) return(d)

  # 2) common dev paths
  for (cand in c("inst/shiny", ".", "shiny")) {
    p <- normalizePath(cand, mustWork = FALSE)
    if (file.exists(file.path(p, "utils", "helpers.R"))) return(p)
  }

  # 3) directory of this file (if available)
  of <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
  if (!is.null(of)) return(dirname(normalizePath(of)))

  getwd()
}

APP_DIR <- find_app_dir()

# ---------- source ALL files into the global env ----------
src_global <- function(rel) {
  path <- file.path(APP_DIR, rel)
  if (!file.exists(path)) stop("Could not find: ", path,
                               "\nWorking dir: ", getwd(),
                               "\nAPP_DIR: ", APP_DIR)
  source(path, local = globalenv(), chdir = TRUE)
}

# load files (helpers first)
src_global("utils/helpers.R")
src_global("modules/import_data.R")
src_global("modules/descriptive.R")
src_global("modules/regression.R")
src_global("modules/advanced.R")
src_global("modules/help_tab.R")

# sanity checks (optional but helpful)
stopifnot(
  exists("ns_gt",                 mode = "function"),
  exists("has_fn",                mode = "function"),
  exists("call_gt",               mode = "function"),
  exists("as_text",               mode = "function"),
  exists("mod_import_server",     mode = "function"),
  exists("mod_descriptive_server",mode = "function"),
  exists("mod_regression_server", mode = "function"),
  exists("mod_advanced_server",   mode = "function"),
  exists("mod_help_server",       mode = "function")
)

ui <- fluidPage(
  useShinyjs(),
  theme = shinytheme("cerulean"),
  titlePanel(tagList(
    span("gtregression GUI"),
    span(class="section-badge", "version 1.1.0")
  )),
  tabsetPanel(
    id = "main_tabs",
    mod_import_ui("import"),
    mod_descriptive_ui("desc"),
    mod_regression_ui("reg"),
    mod_advanced_ui("adv"),
    mod_help_ui("help")
  )
)




server <- function(input, output, session) {
  # central reactive store (shared across modules)
  rv <- reactiveValues(data = NULL)

  # 1) Import tab (owns data)
  mod_import_server("import", rv = rv)

  # 2) Descriptive tab (reads rv$data)
  mod_descriptive_server("desc", rv = rv)

  # 3) Regression tab (reads rv$data)
  mod_regression_server("reg", rv = rv)

  # 4) Advanced tab (reads rv$data)
  mod_advanced_server("adv", rv = rv)

  # 5) Help tab (no server needed beyond namespace)
  mod_help_server("help")
}

shinyApp(ui, server)
