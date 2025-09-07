# inst/shiny/modules/regression.R

mod_regression_ui <- function(id) {
  ns <- NS(id)
  tabPanel(
    tagList("Regression Analysis", span(class="section-badge","")),
    fluidRow(
      column(
        width = 4,
        div(class="card-like sidebar-sticky",
            h3("Model Controls", class="mb-md"),
            div(class="control-help",
                "Select the outcome, exposures, and regression type."),
            selectInput(ns("reg_outcome"), "Select Outcome (One)", choices = NULL),
            checkboxGroupInput(ns("reg_exposures"), "Select Exposures (Multiple)", choices = NULL),
            selectInput(ns("reg_approach"), "Regression Type",
                        choices = c("Logistic (logit)" = "logit",
                                    "Log-Binomial (log-binomial)" = "log-binomial",
                                    "Poisson (poisson)" = "poisson",
                                    "Robust Poisson (robpoisson)" = "robpoisson",
                                    "Negative Binomial (negbin)" = "negbin",
                                    "Linear (linear)" = "linear")),
            tags$hr(),
            h4("Stratified options"),
            selectInput(ns("reg_strat_by"), "Stratification Variable (Unique from exposures and outcome)",
                        choices = c("None")),
            checkboxInput(ns("reg_strat_multi"),
                          "Multivariable? (Otherwise Univariate)",
                          value = FALSE)
        )
      ),
      column(
        width = 8,
        div(class="card-like",
            h4(tagList(icon("layer-group"), " Results")),
            tabsetPanel(
              tabPanel("Univariate",
                       div(class="control-help","Run univariate regression for each exposure."),
                       actionButton(ns("run_uni"), label = tagList(icon("play"), " Run Univariate Regression"),
                                    class = "btn-primary mb-md"),
                       tabsetPanel(
                         tabPanel("Table",
                                  gt::gt_output(ns("uni_table")),
                                  br(),
                                  strong("Export"),
                                  div(class="mt-sm",
                                      downloadButton(ns("uni_table_docx"), "DOCX"),
                                      downloadButton(ns("uni_table_html"), "HTML"),
                                      downloadButton(ns("uni_table_pdf"),  "PDF"),
                                      downloadButton(ns("uni_table_png"),  "PNG")
                                  )
                         ),
                         tabPanel("Plot",
                                  plotOutput(ns("uni_plot")),
                                  br(),
                                  strong("Export"),
                                  div(class="mt-sm",
                                      downloadButton(ns("uni_plot_png"), "PNG"),
                                      downloadButton(ns("uni_plot_pdf"), "PDF")
                                  )
                         ),
                         tabPanel("Model Summary",
                                  verbatimTextOutput(ns("uni_summary")),
                                  br(),
                                  downloadButton(ns("uni_summary_txt"), "Export Summary as TXT")
                         ),
                         tabPanel("Reproducible Code",
                                  div(class="control-help","Copy & paste — assumes your data frame is named "),
                                  tags$code("df"),
                                  br(), br(),
                                  downloadButton(ns("uni_code_download"), "Download R code"),
                                  br(), br(),
                                  verbatimTextOutput(ns("uni_code"))
                         )
                       )
              ),
              tabPanel("Multivariable",
                       div(class="control-help","Run multivariable regression with all selected exposures."),
                       actionButton(ns("run_multi"), label = tagList(icon("play"), " Run Multivariable Regression"),
                                    class = "btn-primary mb-md"),
                       tabsetPanel(
                         tabPanel("Table",
                                  gt::gt_output(ns("multi_table")),
                                  br(),
                                  strong("Export"),
                                  div(class="mt-sm",
                                      downloadButton(ns("multi_table_docx"), "DOCX"),
                                      downloadButton(ns("multi_table_html"), "HTML"),
                                      downloadButton(ns("multi_table_pdf"),  "PDF"),
                                      downloadButton(ns("multi_table_png"),  "PNG")
                                  )
                         ),
                         tabPanel("Plot",
                                  plotOutput(ns("multi_plot")),
                                  br(),
                                  strong("Export"),
                                  div(class="mt-sm",
                                      downloadButton(ns("multi_plot_png"), "PNG"),
                                      downloadButton(ns("multi_plot_pdf"), "PDF")
                                  )
                         ),
                         tabPanel("Model Summary",
                                  verbatimTextOutput(ns("multi_summary")),
                                  br(),
                                  downloadButton(ns("multi_summary_txt"), "Export Summary as TXT")
                         ),
                         tabPanel("Reproducible Code",
                                  div(class="control-help","Copy & paste — assumes your data frame is named "),
                                  tags$code("df"),
                                  br(), br(),
                                  downloadButton(ns("multi_code_download"), "Download R code"),
                                  br(), br(),
                                  verbatimTextOutput(ns("multi_code"))
                         )
                       )
              ),
              tabPanel("Stratified",
                       div(class="control-help","Run stratified regression (uni or multi based on option).
                       Stratifier should be unique from exposures and outcome.
                           Click the button below."),
                       actionButton(ns("run_strat"), label = tagList(icon("play"), " Run Stratified Regression"),
                                    class = "btn-primary mb-md"),
                       tabsetPanel(
                         tabPanel("Table",
                                  gt::gt_output(ns("strat_table")),
                                  br(),
                                  strong("Export"),
                                  div(class="mt-sm",
                                      downloadButton(ns("strat_table_docx"), "DOCX"),
                                      downloadButton(ns("strat_table_html"), "HTML"),
                                      downloadButton(ns("strat_table_pdf"),  "PDF"),
                                      downloadButton(ns("strat_table_png"),  "PNG")
                                  )
                         ),
                         tabPanel("Model Summary",
                                  verbatimTextOutput(ns("strat_summary")),
                                  br(),
                                  downloadButton(ns("strat_summary_txt"), "Export Summary as TXT")
                         ),
                         tabPanel("Reproducible Code",
                                  div(class="control-help","Copy & paste — assumes your data frame is named "),
                                  tags$code("df"),
                                  br(), br(),
                                  downloadButton(ns("strat_code_download"), "Download R code"),
                                  br(), br(),
                                  verbatimTextOutput(ns("strat_code"))
                         )
                       )
              ),
              # --- Combine Results ---
              tabPanel("Combine Results",
                       tabsetPanel(
                         tabPanel("Tables",
                                  div(class="control-help",
                                      "Select which tables to merge: ",
                                      tags$code("Descriptive"), ", ",
                                      tags$code("Univariate"), ", ",
                                      tags$code("Multivariable"), "."),
                                  uiOutput(ns("combine_tbl_choices_ui")),
                                  actionButton(ns("run_combine_tbl"),
                                               "Combine Tables",
                                               class="btn-primary mt-sm"),
                                  tags$hr(),
                                  gt::gt_output(ns("combined_table")),
                                  br(),
                                  strong("Export"),
                                  div(class="mt-sm",
                                      downloadButton(ns("combined_table_docx"), "DOCX"),
                                      downloadButton(ns("combined_table_html"), "HTML"),
                                      downloadButton(ns("combined_table_pdf"),  "PDF"),
                                      downloadButton(ns("combined_table_png"),  "PNG")
                                  ),
                                  tags$hr(),
                                  strong("Reproducible code (merge tables)"),
                                  div(class="control-help",
                                      "Assumes you already created: ",
                                      tags$code("desc_tbl"), ", ",
                                      tags$code("uni_res"), ", ",
                                      tags$code("multi_res"),
                                      "."),
                                  div(class="mt-sm",
                                      downloadButton(ns("combined_tbl_code_download"), "Download R code")
                                  ),
                                  br(), br(),
                                  verbatimTextOutput(ns("combined_tbl_code"))
                         ),
                         tabPanel("Plots",
                                  div(class="control-help",
                                      "Combine forest plots from Univariate and Multivariable fits."),
                                  checkboxInput(ns("combine_plots_enable"),
                                                "Combine Univariate + Multivariable plots", value = FALSE),
                                  actionButton(ns("run_combine_plot"), "Combine Plots", class="btn-primary mt-sm"),
                                  tags$hr(),
                                  plotOutput(ns("combined_plot")),
                                  br(),
                                  strong("Export"),
                                  div(class="mt-sm",
                                      downloadButton(ns("combined_plot_png"), "PNG"),
                                      downloadButton(ns("combined_plot_pdf"), "PDF")
                                      ),
                                  tags$hr(),
                                  strong("Reproducible code (combine plots)"),
                                  div(class="control-help",
                                      "Assumes you already created: ",
                                      tags$code("uni_res"), " and ",
                                      tags$code("multi_res"),
                                      "."),
                                  div(class="mt-sm",
                                      downloadButton(ns("combined_plot_code_download"), "Download R code")
                                  ),
                                  br(), br(),
                                  verbatimTextOutput(ns("combined_plot_code"))
                                  )
                         )
                       )
              )
            )
        )
      )
    )
}

mod_regression_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    has_pkg <- requireNamespace("gtregression", quietly = TRUE)

    `%||%` <- function(a, b) if (!is.null(a)) a else b
    .build_vec <- function(x) paste(sprintf('"%s"', x), collapse = ", ")

    # ---- helpers ---------------------------------------------------------------
    .to_gt <- function(x) {
      # Accept a full result or a table; prefer gtregression::as_gt, else gtsummary::as_gt
      if (!is.null(x$table)) x <- x$table
      if (requireNamespace("gtregression", quietly = TRUE) &&
          "as_gt" %in% getNamespaceExports("gtregression")) {
        return(gtregression::as_gt(x))
      }
      if (requireNamespace("gtsummary", quietly = TRUE)) {
        return(gtsummary::as_gt(x))
      }
      stop("Install 'gtregression' or 'gtsummary' to render tables.")
    }

    .has_fn <- function(pkg, fn) {
      isTRUE(requireNamespace(pkg, quietly = TRUE)) &&
        fn %in% getNamespaceExports(pkg)
    }

    .validate_basic <- function() {
      if (!has_pkg) {
        showNotification("Package 'gtregression' is required.", type = "error")
        return(FALSE)
      }
      if (is.null(rv$data)) {
        showNotification("Load data first.", type = "message")
        return(FALSE)
      }
      if (!nzchar(input$reg_outcome) ||
          is.null(input$reg_exposures) || !length(input$reg_exposures)) {
        showNotification("Choose one outcome and at least one exposure.", type = "message")
        return(FALSE)
      }
      if (input$reg_outcome %in% input$reg_exposures) {
        showNotification("Outcome cannot be listed among exposures.", type = "error")
        return(FALSE)
      }
      TRUE
    }

    .render_table <- function(res_reactive, output_id) {
      output[[output_id]] <- gt::render_gt({
        req(res_reactive())
        .to_gt(res_reactive()$table %||% res_reactive())
      })
    }

    .render_plot <- function(res_reactive, output_id) {
      output[[output_id]] <- renderPlot({
        req(res_reactive())
        p <- try(gtregression::plot_reg(res_reactive()), silent = TRUE)
        validate(need(!inherits(p, "try-error") && !is.null(p), "No plot available."))
        p
      })
    }

    # replaces the helper inside mod_regression_server()
    .render_summary <- function(res_reactive, output_id, multivariate = FALSE) {
      output[[output_id]] <- renderPrint({
        req(res_reactive())

        # 1) print the package's model summaries (if present)
        summ <- res_reactive()$model_summaries %||% res_reactive()
        if (is.null(summ)) cat("No model summary produced.\n") else print(summ)

        # 2) diagnostics for linear models only
        if (identical(input$reg_approach, "linear")) {

          # 2a) reg_check (prefer function if exported; otherwise use $reg_check if present)
          cat("\n\n---- Model diagnostics (reg_check) ----\n")
          if (requireNamespace("gtregression", quietly = TRUE) &&
              "reg_check" %in% getNamespaceExports("gtregression")) {
            rc <- try(gtregression::reg_check(res_reactive()), silent = TRUE)
            if (!inherits(rc, "try-error")) print(rc) else cat("reg_check not available for this object.\n")
          } else if (!is.null(res_reactive()$reg_check)) {
            print(res_reactive()$reg_check)
          } else {
            cat("reg_check function or object not available.\n")
          }

          # 2b) collinearity (VIF) ONLY for multivariable linear models
          if (isTRUE(multivariate) &&
              requireNamespace("gtregression", quietly = TRUE) &&
              "check_collinearity" %in% getNamespaceExports("gtregression")) {
            cat("\n\n---- Collinearity (VIF) ----\n")
            vif_tbl <- try(gtregression::check_collinearity(res_reactive()), silent = TRUE)
            if (!inherits(vif_tbl, "try-error")) print(vif_tbl) else cat("VIF not available for this object.\n")
          }
        }
      })
    }

    .make_table_exports <- function(prefix, res_reactive, table_get = function(r) r$table) {
      output[[paste0(prefix, "_docx")]] <- downloadHandler(
        filename = paste0(prefix, ".docx"),
        content = function(file) { req(res_reactive()); gtregression::save_table(table_get(res_reactive()), filename = file, format = "docx") }
      )
      output[[paste0(prefix, "_html")]] <- downloadHandler(
        filename = paste0(prefix, ".html"),
        content = function(file) { req(res_reactive()); gtregression::save_table(table_get(res_reactive()), filename = file, format = "html") }
      )
      output[[paste0(prefix, "_pdf")]] <- downloadHandler(
        filename = paste0(prefix, ".pdf"),
        content = function(file) { req(res_reactive()); gtregression::save_table(table_get(res_reactive()), filename = file, format = "pdf") }
      )
      output[[paste0(prefix, "_png")]] <- downloadHandler(
        filename = paste0(prefix, ".png"),
        content = function(file) { req(res_reactive()); gt::gtsave(.to_gt(table_get(res_reactive())), filename = file) }
      )
    }

    .make_plot_exports <- function(prefix, plot_reactive) {
      output[[paste0(prefix, "_png")]] <- downloadHandler(
        filename = paste0(prefix, ".png"),
        content = function(file) { req(plot_reactive()); ggplot2::ggsave(file, plot = plot_reactive(), device = "png") }
      )
      output[[paste0(prefix, "_pdf")]] <- downloadHandler(
        filename = paste0(prefix, ".pdf"),
        content = function(file) { req(plot_reactive()); ggplot2::ggsave(file, plot = plot_reactive(), device = "pdf") }
      )
    }

    # ---- choices refresh --------------------------------------------------------
    observeEvent(rv$data, {
      req(rv$data)
      vars <- names(rv$data)
      updateSelectInput(session, "reg_outcome",    choices = c("", vars))
      updateCheckboxGroupInput(session, "reg_exposures", choices = vars)
      updateSelectInput(session, "reg_strat_by",   choices = c("None", vars))
    }, ignoreInit = FALSE)

    observeEvent(input$reg_outcome, {
      req(rv$data)
      vars <- names(rv$data)
      if (nzchar(input$reg_outcome))
        updateCheckboxGroupInput(session, "reg_exposures", choices = setdiff(vars, input$reg_outcome))
      else
        updateCheckboxGroupInput(session, "reg_exposures", choices = vars)
    }, ignoreInit = TRUE)

    # ---- Univariate -------------------------------------------------------------
    uni_results <- eventReactive(input$run_uni, {
      if (!.validate_basic()) return(NULL)
      res <- try(gtregression::uni_reg(
        data = rv$data,
        outcome = input$reg_outcome,
        exposures = input$reg_exposures,
        approach = input$reg_approach
      ), silent = TRUE)
      if (inherits(res, "try-error")) {
        showNotification(paste("Univariate regression failed:", as.character(res)), type = "error"); return(NULL)
      }
      session$userData$last_uni <- res
      res
    })

    .render_table(uni_results, "uni_table")
    .render_plot(uni_results, "uni_plot")
    .render_summary(uni_results, "uni_summary", multivariate = FALSE)
    .make_table_exports("uni_table", function() uni_results(), function(r) r$table)
    .make_plot_exports("uni_plot", function() { req(uni_results()); gtregression::plot_reg(uni_results()) })
    output$uni_summary_txt <- downloadHandler("uni_summary.txt", function(file) {
      req(uni_results()); writeLines(utils::capture.output(print(uni_results()$model_summaries)), file)
    })

    # ---- Multivariable ----------------------------------------------------------
    multi_results <- eventReactive(input$run_multi, {
      if (!.validate_basic()) return(NULL)
      res <- try(gtregression::multi_reg(
        data = rv$data,
        outcome = input$reg_outcome,
        exposures = input$reg_exposures,
        approach = input$reg_approach
      ), silent = TRUE)
      if (inherits(res, "try-error")) {
        showNotification(paste("Multivariable regression failed:", as.character(res)), type = "error"); return(NULL)
      }
      session$userData$last_multi <- res
      res
    })

    .render_table(multi_results, "multi_table")
    .render_plot(multi_results, "multi_plot")
    .render_summary(multi_results, "multi_summary", multivariate = TRUE)
    .make_table_exports("multi_table", function() multi_results(), function(r) r$table)
    .make_plot_exports("multi_plot", function() { req(multi_results()); gtregression::plot_reg(multi_results()) })
    output$multi_summary_txt <- downloadHandler("multi_summary.txt", function(file) {
      req(multi_results()); writeLines(utils::capture.output(print(multi_results()$model_summaries)), file)
    })

    # ---- Stratified -------------------------------------------------------------
    strat_results <- eventReactive(input$run_strat, {
      if (!.validate_basic()) return(NULL)
      if (identical(input$reg_strat_by, "None")) {
        showNotification("Pick a stratification variable.", type = "message"); return(NULL)
      }
      res <- try({
        if (isTRUE(input$reg_strat_multi)) {
          gtregression::stratified_multi_reg(
            data = rv$data, outcome = input$reg_outcome,
            exposures = input$reg_exposures, stratifier = input$reg_strat_by,
            approach = input$reg_approach
          )
        } else {
          gtregression::stratified_uni_reg(
            data = rv$data, outcome = input$reg_outcome,
            exposures = input$reg_exposures, stratifier = input$reg_strat_by,
            approach = input$reg_approach
          )
        }
      }, silent = TRUE)
      if (inherits(res, "try-error")) {
        showNotification(paste("Stratified regression failed:", as.character(res)), type = "error"); return(NULL)
      }
      session$userData$last_strat <- res
      res
    })

    .render_table(strat_results, "strat_table")
    output$strat_summary <- renderPrint({ req(strat_results()); print(strat_results()$model_summaries) })
    .make_table_exports("strat_table", function() strat_results(), function(r) r$table)
    output$strat_summary_txt <- downloadHandler("strat_summary.txt", function(file) {
      req(strat_results()); writeLines(utils::capture.output(print(strat_results()$model_summaries)), file)
    })

    # ---- Reproducible code (unchanged UI) --------------------------------------
    output$uni_code <- renderText({
      req(nzchar(input$reg_outcome), length(input$reg_exposures))
      paste0(
        '# ---- Univariate regression ----
# install.packages("gtregression") # if needed
library(gtregression)

res <- gtregression::uni_reg(
  data = df,
  outcome = "', input$reg_outcome, '",
  exposures = c(', .build_vec(input$reg_exposures), '),
  approach = "', input$reg_approach, '"
)

gt_tbl <- gtregression::as_gt(res$table)
p <- gtregression::plot_reg(res)

# Exports:
# gtregression::save_table(res$table, filename = "uni_table.docx", format = "docx")
# ggplot2::ggsave("uni_plot.png", plot = p, device = "png")'
      )
    })
  output$uni_code_download <- downloadHandler(
    filename = function() "uni_regression_code.R",
    content  = function(file) writeLines(output$uni_code() %||% "", con = file)
  )

  output$multi_code <- renderText({
    req(nzchar(input$reg_outcome), length(input$reg_exposures))
    paste0(
      '# ---- Multivariable regression ----
# install.packages("gtregression") # if needed
library(gtregression)

res <- gtregression::multi_reg(
  data = df,
  outcome = "', input$reg_outcome, '",
  exposures = c(', .build_vec(input$reg_exposures), '),
  approach = "', input$reg_approach, '"
)

gt_tbl <- gtregression::as_gt(res$table)
p <- gtregression::plot_reg(res)

# Exports:
# gtregression::save_table(res$table, filename = "multi_table.docx", format = "docx")
# ggplot2::ggsave("multi_plot.png", plot = p, device = "png")'
    )
  })
output$multi_code_download <- downloadHandler(
  filename = function() "multivariable_regression_code.R",
  content  = function(file) writeLines(output$multi_code() %||% "", con = file)
)

output$strat_code <- renderText({
  req(nzchar(input$reg_outcome), length(input$reg_exposures), !identical(input$reg_strat_by, "None"))
  fun <- if (isTRUE(input$reg_strat_multi)) "stratified_multi_reg" else "stratified_uni_reg"
  paste0(
    '# ---- Stratified regression ----
# install.packages("gtregression") # if needed
library(gtregression)

res <- gtregression::', fun, '(
  data = df,
  outcome = "', input$reg_outcome, '",
  exposures = c(', .build_vec(input$reg_exposures), '),
  stratifier = "', input$reg_strat_by, '",
  approach = "', input$reg_approach, '"
)

gt_tbl <- gtregression::as_gt(res$table)

# Exports:
# gtregression::save_table(res$table, filename = "strat_table.docx", format = "docx")'
  )
})
output$strat_code_download <- downloadHandler(
  filename = function() "stratified_regression_code.R",
  content  = function(file) writeLines(output$strat_code() %||% "", con = file)
)

# ---- Combine Results: Tables ------------------------------------------------
get_desc_tbl <- function(x) {
  if (is.null(x)) return(NULL)
  if (!is.null(x$table)) x$table else x
}

.as_gtsummary <- function(x) {
  if (!is.null(x$table)) x <- x$table
  if (inherits(x, c("tbl_summary", "tbl_regression", "gtsummary"))) return(x)
  stop("Object is not a gtsummary table.")
}

.combine_with_spanners <- function(sel, src_list) {
  lbl_map <- c(desc = "Descriptive", uni = "Univariate", multi = "Multivariable")
  tbls    <- lapply(sel, function(k) .as_gtsummary(src_list[[k]]))
  spn     <- unname(lbl_map[sel])

  if (.has_fn("gtsummary", "tbl_merge")) {
    out <- try(gtsummary::tbl_merge(tbls = tbls, tab_spanner = spn), silent = TRUE)
    if (!inherits(out, "try-error")) return(out)
  }
  if (.has_fn("gtregression", "merge_tables")) {
    out2 <- try(Reduce(gtregression::merge_tables, tbls), silent = TRUE)
    if (!inherits(out2, "try-error")) return(out2)
  }
  stop("Merging failed. Tables may have incompatible structures.")
}

has_desc  <- reactive({ !is.null(session$userData$last_desc_table) })
has_uni   <- reactive({ !is.null(uni_results()) })
has_multi <- reactive({ !is.null(multi_results()) })

output$combine_tbl_choices_ui <- renderUI({
  choices <- character(0)
  if (isTRUE(has_desc()))  choices <- c(choices, "Descriptive"   = "desc")
  if (isTRUE(has_uni()))   choices <- c(choices, "Univariate"    = "uni")
  if (isTRUE(has_multi())) choices <- c(choices, "Multivariable" = "multi")

  if (!length(choices)) return(div(class="control-help", "Run an analysis to enable combining."))

  prev <- isolate(input$combine_sources_tbl)
  default <- intersect(prev %||% character(0), names(choices))
  checkboxGroupInput(ns("combine_sources_tbl"), "Select tables to merge",
                     choices = choices, selected = default)
})

combined_table <- eventReactive(input$run_combine_tbl, {
  sel <- input$combine_sources_tbl
  validate(need(length(sel) >= 1, "Select at least one table to merge."))

  src <- list(
    desc  = if (isTRUE(has_desc()))  get_desc_tbl(session$userData$last_desc_table) else NULL,
    uni   = if (isTRUE(has_uni()))   uni_results()$table else NULL,
    multi = if (isTRUE(has_multi())) multi_results()$table else NULL
  )
  src <- src[sel]

  if (length(src) == 1) return(src[[1]])

  res <- try(.combine_with_spanners(sel, src), silent = TRUE)
  if (inherits(res, "try-error")) {
    showNotification("Could not merge selected tables (structures may differ). Showing first table only.",
                     type = "warning")
    return(src[[1]])
  }
  res
})

output$combined_table <- gt::render_gt({
  req(combined_table()); .to_gt(combined_table())
})

output$combined_table_docx <- downloadHandler("combined_table.docx", function(file) {
  req(combined_table()); gtregression::save_table(combined_table(), filename = file, format = "docx")
})
output$combined_table_html <- downloadHandler("combined_table.html", function(file) {
  req(combined_table()); gtregression::save_table(combined_table(), filename = file, format = "html")
})
output$combined_table_pdf  <- downloadHandler("combined_table.pdf", function(file) {
  req(combined_table()); gtregression::save_table(combined_table(), filename = file, format = "pdf")
})
output$combined_table_png  <- downloadHandler("combined_table.png", function(file) {
  req(combined_table()); gt::gtsave(.to_gt(combined_table()), filename = file)
})

# ---- Combine Results: Plots -------------------------------------------------
combined_plot <- eventReactive(input$run_combine_plot, {
  validate(need(isTRUE(input$combine_plots_enable), "Tick the box to combine plots."))
  validate(need(isTRUE(has_uni()) && isTRUE(has_multi()),
                "Need both Univariate and Multivariable results to combine plots."))

  if (.has_fn("gtregression", "plot_reg_combine"))
    return(gtregression::plot_reg_combine(uni_results(), multi_results()))

  # simple fallback: show uni plot (keeps UX consistent without extra deps)
  showNotification("plot_reg_combine() not found; showing univariate plot only.", type = "warning")
  gtregression::plot_reg(uni_results())
})

output$combined_plot <- renderPlot({ req(combined_plot()); combined_plot() })
output$combined_plot_png <- downloadHandler("combined_plot.png", function(file) {
  req(combined_plot()); ggplot2::ggsave(file, plot = combined_plot(), device = "png")
})
output$combined_plot_pdf <- downloadHandler("combined_plot.pdf", function(file) {
  req(combined_plot()); ggplot2::ggsave(file, plot = combined_plot(), device = "pdf")
})

# ---- Reproducible code for combine (kept, minimal) -------------------------
build_combine_tbl_code <- reactive({
  sel <- input$combine_sources_tbl %||% character(0)
  validate(need(length(sel) >= 1, "Select at least one table to merge to build code."))

  lbl_map <- c(desc = "Descriptive", uni = "Univariate", multi = "Multivariable")
  obj_map <- c(desc = "desc_tbl",     uni = "uni_res$table",  multi = "multi_res$table")
  lbls <- lbl_map[sel]; objs <- obj_map[sel]

  if (length(sel) == 1L) {
    return(paste0(
      '# ---- Combine tables (single selection) ----
# Assumes object exists: ', objs, '
# install.packages("gtregression"); library(gtregression)
tbl_merged <- ', objs, '
gt_tbl <- gtregression::as_gt(tbl_merged)'
    ))
  }

  paste0(
    '# ---- Combine tables with spanners ----
# Assumes objects exist: desc_tbl (optional), uni_res, multi_res
# install.packages("gtsummary"); install.packages("gtregression")
library(gtsummary); library(gtregression)
tbl_merged <- gtsummary::tbl_merge(
  tbls = list(', paste(objs, collapse = ", "), '),
  tab_spanner = c(', .build_vec(lbls), ')
)
gt_tbl <- gtregression::as_gt(tbl_merged)

# Fallback:
# tbl_merged <- Reduce(gtregression::merge_tables, list(', paste(objs, collapse = ", "), '))
# gt_tbl <- gtregression::as_gt(tbl_merged)'
  )
})
output$combined_tbl_code <- renderText(build_combine_tbl_code())
output$combined_tbl_code_download <- downloadHandler(
  filename = function() "combined_tables_code.R",
  content  = function(file) writeLines(build_combine_tbl_code(), con = file)
)

build_combine_plot_code <- reactive({
  validate(need(isTRUE(has_uni()) && isTRUE(has_multi()),
                "Need both Univariate and Multivariable results to build plot code."))
  paste0(
    '# ---- Combine regression plots ----
# Assumes objects exist: uni_res, multi_res
# install.packages("gtregression"); library(gtregression)
if ("plot_reg_combine" %in% getNamespaceExports("gtregression")) {
  p_combined <- gtregression::plot_reg_combine(uni_res, multi_res)
} else {
  # Fallback: show univariate plot when combiner not available
  p_combined <- gtregression::plot_reg(uni_res)
}
# ggplot2::ggsave("combined_plot.png", plot = p_combined)'
  )
})
output$combined_plot_code <- renderText(build_combine_plot_code())
output$combined_plot_code_download <- downloadHandler(
  filename = function() "combined_plots_code.R",
  content  = function(file) writeLines(build_combine_plot_code(), con = file)
)
  })
}
