# inst/shiny/modules/descriptive.R

mod_descriptive_ui <- function(id) {
  ns <- NS(id)
  tabPanel(
    tagList("Descriptive analysis", span(class = "section-badge","")),
    fluidRow(
      column(
        width = 4,
        div(class="card-like sidebar-sticky",
            h3("Create Descriptive Summary Table", class="mb-md"),
            div(class="control-help",
                "Select exposures to summarise.
                Optionally stratify (e.g., by outcome).
                Do not select same variable as exposure and stratify."),
            checkboxGroupInput(ns("desc_exposures"), "Select Exposures (Multiple)", choices = NULL),
            selectInput(ns("desc_by"), "Stratify By (Optional, e.g., Outcome)", choices = c("None")),
            selectInput(ns("desc_percent"), "Percentage Type",
                        choices = c("Column (default)" = "column",
                                    "Row" = "row"),
                        selected = "column"),
            selectInput(ns("desc_show_overall"), "Adds Overall Column to the table",
                        choices = c("No (default)" = "no", "First" = "first", "Last" = "last"),
                        selected = "no"),
            actionButton(ns("run_desc"),
                         label = tagList(icon("chart-simple"), "Run Descriptive Table"),
                         class = "btn-primary mt-md"),
            tags$hr(),
            strong("Export"),
            div(class="mt-sm",
                downloadButton(ns("desc_export_docx"), "DOCX"),
                downloadButton(ns("desc_export_html"), "HTML"),
                downloadButton(ns("desc_export_pdf"),  "PDF"),
                downloadButton(ns("desc_export_png"),  "PNG")
            ),
            tags$hr(),
            strong("Reproducible code"),
            div(class="mt-sm",
                downloadButton(ns("desc_code_download"), "Download R code")
            )
        )
      ),
      column(
        width = 8,
        div(class="card-like",
            h4(tagList(icon("table-list"), " Descriptive Table")),
            gt::gt_output(ns("desc_table")),
            tags$hr(),
            h5("R code to reproduce this table"),
            div(class="control-help",
                "Assumes your data frame is named ",
                tags$code("df"),
                ". Copy & paste into your script."
            ),
            verbatimTextOutput(ns("desc_code"))
        )
      )
    )
  )
}

mod_descriptive_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ---------- 1) Update variable choices whenever data changes ----------
    observeEvent(rv$data, {
      req(rv$data)
      vars <- names(rv$data)
      updateCheckboxGroupInput(session, "desc_exposures", choices = vars)
      updateSelectInput(session, "desc_by", choices = c("None", vars))
    }, ignoreInit = FALSE)

    # ---------- helper: build reproducible code as a string ----------
    build_desc_code <- reactive({
      req(rv$data)
      exps <- input$desc_exposures
      if (is.null(exps) || !length(exps)) {
        return("# Select at least one exposure to generate code")
      }
      exp_str <- paste(sprintf('"%s"', exps), collapse = ", ")

      by_var <- if (identical(input$desc_by, "None")) NULL else input$desc_by
      percent <- if (identical(input$desc_percent, "row")) "row" else "column"
      so <- input$desc_show_overall
      show_overall <- if (is.null(so) || identical(so, "no")) NULL else match.arg(so, c("first", "last"))

      args <- c(
        'data = df',
        sprintf('exposures = c(%s)', exp_str),
        if (!is.null(by_var)) sprintf('by = "%s"', by_var),
        sprintf('percent = "%s"', percent),
        if (!is.null(show_overall)) sprintf('show_overall = "%s"', show_overall)
      )

      paste0(
        '# ---- reproducible code for descriptive table ----
# install.packages("gtregression")  # if needed
library(gtregression)

# df <- <your data frame>  # ensure your dataset is assigned to `df`

tbl <- gtregression::descriptive_table(
  ', paste(args, collapse = ",\n  "), '
)

# As a gt object for display
gt_tbl <- gtsummary::as_gt(tbl)

# Optional exports:
# gtregression::save_table(tbl, filename = "descriptive_table.docx", format = "docx")
# gtregression::save_table(tbl, filename = "descriptive_table.html", format = "html")
# gtregression::save_table(tbl, filename = "descriptive_table.pdf",  format = "pdf")'
      )
    })

  # ---------- 2) Compute result when user clicks "Run" ----------
  desc_results <- eventReactive(input$run_desc, {
    req(rv$data)
    if (!requireNamespace("gtregression", quietly = TRUE)) {
      showNotification("Package 'gtregression' is required for descriptive tables.",
                       type = "error")
      return(NULL)
    }
    if (is.null(input$desc_exposures) || length(input$desc_exposures) == 0) {
      showNotification("Select at least one exposure variable.", type = "message")
      return(NULL)
    }

    by_var <- if (identical(input$desc_by, "None")) NULL else input$desc_by
    percent <- if (identical(input$desc_percent, "row")) "row" else "column"
    so <- input$desc_show_overall
    show_overall <- if (is.null(so) || identical(so, "no")) NULL else match.arg(so, c("first", "last"))

    res <- try(
      gtregression::descriptive_table(
        data = rv$data,
        exposures = input$desc_exposures,
        by = by_var,
        percent = percent,
        show_overall = show_overall
      ),
      silent = TRUE
    )

    if (inherits(res, "try-error")) {
      showNotification(paste("Failed to build table:", as.character(res)), type = "error")
      return(NULL)
    }

    # expose to Combine Results (optional)
    session$userData$last_desc_table <- res
    res
  })

  # ---------- 3) Render table ----------
  output$desc_table <- gt::render_gt({
    req(desc_results())
    gtsummary::as_gt(desc_results())
  })

  # ---------- 4) Exports ----------
  output$desc_export_docx <- downloadHandler(
    filename = "descriptive_table.docx",
    content = function(file) {
      req(desc_results()); gtregression::save_table(desc_results(), filename = file, format = "docx")
    }
  )
  output$desc_export_html <- downloadHandler(
    filename = "descriptive_table.html",
    content = function(file) {
      req(desc_results()); gtregression::save_table(desc_results(), filename = file, format = "html")
    }
  )
  output$desc_export_pdf <- downloadHandler(
    filename = "descriptive_table.pdf",
    content = function(file) {
      req(desc_results()); gtregression::save_table(desc_results(), filename = file, format = "pdf")
    }
  )
  output$desc_export_png <- downloadHandler(
    filename = "descriptive_table.png",
    content = function(file) {
      req(desc_results()); gt::gtsave(gtsummary::as_gt(desc_results()), filename = file)
    }
  )

  # ---------- 5) Show + download reproducible code ----------
  output$desc_code <- renderText(build_desc_code())

  output$desc_code_download <- downloadHandler(
    filename = function() "descriptive_table_code.R",
    content = function(file) {
      writeLines(build_desc_code(), con = file)
    }
  )
  })
}
