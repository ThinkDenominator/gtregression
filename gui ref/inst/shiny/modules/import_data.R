# inst/shiny/modules/import_data.R

mod_import_ui <- function(id) {
  ns <- NS(id)
  tabPanel(
    tagList("Import Data", span(class = "section-badge","")),
    fluidRow(
      column(
        width = 4,
        div(class="card-like sidebar-sticky",
            h3("Upload Your Dataset", class="mb-md"),
            div(class="control-help",
                "Upload a CSV or XLSX file, or load a built-in dataset."),

            fileInput(ns("data_upload"),
                      label = tagList(icon("file"), " Upload File"),
                      accept = c(".csv", ".xlsx")),

            # --- CSV options (collapsible) ---
            tags$details(
              tags$summary("Import options (CSV)"),
              radioButtons(ns("csv_sep"), "Separator", inline = TRUE,
                           choices = c(Comma = ",", Semicolon = ";", Tab = "\\t"), selected = ","),
              selectInput(ns("csv_dec"), "Decimal mark", choices = c("." = ".", "," = ","), selected = "."),
              checkboxInput(ns("csv_header"), "First row is header", value = TRUE),
              selectInput(ns("csv_encoding"), "Encoding",
                          choices = c("UTF-8", "Latin1", "Windows-1252", "MacRoman"), selected = "UTF-8"),
              textInput(ns("csv_na"), "NA strings (comma-separated)", placeholder = "NA, , .")
            ),

            # --- Excel options (collapsible; shown only when .xlsx uploaded) ---
            tags$details(
              tags$summary("Excel options"),
              uiOutput(ns("xlsx_sheet_ui"))
            ),

            tags$hr(),

            selectInput(ns("builtin_dataset"),
                        label = tagList(icon("database"), " Load Built-in gtregression Dataset"),
                        choices = "None"),
            actionButton(ns("load_data"),
                         label = tagList(icon("play"), " Load Dataset"),
                         class = "btn-primary mt-sm"),
            actionButton(ns("reset_import"),
                         label = tagList(icon("undo"), " Reset"),
                         class = "mt-sm"),

            tags$hr(),
            strong("Save data"),
            div(class="mt-sm",
                downloadButton(ns("download_csv"), "Download CSV"),
                downloadButton(ns("download_rds"), "Download RDS"),
                downloadButton(ns("download_xlsx"), "Download XLSX")
            )
        )
      ),
      column(
        width = 8,
        div(class="card-like",
            h4(tagList(icon("table"), " Data Preview")),
            div(class="control-help","Use the length menu to change rows per page"),
            DT::dataTableOutput(ns("data_preview")),
            tags$hr(),
            h5("Data info"),
            verbatimTextOutput(ns("data_info")),
            h5("Reproducible import code"),
            verbatimTextOutput(ns("import_code"))
        )
      )
    )
  )
}

mod_import_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # small helper
    `%||%` <- function(a, b) if (!is.null(a) && nzchar(a)) a else b
    has_rio <- requireNamespace("rio", quietly = TRUE)

    # -------- Populate built-in datasets (if gtregression is available) --------
    observe({
      if (requireNamespace("gtregression", quietly = TRUE)) {
        items <- try(utils::data(package = "gtregression")$results[, "Item"], silent = TRUE)
        items <- items[!is.na(items) & nzchar(items)]
        if (length(items) == 0) items <- NULL
        updateSelectInput(session, "builtin_dataset",
                          choices = c("None", items))
      } else {
        updateSelectInput(session, "builtin_dataset", choices = "None")
      }
    })

    # -------- Excel sheet UI (only when an .xlsx is uploaded) --------
    output$xlsx_sheet_ui <- renderUI({
      up <- input$data_upload
      if (is.null(up)) return(NULL)
      ext <- tolower(tools::file_ext(up$name))
      if (ext != "xlsx") return(NULL)

      if (!requireNamespace("readxl", quietly = TRUE)) {
        return(div(class = "control-help",
                   "Install 'readxl' to list sheets (install.packages('readxl'))."))
      }
      sheets <- readxl::excel_sheets(up$datapath)
      tagList(
        selectInput(ns("xlsx_sheet"), "Sheet", choices = sheets),
        textInput(ns("xlsx_range"), "Range (optional)", placeholder = "e.g. A1:H200")
      )
    })

    # -------- Reactive: read current upload (rio-first, fallback to base/readxl) --------
    read_current_upload <- reactive({
      up <- input$data_upload
      req(up)
      ext <- tolower(tools::file_ext(up$name))

      if (has_rio) {
        args <- list(file = up$datapath)
        if (ext == "xlsx") {
          if (!is.null(input$xlsx_sheet) && nzchar(input$xlsx_sheet)) args$sheet <- input$xlsx_sheet
          if (!is.null(input$xlsx_range) && nzchar(input$xlsx_range)) args$range <- input$xlsx_range
        }
        withProgress(message = "Reading file (rio)...", value = 0.2, {
          df <- do.call(rio::import, args)
          incProgress(0.8)
          return(as.data.frame(df))
        })
      } else {
        if (ext == "csv") {
          nas <- unique(trimws(unlist(strsplit(input$csv_na %||% "NA", ","))))
          nas <- nas[nzchar(nas)]
          sep <- switch(input$csv_sep, "," = ",", ";" = ";", "\\t" = "\t")
          dec <- input$csv_dec
          enc <- input$csv_encoding
          withProgress(message = "Reading CSV...", value = 0.2, {
            df <- utils::read.csv(
              up$datapath,
              sep = sep, dec = dec, header = isTRUE(input$csv_header),
              na.strings = if (length(nas)) nas else "NA",
              fileEncoding = enc, stringsAsFactors = TRUE
            )
            incProgress(0.8)
            as.data.frame(df)
          })
        } else if (ext == "xlsx") {
          if (!requireNamespace("readxl", quietly = TRUE)) {
            stop("Package 'readxl' is required to read .xlsx when 'rio' is not installed.")
          }
          sh  <- input$xlsx_sheet %||% 1
          rng <- input$xlsx_range
          withProgress(message = "Reading Excel...", value = 0.2, {
            df <- if (!is.null(rng) && nzchar(rng)) {
              readxl::read_excel(up$datapath, sheet = sh, range = rng)
            } else {
              readxl::read_excel(up$datapath, sheet = sh)
            }
            incProgress(0.8)
            as.data.frame(df)
          })
        } else {
          stop("Unsupported file type: .", ext)
        }
      }
    })

    # -------- Apply upload + option changes to rv$data --------
    observeEvent(
      list(input$data_upload$datapath, input$csv_sep, input$csv_dec,
           input$csv_header, input$csv_encoding, input$csv_na,
           input$xlsx_sheet, input$xlsx_range),
      {
        req(input$data_upload$datapath)
        df <- try(read_current_upload(), silent = TRUE)
        if (inherits(df, "try-error")) {
          showNotification(paste("Failed to read file:", as.character(df)), type = "error")
          return(invisible())
        }
        if (nrow(df) == 0) {
          showNotification("Uploaded file has 0 rows.", type = "error")
          return(invisible())
        }
        rv$data <- df
      },
      ignoreInit = TRUE
    )

    # -------- Load built-in dataset from gtregression --------
    observeEvent(input$load_data, {
      ds <- input$builtin_dataset
      if (is.null(ds) || ds == "None") {
        showNotification("Select a built-in dataset or upload a file.", type = "message")
        return(invisible())
      }
      if (!requireNamespace("gtregression", quietly = TRUE)) {
        showNotification("Package 'gtregression' not installed; built-ins unavailable.", type = "error")
        return(invisible())
      }
      env <- new.env(parent = emptyenv())
      ok  <- try(utils::data(list = ds, package = "gtregression", envir = env), silent = TRUE)
      if (!inherits(ok, "try-error") && exists(ds, envir = env, inherits = FALSE)) {
        rv$data <- get(ds, envir = env)
      } else if (exists(ds, envir = asNamespace("gtregression"), inherits = FALSE)) {
        rv$data <- get(ds, envir = asNamespace("gtregression"))
      } else {
        showNotification(paste0("Couldn't find dataset '", ds, "' in gtregression."), type = "error")
      }
    })

    # -------- Reset --------
    observeEvent(input$reset_import, {
      updateSelectInput(session, "builtin_dataset", selected = "None")
      updateTextInput(session, "csv_na", value = "")
      rv$data <- NULL
    })

    # -------- Preview (full data with paging & 'All') --------
    output$data_preview <- DT::renderDataTable({
      req(rv$data)
      DT::datatable(
        rv$data,
        filter   = "top",
        rownames = FALSE,
        options  = list(
          scrollX     = TRUE,
          deferRender = TRUE,
          pageLength  = 10,
          lengthMenu  = list(c(10, 25, 50, 100, -1), c(10, 25, 50, 100, "All"))
        )
      )
    }, server = TRUE)

    # -------- Info box --------
    output$data_info <- renderText({
      req(rv$data)
      df <- rv$data
      types <- vapply(df, function(x) class(x)[1], character(1))
      counts <- sort(table(types), decreasing = TRUE)
      paste0(
        nrow(df), " rows × ", ncol(df), " cols | Size: ", format(object.size(df), units = "auto"), "\n",
        "Types: ", paste(paste(names(counts), counts, sep = "×"), collapse = ", ")
      )
    })

    # -------- Reproducible code --------
    output$import_code <- renderText({
      up <- input$data_upload
      ds <- input$builtin_dataset

      if (!is.null(up)) {
        ext <- tolower(tools::file_ext(up$name))
        if (ext == "csv") {
          code <- 'df <- rio::import("path/to/your.csv")'
          if (!has_rio) code <- paste0('# install.packages("rio")\n', code)
          return(code)
        } else if (ext == "xlsx") {
          sheet <- input$xlsx_sheet %||% ""
          range <- input$xlsx_range %||% ""
          args  <- c(
            if (nzchar(sheet)) sprintf('sheet = "%s"', sheet) else NULL,
            if (nzchar(range)) sprintf('range = "%s"', range) else NULL
          )
          code <- sprintf(
            'df <- rio::import("path/to/your.xlsx"%s)',
            if (length(args)) paste0(", ", paste(args, collapse = ", ")) else ""
          )
          if (!has_rio) code <- paste0('# install.packages("rio")\n', code)
          return(code)
        }
        return("")
      }

      if (!is.null(ds) && ds != "None") {
        return(sprintf('data("%s", package = "gtregression"); df <- %s', ds, ds))
      }

      ""
    })

    # -------- Downloads --------
    output$download_csv <- downloadHandler(
      filename = function() "data.csv",
      content  = function(file) {
        if (has_rio) rio::export(rv$data, file) else utils::write.csv(rv$data, file, row.names = FALSE)
      }
    )
    output$download_rds <- downloadHandler(
      filename = function() "data.rds",
      content  = function(file) {
        if (has_rio) rio::export(rv$data, file) else saveRDS(rv$data, file)
      }
    )

    output$download_xlsx <- downloadHandler(
     filename = function() "data.xlsx",
     content  = function(file) {
         if (has_rio) rio::export(rv$data, file) else stop("Install 'rio' for XLSX export.")
       }
     )
  })
}
