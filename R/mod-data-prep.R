# Internal Shiny module for safe, reversible data preparation.

mod_data_prep_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::tags$style(shiny::HTML(".gtx-prep-toolbar .nav { border-bottom: 1px solid #D9D9D4; margin-bottom: 0; display: flex; flex-wrap: wrap; gap: 2px; } .gtx-prep-toolbar .nav > li { float: none; } .gtx-prep-toolbar .nav > li > a { font-weight: 750; padding: 9px 13px; color:#3D3D39; border-radius: 7px 7px 0 0; } .gtx-prep-toolbar .nav > li > a:hover, .gtx-prep-toolbar .nav > li > a:focus { background:#ECECEA; color:#111111; } .gtx-prep-templates { margin-top: 12px; padding: 10px 12px; border-radius: 8px; background: #F7F7F5; border: 1px solid #E3E3DE; } .gtx-prep-templates strong { margin-right: 8px; } .gtx-prep-templates .btn { margin: 3px 5px 3px 0; } .gtx-prep-actions .btn { margin-right: 7px; margin-bottom: 8px; } .gtx-prep-apply { margin-top: 18px; padding-top: 14px; border-top: 1px solid #E3E3DE; } .gtx-value-guide { margin-top: 12px; padding: 11px 13px; border: 1px solid #D9D9D4; border-radius: 8px; background: #FAFAF8; } .gtx-value-guide h5 { margin: 0 0 6px; font-weight: 750; } .gtx-value-guide pre { white-space: pre-wrap; margin: 8px 0 0; padding: 8px; font-size: .9em; } .gtx-recode-map { margin-top: 14px; padding: 12px; border: 1px solid #D9D9D4; border-radius: 8px; background: #FDFDFC; } .gtx-recode-map h4 { margin: 0 0 4px; } .gtx-impact-table { width: 100%; margin-top: 8px; font-size: .92em; background: #fff; } .gtx-impact-table th, .gtx-impact-table td { padding: 7px 8px; border-bottom: 1px solid #E3E3DE; text-align: left; vertical-align: middle; } .gtx-impact-table th { font-weight: 750; color: #3D3D39; background: #F5F5F2; } .gtx-impact-table .form-group { margin-bottom: 0; } .gtx-impact-table .form-control { min-width: 170px; } @media (max-width: 767px) { .gtx-prep-toolbar .nav > li > a { padding: 8px 9px; font-size: .88em; } .gtx-impact-table { font-size: .85em; } .gtx-impact-table .form-control { min-width: 120px; } }")),
    shiny::div(class = "gtx-card",
      shiny::tags$h3("Data Prep"),
      shiny::tags$p(class = "help-copy", "Choose one clear task below. Each change is made to a working copy, can be undone, and becomes reproducible R code."),
      shiny::div(class = "gtx-prep-toolbar",
        shiny::tags$ul(class = "nav nav-pills",
          shiny::tags$li(shiny::actionLink(ns("menu_label"), "Set display label")),
          shiny::tags$li(shiny::actionLink(ns("menu_rename"), "Rename variable")),
          shiny::tags$li(shiny::actionLink(ns("menu_recode"), "Recode values")),
          shiny::tags$li(shiny::actionLink(ns("menu_type"), "Set type & order")),
          shiny::tags$li(shiny::actionLink(ns("menu_missing"), "Manage missing values")),
          shiny::tags$li(shiny::actionLink(ns("menu_calculation"), "Create calculation")),
          shiny::tags$li(shiny::actionLink(ns("menu_groups"), "Create groups")),
          shiny::tags$li(shiny::actionLink(ns("menu_filter"), "Filter observations")),
          shiny::tags$li(shiny::actionLink(ns("menu_keep"), "Keep variables"))
        )
      ),
      shiny::div(class = "gtx-prep-templates",
        shiny::tags$strong("Quick starts:"),
        shiny::actionButton(ns("template_age"), "Age groups", class = "btn-default btn-sm"),
        shiny::actionButton(ns("template_binary"), "0/1 to No/Yes", class = "btn-default btn-sm"),
        shiny::actionButton(ns("template_bmi"), "BMI-style calculation", class = "btn-default btn-sm"),
        shiny::actionButton(ns("template_missing"), "99/999 to missing", class = "btn-default btn-sm"),
        shiny::tags$span(class = "help-copy", "They fill a starting point only; review before applying.")
      )
    ),
    shiny::fluidRow(
      shiny::column(4, shiny::div(class = "cardish",
        shiny::tags$h4("Set up this change"), shiny::uiOutput(ns("operation_hint")), shiny::uiOutput(ns("controls")), shiny::uiOutput(ns("impact_preview")), shiny::uiOutput(ns("value_guide")),
        shiny::div(class = "gtx-prep-apply", shiny::tags$p(class = "help-copy", "Review the preview, then apply this one change."), shiny::actionButton(ns("apply"), "Apply this change", class = "btn-primary"))
      )),
      shiny::column(8, shiny::div(class = "gtx-card",
        shiny::tags$div(class = "card-heading", shiny::tags$h4("Working-data preview"), shiny::textOutput(ns("status"))),
        gt::gt_output(ns("preview"))
      ))
    ),
    shiny::div(class = "gtx-card gtx-prep-actions",
      shiny::tags$h4("Change controls"),
      shiny::actionButton(ns("undo"), "Undo last change"),
      shiny::actionButton(ns("redo"), "Redo"),
      shiny::actionButton(ns("reset"), "Reset all"),
      shiny::tags$hr(),
      shiny::tags$strong("Analysis dataset: "),
      shiny::textOutput(ns("analysis_dataset"), inline = TRUE),
      shiny::tags$p(
        class = "help-copy",
        "Analyses always use the current working data: the original data until you apply a change, then the prepared data. Undo, Redo and Reset update it immediately."
      ),
      shiny::tags$hr(),
      shiny::tags$strong("Download the prepared data: "),
      shiny::selectInput(ns("prepared_format"), NULL, choices = c("CSV" = "csv", "Excel (.xlsx)" = "xlsx", "R data (.rds)" = "rds", "Stata (.dta)" = "dta"), selected = "csv", width = "160px"),
      shiny::downloadButton(ns("download_prepared"), "Download prepared data")
    ),
    shiny::tabsetPanel(type = "tabs",
      shiny::tabPanel("Change log", shiny::div(class = "gtx-card", shiny::verbatimTextOutput(ns("history")))),
      shiny::tabPanel("Reusable code", shiny::div(class = "gtx-card", shiny::tags$div(class = "card-heading", shiny::tags$h4("Copy this into RStudio"),
        shiny::tags$button(type = "button", class = "btn btn-default copy-code", `data-copy-target` = ns("code"), "Copy code"),
        shiny::downloadButton(ns("download_code"), "Download .R")), shiny::verbatimTextOutput(ns("code"))))
    )
  )
}

mod_data_prep_server <- function(id, source_data) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # One authoritative working dataset: it is the original data until a change
    # is applied, and Apply, Undo, Redo and Reset all update it. Analyses use it.
    state <- shiny::reactiveValues(raw_data = NULL, working_data = NULL, history = list(), redo_stack = list(), renames = list(), source_id = 0L)
    gtx_safe_observe_event(source_data(), {
      state$raw_data <- source_data(); state$working_data <- source_data()
      state$history <- list(); state$redo_stack <- list(); state$renames <- list()
      prefill(list())
      # Identifies the imported dataset; preparation changes keep it.
      state$source_id <- shiny::isolate(state$source_id) + 1L
    }, ignoreInit = FALSE)
    # Variable-name changes, in order, as the working data moves between
    # states (Apply, Undo, Redo, Reset), so that selections elsewhere in the
    # app can follow a renamed variable.
    rename_events <- function(entry, reverse = FALSE) {
      events <- lapply(Filter(function(st) identical(st$operation, "rename"), entry$steps %||% list()), function(st) {
        if (reverse) c(from = st$args$new_name, to = st$args$variable) else c(from = st$args$variable, to = st$args$new_name)
      })
      if (reverse) rev(events) else events
    }
    log_renames <- function(events) if (length(events)) state$renames <- c(state$renames, events)
    # A variable chosen elsewhere in the Prepare workspace (variable-first
    # actions). Fields that choose a variable start from it when it applies.
    focus_in <- function(choices) {
      focus <- shiny::isolate(input$focus_variable) %||% ""
      if (nzchar(focus) && focus %in% choices) focus else NULL
    }
    variables <- shiny::reactive(names(state$working_data %||% data.frame()))
    operation <- shiny::reactiveVal("label")
    # Quick-start values for the fields of the chosen operation. Fields read
    # them when they are created, so values reach inputs that only exist
    # after nested controls render (no timing-dependent updates).
    prefill <- shiny::reactiveVal(list())
    pf <- function(id, default) {
      value <- prefill()[[id]]
      if (is.null(value)) default else value
    }
    transform_mode <- shiny::reactiveVal("arithmetic")
    choose_operation <- function(selected_operation, mode = NULL) {
      operation(selected_operation)
      if (!is.null(mode)) transform_mode(mode)
    }
    gtx_safe_observe_event(input$menu_rename, { prefill(list()); choose_operation("rename") }, ignoreInit = TRUE)
    gtx_safe_observe_event(input$menu_label, { prefill(list()); choose_operation("label") }, ignoreInit = TRUE)
    gtx_safe_observe_event(input$menu_recode, { prefill(list()); choose_operation("recode") }, ignoreInit = TRUE)
    gtx_safe_observe_event(input$menu_type, { prefill(list()); choose_operation("type") }, ignoreInit = TRUE)
    gtx_safe_observe_event(input$menu_missing, { prefill(list()); choose_operation("missing") }, ignoreInit = TRUE)
    gtx_safe_observe_event(input$menu_filter, { prefill(list()); choose_operation("filter") }, ignoreInit = TRUE)
    gtx_safe_observe_event(input$menu_keep, { prefill(list()); choose_operation("keep") }, ignoreInit = TRUE)
    gtx_safe_observe_event(input$menu_calculation, { prefill(list()); choose_operation("transform", "arithmetic") }, ignoreInit = TRUE)
    gtx_safe_observe_event(input$menu_groups, { prefill(list()); choose_operation("transform", "case_when") }, ignoreInit = TRUE)
    gtx_safe_observe_event(list(operation(), input$variable), {
      if (!identical(operation(), "label")) return(NULL)
      variable <- input$variable %||% ""
      if (!nzchar(variable) || !variable %in% names(state$working_data)) return(NULL)
      label <- attr(state$working_data[[variable]], "label", exact = TRUE) %||% ""
      session$onFlushed(function() {
        shiny::updateTextInput(session, "variable_label", value = label)
      }, once = TRUE)
    }, ignoreInit = TRUE)
    gtx_safe_observe_event(input$focus_variable, {
      focus <- input$focus_variable %||% ""
      data <- state$working_data
      if (!nzchar(focus) || is.null(data) || !focus %in% names(data)) return(NULL)
      for (id in c("variable", "group_variable", "variable1")) shiny::updateSelectInput(session, id, selected = focus)
      if (is.numeric(data[[focus]])) shiny::updateSelectInput(session, "transform_source", selected = focus)
    }, ignoreInit = TRUE)
    focus_variable <- function() {
      focus <- input$focus_variable %||% ""
      if (nzchar(focus) && focus %in% names(state$working_data)) focus else ""
    }
    numeric_variable <- function(preferred = NULL) {
      data <- state$working_data
      if (is.null(data)) return("")
      candidates <- names(data)[vapply(data, is.numeric, logical(1))]
      if (!length(candidates)) return("")
      if (!is.null(preferred) && preferred %in% candidates) preferred else candidates[[1L]]
    }
    gtx_safe_observe_event(input$template_age, {
      variable <- numeric_variable(if (nzchar(focus_variable())) focus_variable() else "age")
      if (!nzchar(variable)) return(shiny::showNotification("Age groups needs a numeric variable. Choose Create groups after importing suitable data.", type = "warning"))
      prefill(list(group_variable = variable, group_count = "2", rule_op_1 = ">=", rule_value_1 = "65",
        rule_result_1 = "65 or older", rule_default = "Under 65", transform_new = paste0(variable, "_group")))
      choose_operation("transform", "case_when")
      shiny::showNotification("Age-group starter loaded. Check the cut-off and labels before applying.", type = "message")
    }, ignoreInit = TRUE)
    gtx_safe_observe_event(input$template_binary, {
      data <- state$working_data
      candidates <- names(data)[vapply(data, function(x) {
        values <- sort(unique(as.character(x[!is.na(x)])))
        identical(values, c("0", "1"))
      }, logical(1))]
      if (!length(candidates)) return(shiny::showNotification("No variable coded exactly 0 and 1 was found. Choose Recode values and select a variable yourself.", type = "warning"))
      if (focus_variable() %in% candidates) candidates <- c(focus_variable(), setdiff(candidates, focus_variable()))
      # Labels are matched to the recorded values 0 and 1, whatever order
      # they are listed in.
      prefill(list(variable = candidates[[1L]], recode_map = c("0" = "No", "1" = "Yes")))
      choose_operation("recode")
      shiny::showNotification("0/1 recoding starter loaded. Confirm that 0 means No and 1 means Yes before applying.", type = "message")
    }, ignoreInit = TRUE)
    gtx_safe_observe_event(input$template_bmi, {
      variable <- numeric_variable(focus_variable())
      if (!nzchar(variable)) return(shiny::showNotification("BMI-style calculation needs numeric variables.", type = "warning"))
      prefill(list(calculation_mode = "ratio_power", transform_source = variable, transform_number = 2, transform_new = "bmi"))
      choose_operation("transform", "arithmetic")
      shiny::showNotification("BMI-style starter loaded. Choose weight in kg and height in metres before applying.", type = "message")
    }, ignoreInit = TRUE)
    gtx_safe_observe_event(input$template_missing, {
      variable <- numeric_variable(focus_variable())
      if (!nzchar(variable)) return(shiny::showNotification("This starter needs a variable that contains recorded missing-data codes.", type = "warning"))
      prefill(list(variable = variable, missing_action = "to_na", codes = "99, 999"))
      choose_operation("missing")
      shiny::showNotification("Missing-code starter loaded. Replace 99/999 unless those are genuinely your missing-data codes.", type = "message")
    }, ignoreInit = TRUE)
    output$operation_hint <- shiny::renderUI({
      hint <- switch(operation(),
        rename = shiny::tagList(
          shiny::tags$strong("Give a variable a clearer name."),
          shiny::tags$p(class = "help-copy", "Example: rename `smoke` to `smoking_status`. This changes the column name only; values are unchanged.")
        ),
        label = shiny::tagList(
          shiny::tags$strong("Give a variable a publication-ready display label."),
          shiny::tags$p(class = "help-copy", "Example: keep the internal name `age`, but display Maternal age in tables and plots. This does not change the column name or values. Leave the label empty to remove it.")
        ),
        recode = shiny::tagList(
          shiny::tags$strong("Give each recorded value a clear label."),
          shiny::tags$p(class = "help-copy", "Each recorded value is shown below with its replacement label. Edit only the labels you want to change; values left unchanged stay as they are.")
        ),
        type = shiny::tagList(
          shiny::tags$strong("Set how a variable is treated in tables and analyses."),
          shiny::tags$p(class = "help-copy", "Use categorical for unordered values such as No/Yes; use ordered categorical for values with a real order, such as None, One, Two or more. The level order is used in tables and charts.")
        ),
        missing = shiny::tagList(
          shiny::tags$strong("Make missingness explicit and reproducible."),
          shiny::tags$p(class = "help-copy", "Convert recorded missing-data codes such as `999` or `Unknown` to R missing values (`NA`). Or, for a categorical variable, show existing NA values as a labelled Missing category for presentation.")
        ),
        transform = shiny::tagList(
          shiny::tags$strong("Create a new variable without changing the source variable."),
          shiny::tags$p(class = "help-copy", "Create calculations such as BMI or unit conversions, or use Create groups for age bands, BMI categories, and risk groups. The source variable is always retained.")
        ),
        filter = shiny::tagList(
          shiny::tags$strong("Keep only records meeting your eligibility criteria."),
          shiny::tags$p(class = "help-copy", "Example: age is at least 18 AND consent equals Yes. Always check the included, excluded, and unknown counts before continuing.")
        ),
        keep = shiny::tagList(
          shiny::tags$strong("Make a smaller analysis dataset without deleting the original import."),
          shiny::tags$p(class = "help-copy", "Tick the variables needed for your analysis. You can Undo this change at any time.")
        )
      )
      shiny::tags$div(class = "gtx-step", hint)
    })
    output$controls <- shiny::renderUI({
      ns <- session$ns; vars <- variables(); shiny::req(length(vars))
      if (identical(operation(), "rename")) shiny::tagList(
        shiny::selectInput(ns("variable"), "Existing variable", vars, selected = pf("variable", focus_in(vars))), shiny::textInput(ns("new_name"), "New variable name")
      ) else if (identical(operation(), "label")) shiny::tagList(
        shiny::selectInput(ns("variable"), "Variable", vars, selected = pf("variable", focus_in(vars))),
        shiny::textInput(ns("variable_label"), "Display label", placeholder = "Example: Maternal age"),
        shiny::tags$p(class = "help-copy", "Use the short internal name when selecting variables for analysis. This label is used in publication-ready output."),
        shiny::uiOutput(ns("label_level_mapping"))
      ) else if (identical(operation(), "recode")) shiny::tagList(
        shiny::selectInput(ns("variable"), "Variable", vars, selected = pf("variable", focus_in(vars))),
        shiny::uiOutput(ns("recode_mapping")),
        shiny::checkboxInput(ns("recode_ordered"), "Treat as ordered categorical, in the order shown", FALSE),
        shiny::tags$p(class = "help-copy", "The recoded variable is categorical. Tick this only when the displayed order has a genuine meaning, such as None, Mild, Moderate, Severe.")
      ) else if (identical(operation(), "type")) shiny::tagList(
        shiny::selectInput(ns("variable"), "Variable", vars, selected = pf("variable", focus_in(vars))),
        shiny::selectInput(ns("variable_type"), "Treat this variable as", c("Categorical" = "factor", "Ordered categorical" = "ordered", "Numeric" = "numeric", "Text" = "text")),
        shiny::uiOutput(ns("type_order_grid"))
      ) else if (identical(operation(), "missing")) shiny::tagList(
        shiny::selectInput(ns("variable"), "Variable", vars, selected = pf("variable", focus_in(vars))),
        shiny::radioButtons(ns("missing_action"), "What do you want to do?", c("Convert recorded codes to missing (NA)" = "to_na", "Show existing missing values as a category" = "show_category"), selected = pf("missing_action", "to_na")),
        shiny::uiOutput(ns("missing_controls"))
      ) else if (identical(operation(), "transform")) {
        mode <- transform_mode()
        numeric_vars <- vars[vapply(state$working_data[vars], is.numeric, logical(1))]
        operators <- c("equal to" = "==", "not equal to" = "!=", "greater than" = ">", "greater than or equal to" = ">=", "less than" = "<", "less than or equal to" = "<=")
        if (identical(mode, "arithmetic") && !length(numeric_vars)) shiny::tags$div(class = "gtx-note", "Create calculation needs at least one numeric variable. Use Create groups to make categories from recorded values.")
        else if (identical(mode, "arithmetic")) shiny::tagList(
          shiny::tags$strong("Create a new numeric variable."),
          shiny::tags$p(class = "help-copy", "Choose a common calculation pattern. Your original variables are always retained."),
          shiny::radioButtons(ns("calculation_mode"), "Calculation type", c("Adjust one variable" = "single", "Combine two variables" = "two_variables", "Divide by another variable raised to a power" = "ratio_power"), selected = pf("calculation_mode", "single")),
          shiny::uiOutput(ns("calculation_controls"))
        ) else shiny::tagList(
          shiny::tags$strong("Create a new grouped variable."),
          shiny::tags$p(class = "help-copy", "Choose one source variable, decide how many groups you need, then give each group a clear label. Groups are checked from top to bottom; the first matching condition wins."),
          shiny::selectInput(ns("group_variable"), "Variable to group", vars, selected = pf("group_variable", focus_in(vars))),
          shiny::radioButtons(ns("group_count"), "How many groups?", c("Two groups" = "2", "Three groups" = "3", "Four groups" = "4"), selected = pf("group_count", "2"), inline = TRUE),
          shiny::uiOutput(ns("group_rules")),
          shiny::textInput(ns("transform_new"), "Name for the new grouped variable", value = pf("transform_new", ""), placeholder = "Example: age_group")
        )
      } else if (identical(operation(), "keep")) shiny::tagList(
        shiny::tags$strong("Choose the variables for your analysis dataset."),
        shiny::tags$p(class = "help-copy", "Start with all variables, remove those you do not need, and search by name when your dataset is large. This does not change the original imported data."),
        shiny::div(class = "gtx-prep-actions",
          shiny::actionButton(ns("keep_all"), "Keep all variables", class = "btn-default btn-sm"),
          shiny::actionButton(ns("keep_none"), "Clear selection", class = "btn-default btn-sm")
        ),
        shiny::selectizeInput(ns("keep_vars"), "Variables to keep", choices = vars, selected = vars, multiple = TRUE, options = list(placeholder = "Search and select variables"))
      ) else shiny::tagList(
        shiny::tags$strong("Keep observations that meet your inclusion criteria."),
        shiny::tags$p(class = "help-copy", "Start with one condition. Add a second only when needed. The preview shows exactly how many records will be kept, excluded, or have an unknown result because of missing data."),
        shiny::uiOutput(ns("filter_conditions"))
      )
    })
    # Values listed for recoding/ordering: an existing factor's own level order
    # (so its reference category is kept); otherwise order of appearance.
    level_order_values <- function(x) {
      if (is.factor(x)) return(levels(x))
      unique(as.character(x[!is.na(x)]))
    }
    recode_values <- shiny::reactive({
      variable <- input$variable %||% ""
      data <- state$working_data
      if (!nzchar(variable) || is.null(data) || !variable %in% names(data)) return(character())
      level_order_values(data[[variable]])
    })
    recode_start <- function(value) {
      map <- prefill()$recode_map
      if (!is.null(map) && identical(prefill()$variable, input$variable) && value %in% names(map)) map[[value]] else value
    }
    output$recode_mapping <- shiny::renderUI({
      values <- recode_values()
      if (!length(values)) return(shiny::tags$p(class = "help-copy", "This variable has no observed values to recode."))
      rows <- lapply(seq_along(values), function(index) {
        shiny::tags$tr(
          shiny::tags$td(shiny::tags$code(values[[index]])),
          shiny::tags$td(shiny::textInput(ns(paste0("recode_to_", index)), NULL, value = recode_start(values[[index]])))
        )
      })
      shiny::tags$div(class = "gtx-recode-map",
        shiny::tags$h4("Value mapping"),
        shiny::tags$p(class = "help-copy", "Change the label beside each recorded value. The left column is read-only."),
        shiny::tags$table(class = "gtx-impact-table",
          shiny::tags$thead(shiny::tags$tr(shiny::tags$th("Recorded value"), shiny::tags$th("New label"))),
          shiny::tags$tbody(rows)
        )
      )
    })
    recode_labels <- shiny::reactive({
      values <- recode_values()
      if (!length(values)) return(character())
      vapply(seq_along(values), function(index) {
        input[[paste0("recode_to_", index)]] %||% values[[index]]
      }, character(1))
    })
    label_level_values <- shiny::reactive({
      variable <- input$variable %||% ""
      data <- state$working_data
      if (!nzchar(variable) || is.null(data) || !variable %in% names(data)) return(character())
      x <- data[[variable]]
      if (!(is.factor(x) || is.character(x) || is.logical(x))) return(character())
      if (is.factor(x)) return(levels(x))
      unique(as.character(x[!is.na(x)]))
    })
    output$label_level_mapping <- shiny::renderUI({
      values <- label_level_values()
      if (!length(values)) return(NULL)
      rows <- lapply(seq_along(values), function(index) {
        shiny::tags$tr(
          shiny::tags$td(shiny::tags$code(values[[index]])),
          shiny::tags$td(shiny::textInput(ns(paste0("label_level_to_", index)), NULL, value = values[[index]]))
        )
      })
      shiny::tags$div(class = "gtx-recode-map",
        shiny::tags$h4("Category display labels"),
        shiny::tags$p(class = "help-copy", "Optional. Change these labels for neat table and plot output. The recorded categories are converted to a factor only when at least one label changes."),
        shiny::tags$table(class = "gtx-impact-table",
          shiny::tags$thead(shiny::tags$tr(shiny::tags$th("Current category"), shiny::tags$th("Display label"))),
          shiny::tags$tbody(rows)
        )
      )
    })
    label_level_labels <- shiny::reactive({
      values <- label_level_values()
      if (!length(values)) return(character())
      vapply(seq_along(values), function(index) {
        input[[paste0("label_level_to_", index)]] %||% values[[index]]
      }, character(1))
    })
    type_order_values <- shiny::reactive({
      variable <- input$variable %||% ""
      data <- state$working_data
      if (!nzchar(variable) || is.null(data) || !variable %in% names(data)) return(character())
      level_order_values(data[[variable]])
    })
    output$type_order_grid <- shiny::renderUI({
      variable_type <- input$variable_type %||% "factor"
      if (identical(variable_type, "factor")) {
        return(shiny::tags$p(class = "help-copy", "Categorical treats values as separate groups with no meaningful order. Their current display order is retained."))
      }
      if (!identical(variable_type, "ordered")) {
        return(shiny::tags$p(class = "help-copy", "Numeric converts recorded values to numbers where possible. Text keeps labels as plain text."))
      }
      values <- type_order_values()
      if (!length(values)) return(shiny::tags$p(class = "help-copy", "This variable has no observed values to order."))
      ranks <- stats::setNames(seq_along(values), paste0(seq_along(values), ifelse(seq_along(values) == 1L, " (first)", "")))
      rows <- lapply(seq_along(values), function(index) {
        shiny::tags$tr(
          shiny::tags$td(shiny::tags$code(values[[index]])),
          shiny::tags$td(shiny::selectInput(ns(paste0("type_rank_", index)), NULL, choices = ranks, selected = index))
        )
      })
      shiny::tags$div(class = "gtx-recode-map",
        shiny::tags$h4("Display order"),
        shiny::tags$p(class = "help-copy", "Give each recorded value a different position. The default keeps the current order. This controls the order in tables and charts."),
        shiny::tags$table(class = "gtx-impact-table",
          shiny::tags$thead(shiny::tags$tr(shiny::tags$th("Recorded value"), shiny::tags$th("Position"))),
          shiny::tags$tbody(rows)
        )
      )
    })
    type_levels <- shiny::reactive({
      values <- type_order_values()
      if (!length(values)) return(character())
      ranks <- vapply(seq_along(values), function(index) {
        suppressWarnings(as.integer(input[[paste0("type_rank_", index)]] %||% index))
      }, integer(1))
      if (anyNA(ranks) || anyDuplicated(ranks) || !setequal(ranks, seq_along(values))) {
        stop("Give every recorded value a different display position.", call. = FALSE)
      }
      values[order(ranks)]
    })
    output$missing_controls <- shiny::renderUI({
      if (identical(input$missing_action %||% "to_na", "show_category")) {
        shiny::tagList(
          shiny::textInput(ns("missing_label"), "Label to show for missing values", value = "Missing"),
          shiny::tags$p(class = "help-copy", "This creates a categorical level for existing NA values. Use it only when showing missingness as a table category is appropriate; it changes how the variable is analysed.")
        )
      } else {
        shiny::tagList(
          shiny::textInput(ns("codes"), "Recorded codes to convert to missing", value = pf("codes", ""), placeholder = "Example: 999, Unknown, N/A"),
          shiny::tags$p(class = "help-copy", "Use this only for codes that truly mean no value was recorded. R uses NA to represent missing data.")
        )
      }
    })
    output$group_rules <- shiny::renderUI({
      count <- suppressWarnings(as.integer(input$group_count %||% 2L))
      count <- if (count %in% 2:4) count else 2L
      numeric_group <- nzchar(input$group_variable %||% "") &&
        input$group_variable %in% names(state$working_data) &&
        is.numeric(state$working_data[[input$group_variable]])
      operators <- c("is equal to (=)" = "==", "is not equal to (!=)" = "!=")
      if (numeric_group) operators <- c(
        operators,
        "is greater than (>)" = ">", "is greater than or equal to (>=)" = ">=",
        "is less than (<)" = "<", "is less than or equal to (<=)" = "<=",
        "is between (inclusive)" = "between", "is outside the range" = "outside"
      )
      rule_rows <- lapply(seq_len(count - 1L), function(index) {
        shiny::tags$div(class = "gtx-step",
          shiny::tags$strong(paste("Group", index)),
          shiny::selectInput(ns(paste0("rule_op_", index)), "Include values that", operators, selected = pf(paste0("rule_op_", index), NULL)),
          shiny::textInput(ns(paste0("rule_value_", index)), "Value or lower bound", value = pf(paste0("rule_value_", index), ""), placeholder = if (index == 1L) "Example: 35" else "Example: 65"),
          shiny::conditionalPanel(
            condition = sprintf("input['%s'] === 'between' || input['%s'] === 'outside'", ns(paste0("rule_op_", index)), ns(paste0("rule_op_", index))),
            shiny::textInput(ns(paste0("rule_value2_", index)), "Upper bound", placeholder = "Example: 64")
          ),
          shiny::textInput(ns(paste0("rule_result_", index)), "Group label", value = pf(paste0("rule_result_", index), ""), placeholder = if (index == 1L) "Example: Older adult" else "Example: Adult")
        )
      })
      shiny::tagList(
        rule_rows,
        shiny::tags$div(class = "gtx-step",
          shiny::tags$strong(paste("Group", count, "(everyone else)")),
          shiny::textInput(ns("rule_default"), "Group label", value = pf("rule_default", ""), placeholder = "Example: Younger adult")
        )
      )
    })
    output$filter_conditions <- shiny::renderUI({
      operators <- c("is equal to" = "==", "is not equal to" = "!=", "is greater than" = ">", "is at least" = ">=", "is less than" = "<", "is at most" = "<=")
      vars <- variables()
      shiny::tagList(
        shiny::tags$div(class = "gtx-step",
          shiny::tags$strong("Condition 1"),
          shiny::selectInput(ns("variable1"), "Variable", vars, selected = focus_in(vars)),
          shiny::selectInput(ns("operator1"), "Keep when", operators),
          shiny::textInput(ns("value1"), "Value", placeholder = "Example: 18")
        ),
        shiny::checkboxInput(ns("filter_second"), "Add another condition", FALSE),
        shiny::uiOutput(ns("filter_second_condition"))
      )
    })
    gtx_safe_observe_event(input$keep_all, {
      shiny::updateSelectizeInput(session, "keep_vars", selected = variables(), server = TRUE)
    }, ignoreInit = TRUE)
    gtx_safe_observe_event(input$keep_none, {
      shiny::updateSelectizeInput(session, "keep_vars", selected = character(), server = TRUE)
    }, ignoreInit = TRUE)
    output$filter_second_condition <- shiny::renderUI({
      if (!isTRUE(input$filter_second)) return(NULL)
      operators <- c("is equal to" = "==", "is not equal to" = "!=", "is greater than" = ">", "is at least" = ">=", "is less than" = "<", "is at most" = "<=")
      shiny::tags$div(class = "gtx-step",
        shiny::tags$strong("Condition 2"),
        shiny::radioButtons(ns("connector"), "Keep records meeting", c("Both conditions" = "AND", "Either condition" = "OR"), selected = "AND", inline = TRUE),
        shiny::selectInput(ns("variable2"), "Variable", variables()),
        shiny::selectInput(ns("operator2"), "Keep when", operators),
        shiny::textInput(ns("value2"), "Value", placeholder = "Example: Yes")
      )
    })
    output$calculation_controls <- shiny::renderUI({
      numeric_vars <- variables()[vapply(state$working_data[variables()], is.numeric, logical(1))]
      mode <- input$calculation_mode %||% "single"
      if (!length(numeric_vars)) return(shiny::tags$p(class = "help-copy", "No numeric variables are available."))
      if (identical(mode, "single")) return(shiny::tagList(
        shiny::selectInput(ns("transform_source"), "Numeric variable", numeric_vars, selected = focus_in(numeric_vars)),
        shiny::selectInput(ns("transform_operator"), "Calculation", c("Add a number" = "+", "Subtract a number" = "-", "Multiply by a number" = "*", "Divide by a number" = "/", "Raise to a power" = "^")),
        shiny::numericInput(ns("transform_number"), "Number", 1),
        shiny::textInput(ns("transform_new"), "Name for the new variable", placeholder = "Example: weight_kg")
      ))
      if (identical(mode, "two_variables")) return(shiny::tagList(
        shiny::selectInput(ns("transform_source"), "First numeric variable", numeric_vars, selected = focus_in(numeric_vars)),
        shiny::selectInput(ns("transform_operator"), "Calculation", c("Add" = "+", "Subtract second from first" = "-", "Multiply" = "*", "Divide first by second" = "/")),
        shiny::selectInput(ns("transform_second"), "Second numeric variable", numeric_vars),
        shiny::textInput(ns("transform_new"), "Name for the new variable", placeholder = "Example: total_score")
      ))
      shiny::tagList(
        shiny::selectInput(ns("transform_source"), "Numerator variable", numeric_vars, selected = pf("transform_source", focus_in(numeric_vars))),
        shiny::selectInput(ns("transform_second"), "Denominator variable", numeric_vars),
        shiny::numericInput(ns("transform_number"), "Denominator power", pf("transform_number", 2)),
        shiny::tags$p(class = "help-copy", "This calculates numerator / (denominator ^ power). For BMI, use weight in kg divided by height in metres with power 2."),
        shiny::textInput(ns("transform_new"), "Name for the new variable", value = pf("transform_new", ""), placeholder = "Example: bmi")
      )
    })
    selected_variable <- shiny::reactive({
      if (operation() %in% c("recode", "type", "missing")) return(input$variable %||% "")
      if (identical(operation(), "transform")) {
        if (identical(transform_mode(), "arithmetic")) return(input$transform_source %||% "")
        return(input$group_variable %||% "")
      }
      if (identical(operation(), "filter")) return(input$variable1 %||% "")
      ""
    })
    output$value_guide <- shiny::renderUI({
      variable <- selected_variable()
      data <- state$working_data
      shiny::req(!is.null(data), nzchar(variable), variable %in% names(data))
      x <- data[[variable]]
      missing <- sum(is.na(x))
      is_categorical <- is.factor(x) || is.character(x) || is.logical(x)
      if (is_categorical) {
        values <- unique(as.character(x[!is.na(x)]))
        shown <- utils::head(values, 40L)
        suffix <- if (length(values) > length(shown)) paste0("\n... plus ", length(values) - length(shown), " more value(s)") else ""
        value_text <- paste0(paste(shown, collapse = ", "), suffix)
        shiny::tags$div(class = "gtx-value-guide",
          shiny::tags$h5("Recorded values"),
          shiny::tags$p(class = "help-copy", paste0("Exact non-missing values in `", variable, "` (", length(values), " unique; ", missing, " missing). Use these spellings when recoding or filtering.")),
          shiny::tags$button(type = "button", class = "btn btn-default btn-xs copy-code", `data-copy-target` = ns("values_to_copy"), "Copy values"),
          shiny::tags$pre(id = ns("values_to_copy"), value_text)
        )
      } else if (is.numeric(x)) {
        observed <- x[is.finite(x)]
        if (!length(observed)) return(shiny::tags$div(class = "gtx-value-guide", shiny::tags$h5("Numeric variable"), "No finite observed values are available."))
        unique_values <- sort(unique(observed))
        details <- paste0("Range: ", format(min(observed), trim = TRUE), " to ", format(max(observed), trim = TRUE), "; ", length(unique_values), " unique; ", missing, " missing.")
        if (length(unique_values) <= 12L) details <- paste0(details, " Recorded values: ", paste(unique_values, collapse = ", "), ".")
        shiny::tags$div(class = "gtx-value-guide", shiny::tags$h5("Numeric variable guide"), shiny::tags$p(class = "help-copy", details))
      } else {
        shiny::tags$div(class = "gtx-value-guide", shiny::tags$h5("Selected variable"), shiny::tags$p(class = "help-copy", paste0("Class: ", paste(class(x), collapse = ", "), "; ", missing, " missing value(s).")))
      }
    })
    impact_table <- function(headers, rows) {
      shiny::tags$table(class = "gtx-impact-table",
        shiny::tags$thead(shiny::tags$tr(lapply(headers, shiny::tags$th))),
        shiny::tags$tbody(lapply(rows, function(row) shiny::tags$tr(lapply(row, shiny::tags$td))))
      )
    }
    output$impact_preview <- shiny::renderUI({
      data <- state$working_data
      shiny::req(!is.null(data))
      content <- tryCatch({
        if (identical(operation(), "rename")) {
          if (!nzchar(input$new_name %||% "")) return(NULL)
          list(message = "Values will not change; only the column name will change.")
        } else if (identical(operation(), "recode")) {
          from <- recode_values()
          if (!length(from) || !nzchar(input$variable %||% "")) return(NULL)
          affected <- sum(as.character(data[[input$variable]]) %in% from, na.rm = TRUE)
          to <- recode_labels()
          if (any(!nzchar(to))) return(list(message = "Enter a new label for every displayed recorded value."))
          counts <- vapply(from, function(value) sum(as.character(data[[input$variable]]) %in% value, na.rm = TRUE), numeric(1))
          changed <- sum(counts[from != to])
          type_note <- if (isTRUE(input$recode_ordered)) " It will be an ordered categorical variable in the displayed order." else " It will be a categorical variable."
          list(message = paste0(changed, " cell(s) will receive a new label.", type_note), table = impact_table(c("Recorded value", "New label", "Affected rows"), Map(function(a, b, n) list(a, b, format(n, trim = TRUE)), from, to, counts)))
        } else if (identical(operation(), "type")) {
          if (!nzchar(input$variable %||% "")) return(NULL)
          list(message = paste0("All values will be retained. The variable will be treated as ", switch(input$variable_type %||% "factor", factor = "categorical", ordered = "ordered categorical", numeric = "numeric", text = "text"), "."))
        } else if (identical(operation(), "missing")) {
          if (identical(input$missing_action %||% "to_na", "show_category")) {
            if (!nzchar(input$variable %||% "")) return(NULL)
            affected <- sum(is.na(data[[input$variable]]))
            label <- trimws(input$missing_label %||% "")
            if (!nzchar(label)) return(list(message = "Enter a label to preview the missing category."))
            return(list(message = paste0(affected, " existing NA value(s) will be shown as the category ", encodeString(label, quote = '"'), ". This changes how the variable is analysed."), table = impact_table(c("Current value", "New category", "Affected rows"), list(list("NA (missing)", label, format(affected, trim = TRUE))))))
          }
          codes <- trimws(strsplit(input$codes %||% "", ",", fixed = TRUE)[[1L]])
          codes <- codes[nzchar(codes)]
          if (!length(codes) || !nzchar(input$variable %||% "")) return(NULL)
          affected <- sum(!is.na(data[[input$variable]]) & as.character(data[[input$variable]]) %in% codes)
          rows <- lapply(codes, function(code) {
            list(code, format(sum(!is.na(data[[input$variable]]) & as.character(data[[input$variable]]) %in% code), trim = TRUE))
          })
          list(message = paste0(affected, " value(s) will become missing (NA)."), table = impact_table(c("Code", "Values becoming missing"), rows))
        } else if (identical(operation(), "transform") && identical(transform_mode(), "arithmetic")) {
          if (!nzchar(input$transform_source %||% "")) return(NULL)
          mode <- input$calculation_mode %||% "single"
          second <- input$transform_second %||% ""
          count <- if (identical(mode, "single")) sum(!is.na(data[[input$transform_source]])) else if (second %in% names(data)) sum(!is.na(data[[input$transform_source]]) & !is.na(data[[second]])) else 0L
          description <- switch(mode, single = "The source variable will be retained.", two_variables = "Both source variables will be retained.", ratio_power = "This uses numerator / (denominator ^ power); both source variables will be retained.")
          list(message = paste0("A new variable will be created with ", count, " non-missing calculated value(s). ", description))
        } else if (identical(operation(), "filter")) {
          if (!nzchar(input$value1 %||% "") || !nzchar(input$variable1 %||% "")) return(NULL)
          variable2 <- if (isTRUE(input$filter_second)) input$variable2 %||% "" else ""
          value2 <- if (isTRUE(input$filter_second)) input$value2 %||% "" else ""
          filtered <- gt_dp_filter(data, input$variable1, input$operator1, input$value1, input$connector %||% "AND", variable2, input$operator2 %||% "==", value2)
          counts <- attr(filtered, "gt_dp_filter_counts")
          list(message = "Check the expected filter result before applying.", table = impact_table(c("Result", "Rows"), list(list("Kept", format(counts[["included"]], trim = TRUE)), list("Excluded", format(counts[["excluded"]], trim = TRUE)), list("Unknown", format(counts[["unknown"]], trim = TRUE)), list("Total", format(counts[["before"]], trim = TRUE)))))
        } else if (identical(operation(), "keep")) {
          selected <- input$keep_vars %||% character()
          if (!length(selected)) return(NULL)
          list(message = paste0(length(selected), " of ", ncol(data), " variable(s) will be retained; ", ncol(data) - length(selected), " will be removed from the working dataset."))
        } else if (identical(operation(), "transform")) {
          count <- suppressWarnings(as.integer(input$group_count %||% 2L))
          count <- if (count %in% 2:4) count else 2L
          indices <- seq_len(count - 1L)
          group_variable <- input$group_variable %||% ""
          if (!nzchar(group_variable) || !group_variable %in% names(data)) return(NULL)
          variables <- rep(group_variable, length(indices))
          operators <- vapply(indices, function(index) input[[paste0("rule_op_", index)]] %||% "==", character(1))
          values <- vapply(indices, function(index) input[[paste0("rule_value_", index)]] %||% "", character(1))
          values2 <- vapply(indices, function(index) input[[paste0("rule_value2_", index)]] %||% "", character(1))
          results <- vapply(indices, function(index) input[[paste0("rule_result_", index)]] %||% "", character(1))
          keep <- nzchar(values) & nzchar(results)
          range_complete <- !operators %in% c("between", "outside") | nzchar(values2)
          if (!all(keep) || !all(range_complete) || !nzchar(input$rule_default %||% "")) return(list(message = "Complete every group condition and label, including both bounds for ranges and the everyone-else group, to preview your groups."))
          grouped <- gt_dp_group_values(
            data, variables, operators, values, results,
            input$rule_default %||% "", values2
          )
          counts <- attr(grouped, "gt_dp_group_counts")
          empty_message <- gt_dp_empty_group_message(data, variables, counts)
          message <- if (is.null(empty_message)) {
            paste0("The new ", count, "-group variable uses the first matching condition.")
          } else {
            paste0("Cannot apply yet. ", empty_message)
          }
          list(
            message = message,
            table = impact_table(
              c("New group", "Rows"),
              lapply(seq_along(counts), function(index) {
                list(names(counts)[[index]], format(unname(counts[[index]]), trim = TRUE))
              })
            )
          )
        }
      }, error = function(e) NULL)
      if (is.null(content)) return(NULL)
      shiny::tags$div(class = "gtx-note", shiny::tags$strong("Before you apply: "), content$message, content$table)
    })
    # Each change stores structured code steps (rendered for any data object
    # name) and a dataset without the operations' internal bookkeeping
    # attributes, so the working data equals the result of the generated code.
    apply_change <- function(label, after, steps) {
      for (name in grep("^gt_dp_", names(attributes(after)), value = TRUE)) attr(after, name) <- NULL
      code <- paste(gt_dp_render_steps(steps, "data"), collapse = "\n")
      entry <- list(label = label, before = state$working_data, after = after, code = code, steps = steps)
      state$history <- c(state$history, list(entry))
      state$working_data <- after; state$redo_stack <- list()
      log_renames(rename_events(entry))
    }
    step <- function(operation, ...) list(operation = operation, args = list(...))
    gtx_safe_observe_event(input$apply, {
      shiny::req(state$working_data)
      tryCatch({
        if (identical(operation(), "rename")) {
          after <- gt_dp_rename(state$working_data, input$variable, input$new_name)
          apply_change(paste("Rename", input$variable, "to", input$new_name), after, list(step("rename", variable = input$variable, new_name = trimws(input$new_name))))
        } else if (identical(operation(), "label")) {
          label <- trimws(input$variable_label %||% "")
          after <- state$working_data
          from <- label_level_values()
          to <- trimws(label_level_labels())
          level_change <- length(from) && !identical(from, to)
          if (level_change) {
            if (any(!nzchar(to))) stop("Enter a display label for every category.", call. = FALSE)
            after <- gt_dp_recode(after, input$variable, from, to, TRUE)
            after <- gt_dp_set_type(after, input$variable, "factor", to)
          }
          # The display label is set last so that relabelling categories keeps it.
          after <- gt_dp_set_label(after, input$variable, label)
          action_label <- if (nzchar(label)) {
            paste("Label", input$variable, "as", label)
          } else {
            paste("Remove display label from", input$variable)
          }
          if (level_change) action_label <- paste(action_label, "and update category labels")
          steps <- list()
          if (level_change) {
            steps <- list(
              step("recode", variable = input$variable, from = from, to = to),
              step("type", variable = input$variable, type = "factor", levels = base::levels(after[[input$variable]]))
            )
          }
          steps <- c(steps, list(step("label", variable = input$variable, label = label)))
          apply_change(action_label, after, steps)
        } else if (identical(operation(), "recode")) {
          from <- recode_values(); to <- recode_labels()
          if (!length(from) || any(!nzchar(to))) stop("Enter a new label for every displayed recorded value.", call. = FALSE)
          after <- gt_dp_recode(state$working_data, input$variable, from, to, TRUE)
          recoded <- attr(after, "gt_dp_affected")
          recode_type <- if (isTRUE(input$recode_ordered)) "ordered" else "factor"
          # New categories follow the listed (original level) order; values
          # combined under one label take the position of the first of them.
          after <- gt_dp_set_type(after, input$variable, recode_type, unique(trimws(to)))
          final_levels <- base::levels(after[[input$variable]])
          steps <- list(
            step("recode", variable = input$variable, from = from, to = to),
            step("type", variable = input$variable, type = recode_type, levels = final_levels)
          )
          type_label <- if (identical(recode_type, "ordered")) " as ordered categorical" else " as categorical"
          apply_change(paste0("Recode ", input$variable, " (", recoded, " cells)", type_label), after, steps)
        } else if (identical(operation(), "type")) {
          level_values <- if (identical(input$variable_type, "ordered")) type_levels() else character()
          after <- gt_dp_set_type(state$working_data, input$variable, input$variable_type, level_values)
          final_levels <- if (input$variable_type %in% c("factor", "ordered")) base::levels(after[[input$variable]]) else character()
          apply_change(paste("Set", input$variable, "as", switch(input$variable_type, factor = "categorical", ordered = "ordered categorical", numeric = "numeric", text = "text")), after, list(step("type", variable = input$variable, type = input$variable_type, levels = final_levels)))
        } else if (identical(operation(), "missing")) {
          if (identical(input$missing_action %||% "to_na", "show_category")) {
            after <- gt_dp_show_missing(state$working_data, input$variable, input$missing_label)
            apply_change(paste("Show missing values in", input$variable, "as", input$missing_label, "(", attr(after, "gt_dp_affected"), "cells)"), after, list(step("missing_category", variable = input$variable, label = trimws(input$missing_label), levels = base::levels(after[[input$variable]]))))
          } else {
            codes <- trimws(strsplit(input$codes, ",", fixed = TRUE)[[1L]]); after <- gt_dp_define_missing(state$working_data, input$variable, codes)
            apply_change(paste("Convert missing-data codes in", input$variable, "to NA (", attr(after, "gt_dp_affected"), "cells)"), after, list(step("missing", variable = input$variable, codes = codes[nzchar(codes)])))
          }
        } else if (identical(operation(), "transform")) {
          if (identical(transform_mode(), "arithmetic")) {
            mode <- input$calculation_mode %||% "single"
            after <- gt_dp_calculate(state$working_data, input$transform_source, input$transform_new, mode, input$transform_operator %||% "+", input$transform_number %||% 1, input$transform_second %||% NULL)
            steps <- list(step("calculate", mode = mode, source = input$transform_source, new_name = trimws(input$transform_new),
              operator = input$transform_operator %||% "+", number = input$transform_number %||% 1, second = input$transform_second %||% ""))
          } else {
            count <- suppressWarnings(as.integer(input$group_count %||% 2L))
            count <- if (count %in% 2:4) count else 2L
            indices <- seq_len(count - 1L)
            group_variable <- input$group_variable %||% ""
            if (!nzchar(group_variable) || !group_variable %in% names(state$working_data)) stop("Choose the variable to group.", call. = FALSE)
            variables <- rep(group_variable, length(indices))
            operators <- vapply(indices, function(index) input[[paste0("rule_op_", index)]] %||% "==", character(1))
            values <- vapply(indices, function(index) input[[paste0("rule_value_", index)]] %||% "", character(1))
            values2 <- vapply(indices, function(index) input[[paste0("rule_value2_", index)]] %||% "", character(1))
            results <- vapply(indices, function(index) input[[paste0("rule_result_", index)]] %||% "", character(1))
            range_complete <- !operators %in% c("between", "outside") | nzchar(values2)
            if (any(!nzchar(values)) || any(!nzchar(results)) || !all(range_complete) || !nzchar(input$rule_default %||% "")) stop("Complete every group condition and label, including both bounds for ranges and the everyone-else group.", call. = FALSE)
            after <- gt_dp_transform_case_when(state$working_data, input$transform_new, variables, operators, values, results, input$rule_default, values2)
            steps <- list(step("groups", new_name = trimws(input$transform_new), variable = group_variable,
              numeric = is.numeric(state$working_data[[group_variable]]), operators = operators, values = values,
              values2 = values2, results = results, default = input$rule_default))
          }
          apply_change(paste("Create", input$transform_new, "(", attr(after, "gt_dp_affected"), "values matched)"), after, steps)
        } else if (identical(operation(), "keep")) {
          after <- gt_dp_keep_variables(state$working_data, input$keep_vars)
          apply_change(paste("Keep", ncol(after), "variable(s); remove", attr(after, "gt_dp_affected")), after, list(step("keep", variables = names(after))))
        } else {
          variable2 <- if (isTRUE(input$filter_second)) input$variable2 %||% "" else ""
          value2 <- if (isTRUE(input$filter_second)) input$value2 %||% "" else ""
          after <- gt_dp_filter(state$working_data, input$variable1, input$operator1, input$value1, input$connector %||% "AND", variable2, input$operator2 %||% "==", value2)
          counts <- attr(after, "gt_dp_filter_counts")
          numeric_column <- function(variable) nzchar(variable) && is.numeric(state$working_data[[variable]])
          filter_step <- step("filter",
            variable1 = input$variable1, operator1 = input$operator1, value1 = input$value1, numeric1 = numeric_column(input$variable1),
            connector = input$connector %||% "AND", variable2 = variable2, operator2 = input$operator2 %||% "==", value2 = value2,
            numeric2 = numeric_column(variable2))
          apply_change(paste0("Filter: before ", counts[["before"]], "; included ", counts[["included"]], "; excluded ", counts[["excluded"]], "; unknown ", counts[["unknown"]]), after, list(filter_step))
        }
        shiny::showNotification("Change applied. Undo is available.", type = "message")
      }, error = function(e) shiny::showNotification(conditionMessage(e), type = "error", duration = NULL))
    })
    gtx_safe_observe_event(input$undo, { if (length(state$history)) { last <- state$history[[length(state$history)]]; state$history <- utils::head(state$history, -1L); state$redo_stack <- c(state$redo_stack, list(last)); state$working_data <- last$before; log_renames(rename_events(last, reverse = TRUE)) } })
    gtx_safe_observe_event(input$redo, { if (length(state$redo_stack)) { last <- state$redo_stack[[length(state$redo_stack)]]; state$redo_stack <- utils::head(state$redo_stack, -1L); state$history <- c(state$history, list(last)); state$working_data <- last$after; log_renames(rename_events(last)) } })
    gtx_safe_observe_event(input$reset, {
      shiny::showModal(shiny::modalDialog(
        title = "Reset all data-preparation changes?",
        "This restores the original imported dataset and clears the change log. You cannot undo a reset.",
        footer = shiny::tagList(shiny::modalButton("Cancel"), shiny::actionButton(ns("confirm_reset"), "Reset all changes", class = "btn-danger"))
      ))
    }, ignoreInit = TRUE)
    gtx_safe_observe_event(input$confirm_reset, {
      undone <- unlist(lapply(rev(state$history), rename_events, reverse = TRUE), recursive = FALSE)
      state$working_data <- state$raw_data; state$history <- list(); state$redo_stack <- list()
      log_renames(undone)
      shiny::removeModal()
      shiny::showNotification("Data preparation reset. Analyses use the original data.", type = "message")
    }, ignoreInit = TRUE)
    prepared <- shiny::reactive(length(state$history) > 0L)
    output$analysis_dataset <- shiny::renderText({
      n <- length(state$history)
      if (n) paste0("prepared data (", n, " change", if (n == 1L) "" else "s", ")") else "original data"
    })
    output$preview <- gt::render_gt({ gt::gt(utils::head(state$working_data, 10L)) })
    output$status <- shiny::renderText({
      analysis_state <- if (is.null(state$working_data)) {
        "original data will be active after loading"
      } else if (isTRUE(prepared())) {
        "analyses use prepared data"
      } else {
        "analyses use original data"
      }
      paste(nrow(state$working_data), "prepared rows |", ncol(state$working_data), "variables |", length(state$history), "applied change(s) |", analysis_state)
    })
    output$history <- shiny::renderText({ if (!length(state$history)) "No changes applied." else paste(vapply(state$history, `[[`, character(1), "label"), collapse = "\n") })
    prep_code <- shiny::reactive({ if (!length(state$history)) "# No data-preparation changes applied." else paste(vapply(state$history, `[[`, character(1), "code"), collapse = "\n") })
    # The same code rendered for another data object (e.g. analysis_data);
    # generated structurally, never by rewriting text.
    code_for <- function(data_name = "data") {
      if (!length(state$history)) return("")
      steps <- unlist(lapply(state$history, `[[`, "steps"), recursive = FALSE)
      paste(gt_dp_render_steps(steps, data_name), collapse = "\n")
    }
    output$code <- shiny::renderText(prep_code())
    output$download_code <- shiny::downloadHandler(
      filename = function() "gtregression-data-prep.R",
      content = function(file) writeLines(prep_code(), file, useBytes = TRUE)
    )
    output$download_prepared <- shiny::downloadHandler(
      filename = function() paste0("gtregression-prepared-data.", input$prepared_format %||% "csv"),
      content = function(file) {
        data <- state$working_data
        format <- input$prepared_format %||% "csv"
        if (identical(format, "csv")) {
          utils::write.csv(data, file, row.names = FALSE, na = "")
        } else if (identical(format, "rds")) {
          saveRDS(data, file)
        } else {
          if (!requireNamespace("rio", quietly = TRUE)) {
            stop("Excel and Stata download require the optional 'rio' package. Install it with install.packages('rio') and restart the app.", call. = FALSE)
          }
          rio::export(data, file)
        }
      }
    )
    list(result = shiny::reactive(state$working_data), working_data = shiny::reactive(state$working_data), changed = prepared, using_prepared = prepared, changes = shiny::reactive(length(state$history)), can_redo = shiny::reactive(length(state$redo_stack) > 0L),
      history_labels = shiny::reactive(vapply(state$history, `[[`, character(1), "label")),
      renames = shiny::reactive(state$renames), source_id = shiny::reactive(state$source_id), code = prep_code, code_for = code_for)
  })
}
