# -------------------------------------------------------------------
# Advanced analysis: model selection, confounders, interaction, convergence
# -------------------------------------------------------------------

mod_advanced_ui <- function(id) {
  ns <- NS(id)
  approach_choices <- c(
    "Logistic (logit)"            = "logit",
    "Log-Binomial" = "log-binomial",
    "Poisson"           = "poisson",
    "Robust Poisson (robpoisson)" = "robpoisson",
    "Negative Binomial (negbin)"  = "negbin",
    "Linear"             = "linear"
  )
  tabPanel(
    tagList("Models and Diagnostics", span(class="section-badge","")),
    tabsetPanel(
      # -------------------- Model selection --------------------------
      tabPanel("Select Models",
               fluidRow(
                 column(
                   width = 4,
                   div(class="card-like sidebar-sticky",
                       h3("Stepwise Model Selection", class="mb-md"),
                       div(class="control-help","Generate multivariate models at once using `select_models` function."),
                       selectInput(ns("sel_outcome"), "Select Outcome", choices = NULL),
                       checkboxGroupInput(ns("sel_exposures"), "Select Exposures (Multiple)", choices = NULL),
                       selectInput(ns("sel_method"), "Direction
                                   (select from the dropdown)",
                                   choices = c("forward"="forward","backward"="backward","both"="both")),
                       selectInput(ns("sel_approach"), "Regression Approach
                                   (select from the dropdown)",
                                   choices = approach_choices, selected = "logit"),
                       actionButton(ns("run_select"), label = tagList(icon("play"), " Generate Models"),
                                    class="btn-primary mt-sm"),
                       tags$hr(),
                       strong("Export"),
                       div(class="mt-sm",
                           downloadButton(ns("select_summary_txt"), "TXT")
                       )
                   )
                 ),
                 column(
                   width = 8,
                   div(class="card-like",
                       h4(tagList(icon("list-check"), " Selection Summary")),
                       verbatimTextOutput(ns("select_summary")),
                       tags$hr(),
                       strong("Reproducible code"),
                       div(class="mt-sm",
                           downloadButton(ns("select_code_download"), "Download R code")
                       ),
                       br(), verbatimTextOutput(ns("select_code"))
                   )
                 )
               )
      ),

      # -------------------- Identify Confounders ---------------------
      tabPanel("Identify Confounders",
               fluidRow(
                 column(
                   width = 4,
                   div(class="card-like sidebar-sticky",
                       h3("Confounding Assessment", class="mb-md"),
                       div(class="control-help",
                           "Compares the crude and adjusted effect estimates of an exposure on an outcome using `identify_confounder()`."),
                       selectInput(ns("conf_outcome"), "Select Outcome", choices = NULL),
                       selectInput(ns("conf_potential_one"), "Select Potential Confounder", choices = NULL),
                       selectInput(ns("conf_exposure_one"), "Select Exposure", choices = NULL),
                       numericInput(ns("conf_threshold"),
                                    label = "Threshold (% change, default 10)",
                                    value = 10, min = 0, max = 100, step = 1),
                       selectInput(ns("conf_approach"), "Regression Approach
                                   (Select from the dropdown)",
                                   choices = approach_choices, selected = "logit"),
                       actionButton(ns("run_conf"), label = tagList(icon("play"), " Check confounding"),
                                    class="btn-primary mt-sm"),
                       tags$hr(),
                       strong("Export"),
                       div(class="mt-sm",
                           downloadButton(ns("conf_output_txt"), "TXT")
                       )
                   )
                 ),
                 column(
                   width = 8,
                   div(class="card-like",
                       h4(tagList(icon("wand-magic-sparkles"), " Confounding Results")),
                       gt::gt_output(ns("conf_table")),         # NEW: formatted
                       div(class="mt-sm"), uiOutput(ns("conf_text")),  # NEW: interpretation
                       tags$hr(),
                       strong("Summary of Results"),
                       verbatimTextOutput(ns("conf_output")),
                       tags$hr(),
                       strong("Reproducible code"),
                       div(class="mt-sm",
                           downloadButton(ns("conf_code_download"), "Download R code")
                       ),
                       br(), verbatimTextOutput(ns("conf_code"))
                   )
                 )
               )   # <-- close fluidRow
      ),         # <-- close tabPanel "Identify Confounders"

      # -------------------- Interaction models ----------------------
      tabPanel("Interaction Models",
               fluidRow(
                 column(
                   width = 4,
                   div(class="card-like sidebar-sticky",
                       h3("Effect-Modification", class="mb-md"),
                       div(class="control-help","Compares two models—one with and one without an interaction term between an exposure and a potential effect modifier using `interaction_models()`."),
                       selectInput(ns("int_outcome"), "Select Outcome", choices = NULL),
                       selectInput(ns("int_exposure_one"), "Select Exposure", choices = NULL),
                       selectInput(ns("int_modifier_one"), "Select Effect Modifier", choices = NULL),
                       checkboxGroupInput(ns("int_covariates"), "Select Covariates (Multiple)", choices = NULL),
                       selectInput(ns("int_approach"), "Regression Approach
                                   (select from the dropdown)",
                                   choices = approach_choices, selected = "logit"),
                       actionButton(ns("run_int"), label = tagList(icon("play"), " Run Interaction Models"),
                                    class="btn-primary mt-sm"),
                       tags$hr(),
                       strong("Export"),
                       div(class="mt-sm",
                           downloadButton(ns("int_output_txt"), "TXT")
                       )
                   )
                 ),
                 column(
                   width = 8,
                   div(class="card-like",
                       h4(tagList(icon("shuffle"), " Interaction Output")),
                       verbatimTextOutput(ns("int_output")),
                       tags$hr(),
                       strong("Reproducible code"),
                       div(class="mt-sm",
                           downloadButton(ns("int_code_download"), "Download R code")
                       ),
                       br(), verbatimTextOutput(ns("int_code"))
                   )
                 )
               )
      ),

      # -------------------- Convergence diagnostics ------------------
      tabPanel("Convergence",
               fluidRow(
                 column(
                   width = 4,
                   div(class="card-like sidebar-sticky",
                       h3("Convergence Checks", class="mb-md"),
                       div(class="control-help",""),
                       selectInput(ns("conv_outcome"), "Select Outcome", choices = NULL),
                       checkboxGroupInput(ns("conv_exposures"), "Select Exposures (Multiple)", choices = NULL),
                       selectInput(ns("conv_approach"), "Regression Approach
                                   (select from the dropdown)",
                                   choices = approach_choices, selected = "logit"),
                       checkboxInput(ns("conv_multivariate"), "Multivariable (else Univariate)", value = TRUE),
                       actionButton(ns("run_conv"), label = tagList(icon("magnifying-glass"), " Run Convergence"),
                                    class="btn-primary mt-sm"),
                       tags$hr(),
                       strong("Export"),
                       div(class="mt-sm",
                           downloadButton(ns("conv_output_txt"), "TXT")
                       )
                   )
                 ),
                 column(
                   width = 8,
                   div(class="card-like",
                       h4(tagList(icon("heartbeat"), " Convergence Output")),
                       verbatimTextOutput(ns("conv_output")),
                       tags$hr(),
                       strong("Reproducible code"),
                       div(class="mt-sm",
                           downloadButton(ns("conv_code_download"), "Download R code")
                       ),
                       br(), verbatimTextOutput(ns("conv_code"))
                   )
                 )
               )
      )
    )
  )
}

mod_advanced_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    `%||%` <- function(a, b) if (!is.null(a)) a else b
    .as_text <- function(x) paste(utils::capture.output(print(x)), collapse = "\n")
    .has_fn <- function(pkg, fn) isTRUE(requireNamespace(pkg, quietly = TRUE)) &&
      fn %in% getNamespaceExports(pkg)
    .call_gt <- function(candidates, ...) {
      if (!requireNamespace("gtregression", quietly = TRUE))
        stop("Package 'gtregression' is required.")
      args <- list(...)
      nm <- Filter(function(f) f %in% getNamespaceExports("gtregression"), candidates)
      if (!length(nm)) stop("None of ", paste(candidates, collapse = ", "),
                            " exported by gtregression.")
      fn <- get(nm[[1]], envir = asNamespace("gtregression"))
      keep <- intersect(names(args), names(formals(fn)))
      do.call(fn, args[keep])
    }

    # helper to capture console output exactly as printed
    .capture_console <- function(expr) {
      val <- NULL
      buf <- utils::capture.output(
        withCallingHandlers(
          {
            val <- eval.parent(substitute(expr))   # run it
            # no explicit NULL here, so nothing extra gets printed/captured
            invisible()
          },
          message = function(m) {                  # turn messages into text
            cat(conditionMessage(m), "\n")
            invokeRestart("muffleMessage")
          }
        )
      )
      list(value = val, console = paste(buf, collapse = "\n"))
    }


    # ---------- utilities to update choices while preserving valid selections ----
    .update_select <- function(id, choices, selected = NULL) {
      updateSelectInput(session, id, choices = choices, selected = selected)
    }
    .update_checkbox <- function(id, choices, selected = NULL) {
      updateCheckboxGroupInput(session, id, choices = choices, selected = selected)
    }

    # -------------------- base list of variables --------------------
    observeEvent(rv$data, {
      req(rv$data)
      vars <- names(rv$data)

      # selection
      .update_select("sel_outcome", c("", vars))
      .update_checkbox("sel_exposures", vars)

      # confounders (order: outcome -> confounders -> exposure)
      .update_select("conf_outcome", c("", vars))
      .update_checkbox("conf_potential", vars)
      .update_select("conf_exposure", c(""))

      # interaction (order: outcome -> exposure -> modifier -> covariates)
      .update_select("int_outcome", c("", vars))
      .update_select("int_exposure", c(""))
      .update_select("int_modifier", c(""))
      .update_checkbox("int_covariates", vars)

      # convergence
      .update_select("conv_outcome", c("", vars))
      .update_checkbox("conv_exposures", vars)
    }, ignoreInit = FALSE)

    # ==================== Model selection ===========================
    # outcome => exposures = vars \ outcome
    observeEvent(input$sel_outcome, {
      req(rv$data)
      vars <- names(rv$data)
      choices <- setdiff(vars, input$sel_outcome %||% "")
      keep <- intersect(input$sel_exposures %||% character(0), choices)
      .update_checkbox("sel_exposures", choices, selected = keep)
    }, ignoreInit = TRUE)

    sel_results <- eventReactive(input$run_select, {
      if (!requireNamespace("gtregression", quietly = TRUE)) {
        showNotification("Package 'gtregression' is required.", type="error"); return(NULL)
      }
      req(rv$data, nzchar(input$sel_outcome), input$sel_exposures)

      mv_fit <- try(
        .call_gt(c("multi_reg","multivariable_regression"),
                 data = rv$data, outcome = input$sel_outcome,
                 exposures = input$sel_exposures, approach = input$sel_approach),
        silent = TRUE
      )
      res <- try(
        .call_gt(c("select_models","model_select","stepwise_select"),
                 fit = if (!inherits(mv_fit, "try-error")) mv_fit else NULL,
                 model = if (!inherits(mv_fit, "try-error")) mv_fit else NULL,
                 data = rv$data, outcome = input$sel_outcome,
                 exposures = input$sel_exposures, approach = input$sel_approach,
                 direction = input$sel_method, criterion = "AIC", max_steps = 10L),
        silent = TRUE
      )
      if (inherits(res, "try-error")) {
        showNotification(paste("Model selection failed:", as.character(res)), type="error")
        return(NULL)
      }
      res
    })
    output$select_summary <- renderPrint({ req(sel_results()); print(sel_results()) })
    output$select_summary_txt <- downloadHandler(
      filename = "select_summary.txt",
      content = function(file) writeLines(.as_text(sel_results()), file)
    )
    output$select_code <- renderText({
      req(nzchar(input$sel_outcome), length(input$sel_exposures))
      paste0(
        '# ---- Stepwise model selection ----
# install.packages("gtregression"); library(gtregression)
mv_fit <- gtregression::multi_reg(
  data = df,
  outcome = "', input$sel_outcome, '",
  exposures = c(', paste(sprintf('"%s"', input$sel_exposures), collapse=", "), '),
  approach = "', input$sel_approach, '"
)
sel <- gtregression::select_models(
  fit = mv_fit,
  direction = "', input$sel_method, '",
  criterion = "AIC", max_steps = 10L
)
print(sel)'
      )
    })
output$select_code_download <- downloadHandler(
  filename = function() "model_selection_code.R",
  content  = function(file) writeLines(output$select_code() %||% "", con = file)
)

# ==================== Confounders ===============================
# --- helpers (optional, local) ---
`%||%` <- function(a, b) if (is.null(a) || !nzchar(a)) b else a

# ==================== Confounders: populate hierarchical choices ====================

# 0) When data arrives, seed all three selects
observeEvent(rv$data, {
  req(rv$data)
  vars <- names(rv$data)

  updateSelectInput(session, "conf_outcome",       choices = c("", vars))
  updateSelectInput(session, "conf_potential_one", choices = c(""))   # empty until outcome picked
  updateSelectInput(session, "conf_exposure_one",  choices = c(""))   # empty until confounder picked
}, ignoreInit = FALSE)

# 1) After OUTCOME chosen -> confounder excludes outcome; exposure excludes outcome+confounder
observeEvent(input$conf_outcome, {
  req(rv$data)
  vars <- names(rv$data)

  conf_choices <- setdiff(vars, input$conf_outcome %||% "")
  sel_conf     <- if ((input$conf_potential_one %||% "") %in% conf_choices) input$conf_potential_one else ""
  updateSelectInput(session, "conf_potential_one", choices = c("", conf_choices), selected = sel_conf)

  exp_choices  <- setdiff(vars, unique(c(input$conf_outcome %||% "", input$conf_potential_one %||% "")))
  sel_exp      <- if ((input$conf_exposure_one %||% "") %in% exp_choices) input$conf_exposure_one else ""
  updateSelectInput(session, "conf_exposure_one",  choices = c("", exp_choices),  selected = sel_exp)
}, ignoreInit = TRUE)

# 2) After CONFOUNDER chosen -> exposure excludes outcome+confounder
observeEvent(input$conf_potential_one, {
  req(rv$data)
  vars <- names(rv$data)

  exp_choices <- setdiff(vars, unique(c(input$conf_outcome %||% "", input$conf_potential_one %||% "")))
  sel_exp     <- if ((input$conf_exposure_one %||% "") %in% exp_choices) input$conf_exposure_one else ""
  updateSelectInput(session, "conf_exposure_one", choices = c("", exp_choices), selected = sel_exp)
}, ignoreInit = TRUE)

# ==================== Confounders (console-exact display) =======================



conf_exec <- eventReactive(input$run_conf, {
  if (!requireNamespace("gtregression", quietly = TRUE)) {
    showNotification("Package 'gtregression' is required.", type = "error"); return(NULL)
  }
  req(rv$data, nzchar(input$conf_outcome), nzchar(input$conf_potential_one), nzchar(input$conf_exposure_one))

  thr_pct <- suppressWarnings(as.numeric(input$conf_threshold))
  if (is.na(thr_pct)) thr_pct <- 10

  call_id <- if ("identify_confounder" %in% getNamespaceExports("gtregression"))
    gtregression::identify_confounder else gtregression::identify_confounders

  out <- try(
    .capture_console(
      call_id(
        data = rv$data,
        outcome = input$conf_outcome,
        exposure = input$conf_exposure_one,
        potential_confounder = input$conf_potential_one,
        approach = input$conf_approach,
        threshold = thr_pct  # NOTE: percent, not proportion
      )
    ),
    silent = TRUE
  )

  if (inherits(out, "try-error")) {
    showNotification(paste("Confounding assessment failed:", as.character(out)), type = "error")
    return(NULL)
  }
  out
})

# object (if you still need it elsewhere)
conf_results <- reactive({ req(conf_exec()); conf_exec()$value })

# render exact console output
output$conf_output <- renderText({
  req(conf_exec())
  sub("\\n?NULL\\s*$", "", conf_exec()$console)
})


# keep export using the same captured text
output$conf_output_txt <- downloadHandler(
  filename = "conf_output.txt",
  content  = function(file) writeLines(conf_exec()$console, file)
)

# reproducible code (unchanged, but ensure threshold stays in percent)
output$conf_code <- renderText({
  req(nzchar(input$conf_outcome), nzchar(input$conf_potential_one), nzchar(input$conf_exposure_one))
  thr <- suppressWarnings(as.numeric(input$conf_threshold)); if (is.na(thr)) thr <- 10
  paste0(
    '# ---- Identify confounder (single) ----
# install.packages("gtregression"); library(gtregression)
identify_confounder(
  data = df,
  outcome = "', input$conf_outcome, '",
  exposure = "', input$conf_exposure_one, '",
  potential_confounder = "', input$conf_potential_one, '",
  approach = "', input$conf_approach, '",
  threshold = ', thr, '
)'
  )
})
output$conf_code_download <- downloadHandler(
  filename = function() "confounding_code.R",
  content  = function(file) writeLines(output$conf_code(), file)
)


# ==================== Interaction ===============================
# ----- Interaction: hierarchical inputs -----
observeEvent(rv$data, {
  req(rv$data)
  vars <- names(rv$data)
  updateSelectInput(session, "int_outcome",      choices = c("", vars))
  updateSelectInput(session, "int_exposure_one", choices = c(""))
  updateSelectInput(session, "int_modifier_one", choices = c(""))
  updateCheckboxGroupInput(session, "int_covariates", choices = vars, selected = character(0))
}, ignoreInit = FALSE)

# Outcome -> exposure excludes outcome
observeEvent(input$int_outcome, {
  req(rv$data)
  vars <- names(rv$data)
  exp_choices <- setdiff(vars, input$int_outcome %||% "")
  updateSelectInput(session, "int_exposure_one",
                    choices = c("", exp_choices),
                    selected = if ((input$int_exposure_one %||% "") %in% exp_choices)
                      input$int_exposure_one else "")
  # modifier excludes outcome + exposure
  mod_choices <- setdiff(vars, unique(c(input$int_outcome %||% "", input$int_exposure_one %||% "")))
  updateSelectInput(session, "int_modifier_one",
                    choices = c("", mod_choices),
                    selected = if ((input$int_modifier_one %||% "") %in% mod_choices)
                      input$int_modifier_one else "")
  # covariates exclude outcome/exposure/modifier
  cov_choices <- setdiff(vars, unique(c(input$int_outcome %||% "", input$int_exposure_one %||% "", input$int_modifier_one %||% "")))
  updateCheckboxGroupInput(session, "int_covariates",
                           choices = cov_choices,
                           selected = intersect(input$int_covariates %||% character(0), cov_choices))
}, ignoreInit = TRUE)

# Exposure -> modifier & covariates updates
observeEvent(input$int_exposure_one, {
  req(rv$data)
  vars <- names(rv$data)
  mod_choices <- setdiff(vars, unique(c(input$int_outcome %||% "", input$int_exposure_one %||% "")))
  updateSelectInput(session, "int_modifier_one",
                    choices = c("", mod_choices),
                    selected = if ((input$int_modifier_one %||% "") %in% mod_choices)
                      input$int_modifier_one else "")
  cov_choices <- setdiff(vars, unique(c(input$int_outcome %||% "", input$int_exposure_one %||% "", input$int_modifier_one %||% "")))
  updateCheckboxGroupInput(session, "int_covariates",
                           choices = cov_choices,
                           selected = intersect(input$int_covariates %||% character(0), cov_choices))
}, ignoreInit = TRUE)

# Modifier -> covariates update
observeEvent(input$int_modifier_one, {
  req(rv$data)
  vars <- names(rv$data)
  cov_choices <- setdiff(vars, unique(c(input$int_outcome %||% "", input$int_exposure_one %||% "", input$int_modifier_one %||% "")))
  updateCheckboxGroupInput(session, "int_covariates",
                           choices = cov_choices,
                           selected = intersect(input$int_covariates %||% character(0), cov_choices))
}, ignoreInit = TRUE)
# Run interaction and capture console output exactly
int_exec <- eventReactive(input$run_int, {
  if (!requireNamespace("gtregression", quietly = TRUE)) {
    showNotification("Package 'gtregression' is required.", type="error"); return(NULL)
  }
  req(rv$data, nzchar(input$int_outcome), nzchar(input$int_exposure_one), nzchar(input$int_modifier_one))

  out <- try(
    .capture_console(
      gtregression::interaction_models(
        data = rv$data,
        outcome = input$int_outcome,
        exposure = input$int_exposure_one,
        effect_modifier = input$int_modifier_one,
        covariates = input$int_covariates,
        approach = input$int_approach
      )
    ),
    silent = TRUE
  )

  if (inherits(out, "try-error")) {
    showNotification(paste("Interaction modelling failed:", as.character(out)), type="error")
    return(NULL)
  }
  out
})

# Object (if you need it elsewhere)
int_results <- reactive({ req(int_exec()); int_exec()$value })

# Show EXACT console output (no NULL)
output$int_output <- renderText({ req(int_exec()); int_exec()$console })

# Export the same console text
output$int_output_txt <- downloadHandler(
  filename = "int_output.txt",
  content  = function(file) writeLines(int_exec()$console, file)
)

# Reproducible code (unchanged)
output$int_code <- renderText({
  req(nzchar(input$int_outcome), nzchar(input$int_exposure_one), nzchar(input$int_modifier_one))
  paste0(
    '# ---- Interaction models ----
# install.packages("gtregression"); library(gtregression)
interaction_models(
  data = df,
  outcome = "', input$int_outcome, '",
  exposure = "', input$int_exposure_one, '",
  effect_modifier = "', input$int_modifier_one, '",
  covariates = c(', paste(sprintf('"%s"', input$int_covariates), collapse = ", "), '),
  approach = "', input$int_approach, '"
)'
  )
})
output$int_code_download <- downloadHandler(
  filename = function() "interaction_code.R",
  content  = function(file) writeLines(output$int_code() %||% "", con = file)
)

# ==================== Convergence ===============================
# outcome => exposures choices = vars \ outcome
observeEvent(input$conv_outcome, {
  req(rv$data)
  vars <- names(rv$data)
  choices <- setdiff(vars, input$conv_outcome %||% "")
  keep <- intersect(input$conv_exposures %||% character(0), choices)
  .update_checkbox("conv_exposures", choices, selected = keep)
}, ignoreInit = TRUE)

conv_results <- eventReactive(input$run_conv, {
  if (!requireNamespace("gtregression", quietly = TRUE)) {
    showNotification("Package 'gtregression' is required.", type="error"); return(NULL)
  }
  if (!.has_fn("gtregression","check_convergence")) {
    showNotification("check_convergence() is not exported by 'gtregression'.", type="error"); return(NULL)
  }
  req(rv$data, nzchar(input$conv_outcome), length(input$conv_exposures))

  res <- try(
    gtregression::check_convergence(
      data = rv$data,
      exposures = input$conv_exposures,
      outcome = input$conv_outcome,
      approach = input$conv_approach,
      multivariate = isTRUE(input$conv_multivariate)
    ),
    silent = TRUE
  )
  if (inherits(res, "try-error")) {
    showNotification(paste("Convergence check failed:", as.character(res)), type="error")
    return(NULL)
  }
  res
})
output$conv_output <- renderPrint({ req(conv_results()); print(conv_results()) })
output$conv_output_txt <- downloadHandler(
  filename = "convergence_output.txt",
  content = function(file) writeLines(.as_text(conv_results()), file)
)
output$conv_code <- renderText({
  req(nzchar(input$conv_outcome), length(input$conv_exposures))
  paste0(
    '# ---- Convergence diagnostics ----
# install.packages("gtregression"); library(gtregression)
gtregression::check_convergence(
  data = df,
  exposures = c(', paste(sprintf('"%s"', input$conv_exposures), collapse=", "), '),
  outcome = "', input$conv_outcome, '",
  approach = "', input$conv_approach, '",
  multivariate = ', if (isTRUE(input$conv_multivariate)) "TRUE" else "FALSE", '
)'
  )
})
output$conv_code_download <- downloadHandler(
  filename = function() "convergence_code.R",
  content  = function(file) writeLines(output$conv_code() %||% "", con = file)
)
  })

}
# ==================== end module ================================
