# inst/shiny/modules/help.R
mod_help_ui <- function(id) {
  ns <- NS(id)
  tabPanel(
    "Help",
    fluidRow(
      column(
        12,
        div(class = "card-like",
            h3(tagList(icon("circle-question"), " Help & Documentation")),
            div(class = "control-help",
                "Everything you need to use the gtregression GUI efficiently. ",
                "This page mirrors the workflow in the other tabs."
            ),
            tags$hr(),

            # ---- Quick start ---------------------------------------------------
            tags$details(open = NA,
                         tags$summary(tagList(icon("bolt"), strong("Quick start (5 steps)"))),
                         tags$ol(
                           tags$li("Go to ", tags$strong("Step 1: Import Data"), " → upload CSV/XLSX or pick a built-in dataset."),
                           tags$li("Open ", tags$strong("Step 2: Descriptive Tables"), " → choose exposures, optional stratifier → ",
                                   tags$strong("Run Descriptive Table"), " → export or copy code."),
                           tags$li("Open ", tags$strong("Step 3: Regression Analysis"),
                                   " → select outcome, exposures, and regression type → run ",
                                   tags$em("Univariate"), " and/or ", tags$em("Multivariable"), "."),
                           tags$li("Optionally ", tags$strong("combine"), " tables/plots or move to ",
                                   tags$strong("Step 4: Advanced Analysis"), " for model selection, confounding, interaction, and convergence."),
                           tags$li("Use the ", tags$strong("Reproducible Code"), " sub-tabs/buttons to copy runnable R code.")
                         )
            ),

            # ---- Import tips ---------------------------------------------------
            tags$details(
              tags$summary(tagList(icon("table"), strong("Import: tips & gotchas"))),
              tags$ul(
                tags$li("Supported files: ", tags$code(".csv"), ", ", tags$code(".xlsx"), "."),
                tags$li("Ensure categorical variables are coded as factors (or strings)."),
                tags$li("Missing values are allowed; some models drop rows with missing values in the selected variables."),
                tags$li("Large files: prefer CSV for speed. If a column looks numeric but should be a category, convert to factor before analysis.")
              ),
              tags$pre(class = "r", tags$code(
                '## Convert to factor before using the GUI
df$sex <- factor(df$sex)
df$age_group <- cut(df$age, breaks = c(0, 30, 50, 120), right = FALSE)'
              ))
            ),

            # ---- Descriptive tables -------------------------------------------
            tags$details(
              tags$summary(tagList(icon("list"), strong("Descriptive tables (Step 2)"))),
              p("Pick one or more exposures; optionally choose a stratifier (e.g., outcome)."),
              p("Percentage: column (default) or row. Overall column can be added first/last."),
              tags$pre(class = "r", tags$code(
                '# Reproducible code (typical)
library(gtregression)
desc_tbl <- descriptive_table(
  data = df,
  exposures = c("sex","age_group","smoke"),
  by = "diabetes",             # or NULL
  percent = "column",          # or "row"
  show_overall = NULL          # "first" or "last"
)'
              ))
            ),

            # ---- Regression ----------------------------------------------------
            tags$details(
              tags$summary(tagList(icon("layer-group"), strong("Regression (Step 3)"))),
              p("Select exactly one outcome, one or more exposures, and an approach (",
                tags$code("logit"), ", ", tags$code("log-binomial"), ", ", tags$code("poisson"),
                ", ", tags$code("robpoisson"), ", ", tags$code("negbin"), ", ", tags$code("linear"), ")."),
              tags$pre(class = "r", tags$code(
                '# Univariate
uni_res <- gtregression::uni_reg(
  data = df, outcome = "diabetes",
  exposures = c("sex","age_group","smoke"),
  approach = "logit"
)

# Multivariable
multi_res <- gtregression::multi_reg(
  data = df, outcome = "diabetes",
  exposures = c("sex","age_group","smoke"),
  approach = "logit"
)

# Forest plots & exports
p1 <- gtregression::plot_reg(uni_res)
p2 <- gtregression::plot_reg(multi_res)'
              ))
            ),

      # ---- Combine results ----------------------------------------------
      tags$details(
        tags$summary(tagList(icon("object-group"), strong("Combine results"))),
        p("You can merge descriptive + univariate + multivariable tables with spanners, or combine forest plots."),
        tags$pre(class = "r", tags$code(
          '# Tables with spanners (requires gtsummary)
library(gtsummary)
tbl_merged <- tbl_merge(
  tbls = list(desc_tbl, uni_res$table, multi_res$table),
  tab_spanner = c("Descriptive", "Univariate", "Multivariable")
)

# Plots (package helper if available)
p_combined <- if ("plot_reg_combine" %in% getNamespaceExports("gtregression")) {
  gtregression::plot_reg_combine(uni_res, multi_res)
} else {
  # Fallbacks
  patchwork::wrap_plots(gtregression::plot_reg(uni_res),
                        gtregression::plot_reg(multi_res), nrow = 1)
}'
        ))
      ),

    # ---- Advanced ------------------------------------------------------
    tags$details(
      tags$summary(tagList(icon("flask"), strong("Advanced analysis (Step 4)"))),
      tags$ul(
        tags$li(tags$strong("Model selection:"), " forward/backward/both on the selected approach."),
        tags$li(tags$strong("Confounders:"), " one exposure + one potential confounder; threshold is in percent (default 10)."),
        tags$li(tags$strong("Interaction:"), " exposure × modifier; optional covariates; prints same as console."),
        tags$li(tags$strong("Convergence:"), " diagnose fit issues for uni/multi models.")
      ),
      tags$pre(class = "r", tags$code(
        '# Select models (example)
sel <- gtregression::select_models(
  data = df, outcome = "diabetes",
  exposures = c("sex","age_group","smoke"), approach = "logit",
  direction = "both", criterion = "AIC", max_steps = 10
)

# Identify one confounder
gtregression::identify_confounder(
  data = df, outcome = "diabetes",
  exposure = "smoke", potential_confounder = "age_group",
  approach = "logit", threshold = 10
)

# Interaction models
gtregression::interaction_models(
  data = df, outcome = "diabetes",
  exposure = "bmi", effect_modifier = "glucose_cat",
  covariates = c("insulin_cat","age_cat","dpf_cat"),
  approach = "logit"
)'
      ))
    ),

# ---- Export & reproducibility -------------------------------------
tags$details(
  tags$summary(tagList(icon("file-export"), strong("Export & reproducibility"))),
  p("Every results panel has export buttons (DOCX/HTML/PDF/PNG) and a Reproducible Code section."),
  tags$pre(class = "r", tags$code(
    '# Tables
gtregression::save_table(multi_res$table, filename = "model_table.docx", format = "docx")

# gt rendering
gt_tbl <- gtregression::as_gt(multi_res$table)
gt::gtsave(gt_tbl, "model_table.png")

# Plots
ggplot2::ggsave("forest.png", plot = gtregression::plot_reg(multi_res), width = 8, height = 6)'
  ))
),

# ---- Troubleshooting ----------------------------------------------
tags$details(
  tags$summary(tagList(icon("triangle-exclamation"), strong("Troubleshooting"))),
  tags$ul(
    tags$li(tags$code("Outcome cannot be listed among exposures"),
            ": remove the outcome from the exposure list."),
    tags$li(tags$code("need at least two non-NA values to interpolate"),
            ": rare categories / quasi-separation. Simplify model or drop near-constant predictors."),
    tags$li(tags$code("Failed to build table"),
            ": choose at least one exposure; check data types of categorical variables."),
    tags$li("For Poisson/Robust Poisson/NegBin, ensure outcome is a count."),
    tags$li("Linear models assume numeric outcome; check for outliers and missingness.")
  )
),

# ---- Glossary ------------------------------------------------------
tags$details(
  tags$summary(tagList(icon("book-open"), strong("Glossary (mini)"))),
  tags$ul(
    tags$li(tags$strong("Exposure:"), " predictor / independent variable."),
    tags$li(tags$strong("Outcome:"), " dependent variable / response."),
    tags$li(tags$strong("Approach:"), " regression family/link choice."),
    tags$li(tags$strong("Stratifier:"), " variable used to split the analysis."),
    tags$li(tags$strong("Confounder:"), " variable associated with both exposure and outcome that distorts the effect.")
  )
),

# ---- About ---------------------------------------------------------
tags$details(
  tags$summary(tagList(icon("circle-info"), strong("About, version & citation"))),
  p(HTML(
    paste0(
      "<b>Package:</b> gtregression ",
      "<span id='", ns("ver_pkg"), "'></span>",
      " &nbsp; | &nbsp; ",
      "<b>GUI:</b> ", "<span id='", ns("ver_app"), "'></span>"
    )
  )),
  p("Please cite gtregression when you use this tool in your work.")
)
        )
      )
    )
  )
}

mod_help_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Version badges (safe if pkg not installed)
    ver_pkg <- tryCatch(as.character(utils::packageVersion("gtregression")), error = function(e) "unknown")
    ver_app <- "v1"

    session$sendCustomMessage("setText", list(id = session$ns("ver_pkg"), text = ver_pkg))
    session$sendCustomMessage("setText", list(id = session$ns("ver_app"), text = ver_app))

    # small JS helper to set text (no extra deps)
    shiny::insertUI("body", "beforeEnd",
                    ui = tags$script(HTML(
                      "Shiny.addCustomMessageHandler('setText', function(x){var el=document.getElementById(x.id); if(el){el.textContent=x.text;}});"
                    ))
    )
  })
}
