options(shiny.sanitize.errors = FALSE)

required_app_packages <- c("shiny", "gt", "flextable", "ggplot2")
missing_app_packages <- required_app_packages[
  !vapply(required_app_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_app_packages)) {
  stop(
    "The gtregression app requires these packages: ",
    paste(missing_app_packages, collapse = ", "),
    ". Install them and run gtregression_app() again.",
    call. = FALSE
  )
}

library(shiny)

`%||%` <- function(a, b) if (!is.null(a)) a else b

gtx_ns <- asNamespace("gtregression")

# The app is sourced by Shiny outside the package namespace. Bind the internal
# modules and helpers here so installed-package behaviour matches load_all().
mod_data_prep_ui <- get("mod_data_prep_ui", envir = gtx_ns)
mod_data_prep_server <- get("mod_data_prep_server", envir = gtx_ns)
gtx_reference_candidates <- get("gtx_reference_candidates", envir = gtx_ns)
gtx_relevel_predictors <- get("gtx_relevel_predictors", envir = gtx_ns)
gtx_reference_code <- get("gtx_reference_code", envir = gtx_ns)

gtx_exported <- function(name) {
  name %in% getNamespaceExports("gtregression")
}

gtx_has_formal <- function(fun, arg) {
  arg %in% names(formals(get(fun, envir = gtx_ns)))
}

gtx_call <- function(fun, ...) {
  args <- list(...)
  f <- get(fun, envir = gtx_ns)
  args <- args[intersect(names(args), names(formals(f)))]
  do.call(f, args)
}

gtx_vec_code <- function(x) {
  if (length(x) == 0) return("character(0)")
  paste0("c(", paste(sprintf('\"%s\"', x), collapse = ", "), ")")
}

gtx_has_text <- function(x) {
  !is.null(x) && length(x) > 0 && !is.na(x[1]) && nzchar(x[1])
}

gtx_null_code <- function(x) {
  if (!gtx_has_text(x)) "NULL" else sprintf('\"%s\"', x[1])
}

gtx_object_name <- function(x, fallback) {
  x <- trimws(x %||% "")
  if (!nzchar(x)) return(fallback)
  out <- make.names(x)
  if (!grepl("^[A-Za-z.]", out)) out <- paste0("model_", out)
  out
}

gtx_bool_code <- function(x) {
  if (isTRUE(x)) "TRUE" else "FALSE"
}

gtx_limits_code <- function(x) {
  if (is.null(x) || length(x) != 2L || anyNA(x)) return("NULL")
  paste0("c(", paste(x, collapse = ", "), ")")
}

gtx_code_call <- function(fun, args) {
  keep <- !vapply(args, is.null, logical(1))
  args <- args[keep]
  if (!length(args)) return(paste0(fun, "()"))
  lines <- sprintf("  %s = %s", names(args), unlist(args, use.names = FALSE))
  paste0(fun, "(\n", paste(lines, collapse = ",\n"), "\n)")
}

gtx_code_assign <- function(name, fun, args) {
  paste0(name, " <- ", gtx_code_call(fun, args))
}

gtx_nonempty_vec_code <- function(x) {
  if (length(x)) gtx_vec_code(x) else NULL
}

gtx_table <- function(x) {
  if (inherits(x, "gt_tbl")) return(x)
  if (!is.null(x$table) && inherits(x$table, "gt_tbl")) return(x$table)
  if (!is.null(x$table) && inherits(x$table, "flextable")) {
    return(x$table)
  }
  if (inherits(x, "flextable")) return(x)
  if (is.data.frame(x)) return(gt::gt(x))
  if (!is.null(x$table_display) && is.data.frame(x$table_display)) return(gt::gt(x$table_display))
  if (!is.null(x$table_body) && is.data.frame(x$table_body)) return(gt::gt(x$table_body))
  stop("Could not find a displayable table in this result.", call. = FALSE)
}

gtx_table_output <- function(id) {
  uiOutput(id)
}

gtx_data_output <- function(id) {
  if (requireNamespace("DT", quietly = TRUE)) {
    DT::dataTableOutput(id)
  } else {
    tableOutput(id)
  }
}

gtx_render_data <- function(expr, page_length = 10) {
  if (requireNamespace("DT", quietly = TRUE)) {
    DT::renderDataTable({
      DT::datatable(expr(), options = list(scrollX = TRUE, pageLength = page_length))
    })
  } else {
    renderTable(expr(), striped = TRUE, bordered = TRUE, spacing = "s")
  }
}

gtx_render_table <- function(expr) {
  renderUI({
    res <- expr()
    req(res)
    tryCatch({
      tb <- gtx_table(res)
      if (inherits(tb, "flextable")) {
        return(flextable::htmltools_value(tb))
      }
      if (inherits(tb, "gt_tbl")) {
        return(HTML(gt::as_raw_html(tb, inline_css = TRUE)))
      }
      tb
    }, error = function(e) {
      div(
        class = "alert alert-danger",
        strong("This result could not be displayed. "),
        conditionMessage(e)
      )
    })
  })
}

gtx_capture <- function(expr) {
  value <- NULL
  text <- utils::capture.output(
    withCallingHandlers(
      {
        value <- eval.parent(substitute(expr))
        if (!inherits(value, c("gtregression", "gt_tbl", "ggplot", "patchwork"))) print(value)
      },
      message = function(m) {
        cat(conditionMessage(m), "\n")
        invokeRestart("muffleMessage")
      },
      warning = function(w) {
        cat("Warning: ", conditionMessage(w), "\n", sep = "")
        invokeRestart("muffleWarning")
      }
    )
  )
  list(value = value, text = paste(text, collapse = "\n"))
}

gtx_dataset_names <- function() {
  objs <- try(utils::data(package = "gtregression")$results[, "Item"], silent = TRUE)
  if (inherits(objs, "try-error")) character(0) else sort(unique(objs))
}

gtx_dataset <- function(name) {
  env <- new.env(parent = emptyenv())
  utils::data(list = name, package = "gtregression", envir = env)
  get(name, envir = env)
}

gtx_approaches <- c(
  "Logistic" = "logit",
  "Firth logistic" = "firth",
  "Log-binomial" = "logbinomial",
  "Poisson" = "poisson",
  "Robust Poisson" = "robpoisson",
  "Negative binomial" = "negbin",
  "Linear" = "linear"
)

gtx_parse_named_stats <- function(x) {
  if (is.null(x) || !nzchar(trimws(x))) return(NULL)
  parts <- strsplit(x, ",", fixed = TRUE)[[1]]
  parts <- trimws(parts[nzchar(trimws(parts))])
  out <- character(0)
  for (part in parts) {
    kv <- strsplit(part, "=", fixed = TRUE)[[1]]
    if (length(kv) != 2L || !nzchar(trimws(kv[1])) || !nzchar(trimws(kv[2]))) {
      stop("Statistic overrides should look like: age = mean, lwt = median", call. = FALSE)
    }
    out[trimws(kv[1])] <- trimws(kv[2])
  }
  out
}

gtx_statistic_code <- function(x) {
  stats <- tryCatch(gtx_parse_named_stats(x), error = function(e) NULL)
  if (is.null(stats) || !length(stats)) return(NULL)
  paste0(
    "c(",
    paste(
      sprintf("%s = %s", names(stats), shQuote(unname(stats))),
      collapse = ", "
    ),
    ")"
  )
}

gtx_parse_numeric_vector <- function(x, expected = NULL, name = "value") {
  if (!gtx_has_text(x)) return(NULL)
  parts <- trimws(strsplit(x, ",", fixed = TRUE)[[1]])
  parts <- parts[nzchar(parts)]
  out <- suppressWarnings(as.numeric(parts))
  if (!length(out) || anyNA(out)) {
    stop("`", name, "` should contain numbers separated by commas.", call. = FALSE)
  }
  if (!is.null(expected) && length(out) != expected) {
    stop("`", name, "` should contain exactly ", expected, " numbers.", call. = FALSE)
  }
  out
}

gtx_numeric_vector_code <- function(x, expected = NULL) {
  vals <- tryCatch(
    gtx_parse_numeric_vector(x, expected = expected, name = "value"),
    error = function(e) NULL
  )
  if (is.null(vals)) return(NULL)
  paste0("c(", paste(vals, collapse = ", "), ")")
}

gtx_inline_help <- function(...) {
  div(class = "gtx-field-help", ...)
}

gtx_select_buttons <- function(id) {
  div(
    class = "gtx-select-buttons",
    actionButton(paste0(id, "_all"), "Select all", class = "btn-default btn-xs"),
    actionButton(paste0(id, "_clear"), "Clear all", class = "btn-default btn-xs")
  )
}

gtx_template_birthwt <- function() {
  df <- gtx_dataset("data_birthwt")
  df$race <- factor(df$race, levels = c(1, 2, 3), labels = c("White", "Black", "Other"))
  df$smoke <- factor(df$smoke, levels = c(0, 1), labels = c("No", "Yes"))
  df$ht <- factor(df$ht, levels = c(0, 1), labels = c("No", "Yes"))
  df$ui <- factor(df$ui, levels = c(0, 1), labels = c("No", "Yes"))
  df$low <- factor(df$low, levels = c(0, 1), labels = c("Normal BW", "Low BW"))
  df$ptl_cat <- factor(ifelse(df$ptl > 0, "Yes", "No"), levels = c("No", "Yes"))
  df$ftv_cat <- ifelse(df$ftv == 0, "None", ifelse(df$ftv == 1, "One", "Two or more"))
  df$ftv_cat <- factor(df$ftv_cat, levels = c("None", "One", "Two or more"))
  df
}

gtx_template_lung <- function() {
  df <- gtx_dataset("data_lungcancer")
  df$trt <- factor(df$trt, levels = c(1, 2), labels = c("Standard treatment", "Test treatment"))
  df$prior <- factor(df$prior, levels = c(0, 10), labels = c("No", "Yes"))
  df$celltype <- factor(
    df$celltype,
    levels = c("squamous", "smallcell", "adeno", "large"),
    labels = c("Squamous", "Small cell", "Adenocarcinoma", "Large cell")
  )
  df
}

gtx_template_code <- function(template) {
  switch(
    template,
    birthwt = paste(
      'data("data_birthwt", package = "gtregression")',
      "",
      "df <- data_birthwt |>",
      "  dplyr::mutate(",
      '    race = factor(race, levels = c(1, 2, 3), labels = c("White", "Black", "Other")),',
      '    smoke = factor(smoke, levels = c(0, 1), labels = c("No", "Yes")),',
      '    ht = factor(ht, levels = c(0, 1), labels = c("No", "Yes")),',
      '    ui = factor(ui, levels = c(0, 1), labels = c("No", "Yes")),',
      '    low = factor(low, levels = c(0, 1), labels = c("Normal BW", "Low BW")),',
      '    ptl_cat = factor(ifelse(ptl > 0, "Yes", "No"), levels = c("No", "Yes")),',
      '    ftv_cat = factor(dplyr::case_when(',
      '      ftv == 0 ~ "None",',
      '      ftv == 1 ~ "One",',
      '      ftv >= 2 ~ "Two or more"',
      '    ), levels = c("None", "One", "Two or more"))',
      "  )",
      sep = "\n"
    ),
    lung = paste(
      'data("data_lungcancer", package = "gtregression")',
      "",
      "df <- data_lungcancer |>",
      "  dplyr::mutate(",
      '    trt = factor(trt, levels = c(1, 2), labels = c("Standard treatment", "Test treatment")),',
      '    prior = factor(prior, levels = c(0, 10), labels = c("No", "Yes")),',
      '    celltype = factor(',
      "      celltype,",
      '      levels = c("squamous", "smallcell", "adeno", "large"),',
      '      labels = c("Squamous", "Small cell", "Adenocarcinoma", "Large cell")',
      "    )",
      "  )",
      sep = "\n"
    ),
    diabetes = paste(
      'data("data_diabetes_mediation", package = "gtregression")',
      "df <- data_diabetes_mediation",
      sep = "\n"
    ),
    endometrial = paste(
      'data("data_endometrial", package = "gtregression")',
      "df <- data_endometrial",
      sep = "\n"
    ),
    ""
  )
}

gtx_validate_roles <- function(
    outcome = NULL, time = NULL, event = NULL, exposures = character(0),
    adjust_for = character(0), stratifier = NULL, context = "analysis") {
  exposures <- exposures[nzchar(exposures)]
  adjust_for <- adjust_for[nzchar(adjust_for)]
  protected <- unique(c(outcome, time, event, stratifier))
  protected <- protected[!is.na(protected) & nzchar(protected)]

  role_overlap <- intersect(exposures, protected)
  if (length(role_overlap)) {
    stop(
      "Role conflict in ", context, ": ",
      paste(role_overlap, collapse = ", "),
      " cannot also be selected as an exposure.",
      call. = FALSE
    )
  }

  adjust_overlap <- intersect(adjust_for, protected)
  if (length(adjust_overlap)) {
    stop(
      "Role conflict in ", context, ": ",
      paste(adjust_overlap, collapse = ", "),
      " cannot also be selected as an adjustment variable.",
      call. = FALSE
    )
  }

  duplicated_model_terms <- intersect(exposures, adjust_for)
  if (length(duplicated_model_terms)) {
    stop(
      "Role conflict in ", context, ": ",
      paste(duplicated_model_terms, collapse = ", "),
      " cannot be both exposures and adjustment variables.",
      call. = FALSE
    )
  }

  invisible(TRUE)
}

gtx_km_break_value <- function(x) {
  if (is.null(x) || !length(x) || is.na(x[1])) return(NULL)
  x[1]
}

gtx_export_size <- function(preset = "standard", is_forest = FALSE) {
  preset <- preset %||% "standard"
  if (is_forest) {
    return(switch(
      preset,
      wide = list(width = 16, height = 9),
      many_rows = list(width = 16, height = 13),
      compact = list(width = 10, height = 7),
      list(width = 13, height = 9)
    ))
  }
  switch(
    preset,
    wide = list(width = 11, height = 7),
    many_rows = list(width = 12, height = 8),
    compact = list(width = 7, height = 5),
    list(width = 9, height = 6)
  )
}

gtx_table_downloads <- function(output, id, result_reactive, table_get = function(x) x) {
  output[[paste0(id, "_docx")]] <- downloadHandler(
    filename = function() paste0(id, ".docx"),
    content = function(file) gtregression::save_table(table_get(result_reactive()), filename = file, format = "docx")
  )
  output[[paste0(id, "_rtf")]] <- downloadHandler(
    filename = function() paste0(id, ".rtf"),
    content = function(file) gtregression::save_table(table_get(result_reactive()), filename = file, format = "rtf")
  )
  output[[paste0(id, "_html")]] <- downloadHandler(
    filename = function() paste0(id, ".html"),
    content = function(file) gtregression::save_table(table_get(result_reactive()), filename = file, format = "html")
  )
}

gtx_plot_downloads <- function(output, id, plot_reactive, is_forest = FALSE, size_get = function() "standard") {
  output[[paste0(id, "_png")]] <- downloadHandler(
    filename = function() paste0(id, ".png"),
    content = function(file) {
      p <- plot_reactive()
      dims <- gtx_export_size(size_get(), is_forest = is_forest)
      if (is_forest) {
        gtregression::save_forest(p, filename = file, format = "png", width = dims$width, height = dims$height)
      } else {
        gtregression::save_plot(p, filename = file, format = "png", width = dims$width, height = dims$height)
      }
    }
  )
  output[[paste0(id, "_pdf")]] <- downloadHandler(
    filename = function() paste0(id, ".pdf"),
    content = function(file) {
      p <- plot_reactive()
      dims <- gtx_export_size(size_get(), is_forest = is_forest)
      if (is_forest) {
        gtregression::save_forest(p, filename = file, format = "pdf", width = dims$width, height = dims$height)
      } else {
        gtregression::save_plot(p, filename = file, format = "pdf", width = dims$width, height = dims$height)
      }
    }
  )
}

tagListDownload <- function(id, table = TRUE, plot = FALSE) {
  tags$div(
    class = "download-strip",
    if (table) tagList(
      downloadButton(paste0(id, "_docx"), "DOCX"),
      downloadButton(paste0(id, "_rtf"), "RTF"),
      downloadButton(paste0(id, "_html"), "HTML")
    ),
    if (plot) tagList(
      downloadButton(paste0(id, "_png"), "PNG"),
      downloadButton(paste0(id, "_pdf"), "PDF")
    )
  )
}

gtx_code_panel <- function(title, id) {
  div(
    class = "gtx-card",
    div(
      class = "gtx-code-head",
      h4(title),
      tags$button(
        type = "button",
        class = "btn btn-default btn-sm gtx-copy",
        onclick = sprintf("gtxCopyCode('%s', this)", id),
        icon("copy"),
        " Copy"
      )
    ),
    verbatimTextOutput(id)
  )
}

css <- HTML("
:root {
  --gtx-charcoal: #262626;
  --gtx-ink: #171717;
  --gtx-soft: #F6F6F3;
  --gtx-muted: #666864;
  --gtx-line: #D9DAD6;
  --gtx-accent: #386A5A;
  --gtx-accent-soft: #E8F0EC;
  --gtx-text: #292A28;
}
body {
  color: var(--gtx-text);
  background: #FCFCFA;
  font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif;
  font-size: 15px;
  line-height: 1.5;
}
.navbar {
  background: var(--gtx-charcoal) !important;
  border: 0;
  border-radius: 0;
  box-shadow: 0 1px 0 rgba(0, 0, 0, 0.18);
}
.navbar-brand {
  color: white !important;
  font-weight: 700;
  letter-spacing: 0;
}
.navbar-nav > li > a {
  color: #E4E4E1 !important;
  font-weight: 600;
}
.navbar-nav > li > a:hover,
.navbar-nav > li > a:focus,
.navbar-nav > .active > a,
.navbar-nav > .active > a:hover,
.navbar-nav > .active > a:focus {
  background: #111 !important;
  color: #fff !important;
}
.container-fluid { max-width: 1480px; }
h1, h2, h3, h4, h5, h6 { color: var(--gtx-ink); font-weight: 700; }
.gtx-card {
  border: 1px solid var(--gtx-line);
  background: #fff;
  border-radius: 8px;
  padding: 16px;
  margin-bottom: 16px;
  box-shadow: 0 1px 3px rgba(20, 20, 20, 0.05);
}
.gtx-side { position: sticky; top: 12px; }
.gtx-tool-menu { padding: 12px 16px 8px; }
.gtx-tool-menu h3 { margin: 0 0 8px; font-size: 1.05rem; }
.gtx-tool-menu .form-group { margin-bottom: 0; }
.gtx-tool-menu .shiny-options-group {
  display: flex;
  flex-wrap: wrap;
  gap: 2px 22px;
}
.gtx-tool-menu .radio-inline {
  border-bottom: 3px solid transparent;
  margin: 0;
  padding: 8px 2px 10px 22px;
  font-weight: 700;
}
.gtx-tool-menu .radio-inline:has(input:checked) {
  border-bottom-color: var(--gtx-accent);
  color: var(--gtx-ink);
}
.gtx-help { color: var(--gtx-muted); font-size: 0.94rem; margin-bottom: 12px; }
.gtx-badge {
  display: inline-block;
  padding: 3px 8px;
  border-radius: 999px;
  background: var(--gtx-accent-soft);
  color: #285143;
  font-size: 0.78rem;
  font-weight: 700;
}
	.download-strip .btn { margin-right: 6px; margin-bottom: 6px; }
	pre {
	  background: #F5F5F2;
	  border: 1px solid var(--gtx-line);
	  border-radius: 6px;
	  color: #242424;
	}
	.nav-tabs { border-bottom-color: var(--gtx-line); }
	.nav-tabs > li > a { color: #555753; font-weight: 650; border-radius: 6px 6px 0 0; }
	.nav-tabs > li.active > a,
	.nav-tabs > li.active > a:hover,
	.nav-tabs > li.active > a:focus {
	  color: var(--gtx-ink);
	  border-color: var(--gtx-line) var(--gtx-line) #fff;
	  box-shadow: inset 0 3px 0 var(--gtx-accent);
	}
	.form-group label { font-weight: 700; }
	.form-control,
	.selectize-input {
	  border-color: #C9CAC6;
	  border-radius: 5px;
	  box-shadow: none;
	}
	.form-control:focus,
	.selectize-input.focus {
	  border-color: var(--gtx-accent);
	  box-shadow: 0 0 0 3px rgba(56, 106, 90, 0.13);
	}
	input[type='checkbox'], input[type='radio'] { accent-color: var(--gtx-accent); }
	.btn { border-radius: 5px; font-weight: 650; }
	.btn-primary,
	.btn-info {
	  background: var(--gtx-charcoal);
	  border-color: var(--gtx-charcoal);
	  color: #fff;
	}
	.btn-primary:hover, .btn-primary:focus, .btn-primary:active,
	.btn-info:hover, .btn-info:focus, .btn-info:active {
	  background: #111;
	  border-color: #111;
	}
	.btn-default { background: #fff; border-color: #BFC0BC; color: #30312F; }
	.btn-default:hover, .btn-default:focus { background: var(--gtx-soft); border-color: #8E908B; }
	.table > thead > tr > th { color: var(--gtx-ink); border-bottom-color: #777873; }
	.table > tbody > tr > td { border-top-color: #E4E4E0; }
	.gtx-code-head {
	  display: flex;
	  align-items: center;
	  justify-content: space-between;
	  gap: 12px;
	  margin-bottom: 8px;
	}
	.gtx-code-head h4 { margin: 0; }
	.gtx-copy { white-space: nowrap; }
	.gtx-close-app {
	  position: fixed;
	  right: 18px;
	  bottom: 18px;
	  z-index: 2000;
	  box-shadow: 0 4px 12px rgba(15, 23, 42, 0.18);
	}
	.gtx-step {
	  border-left: 4px solid var(--gtx-accent);
	  background: var(--gtx-soft);
	  padding: 10px 12px;
	  margin: 10px 0;
	  color: #3D403C;
	}
	.gtx-note {
	  background: #FFF7ED;
	  border: 1px solid #FED7AA;
	  border-radius: 6px;
	  padding: 10px 12px;
	  color: #7C2D12;
	  margin: 10px 0;
	}
	.gtx-workflow-note { margin-bottom: 14px; }
	.gtx-workflow-note h3 { margin-top: 0; }
	.gtx-step-row {
	  display: grid;
	  grid-template-columns: repeat(4, minmax(0, 1fr));
	  gap: 8px;
	  margin-top: 12px;
	}
	.gtx-step-row .gtx-step {
	  margin: 0;
	  min-height: 48px;
	}
	.gtx-section-label {
	  color: var(--gtx-charcoal);
	  font-size: 15px;
	  font-weight: 750;
	  margin: 2px 0 8px;
	}
	.gtx-model-card {
	  background: #FAFAF8;
	  border: 1px solid var(--gtx-line);
	  border-left: 4px solid var(--gtx-accent);
	  border-radius: 6px;
	  margin: 12px 0;
	  padding: 14px 16px 8px;
	}
	.gtx-model-card h4 {
	  color: var(--gtx-charcoal);
	  font-size: 1rem;
	  font-weight: 750;
	  margin: 0 0 12px;
	}
	.gtx-inline-actions {
	  display: flex;
	  gap: 8px;
	  margin: 8px 0 4px;
	}
	.gtx-warning {
	  background: #FFF4E5;
	  border-left: 4px solid #B45309;
	  color: #713F12;
	  padding: 10px 12px;
	  margin: 10px 0;
	}
	@media (max-width: 900px) {
	  .gtx-step-row { grid-template-columns: repeat(2, minmax(0, 1fr)); }
	}
	.gtx-workflow-bar {
	  background: #F3F3F0;
	  border-bottom: 1px solid var(--gtx-line);
	  padding: 10px 24px;
	}
	.gtx-workflow-wrap {
	  max-width: 1480px;
	  margin: 0 auto;
	  display: flex;
	  align-items: center;
	  gap: 8px;
	  flex-wrap: wrap;
	}
	.gtx-workflow-step-pill {
	  border: 1px solid var(--gtx-line);
	  border-radius: 999px;
	  padding: 5px 10px;
	  font-size: 0.86rem;
	  font-weight: 700;
	  background: white;
	  color: var(--gtx-muted);
	}
	.gtx-workflow-step-pill.done {
	  background: var(--gtx-accent-soft);
	  border-color: #B9CEC5;
	  color: #285143;
	}
	.gtx-workflow-step-pill.current {
	  background: var(--gtx-charcoal);
	  border-color: var(--gtx-charcoal);
	  color: white;
	}
	.gtx-workflow-next {
	  margin-left: auto;
	  font-size: 0.9rem;
	  color: #454743;
	}
	.gtx-analysis-strip {
	  background: #fff;
	  border-bottom: 1px solid var(--gtx-line);
	  padding: 7px 24px;
	}
	.gtx-analysis-strip-inner {
	  max-width: 1480px;
	  margin: 0 auto;
	  display: flex;
	  gap: 18px;
	  align-items: center;
	  flex-wrap: wrap;
	  color: #50524e;
	  font-size: 0.86rem;
	}
	.gtx-analysis-item strong { color: var(--gtx-ink); }
	.gtx-preflight {
	  background: var(--gtx-accent-soft);
	  border-left: 4px solid var(--gtx-accent);
	  border-radius: 0 6px 6px 0;
	  padding: 10px 12px;
	  margin: 12px 0;
	  color: #30483f;
	  font-size: 0.9rem;
	}
	.gtx-preflight strong { color: #203b32; }
	.gtx-run-group {
	  display: grid;
	  grid-template-columns: 1fr 1fr;
	  gap: 8px;
	  margin-top: 8px;
	}
	.gtx-run-group .btn { width: 100%; }
	.gtx-field-help {
	  color: var(--gtx-muted);
	  font-size: 0.88rem;
	  margin: -6px 0 10px 0;
	  line-height: 1.35;
	}
	.gtx-template-grid {
	  display: grid;
	  grid-template-columns: 1fr 1fr;
	  gap: 8px;
	  margin-bottom: 10px;
	}
	.gtx-template-grid .btn { white-space: normal; }
	.gtx-app-mode {
	  background: var(--gtx-soft);
	  border: 1px solid var(--gtx-line);
	  border-radius: 8px;
	  padding: 10px 12px;
	  margin-bottom: 14px;
	}
	.gtx-export-grid {
	  display: grid;
	  grid-template-columns: 1fr 1fr;
	  gap: 8px;
	  align-items: end;
	}
	.gtx-select-buttons {
	  display: flex;
	  gap: 6px;
	  margin: -8px 0 12px 0;
	}
	.gtx-select-buttons .btn {
	  padding: 2px 8px;
	  font-size: 0.78rem;
	}
	.gtx-button-stack {
	  display: flex;
	  flex-direction: column;
	  gap: 8px;
	  margin-top: 8px;
	}
	.gtx-advanced-tool-tabs .nav-pills {
	  margin-bottom: 12px;
	}
	.gtx-advanced-tool-tabs .tab-content {
	  padding-top: 4px;
	}
	.nav-pills > li > a { color: #50524E; border-radius: 5px; font-weight: 650; }
	.nav-pills > li.active > a,
	.nav-pills > li.active > a:hover,
	.nav-pills > li.active > a:focus {
	  background: var(--gtx-charcoal);
	  color: white;
	}
	.shiny-output-error-validation {
	  color: #7A2E24;
	  background: #FAF1EE;
	  border-left: 3px solid #A64B3C;
	  padding: 9px 11px;
	}
	@media (max-width: 767px) {
	  body { font-size: 14px; }
	  .gtx-side { position: static; }
	  .gtx-workflow-bar { padding: 8px 12px; }
	  .gtx-workflow-next { width: 100%; margin-left: 0; }
	  .gtx-analysis-strip { padding: 7px 12px; }
	  .gtx-analysis-strip-inner { gap: 8px 14px; }
	  .gtx-run-group { grid-template-columns: 1fr; }
	}
	")

copy_js <- HTML("
function gtxCopyCode(id, button) {
  var el = document.getElementById(id);
  var text = el ? el.innerText : '';
  if (!text.trim()) return;
  navigator.clipboard.writeText(text).then(function() {
    var original = button.innerHTML;
    button.innerHTML = 'Copied';
    setTimeout(function() { button.innerHTML = original; }, 1200);
  }).catch(function() {
    window.prompt('Copy this code:', text);
  });
}
")

ui <- navbarPage(
  title = tagList("gtregression", span(class = "gtx-badge", "App")),
  id = "main_nav",
  header = tagList(
    tags$head(tags$style(css), tags$script(copy_js)),
    div(class = "gtx-close-app", actionButton("close_app", "Close app", icon = icon("power-off"), class = "btn-danger")),
    uiOutput("workflow_guide"),
    uiOutput("analysis_status")
  ),

  tabPanel(
    "Data",
    fluidRow(
      column(
        4,
        div(
          class = "gtx-card gtx-side",
          div(
            class = "gtx-app-mode",
            radioButtons(
              "app_mode",
              "Experience mode",
              choices = c("Simple" = "simple", "Advanced" = "advanced"),
              selected = "simple",
              inline = TRUE
            ),
            gtx_inline_help("Simple mode shows the core workflow. Advanced mode reveals optional modelling and export controls.")
          ),
          h3("Load Data"),
          div(class = "gtx-help", "Start with a built-in teaching dataset or import CSV, Excel, RDS, Stata, SPSS, or SAS data."),
          div(
            class = "gtx-step",
            "Recommended flow: load data, build descriptive tables, run univariable models, reselect variables, then run multivariable or survival models."
          ),
          h4("Quick-start templates"),
          div(
            class = "gtx-template-grid",
            actionButton("template_birthwt", "Birthweight regression"),
            actionButton("template_lung", "Lung survival"),
            actionButton("template_diabetes", "Diabetes mediation"),
            actionButton("template_endometrial", "Firth logistic")
          ),
          gtx_inline_help("Templates load labelled teaching data and preselect common variables. They are safe to overwrite by choosing another dataset."),
          tags$hr(),
          selectInput("dataset_name", "Built-in dataset", choices = c("", gtx_dataset_names())),
          actionButton("load_dataset", "Load built-in data", class = "btn-primary"),
          tags$hr(),
          fileInput(
            "data_file",
            "Import a data file",
            accept = c(".csv", ".xlsx", ".xls", ".rds", ".dta", ".sav", ".zsav", ".por", ".sas7bdat", ".xpt")
          ),
          gtx_inline_help("Supported: CSV, Excel, RDS data frames, Stata (.dta), SPSS (.sav/.zsav/.por), and SAS (.sas7bdat/.xpt)."),
          checkboxInput("csv_header", "CSV has header", TRUE),
          selectInput("csv_sep", "CSV separator", choices = c("Comma" = ",", "Semicolon" = ";", "Tab" = "\t"), selected = ","),
          textInput("xlsx_sheet", "Excel sheet name/index", value = ""),
          gtx_inline_help("CSV settings apply only to CSV files. The sheet field applies only to Excel; leave it blank for the first sheet."),
          actionButton("load_upload", "Load uploaded file", class = "btn-primary")
        )
      ),
      column(
        8,
        div(class = "gtx-card", h3("Preview"), gtx_data_output("data_preview")),
        div(class = "gtx-card", h4("Data Summary"), verbatimTextOutput("data_summary")),
        gtx_code_panel("Reusable Code", "data_code")
      )
    )
  ),

  tabPanel(
    "Data Prep",
    value = "data_prep",
    mod_data_prep_ui("data_prep")
  ),

  tabPanel(
    "Descriptive",
    fluidRow(
      column(
        4,
        div(
          class = "gtx-card gtx-side",
          h3("Descriptive Table"),
          div(class = "gtx-help", "Use this first to understand the data and to create baseline tables for merge_table(), forest_df(), and reports."),
          uiOutput("desc_inputs"),
          selectInput("desc_percent", "Percent", choices = c("column", "row"), selected = "column"),
          selectInput("desc_overall", "Overall column", choices = c("no", "first", "last"), selected = "no"),
          selectInput("desc_missing", "Missing values", choices = c("ifany", "no"), selected = "ifany"),
          selectInput("desc_dich", "Binary display", choices = c("all_levels", "single_row"), selected = "all_levels"),
          conditionalPanel(
            "input.app_mode == 'advanced'",
            textInput("desc_statistic", "Numeric statistic override", placeholder = 'Optional: age = mean, lwt = median'),
            gtx_inline_help("Use named overrides only when a variable needs a different summary from the default.")
          ),
          actionButton("run_desc", "Run descriptive table", class = "btn-primary")
        )
      ),
      column(
        8,
        div(class = "gtx-card", tagListDownload("desc", table = TRUE), gtx_table_output("desc_table")),
        gtx_code_panel("Reusable Code", "desc_code")
      )
    )
  ),

  tabPanel(
    "Regression",
    fluidRow(
      column(
        4,
        div(
          class = "gtx-card gtx-side",
          h3("Regression Tables"),
          div(
            class = "gtx-step",
            "Run univariable analysis first. Then keep the important variables selected and run the multivariable model. Use Show reference categories when you plan to merge tables or create forest plots."
          ),
          uiOutput("reg_inputs"),
          uiOutput("reg_reference_inputs"),
          selectInput("reg_approach", "Approach", choices = gtx_approaches, selected = "logit"),
          checkboxInput("reg_show_ref", "Show reference categories", value = TRUE),
          conditionalPanel(
            "input.app_mode == 'advanced'",
            checkboxInput("reg_model_stats", "Store model statistics", value = FALSE),
            gtx_inline_help("Model statistics are useful for diagnostics and compare_models(), but they are not needed for basic tables.")
          ),
          uiOutput("reg_preflight"),
          div(
            class = "gtx-run-group",
            actionButton("run_uni", "Run univariate", class = "btn-primary"),
            actionButton("run_multi", "Run multivariable", class = "btn-primary")
          ),
          tags$hr(),
          uiOutput("reg_strata_inputs"),
          checkboxInput("reg_strata_multi", "Stratified multivariable", value = FALSE),
          actionButton("run_strat", "Run stratified regression", class = "btn-primary")
        )
      ),
      column(
        8,
        tabsetPanel(
          tabPanel("Univariate", div(class = "gtx-card", tagListDownload("uni", table = TRUE), gtx_table_output("uni_table")), gtx_code_panel("Code", "uni_code")),
          tabPanel("Multivariable", div(class = "gtx-card", tagListDownload("multi", table = TRUE), gtx_table_output("multi_table")), gtx_code_panel("Code", "multi_code")),
          tabPanel("Stratified", div(class = "gtx-card", tagListDownload("strat", table = TRUE), gtx_table_output("strat_table")), gtx_code_panel("Code", "strat_code")),
          tabPanel("Model Stats", div(class = "gtx-card", gtx_data_output("reg_stats")))
        )
      )
    )
  ),

  tabPanel(
    "Survival",
    div(
      class = "gtx-card gtx-tool-menu",
      h3("Choose a survival model"),
      radioButtons(
        "surv_mode",
        label = NULL,
        choices = c(
          "Cox regression" = "cox",
          "Parametric survival" = "survreg"
        ),
        selected = "cox",
        inline = TRUE
      )
    ),
    fluidRow(
      column(
        4,
        div(
          class = "gtx-card gtx-side",
          uiOutput("surv_mode_intro"),
          uiOutput("surv_inputs"),
          checkboxInput("surv_show_ref", "Show reference categories", value = TRUE),
          checkboxInput("surv_multivariable", "Single multivariable model", value = FALSE),
          uiOutput("surv_preflight"),
          conditionalPanel(
            "input.surv_mode == 'cox'",
            actionButton("run_cox", "Run Cox regression", class = "btn-primary")
          ),
          conditionalPanel(
            "input.surv_mode == 'survreg'",
            actionButton("run_survreg", "Run parametric survival regression", class = "btn-primary")
          ),
          tags$hr(),
          h4("Kaplan-Meier curve"),
          div(class = "gtx-help", "This observed survival view is available for either model workflow."),
          uiOutput("km_group_input"),
          selectInput("km_theme", "KM theme", choices = c("classic", "minimal", "bw", "light", "none"), selected = "classic"),
          checkboxInput("km_y_percent", "Display y-axis as percent", TRUE),
          checkboxInput("km_grid", "Show grid", FALSE),
          checkboxInput("km_conf", "Show confidence interval", TRUE),
          checkboxInput("km_p", "Show log-rank p-value", TRUE),
          checkboxInput("km_risk", "Show risk table", TRUE),
          textInput("km_ylim", "Y limits", placeholder = "Optional: 50, 100"),
          textInput("km_xlim", "X limits", placeholder = "Optional: 0, 800"),
          conditionalPanel(
            "input.app_mode == 'advanced'",
            numericInput("km_break", "Time break interval", value = NA, min = 0)
          ),
          actionButton("run_km", "Draw KM plot", class = "btn-primary")
        )
      ),
      column(
        8,
        tabsetPanel(
          id = "surv_output_tab",
          tabPanel(
            "Model Result",
            conditionalPanel(
              "input.surv_mode == 'cox'",
              tabsetPanel(
                id = "cox_result_tab",
                tabPanel(
                  "Exposure models",
                  div(class = "gtx-card", tagListDownload("cox_exposure", table = TRUE), gtx_table_output("cox_exposure_table"))
                ),
                tabPanel(
                  "Multivariable model",
                  div(class = "gtx-card", tagListDownload("cox_multi", table = TRUE), gtx_table_output("cox_multi_table"))
                )
              )
            ),
            conditionalPanel(
              "input.surv_mode == 'survreg'",
              tabsetPanel(
                id = "survreg_result_tab",
                tabPanel(
                  "Exposure models",
                  div(class = "gtx-card", tagListDownload("survreg_exposure", table = TRUE), gtx_table_output("survreg_exposure_table"))
                ),
                tabPanel(
                  "Multivariable model",
                  div(class = "gtx-card", tagListDownload("survreg_multi", table = TRUE), gtx_table_output("survreg_multi_table"))
                )
              )
            )
          ),
          tabPanel("KM Plot", div(class = "gtx-card", tagListDownload("km", table = FALSE, plot = TRUE), plotOutput("km_plot", height = "650px"))),
          tabPanel("Survival Tables", div(class = "gtx-card", gtx_table_output("logrank_table")), div(class = "gtx-card", gtx_table_output("surv_summary_table"))),
          tabPanel("Code", gtx_code_panel("Code", "surv_code"))
        )
      )
    )
  ),

  tabPanel(
    "Visualise & Export",
    div(
      class = "gtx-card gtx-tool-menu",
      h3("Choose a tool"),
      radioButtons(
        "visual_tool",
        label = NULL,
        choices = c(
          "Merge tables" = "merge",
          "Regression plot" = "plot",
          "Forest plot" = "forest",
          "Model fit" = "fit"
        ),
        selected = "merge",
        inline = TRUE
      )
    ),
    fluidRow(
      column(
        4,
        div(
          class = "gtx-card gtx-side",
          conditionalPanel(
            "input.visual_tool == 'merge'",
            h3("Merge Tables"),
            div(class = "gtx-help", "Choose two or more completed tables. They are merged in the order shown below."),
            uiOutput("merge_table_choices"),
            gtx_inline_help("After merging, use DOCX or RTF for editable manuscripts and HTML for a browser-ready copy."),
            actionButton("run_merge", "Merge selected tables", class = "btn-primary")
          ),
          conditionalPanel(
            "input.visual_tool == 'plot'",
            h3("Regression Plot"),
            uiOutput("plot_content_controls"),
            checkboxInput("plot_log_x", "Log x-axis where appropriate", TRUE),
            actionButton("run_plot_reg", "Preview regression plot", class = "btn-primary")
          ),
          conditionalPanel(
            "input.visual_tool == 'forest'",
            h3("Forest Plot"),
            uiOutput("forest_content_controls"),
            selectInput("forest_side", "Forest plot side", choices = c("Text then plot" = "right", "Plot then text" = "left"), selected = "right"),
            gtx_inline_help("Choose which side contains the plotted confidence intervals; this does not change the estimates."),
            numericInput("forest_ci_width", "CI column width", value = 20, min = 4, step = 1),
            gtx_inline_help("Increase this when confidence intervals or tick labels overlap. Wider columns need a wider export preset."),
            textInput("forest_xlim", "Forest x limits", placeholder = "Optional: 0.25, 12"),
            gtx_inline_help("Enter two comma-separated limits. Leave blank to use forestploter's automatic range."),
            textInput("forest_ticks", "Forest tick marks", placeholder = "Optional: 0.5, 1, 2, 4, 8"),
            gtx_inline_help("Enter fewer comma-separated ticks when axis labels overlap. Every tick must lie within the x limits."),
            div(
              class = "gtx-help",
              "Combine descriptive, crude, and adjusted results when available. For overlapping labels, use fewer ticks, wider x limits, or a wider CI column."
            ),
            actionButton("run_forest", "Preview forest plot", class = "btn-primary")
          ),
          conditionalPanel(
            "input.visual_tool == 'fit'",
            h3("Model Fit"),
            uiOutput("fit_source_controls"),
            selectInput("fit_type", "Type", choices = c("diagnostics", "calibration", "residuals", "cooks"), selected = "diagnostics"),
            textInput("fit_model_name", "Model name for univariate objects", placeholder = "Optional, e.g. smoke"),
            numericInput("fit_bins", "Calibration bins", value = 6, min = 2),
            actionButton("run_fit_plot", "Draw model fit", class = "btn-primary")
          ),
          tags$hr(),
          div(
            class = "gtx-note",
            "Browser outputs are previews. Download tables directly, or copy the generated plot code into RStudio for final publication sizing."
          ),
          tags$hr(),
          h3("Complete R script"),
          div(
            class = "gtx-help",
            "Download one reproducible script containing the data source, preparation steps, reference categories, completed analyses, visualisations, and export commands from this session."
          ),
          downloadButton(
            "download_session_script",
            "Download complete R script",
            class = "btn-primary"
          ),
          selectInput(
            "export_preset",
            "Export size preset",
            choices = c("Standard" = "standard", "Wide" = "wide", "Many rows" = "many_rows", "Compact" = "compact"),
            selected = "standard"
          ),
          gtx_inline_help("Wide adds horizontal room; Many rows adds width and height. These settings affect downloaded PNG/PDF files, not model results."),
          uiOutput("export_size_hint")
        )
      ),
      column(
        8,
        tabsetPanel(
          id = "visual_output_tab",
          tabPanel(
            "Merged Table",
            div(
              class = "gtx-card",
              tagListDownload("merged", table = TRUE),
              gtx_inline_help("HTML preserves a browser-viewable table; DOCX and RTF are intended for editable reports."),
              gtx_table_output("merged_table")
            )
          ),
          tabPanel("plot_reg", div(class = "gtx-card", tagListDownload("plotreg", table = FALSE, plot = TRUE), plotOutput("plotreg_plot", height = "620px"))),
          tabPanel("forest_reg", div(class = "gtx-card", tagListDownload("forest", table = FALSE, plot = TRUE), plotOutput("forest_plot", height = "760px"))),
          tabPanel("Model fit", div(class = "gtx-card", tagListDownload("fitplot", table = FALSE, plot = TRUE), plotOutput("fit_plot", height = "680px"))),
          tabPanel("Code", gtx_code_panel("Code", "visual_code")),
          tabPanel("Full Workflow Code", gtx_code_panel("Full Workflow Code", "full_workflow_code"))
        )
      )
    )
  ),

  tabPanel(
    "Advanced",
    div(
      class = "gtx-card gtx-workflow-note",
      h3("Model building and diagnostic tools"),
      p("Start with model selection when you want the app to build a candidate path from an outcome and predictor set. Use model comparison after fitting two or more models in the Regression or Survival tabs."),
      div(
        class = "gtx-step-row",
        span(class = "gtx-step", strong("1  Select"), " candidate predictors"),
        span(class = "gtx-step", strong("2  Fit"), " candidate models"),
        span(class = "gtx-step", strong("3  Compare"), " saved models"),
        span(class = "gtx-step", strong("4  Check"), " assumptions")
      )
    ),
    div(
      class = "gtx-card gtx-tool-menu",
      h3("Choose an advanced tool"),
      radioButtons(
        "advanced_tool",
        label = NULL,
        choices = c(
          "Select models" = "selection",
          "Compare models" = "comparison",
          "Confounder" = "confounder",
          "Interaction" = "interaction",
          "Convergence" = "convergence",
          "Collinearity" = "collinearity"
        ),
        selected = "selection",
        inline = TRUE
      )
    ),
    fluidRow(
      column(
        4,
        div(
          class = "gtx-card gtx-side",
          uiOutput("advanced_config")
        )
      ),
      column(
        8,
        tabsetPanel(
          id = "advanced_output_tab",
          tabPanel(
            "Output",
            div(
              class = "gtx-card",
              uiOutput("advanced_result_heading"),
              uiOutput("advanced_downloads"),
              gtx_table_output("advanced_table")
            ),
            div(class = "gtx-card", uiOutput("advanced_guidance"), verbatimTextOutput("advanced_text"))
          ),
          tabPanel("Code", gtx_code_panel("Code", "advanced_code"))
        )
      )
    )
  ),

  tabPanel(
    "Mediation",
    fluidRow(
      column(
        4,
        div(
          class = "gtx-card gtx-side",
          h3("Causal Mediation"),
          uiOutput("med_inputs"),
          selectInput("med_approach", "Outcome model", choices = c("logit", "linear"), selected = "logit"),
          numericInput("med_sims", "Bootstrap replicates", value = 300, min = 50, step = 50),
          numericInput("med_seed", "Seed", value = 123, min = 1),
          actionButton("run_mediation", "Run mediation analysis", class = "btn-primary")
        )
      ),
      column(
        8,
        tabsetPanel(
          tabPanel("Table", div(class = "gtx-card", tagListDownload("med", table = TRUE), gtx_table_output("med_table"))),
          tabPanel("Diagram", div(class = "gtx-card", tagListDownload("medplot", table = FALSE, plot = TRUE), plotOutput("med_plot", height = "620px"))),
          tabPanel("Code", gtx_code_panel("Code", "med_code"))
        )
      )
    )
  ),

  tabPanel(
    "Help",
    fluidRow(
      column(
        12,
        div(
          class = "gtx-card",
          h2("gtregression App"),
          p("This app is a menu-driven companion to the R package. It is designed for beginners who want publication-ready tables and plots, while always showing reproducible R code."),
          tags$ol(
            tags$li("Load a dataset."),
            tags$li("Create descriptive tables."),
            tags$li("Run univariable analysis to screen variables."),
            tags$li("Reselect the variables you want, then run multivariable, Cox, or survival models."),
            tags$li("Use confounder, interaction, model comparison, diagnostics, or mediation tools when needed."),
            tags$li("Use the Code tabs to copy the exact R commands into RStudio for reproducible saving and editing.")
          ),
          p("Use the Close app button in the bottom-right corner to stop the Shiny session cleanly."),
          h3("Dependency Philosophy"),
          p("The package keeps core statistical dependencies in Imports. App packages are loaded only when this app is launched, which keeps command-line gtregression light and CRAN-friendly."),
          h3("Important Notes"),
          tags$ul(
            tags$li("Use reference categories when you want binary variables to align cleanly across descriptive, regression, and forest outputs."),
            tags$li("For crowded forest plots, copy the forest code into RStudio and increase save_forest() width/height, x limits, or CI column width."),
            tags$li("If a browser preview is cramped, the saved output may still be fine when exported on a wider canvas."),
            tags$li("Mediation output is a causal aid only when the assumptions are supported by design, temporal order, and subject-matter knowledge.")
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  rv <- reactiveValues(
    data = NULL, data_name = NULL, data_code = "",
    desc = NULL, uni = NULL, multi = NULL, strat = NULL,
    cox = NULL, survreg = NULL,
    cox_exposure = NULL, cox_multi = NULL,
    survreg_exposure = NULL, survreg_multi = NULL,
    km = NULL, mediation = NULL,
    plotreg = NULL, forest = NULL, fitplot = NULL, merged = NULL,
    advanced = NULL, advanced_text = "", advanced_code = "",
    advanced_title = "Choose a tool to begin",
    advanced_guidance = "The app will explain how to read the selected result here.",
    compare_count = 2L,
    last_message = "Load data or choose a quick-start template."
  )

  observeEvent(input$close_app, {
    stopApp()
  })

  data_prep <- mod_data_prep_server("data_prep", reactive(rv$data))

  analysis_data <- reactive({
    data_prep$result()
  })

  require_analysis_data <- function() {
    data <- analysis_data()
    if (is.null(data)) {
      showNotification(
        "Open Data Prep and choose Use original data or Use prepared data before analysis.",
        type = "warning",
        duration = 7
      )
      return(NULL)
    }
    data
  }

  run_safely <- function(action, expr, on_error = NULL) {
    tryCatch(
      eval.parent(substitute(expr)),
      shiny.silent.error = function(e) NULL,
      error = function(e) {
        if (is.function(on_error)) on_error()
        message <- conditionMessage(e)
        rv$last_message <- paste0(action, " could not be completed. Review the inputs and try again.")
        showNotification(
          paste0(action, " could not be completed: ", message),
          type = "error",
          duration = 10
        )
        NULL
      }
    )
  }

  clear_advanced_error <- function() {
    rv$advanced <- NULL
    rv$advanced_text <- ""
    rv$advanced_guidance <- "Correct the highlighted choices and run the tool again. The app is still ready to use."
  }

  select_candidates <- function(exclude = character(0)) {
    vars <- names(analysis_data() %||% data.frame())
    setdiff(vars, exclude[nzchar(exclude)])
  }

  wire_select_buttons <- function(id, selected) {
    observeEvent(input[[paste0(id, "_all")]], {
      updateCheckboxGroupInput(session, id, selected = selected())
    }, ignoreInit = TRUE)

    observeEvent(input[[paste0(id, "_clear")]], {
      updateCheckboxGroupInput(session, id, selected = character(0))
    }, ignoreInit = TRUE)
  }

  wire_select_buttons("desc_exposures", reactive({
    select_candidates(input$desc_by %||% character(0))
  }))
  wire_select_buttons("reg_exposures", reactive({
    select_candidates(c(input$reg_outcome, input$reg_adjust %||% character(0)))
  }))
  wire_select_buttons("reg_adjust", reactive({
    select_candidates(c(input$reg_outcome, input$reg_exposures %||% character(0)))
  }))
  wire_select_buttons("surv_exposures", reactive({
    select_candidates(c(
      input$surv_time,
      input$surv_event,
      input$surv_stratifier,
      input$surv_adjust %||% character(0)
    ))
  }))
  wire_select_buttons("surv_adjust", reactive({
    select_candidates(c(
      input$surv_time,
      input$surv_event,
      input$surv_stratifier,
      input$surv_exposures %||% character(0)
    ))
  }))
  wire_select_buttons("adv_exposures", reactive({
    select_candidates(c(
      input$adv_outcome,
      input$adv_time,
      input$adv_event,
      input$adv_covariates %||% character(0)
    ))
  }))
  wire_select_buttons("adv_covariates", reactive({
    select_candidates(c(
      input$adv_outcome,
      input$adv_time,
      input$adv_event,
      input$adv_exposures %||% character(0)
    ))
  }))
  wire_select_buttons("med_covariates", reactive({
    select_candidates(c(input$med_exposure, input$med_mediator, input$med_outcome))
  }))

  output$workflow_guide <- renderUI({
    steps <- c(
      Data = !is.null(rv$data),
      `Data Prep` = !is.null(analysis_data()),
      Describe = !is.null(rv$desc),
      Model = !is.null(rv$uni) || !is.null(rv$multi) || !is.null(rv$strat) ||
        !is.null(rv$cox) || !is.null(rv$survreg) || !is.null(rv$mediation),
      Visualise = !is.null(rv$km) || !is.null(rv$plotreg) ||
        !is.null(rv$forest) || !is.null(rv$fitplot),
      Export = !is.null(rv$merged)
    )
    first_pending <- if (any(!steps)) names(steps)[which(!steps)[1]] else "Export"
    div(
      class = "gtx-workflow-bar",
      div(
        class = "gtx-workflow-wrap",
        lapply(names(steps), function(nm) {
          cls <- if (isTRUE(steps[[nm]])) "done" else if (identical(nm, first_pending)) "current" else ""
          span(class = paste("gtx-workflow-step-pill", cls), nm)
        }),
        span(class = "gtx-workflow-next", strong("Suggested action: "), rv$last_message)
      )
    )
  })

  output$analysis_status <- renderUI({
    data <- analysis_data()
    source <- if (is.null(data)) {
      "Not selected"
    } else if (isTRUE(data_prep$using_prepared())) {
      "Prepared data"
    } else {
      "Original data"
    }
    outcome <- input$reg_outcome %||% ""
    model <- input$reg_approach %||% ""
    div(
      class = "gtx-analysis-strip",
      div(
        class = "gtx-analysis-strip-inner",
        span(class = "gtx-analysis-item", strong("Dataset: "), rv$data_name %||% "None"),
        span(class = "gtx-analysis-item", strong("Analysis source: "), source),
        if (!is.null(data)) span(class = "gtx-analysis-item", strong("Size: "), paste0(nrow(data), " rows x ", ncol(data), " columns")),
        if (gtx_has_text(outcome)) span(class = "gtx-analysis-item", strong("Outcome: "), outcome),
        if (gtx_has_text(model)) span(class = "gtx-analysis-item", strong("Approach: "), model)
      )
    )
  })

  output$reg_preflight <- renderUI({
    data <- analysis_data()
    exposures <- input$reg_exposures %||% character(0)
    if (is.null(data) || !gtx_has_text(input$reg_outcome) || !length(exposures)) {
      return(div(class = "gtx-preflight", strong("Before running: "), "choose an outcome and at least one exposure."))
    }
    adjustment <- input$reg_adjust %||% character(0)
    references <- selected_references()
    reference_text <- if (length(references)) {
      paste(paste(names(references), unlist(references), sep = " = "), collapse = "; ")
    } else {
      "No categorical predictor baselines selected"
    }
    div(
      class = "gtx-preflight",
      strong("Ready to run: "),
      paste0(
        input$reg_approach, " model; outcome = ", input$reg_outcome,
        "; ", length(exposures), " exposure", if (length(exposures) == 1) "" else "s",
        "; ", length(adjustment), " adjustment variable", if (length(adjustment) == 1) "" else "s",
        "; N = ", nrow(data), "."
      ),
      tags$br(),
      span("Reference categories: ", reference_text, ".")
    )
  })

  output$surv_preflight <- renderUI({
    data <- analysis_data()
    exposures <- input$surv_exposures %||% character(0)
    ready <- !is.null(data) && gtx_has_text(input$surv_time) &&
      gtx_has_text(input$surv_event) && length(exposures)
    if (!ready) {
      return(div(class = "gtx-preflight", strong("Before running: "), "choose time, event, and at least one exposure."))
    }
    adjustment <- input$surv_adjust %||% character(0)
    mode <- if (isTRUE(input$surv_multivariable)) "single multivariable model" else "separate exposure models"
    model_label <- if (identical(input$surv_mode, "survreg")) {
      paste0("parametric survival (", input$surv_dist %||% "weibull", ")")
    } else {
      "Cox regression"
    }
    div(
      class = "gtx-preflight",
      strong("Ready to run: "),
      paste0(
        model_label, "; time = ", input$surv_time, "; event = ", input$surv_event,
        "; ", length(exposures), " exposure", if (length(exposures) == 1) "" else "s",
        "; ", length(adjustment), " adjustment variable", if (length(adjustment) == 1) "" else "s",
        "; ", mode, "; N = ", nrow(data), "."
      )
    )
  })

  output$export_size_hint <- renderUI({
    preset <- input$export_preset %||% "standard"
    plot_dims <- gtx_export_size(preset, is_forest = FALSE)
    forest_dims <- gtx_export_size(preset, is_forest = TRUE)
    gtx_inline_help(
      paste0(
        "Downloads use approximately ",
        plot_dims$width, " x ", plot_dims$height,
        " inches for standard plots and ",
        forest_dims$width, " x ", forest_dims$height,
        " inches for forest plots."
      )
    )
  })

  load_template <- function(template, data, name, updates = list()) {
    rv$data <- data
    rv$data_name <- name
    rv$data_code <- gtx_template_code(template)
    rv$last_message <- "Open Data Prep and choose whether analyses use original or prepared data."
    showNotification(paste(name, "template loaded. Choose an analysis dataset in Data Prep."), type = "message")
    session$onFlushed(function() {
      for (id in names(updates)) {
        value <- updates[[id]]
        if (id %in% c("desc_exposures", "reg_exposures", "reg_adjust", "surv_exposures", "surv_adjust", "adv_exposures", "adv_covariates", "med_covariates")) {
          updateCheckboxGroupInput(session, id, selected = value)
        } else {
          updateSelectInput(session, id, selected = value)
        }
      }
    }, once = TRUE)
  }

  observeEvent(input$template_birthwt, {
    vars <- c("age", "lwt", "race", "smoke", "ht", "ui", "ptl_cat", "ftv_cat")
    load_template(
      "birthwt",
      gtx_template_birthwt(),
      "Birthweight regression",
      list(
        desc_exposures = vars,
        desc_by = "low",
        reg_outcome = "low",
        reg_exposures = vars,
        reg_adjust = c("age", "lwt", "race", "ptl_cat", "ftv_cat"),
        plot_source = "uni"
      )
    )
  })

  observeEvent(input$template_lung, {
    vars <- c("age", "karno", "trt", "celltype", "prior")
    load_template(
      "lung",
      gtx_template_lung(),
      "Lung survival",
      list(
        desc_exposures = vars,
        desc_by = "trt",
        surv_time = "time",
        surv_event = "status",
        surv_exposures = vars,
        surv_adjust = c("age", "karno"),
        km_by = "trt",
        adv_time = "time",
        adv_event = "status",
        adv_exposures = vars,
        adv_covariates = c("age", "karno")
      )
    )
  })

  observeEvent(input$template_diabetes, {
    covars <- c("age", "blood_pressure", "pregnancies", "diabetes_pedigree")
    load_template(
      "diabetes",
      gtx_dataset("data_diabetes_mediation"),
      "Diabetes mediation",
      list(
        med_exposure = "obesity",
        med_mediator = "glucose",
        med_outcome = "diabetes",
        med_covariates = covars,
        desc_exposures = c("obesity", "glucose", "diabetes", covars),
        desc_by = "diabetes"
      )
    )
  })

  observeEvent(input$template_endometrial, {
    df <- gtx_dataset("data_endometrial")
    load_template(
      "endometrial",
      df,
      "Endometrial Firth logistic",
      list(
        reg_outcome = names(df)[1],
        reg_exposures = setdiff(names(df), names(df)[1]),
        reg_approach = "firth"
      )
    )
  })

  observeEvent(input$load_dataset, {
    req(gtx_has_text(input$dataset_name))
    rv$data <- gtx_dataset(input$dataset_name)
    rv$data_name <- input$dataset_name
    rv$data_code <- paste0('data("', input$dataset_name, '", package = "gtregression")\ndf <- ', input$dataset_name)
    rv$last_message <- "Open Data Prep and choose whether analyses use original or prepared data."
    showNotification("Dataset loaded. Choose an analysis dataset in Data Prep before modelling.", type = "message")
  })

  observeEvent(input$load_upload, {
    req(input$data_file)
    path <- input$data_file$datapath
    ext <- tolower(tools::file_ext(input$data_file$name))
    if (ext == "csv") {
      rv$data <- utils::read.csv(path, header = input$csv_header, sep = input$csv_sep, check.names = FALSE)
      rv$data_code <- sprintf('df <- read.csv("path/to/file.csv", header = %s, sep = "%s", check.names = FALSE)', input$csv_header, input$csv_sep)
    } else if (ext %in% c("xls", "xlsx")) {
      if (!requireNamespace("readxl", quietly = TRUE)) {
        showNotification("Install readxl to import Excel files.", type = "error")
        return(NULL)
      }
      sheet <- input$xlsx_sheet
      sheet <- if (gtx_has_text(sheet)) sheet else 1
      rv$data <- readxl::read_excel(path, sheet = sheet) |> as.data.frame()
      rv$data_code <- sprintf('df <- readxl::read_excel("path/to/file.xlsx", sheet = %s)', shQuote(as.character(sheet)))
    } else if (ext == "rds") {
      imported <- readRDS(path)
      if (!is.data.frame(imported)) {
        showNotification("The RDS file must contain a data.frame or tibble.", type = "error")
        return(NULL)
      }
      rv$data <- as.data.frame(imported)
      rv$data_code <- 'df <- readRDS("path/to/file.rds")'
    } else if (ext %in% c("dta", "sav", "zsav", "por", "sas7bdat", "xpt")) {
      if (!requireNamespace("haven", quietly = TRUE)) {
        showNotification("Install haven to import Stata, SPSS, or SAS files.", type = "error")
        return(NULL)
      }
      reader <- switch(
        ext,
        dta = "read_dta",
        sav = "read_sav",
        zsav = "read_sav",
        por = "read_por",
        sas7bdat = "read_sas",
        xpt = "read_xpt"
      )
      rv$data <- as.data.frame(getExportedValue("haven", reader)(path))
      rv$data_code <- sprintf('df <- haven::%s("path/to/file.%s")', reader, ext)
    } else {
      showNotification("Unsupported file type. Use CSV, Excel, RDS, Stata, SPSS, or SAS.", type = "error")
      return(NULL)
    }
    rv$data_name <- input$data_file$name
    rv$last_message <- "Open Data Prep and choose whether analyses use original or prepared data."
    showNotification("Uploaded data loaded. Choose an analysis dataset in Data Prep before modelling.", type = "message")
  })

  output$data_preview <- gtx_render_data(function() {
    req(rv$data)
    utils::head(rv$data, 100)
  }, page_length = 10)

  output$data_summary <- renderPrint({
    req(rv$data)
    cat("Dataset:", rv$data_name %||% "data", "\n")
    cat("Rows:", nrow(rv$data), "\n")
    cat("Columns:", ncol(rv$data), "\n\n")
    print(utils::str(rv$data))
  })

  output$data_code <- renderText(rv$data_code)

  output$desc_inputs <- renderUI({
    req(analysis_data())
    vars <- names(analysis_data())
    tagList(
      checkboxGroupInput("desc_exposures", "Variables to summarise", choices = vars),
      gtx_select_buttons("desc_exposures"),
      selectInput("desc_by", "Group by", choices = c("None" = "", vars))
    )
  })

  output$reg_inputs <- renderUI({
    req(analysis_data())
    vars <- names(analysis_data())
    tagList(
      selectInput("reg_outcome", "Outcome", choices = c("", vars)),
      checkboxGroupInput("reg_exposures", "Exposures", choices = vars),
      gtx_select_buttons("reg_exposures"),
      checkboxGroupInput("reg_adjust", "Adjust for", choices = vars),
      gtx_select_buttons("reg_adjust"),
      textInput("reg_interaction", "Interaction term", placeholder = "Optional: smoke*race")
    )
  })

  reference_candidates <- reactive({
    req(analysis_data())
    gtx_reference_candidates(
      analysis_data(),
      unique(c(input$reg_exposures %||% character(0), input$reg_adjust %||% character(0))),
      outcome = input$reg_outcome
    )
  })

  output$reg_reference_inputs <- renderUI({
    candidates <- reference_candidates()
    if (!length(candidates)) return(NULL)
    controls <- Map(function(variable, levels, index) {
      selectInput(
        paste0("reg_reference_", index),
        paste0(variable, " reference category"),
        choices = levels,
        selected = levels[[1]]
      )
    }, names(candidates), candidates, seq_along(candidates))
    tagList(
      tags$hr(),
      h4("Reference categories"),
      div(class = "gtx-help", "The selected category is the baseline. Every reported category is interpreted relative to this baseline."),
      controls
    )
  })

  selected_references <- reactive({
    candidates <- reference_candidates()
    if (!length(candidates)) return(list())
    values <- lapply(seq_along(candidates), function(index) input[[paste0("reg_reference_", index)]] %||% candidates[[index]][[1]])
    stats::setNames(values, names(candidates))
  })

  regression_data <- reactive({
    req(analysis_data())
    gtx_relevel_predictors(analysis_data(), selected_references())
  })

  regression_code_prefix <- reactive({
    candidates <- reference_candidates()
    references <- selected_references()
    lines <- c(
      analysis_code_prefix(),
      gtx_reference_code(references, candidates, data_name = "analysis_data")
    )
    paste(lines, collapse = "\n")
  })

  analysis_code_prefix <- reactive({
    source_label <- if (isTRUE(data_prep$using_prepared())) "prepared" else "original"
    lines <- c(
      paste0("# The app was set to use the ", source_label, " dataset."),
      "analysis_data <- df"
    )
    if (isTRUE(data_prep$using_prepared()) && isTRUE(data_prep$changed())) {
      prep <- gsub("\\bdata\\b", "analysis_data", data_prep$code(), perl = TRUE)
      lines <- c(lines, "# Reproduce the preparation choices made in the app.", prep)
    }
    paste(lines, collapse = "\n")
  })

  output$reg_strata_inputs <- renderUI({
    req(analysis_data())
    selectInput("reg_stratifier", "Stratifier", choices = c("None" = "", names(analysis_data())))
  })

  output$surv_inputs <- renderUI({
    req(analysis_data())
    vars <- names(analysis_data())
    tagList(
      selectInput("surv_time", "Time", choices = c("", vars)),
      selectInput("surv_event", "Event", choices = c("", vars)),
      checkboxGroupInput("surv_exposures", "Exposures", choices = vars),
      gtx_select_buttons("surv_exposures"),
      checkboxGroupInput("surv_adjust", "Adjust for", choices = vars),
      gtx_select_buttons("surv_adjust"),
      selectInput("surv_stratifier", "Stratifier", choices = c("None" = "", vars)),
      conditionalPanel(
        "input.surv_mode == 'survreg'",
        selectInput("surv_dist", "Parametric distribution", choices = c("weibull", "exponential", "lognormal", "loglogistic"), selected = "weibull")
      )
    )
  })

  output$km_group_input <- renderUI({
    req(analysis_data())
    selectInput("km_by", "Group curves by", choices = c("None" = "", names(analysis_data())))
  })

  output$surv_mode_intro <- renderUI({
    if (identical(input$surv_mode, "survreg")) {
      tagList(
        h3("Parametric Survival"),
        div(class = "gtx-help", "Estimate time ratios with a selected survival distribution. Choose the distribution below before running the model.")
      )
    } else {
      tagList(
        h3("Cox Regression"),
        div(class = "gtx-help", "Estimate crude or adjusted hazard ratios without specifying the baseline hazard distribution.")
      )
    }
  })

  output$advanced_config <- renderUI({
    req(analysis_data())
    vars <- names(analysis_data())
    tool <- input$advanced_tool %||% "selection"
    model_choices <- names(Filter(Negate(is.null), list(
      "Univariate regression" = rv$uni,
      "Multivariable regression" = rv$multi,
      "Cox exposure models" = rv$cox_exposure,
      "Cox multivariable model" = rv$cox_multi,
      "Parametric exposure models" = rv$survreg_exposure,
      "Parametric multivariable model" = rv$survreg_multi
    )))
    model_values <- unname(c(
      "Univariate regression" = "uni",
      "Multivariable regression" = "multi",
      "Cox exposure models" = "cox_exposure",
      "Cox multivariable model" = "cox_multi",
      "Parametric exposure models" = "survreg_exposure",
      "Parametric multivariable model" = "survreg_multi"
    )[model_choices])
    named_model_choices <- stats::setNames(model_values, model_choices)
    collinearity_choices <- named_model_choices[named_model_choices %in% c("multi", "cox_exposure", "cox_multi", "survreg_exposure", "survreg_multi")]
    tool_copy <- switch(
      tool,
      selection = list("Model selection", "Explore forward, backward, or bidirectional candidate-model paths. Selection is a screening aid and should be combined with clinical reasoning."),
      comparison = list("Model comparison", "Build two or more named candidate models here, then compare their samples, nesting, fit statistics, and primary effect estimate."),
      confounder = list("Confounder assessment", "Estimate how a candidate variable changes the exposure effect. Use this as a transparent screening aid alongside a prespecified DAG."),
      interaction = list("Interaction assessment", "Test whether the exposure association differs across levels of a possible effect modifier."),
      convergence = list("Convergence check", "Check whether regression models were fitted reliably before interpreting their estimates."),
      collinearity = list("Collinearity check", "Inspect a fitted multivariable model for predictors that contain overlapping information.")
    )

    approach_choices <- if (tool == "convergence") {
      gtx_approaches
    } else {
      c(gtx_approaches, "Cox" = "cox", "Surv Reg" = "survreg")
    }

    model_roles <- function(include_covariates = FALSE, include_exposures = FALSE) {
      current_approach <- input$adv_approach %||% "logit"
      survival_advanced <- isTRUE(current_approach %in% c("cox", "survreg"))
      tagList(
        selectInput("adv_approach", "Regression approach", choices = approach_choices, selected = current_approach),
        if (survival_advanced) tagList(
          selectInput("adv_time", "Follow-up time", choices = c("Choose a variable" = "", vars), selected = input$adv_time %||% ""),
          selectInput("adv_event", "Event status", choices = c("Choose a variable" = "", vars), selected = input$adv_event %||% "")
        ) else selectInput("adv_outcome", "Outcome", choices = c("Choose a variable" = "", vars), selected = input$adv_outcome %||% ""),
        if (identical(current_approach, "survreg"))
          selectInput("adv_distribution", "Survival distribution", choices = c("weibull", "exponential", "lognormal", "loglogistic"), selected = input$adv_distribution %||% "weibull"),
        if (include_exposures) tagList(
          checkboxGroupInput("adv_exposures", "Candidate predictors", choices = vars, selected = input$adv_exposures %||% character(0)),
          gtx_select_buttons("adv_exposures")
        ),
        if (include_covariates) tagList(
          checkboxGroupInput("adv_covariates", "Adjustment variables", choices = vars, selected = input$adv_covariates %||% character(0)),
          gtx_select_buttons("adv_covariates")
        )
      )
    }

    comparison_roles <- function() {
      current_approach <- input$compare_approach %||% "logit"
      survival_comparison <- current_approach %in% c("cox", "survreg")
      excluded <- if (survival_comparison) {
        c(input$compare_time, input$compare_event)
      } else {
        input$compare_outcome
      }
      predictors <- setdiff(vars, excluded[nzchar(excluded)])
      cards <- lapply(seq_len(rv$compare_count), function(i) {
        exposure_id <- paste0("compare_exposures_", i)
        adjust_id <- paste0("compare_adjust_", i)
        int_a_id <- paste0("compare_interaction_a_", i)
        int_b_id <- paste0("compare_interaction_b_", i)
        selected_exposures <- intersect(input[[exposure_id]] %||% character(0), predictors)
        selected_adjust <- intersect(input[[adjust_id]] %||% character(0), predictors)
        interaction_choices <- unique(c(selected_exposures, selected_adjust))
        div(
          class = "gtx-model-card",
          h4(paste("Candidate model", i)),
          textInput(
            paste0("compare_name_", i),
            "Model name",
            value = input[[paste0("compare_name_", i)]] %||% paste("Model", i)
          ),
          checkboxGroupInput(exposure_id, "Reported exposures", choices = predictors, selected = selected_exposures),
          checkboxGroupInput(adjust_id, "Adjustment variables", choices = predictors, selected = selected_adjust),
          gtx_inline_help("The app combines reported exposures and adjustment variables into one fitted candidate model."),
          selectInput(int_a_id, "Interaction term 1 (optional)", choices = c("None" = "", interaction_choices), selected = input[[int_a_id]] %||% ""),
          selectInput(int_b_id, "Interaction term 2 (optional)", choices = c("None" = "", interaction_choices), selected = input[[int_b_id]] %||% "")
        )
      })
      tagList(
        selectInput("compare_approach", "Regression approach", choices = c(gtx_approaches, "Cox" = "cox", "Surv Reg" = "survreg"), selected = current_approach),
        if (survival_comparison) tagList(
          selectInput("compare_time", "Follow-up time", choices = c("Choose a variable" = "", vars), selected = input$compare_time %||% ""),
          selectInput("compare_event", "Event status", choices = c("Choose a variable" = "", vars), selected = input$compare_event %||% "")
        ) else selectInput("compare_outcome", "Outcome", choices = c("Choose a variable" = "", vars), selected = input$compare_outcome %||% ""),
        if (identical(current_approach, "survreg"))
          selectInput("compare_distribution", "Survival distribution", choices = c("weibull", "exponential", "lognormal", "loglogistic"), selected = input$compare_distribution %||% "weibull"),
        selectInput("adv_primary_exposure", "Primary exposure to track (optional)", choices = c("Not specified" = "", predictors), selected = input$adv_primary_exposure %||% ""),
        div(
          class = "gtx-inline-actions",
          actionButton("compare_add_model", icon("plus"), " Add model"),
          actionButton("compare_remove_model", icon("minus"), " Remove last", disabled = if (rv$compare_count <= 2L) "disabled" else NULL)
        ),
        cards
      )
    }

    controls <- switch(
      tool,
      selection = tagList(
        div(class = "gtx-section-label", "Build a candidate model path"),
        gtx_inline_help("Choose one outcome and the predictors that may enter the model. This tool fits the candidate sequence for you; it does not require a previously fitted model."),
        model_roles(include_exposures = TRUE),
        selectInput("select_direction", "Selection direction", choices = c("Forward" = "forward", "Backward" = "backward", "Both directions" = "both"), selected = input$select_direction %||% "both"),
        gtx_inline_help("The output shows the route taken and marks the model preferred by AIC."),
        actionButton("run_select_models", icon("play"), " Run model selection", class = "btn-primary btn-block")
      ),
      comparison = tagList(
        div(class = "gtx-section-label", "Build and compare candidate models"),
        gtx_inline_help("Choose the common outcome and approach once. Give each model a meaningful name, then choose its exposures, adjustment variables, and optional interaction."),
        comparison_roles(),
        gtx_inline_help("Interactions can use only variables included in that candidate. The comparison table flags different analysis samples and non-nested model pairs."),
        actionButton("run_compare", icon("table-columns"), " Fit and compare models", class = "btn-primary btn-block")
      ),
      confounder = tagList(
        model_roles(),
        selectInput("conf_exposure", "Exposure of interest", choices = c("Choose a variable" = "", vars)),
        selectInput("conf_candidate", "Potential confounder", choices = c("Choose a variable" = "", vars)),
        gtx_inline_help("A change-in-estimate result cannot establish causality. Confirm confounding decisions with a DAG and subject-matter knowledge."),
        actionButton("run_confounder", icon("magnifying-glass-chart"), " Assess confounding", class = "btn-primary btn-block")
      ),
      interaction = tagList(
        model_roles(include_covariates = TRUE),
        selectInput("conf_exposure", "Exposure of interest", choices = c("Choose a variable" = "", vars)),
        selectInput("conf_candidate", "Possible effect modifier", choices = c("Choose a variable" = "", vars)),
        gtx_inline_help("A small interaction p-value suggests that the exposure effect may differ between groups. Interpret the stratum-specific estimates as well."),
        actionButton("run_interaction", icon("code-branch"), " Test interaction", class = "btn-primary btn-block")
      ),
      convergence = tagList(
        model_roles(include_exposures = TRUE),
        gtx_inline_help("Convergence checks apply to the standard regression approaches shown here. Resolve warnings before reporting estimates."),
        actionButton("run_convergence", icon("circle-check"), " Check convergence", class = "btn-primary btn-block")
      ),
      collinearity = tagList(
        if (!length(collinearity_choices)) div(class = "gtx-note", "Fit a multivariable, Cox, or parametric survival model first."),
        selectInput("adv_collinearity_model", "Fitted model", choices = collinearity_choices),
        gtx_inline_help("Use the fitted model that contains all predictors you want to assess. Collinearity is a property of predictors fitted together."),
        if (length(collinearity_choices)) {
          actionButton("run_collinearity", icon("arrows-left-right-to-line"), " Check collinearity", class = "btn-primary btn-block")
        } else {
          actionButton("run_collinearity", icon("arrows-left-right-to-line"), " Check collinearity", class = "btn-primary btn-block", disabled = "disabled")
        }
      )
    )

    tagList(
      h3(tool_copy[[1]]),
      div(class = "gtx-help", tool_copy[[2]]),
      controls
    )
  })

  output$med_inputs <- renderUI({
    req(analysis_data())
    vars <- names(analysis_data())
    tagList(
      selectInput("med_exposure", "Exposure", choices = c("", vars)),
      selectInput("med_mediator", "Mediator", choices = c("", vars)),
      selectInput("med_outcome", "Outcome", choices = c("", vars)),
      checkboxGroupInput("med_covariates", "Covariates", choices = vars),
      gtx_select_buttons("med_covariates")
    )
  })

  observeEvent(input$run_desc, {
    data <- require_analysis_data()
    req(data, input$desc_exposures)
    rv$desc <- tryCatch(
      gtx_call(
        "descriptive_table",
        data = data,
        exposures = input$desc_exposures,
        by = if (gtx_has_text(input$desc_by)) input$desc_by else NULL,
        percent = input$desc_percent,
        show_overall = input$desc_overall,
        show_missing = input$desc_missing,
        show_dichotomous = input$desc_dich,
        statistic = gtx_parse_named_stats(input$desc_statistic),
        format = "flextable"
      ),
      error = function(e) {
        showNotification(conditionMessage(e), type = "error")
        NULL
      }
    )
    if (!is.null(rv$desc)) {
      rv$last_message <- "Run univariable regression, survival analysis, or export the descriptive table."
      showNotification("Descriptive table ready. Run univariable models when you are ready.", type = "message")
    }
  })

  output$desc_table <- gtx_render_table(function() rv$desc)
  gtx_table_downloads(output, "desc", reactive(rv$desc))
  output$desc_code <- renderText({
    req(input$desc_exposures)
    paste(analysis_code_prefix(), gtx_code_assign(
      "desc_result",
      "descriptive_table",
      list(
        data = "analysis_data",
        exposures = gtx_vec_code(input$desc_exposures),
        by = gtx_null_code(input$desc_by),
        percent = shQuote(input$desc_percent),
        show_overall = shQuote(input$desc_overall),
        show_missing = shQuote(input$desc_missing),
        show_dichotomous = shQuote(input$desc_dich),
        statistic = gtx_statistic_code(input$desc_statistic),
        format = shQuote("flextable")
      )
    ), sep = "\n\n")
  })

  run_regression <- function(kind) {
    data <- require_analysis_data()
    req(data, input$reg_outcome, input$reg_exposures)
    gtx_validate_roles(
      outcome = input$reg_outcome,
      exposures = input$reg_exposures,
      adjust_for = if (kind == "multi") input$reg_adjust %||% character(0) else character(0),
      context = if (kind == "uni") "univariable regression" else "multivariable regression"
    )
    fun <- if (kind == "uni") "uni_reg" else "multi_reg"
    gtx_call(
      fun,
      data = regression_data(),
      outcome = input$reg_outcome,
      exposures = input$reg_exposures,
      adjust_for = if (kind == "multi" && length(input$reg_adjust)) input$reg_adjust else NULL,
      approach = input$reg_approach,
      interaction = if (gtx_has_text(input$reg_interaction)) input$reg_interaction else NULL,
      show_ref = input$reg_show_ref,
      model_stats = input$reg_model_stats,
      format = "flextable"
    )
  }

  observeEvent(input$run_uni, {
    rv$uni <- tryCatch(run_regression("uni"), error = function(e) { showNotification(conditionMessage(e), type = "error"); NULL })
    if (!is.null(rv$uni)) {
      rv$last_message <- "Choose final variables, then run the multivariable model or visualise crude estimates."
      showNotification("Univariable table ready. Choose final variables for multivariable analysis.", type = "message")
    }
  })

  observeEvent(input$run_multi, {
    rv$multi <- tryCatch(run_regression("multi"), error = function(e) { showNotification(conditionMessage(e), type = "error"); NULL })
    if (!is.null(rv$multi)) {
      rv$last_message <- "Merge tables, visualise estimates, check diagnostics, or export outputs."
      showNotification("Multivariable table ready. Merge tables or visualise results.", type = "message")
    }
  })

  observeEvent(input$run_strat, {
    data <- require_analysis_data()
    req(data, input$reg_outcome, input$reg_exposures, gtx_has_text(input$reg_stratifier))
    gtx_validate_roles(
      outcome = input$reg_outcome,
      exposures = input$reg_exposures,
      adjust_for = if (isTRUE(input$reg_strata_multi)) input$reg_adjust %||% character(0) else character(0),
      stratifier = input$reg_stratifier,
      context = "stratified regression"
    )
    fun <- if (isTRUE(input$reg_strata_multi)) "stratified_multi_reg" else "stratified_uni_reg"
    rv$strat <- tryCatch(
      gtx_call(
        fun,
        data = regression_data(),
        outcome = input$reg_outcome,
        exposures = input$reg_exposures,
        adjust_for = if (isTRUE(input$reg_strata_multi) && length(input$reg_adjust)) input$reg_adjust else NULL,
        stratifier = input$reg_stratifier,
        approach = input$reg_approach,
        show_ref = input$reg_show_ref,
        format = "flextable"
      ),
      error = function(e) { showNotification(conditionMessage(e), type = "error"); NULL }
    )
    if (!is.null(rv$strat)) {
      rv$last_message <- "Review strata-specific estimates, then export the table or use forest_reg()."
      showNotification("Stratified table ready. Review strata-specific estimates.", type = "message")
    }
  })

  output$uni_table <- gtx_render_table(function() rv$uni)
  output$multi_table <- gtx_render_table(function() rv$multi)
  output$strat_table <- gtx_render_table(function() rv$strat)
  gtx_table_downloads(output, "uni", reactive(rv$uni))
  gtx_table_downloads(output, "multi", reactive(rv$multi))
  gtx_table_downloads(output, "strat", reactive(rv$strat))

  output$reg_stats <- gtx_render_data(function() {
    stats <- lapply(
      list(rv$uni, rv$multi, rv$cox_exposure, rv$cox_multi, rv$survreg_exposure, rv$survreg_multi),
      function(x) if (is.null(x)) NULL else x$model_stats
    )
    stats <- Filter(Negate(is.null), stats)
    req(length(stats))
    dplyr::bind_rows(stats)
  })

  output$uni_code <- renderText({
    req(input$reg_outcome, input$reg_exposures)
    paste(regression_code_prefix(), gtx_code_assign(
      "uni_result",
      "uni_reg",
      list(
        data = "analysis_data",
        outcome = gtx_null_code(input$reg_outcome),
        exposures = gtx_vec_code(input$reg_exposures),
        approach = shQuote(input$reg_approach),
        interaction = if (gtx_has_text(input$reg_interaction)) shQuote(input$reg_interaction) else NULL,
        show_ref = gtx_bool_code(input$reg_show_ref),
        model_stats = gtx_bool_code(input$reg_model_stats),
        format = shQuote("flextable")
      )
    ), sep = "\n\n")
  })
  output$multi_code <- renderText({
    req(input$reg_outcome, input$reg_exposures)
    paste(regression_code_prefix(), gtx_code_assign(
      "multi_result",
      "multi_reg",
      list(
        data = "analysis_data",
        outcome = gtx_null_code(input$reg_outcome),
        exposures = gtx_vec_code(input$reg_exposures),
        adjust_for = gtx_nonempty_vec_code(input$reg_adjust %||% character(0)),
        approach = shQuote(input$reg_approach),
        interaction = if (gtx_has_text(input$reg_interaction)) shQuote(input$reg_interaction) else NULL,
        show_ref = gtx_bool_code(input$reg_show_ref),
        model_stats = gtx_bool_code(input$reg_model_stats),
        format = shQuote("flextable")
      )
    ), sep = "\n\n")
  })
  output$strat_code <- renderText({
    req(input$reg_outcome, input$reg_exposures)
    fun <- if (isTRUE(input$reg_strata_multi)) "stratified_multi_reg" else "stratified_uni_reg"
    paste(regression_code_prefix(), gtx_code_assign(
      "stratified_result",
      fun,
      list(
        data = "analysis_data",
        outcome = gtx_null_code(input$reg_outcome),
        exposures = gtx_vec_code(input$reg_exposures),
        adjust_for = if (isTRUE(input$reg_strata_multi)) gtx_nonempty_vec_code(input$reg_adjust %||% character(0)) else NULL,
        stratifier = gtx_null_code(input$reg_stratifier),
        approach = shQuote(input$reg_approach),
        show_ref = gtx_bool_code(input$reg_show_ref),
        format = shQuote("flextable")
      )
    ), sep = "\n\n")
  })

  parse_limits <- function(x) {
    if (!gtx_has_text(x)) return(NULL)
    vals <- suppressWarnings(as.numeric(strsplit(x, ",")[[1]]))
    if (length(vals) == 2 && all(!is.na(vals))) vals else NULL
  }

  observeEvent(input$run_cox, {
    data <- require_analysis_data()
    req(data, input$surv_time, input$surv_event, input$surv_exposures)
    gtx_validate_roles(
      time = input$surv_time,
      event = input$surv_event,
      exposures = input$surv_exposures,
      adjust_for = input$surv_adjust %||% character(0),
      stratifier = if (gtx_has_text(input$surv_stratifier)) input$surv_stratifier else NULL,
      context = "Cox regression"
    )
    result <- tryCatch(
      gtx_call(
        "cox_reg",
        data = data,
        time = input$surv_time,
        event = input$surv_event,
        exposures = input$surv_exposures,
        adjust_for = input$surv_adjust,
        multivariable = input$surv_multivariable,
        multivariate = input$surv_multivariable,
        stratifier = if (gtx_has_text(input$surv_stratifier)) input$surv_stratifier else NULL,
        show_ref = input$surv_show_ref,
        format = "flextable"
      ),
      error = function(e) { showNotification(conditionMessage(e), type = "error"); NULL }
    )
    if (!is.null(result)) {
      rv$cox <- result
      if (isTRUE(input$surv_multivariable)) {
        rv$cox_multi <- result
        updateTabsetPanel(session, "cox_result_tab", selected = "Multivariable model")
      } else {
        rv$cox_exposure <- result
        updateTabsetPanel(session, "cox_result_tab", selected = "Exposure models")
      }
      rv$last_message <- "Draw KM curves, compare candidate models, or create a forest plot."
      updateTabsetPanel(session, "surv_output_tab", selected = "Model Result")
      showNotification("Cox regression table ready. Draw KM curves or visualise estimates.", type = "message")
    }
  })

  observeEvent(input$run_survreg, {
    data <- require_analysis_data()
    req(data, input$surv_time, input$surv_event, input$surv_exposures)
    gtx_validate_roles(
      time = input$surv_time,
      event = input$surv_event,
      exposures = input$surv_exposures,
      adjust_for = input$surv_adjust %||% character(0),
      stratifier = if (gtx_has_text(input$surv_stratifier)) input$surv_stratifier else NULL,
      context = "parametric survival regression"
    )
    result <- tryCatch(
      gtx_call(
        "surv_reg",
        data = data,
        time = input$surv_time,
        event = input$surv_event,
        exposures = input$surv_exposures,
        adjust_for = input$surv_adjust,
        multivariable = input$surv_multivariable,
        multivariate = input$surv_multivariable,
        stratifier = if (gtx_has_text(input$surv_stratifier)) input$surv_stratifier else NULL,
        distribution = input$surv_dist,
        show_ref = input$surv_show_ref,
        format = "flextable"
      ),
      error = function(e) { showNotification(conditionMessage(e), type = "error"); NULL }
    )
    if (!is.null(result)) {
      rv$survreg <- result
      if (isTRUE(input$surv_multivariable)) {
        rv$survreg_multi <- result
        updateTabsetPanel(session, "survreg_result_tab", selected = "Multivariable model")
      } else {
        rv$survreg_exposure <- result
        updateTabsetPanel(session, "survreg_result_tab", selected = "Exposure models")
      }
      rv$last_message <- "Compare survival models, visualise estimates, or create a forest plot."
      updateTabsetPanel(session, "surv_output_tab", selected = "Model Result")
      showNotification("Parametric survival table ready. Compare or visualise results.", type = "message")
    }
  })

  observeEvent(input$run_km, {
    data <- require_analysis_data()
    req(data, input$surv_time, input$surv_event)
    rv$km <- tryCatch(
      gtregression::km_plot(
        data = data,
        time = input$surv_time,
        event = input$surv_event,
        by = if (gtx_has_text(input$km_by)) input$km_by else NULL,
        conf.int = input$km_conf,
        risk_table = input$km_risk,
        p_value = input$km_p,
        break_time_by = gtx_km_break_value(input$km_break),
        xlim = parse_limits(input$km_xlim),
        ylim = parse_limits(input$km_ylim),
        y_percent = input$km_y_percent,
        theme = input$km_theme,
        grid = input$km_grid
      ),
      error = function(e) { showNotification(conditionMessage(e), type = "error"); NULL }
    )
    if (!is.null(rv$km)) {
      rv$last_message <- "Tune x/y limits for publication or export the plot."
      updateTabsetPanel(session, "surv_output_tab", selected = "KM Plot")
      showNotification("KM plot ready. Tune x/y limits or export.", type = "message")
    }
  })

  output$cox_exposure_table <- gtx_render_table(function() rv$cox_exposure)
  output$cox_multi_table <- gtx_render_table(function() rv$cox_multi)
  output$survreg_exposure_table <- gtx_render_table(function() rv$survreg_exposure)
  output$survreg_multi_table <- gtx_render_table(function() rv$survreg_multi)
  output$km_plot <- renderPlot({ req(rv$km); print(rv$km) })
  gtx_table_downloads(output, "cox_exposure", reactive(rv$cox_exposure))
  gtx_table_downloads(output, "cox_multi", reactive(rv$cox_multi))
  gtx_table_downloads(output, "survreg_exposure", reactive(rv$survreg_exposure))
  gtx_table_downloads(output, "survreg_multi", reactive(rv$survreg_multi))
  gtx_plot_downloads(output, "km", reactive(rv$km))

  output$logrank_table <- gtx_render_table(function() {
    req(analysis_data(), input$surv_time, input$surv_event, gtx_has_text(input$km_by))
    gtregression::logrank_test(analysis_data(), time = input$surv_time, event = input$surv_event, by = input$km_by, format = "flextable")
  })
  output$surv_summary_table <- gtx_render_table(function() {
    req(analysis_data(), input$surv_time, input$surv_event)
    gtregression::survival_summary(analysis_data(), time = input$surv_time, event = input$surv_event, by = if (gtx_has_text(input$km_by)) input$km_by else NULL, format = "flextable")
  })
  output$surv_code <- renderText({
    ylim_code <- gtx_limits_code(parse_limits(input$km_ylim))
    xlim_code <- gtx_limits_code(parse_limits(input$km_xlim))
    result_name <- if (identical(input$surv_mode, "survreg")) {
      if (isTRUE(input$surv_multivariable)) "survreg_multi_result" else "survreg_exposure_result"
    } else {
      if (isTRUE(input$surv_multivariable)) "cox_multi_result" else "cox_exposure_result"
    }
    model_code <- if (identical(input$surv_mode, "survreg")) {
      gtx_code_assign(
        result_name,
        "surv_reg",
        list(
          data = "analysis_data",
          time = gtx_null_code(input$surv_time),
          event = gtx_null_code(input$surv_event),
          exposures = gtx_vec_code(input$surv_exposures %||% character(0)),
          adjust_for = gtx_nonempty_vec_code(input$surv_adjust %||% character(0)),
          multivariable = gtx_bool_code(input$surv_multivariable),
          stratifier = gtx_null_code(input$surv_stratifier),
          distribution = shQuote(input$surv_dist %||% "weibull"),
          show_ref = gtx_bool_code(input$surv_show_ref),
          format = shQuote("flextable")
        )
      )
    } else {
      gtx_code_assign(
        result_name,
        "cox_reg",
        list(
          data = "analysis_data",
          time = gtx_null_code(input$surv_time),
          event = gtx_null_code(input$surv_event),
          exposures = gtx_vec_code(input$surv_exposures %||% character(0)),
          adjust_for = gtx_nonempty_vec_code(input$surv_adjust %||% character(0)),
          multivariable = gtx_bool_code(input$surv_multivariable),
          stratifier = gtx_null_code(input$surv_stratifier),
          show_ref = gtx_bool_code(input$surv_show_ref),
          format = shQuote("flextable")
        )
      )
    }
    paste0(
      analysis_code_prefix(),
      "\n\n",
      model_code,
      "\n\n",
      gtx_code_assign(
        "km_result",
        "km_plot",
        list(
          data = "analysis_data",
          time = gtx_null_code(input$surv_time),
          event = gtx_null_code(input$surv_event),
          by = gtx_null_code(input$km_by),
          conf.int = gtx_bool_code(input$km_conf),
          risk_table = gtx_bool_code(input$km_risk),
          p_value = gtx_bool_code(input$km_p),
          break_time_by = if (is.null(gtx_km_break_value(input$km_break))) NULL else as.character(gtx_km_break_value(input$km_break)),
          xlim = if (identical(xlim_code, "NULL")) NULL else xlim_code,
          ylim = if (identical(ylim_code, "NULL")) NULL else ylim_code,
          y_percent = gtx_bool_code(input$km_y_percent),
          theme = shQuote(input$km_theme),
          grid = gtx_bool_code(input$km_grid)
        )
      ),
      "\n",
      "save_plot(km_result, filename = \"km_plot.png\", width = 8, height = 6)"
    )
  })

  result_for <- function(source) {
    switch(source,
           uni = rv$uni, multi = rv$multi,
           cox_exposure = rv$cox_exposure, cox_multi = rv$cox_multi,
           survreg_exposure = rv$survreg_exposure, survreg_multi = rv$survreg_multi,
           cox = rv$cox, survreg = rv$survreg, strat = rv$strat)
  }

  is_stratified_result <- function(x) {
    if (is.null(x)) return(FALSE)
    isTRUE(x$stratified) || inherits(
      x,
      c(
        "stratified_uni_reg", "stratified_multi_reg",
        "stratified_cox_reg", "stratified_surv_reg"
      )
    )
  }

  pair_can_combine <- function(first, second) {
    !is.null(first) && !is.null(second) &&
      !is_stratified_result(first) && !is_stratified_result(second)
  }

  selected_result <- reactive({
    source <- input$plot_source
    if (is.null(source)) {
      choices <- single_plot_choices()
      source <- if (length(choices)) unname(choices[1]) else NULL
    }
    result_for(source)
  })

  available_merge_tables <- reactive({
    Filter(Negate(is.null), list(
      "Descriptive" = rv$desc,
      "Crude" = rv$uni,
      "Adjusted" = rv$multi,
      "Cox exposure models" = rv$cox_exposure,
      "Cox multivariable model" = rv$cox_multi,
      "Parametric exposure models" = rv$survreg_exposure,
      "Parametric multivariable model" = rv$survreg_multi
    ))
  })

  output$merge_table_choices <- renderUI({
    tbls <- available_merge_tables()
    if (!length(tbls)) {
      return(div(class = "gtx-note", "No completed tables yet. Run descriptive or regression analyses first."))
    }
    checkboxGroupInput(
      "merge_selection",
      "Available tables",
      choices = stats::setNames(names(tbls), names(tbls)),
      selected = names(tbls)
    )
  })

  single_plot_choices <- reactive({
    choices <- c()
    if (!is.null(rv$uni)) choices["Univariate (crude)"] <- "uni"
    if (!is.null(rv$multi)) choices["Multivariable (adjusted)"] <- "multi"
    if (!is.null(rv$cox_exposure)) {
      label <- if (is_stratified_result(rv$cox_exposure)) "Cox exposure models (stratified)" else "Cox exposure models"
      choices[label] <- "cox_exposure"
    }
    if (!is.null(rv$cox_multi)) {
      label <- if (is_stratified_result(rv$cox_multi)) "Cox multivariable model (stratified)" else "Cox multivariable model"
      choices[label] <- "cox_multi"
    }
    if (!is.null(rv$survreg_exposure)) {
      label <- if (is_stratified_result(rv$survreg_exposure)) "Parametric exposure models (stratified)" else "Parametric exposure models"
      choices[label] <- "survreg_exposure"
    }
    if (!is.null(rv$survreg_multi)) {
      label <- if (is_stratified_result(rv$survreg_multi)) "Parametric multivariable model (stratified)" else "Parametric multivariable model"
      choices[label] <- "survreg_multi"
    }
    if (!is.null(rv$strat)) choices["Regression result (stratified)"] <- "strat"
    choices
  })

  output$plot_content_controls <- renderUI({
    choices <- single_plot_choices()
    combined_ready <- pair_can_combine(rv$uni, rv$multi)
    cox_combined_ready <- pair_can_combine(rv$cox_exposure, rv$cox_multi)
    survreg_combined_ready <- pair_can_combine(rv$survreg_exposure, rv$survreg_multi)
    stratified_ready <- any(vapply(unname(choices), function(source) {
      is_stratified_result(result_for(source))
    }, logical(1)))
    if (!length(choices)) {
      return(div(class = "gtx-note", "No regression results yet. Run a regression table first."))
    }
    tagList(
      radioButtons(
        "plot_layout",
        "Plot content",
        choices = c(
          "Selected result" = "single",
          if (combined_ready) c("Regression crude + adjusted" = "combined"),
          if (cox_combined_ready) c("Cox exposure + multivariable" = "cox_combined"),
          if (survreg_combined_ready) c("Parametric exposure + multivariable" = "survreg_combined")
        ),
        selected = if (identical(input$surv_mode, "cox") && cox_combined_ready) {
          "cox_combined"
        } else if (identical(input$surv_mode, "survreg") && survreg_combined_ready) {
          "survreg_combined"
        } else if (combined_ready) {
          "combined"
        } else {
          "single"
        }
      ),
      conditionalPanel(
        "input.plot_layout == 'single'",
        selectInput("plot_source", "Result", choices = choices, selected = unname(choices[1]))
      ),
      if (stratified_ready) div(
        class = "gtx-note",
        "Stratified results use one source: plot_reg() creates strata-specific panels. Crude + adjusted combination is intentionally unavailable for stratified results."
      )
    )
  })

  output$forest_content_controls <- renderUI({
    choices <- single_plot_choices()
    combined_ready <- pair_can_combine(rv$uni, rv$multi)
    cox_combined_ready <- pair_can_combine(rv$cox_exposure, rv$cox_multi)
    survreg_combined_ready <- pair_can_combine(rv$survreg_exposure, rv$survreg_multi)
    stratified_ready <- any(vapply(unname(choices), function(source) {
      is_stratified_result(result_for(source))
    }, logical(1)))
    desc_ready <- !is.null(rv$desc)
    forest_choices <- c("Selected result" = "single")
    if (combined_ready) forest_choices["Regression crude + adjusted"] <- "combined"
    if (combined_ready && desc_ready) forest_choices["Descriptive + regression crude + adjusted"] <- "all"
    if (cox_combined_ready) forest_choices["Cox exposure + multivariable"] <- "cox_combined"
    if (cox_combined_ready && desc_ready) forest_choices["Descriptive + Cox exposure + multivariable"] <- "cox_all"
    if (survreg_combined_ready) forest_choices["Parametric exposure + multivariable"] <- "survreg_combined"
    if (survreg_combined_ready && desc_ready) forest_choices["Descriptive + parametric exposure + multivariable"] <- "survreg_all"
    if (!length(choices)) {
      return(div(class = "gtx-note", "No compatible results yet. Run descriptive and regression analyses first."))
    }
    tagList(
      radioButtons(
        "forest_content",
        "Forest content",
        choices = forest_choices,
        selected = if (identical(input$surv_mode, "cox") && cox_combined_ready && desc_ready) {
          "cox_all"
        } else if (identical(input$surv_mode, "cox") && cox_combined_ready) {
          "cox_combined"
        } else if (identical(input$surv_mode, "survreg") && survreg_combined_ready && desc_ready) {
          "survreg_all"
        } else if (identical(input$surv_mode, "survreg") && survreg_combined_ready) {
          "survreg_combined"
        } else if (combined_ready && desc_ready) {
          "all"
        } else if (combined_ready) {
          "combined"
        } else {
          "single"
        }
      ),
      conditionalPanel(
        "input.forest_content == 'single'",
        selectInput("forest_source", "Result", choices = choices, selected = unname(choices[1]))
      ),
      if (stratified_ready) div(
        class = "gtx-note",
        "Stratified results use one source: forest_reg() places strata side by side and keeps the characteristic labels once. Multiple stratified objects cannot be combined."
      )
    )
  })

  output$fit_source_controls <- renderUI({
    choices <- single_plot_choices()
    if (!length(choices)) {
      return(div(class = "gtx-note", "No fitted model yet. Run a regression analysis first."))
    }
    selectInput("fit_source", "Fitted result", choices = choices, selected = unname(choices[1]))
  })

  observeEvent(input$visual_tool, {
    tab <- switch(
      input$visual_tool,
      merge = "Merged Table",
      plot = "plot_reg",
      forest = "forest_reg",
      fit = "Model fit",
      "Merged Table"
    )
    updateTabsetPanel(session, "visual_output_tab", selected = tab)
  }, ignoreInit = TRUE)

  observeEvent(input$run_plot_reg, {
    if (input$plot_layout %in% c("combined", "cox_combined", "survreg_combined")) {
      pair <- switch(
        input$plot_layout,
        combined = list(rv$uni, rv$multi),
        cox_combined = list(rv$cox_exposure, rv$cox_multi),
        survreg_combined = list(rv$survreg_exposure, rv$survreg_multi)
      )
      if (any(vapply(pair, is.null, logical(1)))) {
        showNotification("Run both the exposure models and multivariable model before creating the combined plot.", type = "warning")
        return(NULL)
      }
      rv$plotreg <- tryCatch(
        gtregression::plot_reg_combine(pair[[1]], pair[[2]], log_x = input$plot_log_x),
        error = function(e) { showNotification(conditionMessage(e), type = "error"); NULL }
      )
    } else {
      req(selected_result())
      rv$plotreg <- tryCatch(
        gtregression::plot_reg(selected_result(), log_x = input$plot_log_x),
        error = function(e) { showNotification(conditionMessage(e), type = "error"); NULL }
      )
    }
    if (!is.null(rv$plotreg)) {
      rv$last_message <- "Copy the plot code or export with a wider preset."
      showNotification("Regression plot preview ready. Use the Code tab for reproducible export.", type = "message")
    }
  })

  observeEvent(input$run_merge, {
    tbls <- available_merge_tables()
    selected <- input$merge_selection %||% character(0)
    selected <- names(tbls)[names(tbls) %in% selected]
    if (length(selected) < 2L) {
      showNotification("Run at least two tables before merging. For example: descriptive, univariable, then multivariable.", type = "message")
      return(NULL)
    }
    tbls <- tbls[selected]
    rv$merged <- tryCatch(
      do.call(gtregression::merge_tables, c(unname(tbls), list(spanners = names(tbls)))),
      error = function(e) { showNotification(conditionMessage(e), type = "error"); NULL }
    )
    if (!is.null(rv$merged)) {
      rv$last_message <- "Download DOCX/HTML/PDF or apply modify_table() in RStudio."
      showNotification("Merged table ready. Tip: use show_ref = TRUE in regression tables when you want reference rows displayed.", type = "message")
    }
  })

  observeEvent(input$run_forest, {
    rv$forest <- tryCatch({
      forest_xlim <- gtx_parse_numeric_vector(input$forest_xlim, expected = 2, name = "forest x limits")
      forest_ticks <- gtx_parse_numeric_vector(input$forest_ticks, name = "forest tick marks")
      ci_width <- input$forest_ci_width %||% 20
      if (input$forest_content %in% c("combined", "all", "cox_combined", "cox_all", "survreg_combined", "survreg_all")) {
        pair <- switch(
          input$forest_content,
          combined =, all = list(rv$uni, rv$multi),
          cox_combined =, cox_all = list(rv$cox_exposure, rv$cox_multi),
          survreg_combined =, survreg_all = list(rv$survreg_exposure, rv$survreg_multi)
        )
        if (any(vapply(pair, is.null, logical(1)))) {
          stop("Run both the exposure models and multivariable model before creating the combined forest plot.", call. = FALSE)
        }
        desc <- if (input$forest_content %in% c("all", "cox_all", "survreg_all")) rv$desc else NULL
        fd <- gtregression::forest_df(pair[[1]], pair[[2]], desc = desc)
      } else {
        source <- result_for(input$forest_source)
        req(source)
        fd <- gtregression::forest_df(source)
      }
      gtregression::forest_reg(
        fd,
        side = input$forest_side,
        ci_col_width = ci_width,
        xlim = forest_xlim,
        ticks_at = forest_ticks
      )
    }, error = function(e) { showNotification(conditionMessage(e), type = "error"); NULL })
    if (!is.null(rv$forest)) {
      rv$last_message <- "Export with Wide or Many rows preset if the browser preview is cramped."
      showNotification("Forest preview ready. For publication, export with a wider preset or use save_forest() code.", type = "message")
    }
  })

  observeEvent(input$run_fit_plot, {
    fit_result <- result_for(input$fit_source)
    req(fit_result)
    rv$fitplot <- tryCatch(
      gtregression::plot_model_fit(
        fit_result,
        model_name = if (gtx_has_text(input$fit_model_name)) input$fit_model_name else NULL,
        type = input$fit_type,
        bins = input$fit_bins
      ),
      error = function(e) { showNotification(conditionMessage(e), type = "error"); NULL }
    )
    if (!is.null(rv$fitplot)) {
      rv$last_message <- "Review diagnostics and copy the code into your analysis script."
      showNotification("Model fit plot ready. Use this as a diagnostic aid, not a final model decision alone.", type = "message")
    }
  })

  output$plotreg_plot <- renderPlot({ req(rv$plotreg); print(rv$plotreg) })
  output$forest_plot <- renderPlot({ req(rv$forest); print(rv$forest) })
  output$fit_plot <- renderPlot({ req(rv$fitplot); print(rv$fitplot) })
  output$merged_table <- gtx_render_table(function() rv$merged)
  gtx_table_downloads(output, "merged", reactive(rv$merged))
  gtx_plot_downloads(output, "plotreg", reactive(rv$plotreg), size_get = reactive(input$export_preset %||% "standard"))
  gtx_plot_downloads(output, "forest", reactive(rv$forest), is_forest = TRUE, size_get = reactive(input$export_preset %||% "standard"))
  gtx_plot_downloads(output, "fitplot", reactive(rv$fitplot), size_get = reactive(input$export_preset %||% "standard"))
  output$visual_code <- renderText({
    plot_result_name <- switch(
      input$plot_source %||% "uni",
      uni = "uni_result",
      multi = "multi_result",
      cox_exposure = "cox_exposure_result",
      cox_multi = "cox_multi_result",
      survreg_exposure = "survreg_exposure_result",
      survreg_multi = "survreg_multi_result",
      cox = "cox_result",
      survreg = "survreg_result",
      strat = "stratified_result"
    )
    forest_result_name <- switch(
      input$forest_source %||% "uni",
      uni = "uni_result",
      multi = "multi_result",
      cox_exposure = "cox_exposure_result",
      cox_multi = "cox_multi_result",
      survreg_exposure = "survreg_exposure_result",
      survreg_multi = "survreg_multi_result",
      strat = "stratified_result"
    )
    forest_xlim_code <- gtx_numeric_vector_code(input$forest_xlim, expected = 2)
    forest_ticks_code <- gtx_numeric_vector_code(input$forest_ticks)
    ci_width <- input$forest_ci_width %||% 20
    plot_pair <- switch(
      input$plot_layout %||% "single",
      combined = c("uni_result", "multi_result"),
      cox_combined = c("cox_exposure_result", "cox_multi_result"),
      survreg_combined = c("survreg_exposure_result", "survreg_multi_result"),
      NULL
    )
    plot_code <- if (!is.null(plot_pair)) {
      paste0(
        "combined_plot <- plot_reg_combine(", plot_pair[1], ", ", plot_pair[2], ", log_x = ",
        gtx_bool_code(input$plot_log_x), ")\n",
        "save_plot(combined_plot, filename = \"plot_reg_combined.png\", width = 10, height = 7)"
      )
    } else {
      paste0(
        "reg_plot <- plot_reg(", plot_result_name, ", log_x = ", gtx_bool_code(input$plot_log_x), ")\n",
        "save_plot(reg_plot, filename = \"plot_reg.png\", width = 9, height = 6)"
      )
    }
    forest_data_code <- switch(
      input$forest_content %||% "single",
      all = "forest_data <- forest_df(uni_result, multi_result, desc = desc_result)",
      combined = "forest_data <- forest_df(uni_result, multi_result)",
      cox_all = "forest_data <- forest_df(cox_exposure_result, cox_multi_result, desc = desc_result)",
      cox_combined = "forest_data <- forest_df(cox_exposure_result, cox_multi_result)",
      survreg_all = "forest_data <- forest_df(survreg_exposure_result, survreg_multi_result, desc = desc_result)",
      survreg_combined = "forest_data <- forest_df(survreg_exposure_result, survreg_multi_result)",
      paste0("forest_data <- forest_df(", forest_result_name, ")")
    )
    forest_args <- paste0(
      "forest_plot <- forest_reg(\n",
      "  forest_data,\n",
      "  side = \"", input$forest_side, "\",\n",
      "  ci_col_width = ", ci_width,
      if (!is.null(forest_xlim_code)) paste0(",\n  xlim = ", forest_xlim_code) else "",
      if (!is.null(forest_ticks_code)) paste0(",\n  ticks_at = ", forest_ticks_code) else "",
      "\n)"
    )
    paste0(
      "# Browser previews are quick checks. For final output, run this in RStudio.\n",
      "# First run the Code tabs that create every result object used below.\n\n",
      plot_code, "\n\n",
      "# Merge tables for manuscript output after descriptive/univariable/multivariable steps.\n",
      "merged_table <- merge_tables(\n",
      "  desc_result,\n",
      "  uni_result,\n",
      "  multi_result,\n",
      "  spanners = c(\"Descriptive\", \"Crude\", \"Adjusted\")\n",
      ")\n",
      "save_table(merged_table, filename = \"merged_table.docx\")\n\n",
      forest_data_code, "\n",
      forest_args, "\n",
      "save_forest(forest_plot, filename = \"forest_reg.pdf\", width = 12, height = 8)\n\n",
      "# If labels or confidence intervals are crowded, increase width/height.\n",
      "# For overlapping x-axis text, set forest_reg(xlim = c(...), ticks_at = c(...)).\n",
      "# For narrow forest panels, increase ci_col_width."
    )
  })

  complete_workflow_code <- reactive({
    sections <- character(0)

    if (gtx_has_text(rv$data_code)) {
      sections <- c(sections, paste("# 1. Load data", rv$data_code, sep = "\n"))
      sections <- c(sections, paste("# 2. Select and prepare analysis data", analysis_code_prefix(), sep = "\n"))
    }

    reference_lines <- gtx_reference_code(
      selected_references(),
      reference_candidates(),
      data_name = "analysis_data"
    )
    if (length(reference_lines)) {
      sections <- c(sections, paste(
        "# 3. Set categorical predictor reference categories",
        paste(reference_lines, collapse = "\n"),
        sep = "\n"
      ))
    }

    if (!is.null(rv$desc) && length(input$desc_exposures %||% character(0))) {
      desc_args <- list(
        data = "analysis_data",
        exposures = gtx_vec_code(input$desc_exposures),
        by = gtx_null_code(input$desc_by),
        percent = shQuote(input$desc_percent),
        show_overall = shQuote(input$desc_overall),
        show_missing = shQuote(input$desc_missing),
        show_dichotomous = shQuote(input$desc_dich),
        statistic = gtx_statistic_code(input$desc_statistic),
        format = shQuote("flextable")
      )
      sections <- c(sections, paste("# 3. Descriptive table", gtx_code_assign("desc_result", "descriptive_table", desc_args), sep = "\n"))
    }

    if (!is.null(rv$uni) && gtx_has_text(input$reg_outcome) && length(input$reg_exposures %||% character(0))) {
      sections <- c(sections, paste(
        "# 4. Univariable regression",
        gtx_code_assign(
          "uni_result",
          "uni_reg",
          list(
            data = "analysis_data",
            outcome = gtx_null_code(input$reg_outcome),
            exposures = gtx_vec_code(input$reg_exposures),
            approach = shQuote(input$reg_approach),
            interaction = if (gtx_has_text(input$reg_interaction)) shQuote(input$reg_interaction) else NULL,
            show_ref = gtx_bool_code(input$reg_show_ref),
            model_stats = gtx_bool_code(input$reg_model_stats %||% FALSE),
            format = shQuote("flextable")
          )
        ),
        sep = "\n"
      ))
    }

    if (!is.null(rv$multi) && gtx_has_text(input$reg_outcome) && length(input$reg_exposures %||% character(0))) {
      sections <- c(sections, paste(
        "# 5. Multivariable regression",
        gtx_code_assign(
          "multi_result",
          "multi_reg",
          list(
            data = "analysis_data",
            outcome = gtx_null_code(input$reg_outcome),
            exposures = gtx_vec_code(input$reg_exposures),
            adjust_for = gtx_nonempty_vec_code(input$reg_adjust %||% character(0)),
            approach = shQuote(input$reg_approach),
            interaction = if (gtx_has_text(input$reg_interaction)) shQuote(input$reg_interaction) else NULL,
            show_ref = gtx_bool_code(input$reg_show_ref),
            model_stats = gtx_bool_code(input$reg_model_stats %||% FALSE),
            format = shQuote("flextable")
          )
        ),
        sep = "\n"
      ))
    }

    if (!is.null(rv$strat) && gtx_has_text(input$reg_stratifier)) {
      fun <- if (isTRUE(input$reg_strata_multi)) "stratified_multi_reg" else "stratified_uni_reg"
      sections <- c(sections, paste(
        "# 5. Stratified regression",
        gtx_code_assign(
          "stratified_result",
          fun,
          list(
            data = "analysis_data",
            outcome = gtx_null_code(input$reg_outcome),
            exposures = gtx_vec_code(input$reg_exposures),
            adjust_for = if (isTRUE(input$reg_strata_multi)) gtx_nonempty_vec_code(input$reg_adjust %||% character(0)) else NULL,
            stratifier = gtx_null_code(input$reg_stratifier),
            approach = shQuote(input$reg_approach),
            show_ref = gtx_bool_code(input$reg_show_ref),
            format = shQuote("flextable")
          )
        ),
        sep = "\n"
      ))
    }

    if ((!is.null(rv$cox) || !is.null(rv$survreg) || !is.null(rv$km)) && gtx_has_text(input$surv_time) && gtx_has_text(input$surv_event)) {
      sections <- c(sections, paste(
        "# 6. Survival analysis",
        gtx_code_assign(
          "cox_result",
          "cox_reg",
          list(
            data = "analysis_data",
            time = gtx_null_code(input$surv_time),
            event = gtx_null_code(input$surv_event),
            exposures = gtx_vec_code(input$surv_exposures %||% character(0)),
            adjust_for = gtx_nonempty_vec_code(input$surv_adjust %||% character(0)),
            multivariable = gtx_bool_code(input$surv_multivariable),
            stratifier = gtx_null_code(input$surv_stratifier),
            show_ref = gtx_bool_code(input$surv_show_ref),
            format = shQuote("flextable")
          )
        ),
        "",
        gtx_code_assign(
          "km_result",
          "km_plot",
          list(
            data = "analysis_data",
            time = gtx_null_code(input$surv_time),
            event = gtx_null_code(input$surv_event),
            by = gtx_null_code(input$km_by),
            y_percent = gtx_bool_code(input$km_y_percent),
            grid = gtx_bool_code(input$km_grid),
            p_value = gtx_bool_code(input$km_p)
          )
        ),
        sep = "\n"
      ))
    }

    if (!is.null(rv$merged) || !is.null(rv$plotreg) || !is.null(rv$forest) || !is.null(rv$fitplot)) {
      ci_width <- input$forest_ci_width %||% 20
      sections <- c(sections, paste(
        "# 7. Visualise and export",
        "merged_table <- merge_tables(desc_result, uni_result, multi_result, spanners = c(\"Descriptive\", \"Crude\", \"Adjusted\"))",
        "reg_plot <- plot_reg(uni_result)",
        "combined_plot <- plot_reg_combine(uni_result, multi_result)",
        "forest_data <- forest_df(uni_result, multi_result, desc = desc_result)",
        paste0("forest_plot <- forest_reg(forest_data, ci_col_width = ", ci_width, ")"),
        "save_table(merged_table, filename = \"merged_table.docx\")",
        "save_plot(combined_plot, filename = \"plot_reg_combined.png\", width = 10, height = 7)",
        "save_forest(forest_plot, filename = \"forest_reg.pdf\", width = 13, height = 9)",
        sep = "\n"
      ))
    }

    if (!is.null(rv$advanced) && gtx_has_text(rv$advanced_code)) {
      sections <- c(sections, paste("# 8. Advanced analysis", rv$advanced_code, sep = "\n"))
    }

    if (!is.null(rv$mediation)) {
      sections <- c(sections, paste(
        "# 9. Causal mediation",
        gtx_code_assign(
          "mediation_result",
          "mediation_analysis",
          list(
            data = "analysis_data",
            exposure = gtx_null_code(input$med_exposure),
            mediator = gtx_null_code(input$med_mediator),
            outcome = gtx_null_code(input$med_outcome),
            covariates = gtx_nonempty_vec_code(input$med_covariates %||% character(0)),
            outcome_approach = shQuote(input$med_approach),
            sims = as.character(input$med_sims),
            seed = as.character(input$med_seed),
            format = shQuote("flextable")
          )
        ),
        "mediation_plot <- plot_mediation(mediation_result)",
        sep = "\n"
      ))
    }

    if (!length(sections)) {
      return("# Load data and run an analysis. The app will build a full reproducible workflow here.")
    }

    paste(
      "# Generated by the gtregression app",
      "library(gtregression)",
      paste(sections, collapse = "\n\n"),
      sep = "\n\n"
    )
  })

  output$full_workflow_code <- renderText(complete_workflow_code())

  output$download_session_script <- downloadHandler(
    filename = function() "gtregression-session.R",
    content = function(file) {
      writeLines(complete_workflow_code(), file, useBytes = TRUE)
    }
  )

  observeEvent(input$run_select_models, {
    data <- require_analysis_data()
    req(data, input$adv_exposures)
    survival_advanced <- input$adv_approach %in% c("cox", "survreg")
    if (survival_advanced) {
      req(input$adv_time, input$adv_event)
    } else {
      req(input$adv_outcome)
    }
    res <- run_safely("Model selection", gtx_capture(gtx_call(
      "select_models",
      data = data,
      outcome = if (survival_advanced) NULL else input$adv_outcome,
      time = if (survival_advanced) input$adv_time else NULL,
      event = if (survival_advanced) input$adv_event else NULL,
      exposures = input$adv_exposures,
      approach = input$adv_approach,
      distribution = if (identical(input$adv_approach, "survreg")) input$adv_distribution else NULL,
      direction = input$select_direction,
      format = "flextable"
    )), on_error = clear_advanced_error)
    if (is.null(res)) return(NULL)
    rv$advanced <- res$value
    rv$advanced_text <- res$text
    rv$advanced_title <- "Model-selection results"
    rv$advanced_guidance <- "The highlighted model has the preferred AIC within this candidate path. Retain variables using study design, clinical relevance, and diagnostics rather than automated selection alone."
    rv$advanced_code <- paste(analysis_code_prefix(), gtx_code_assign(
      "selected_models",
      "select_models",
      list(
        data = "analysis_data",
        outcome = if (input$adv_approach %in% c("cox", "survreg")) NULL else gtx_null_code(input$adv_outcome),
        time = if (input$adv_approach %in% c("cox", "survreg")) gtx_null_code(input$adv_time) else NULL,
        event = if (input$adv_approach %in% c("cox", "survreg")) gtx_null_code(input$adv_event) else NULL,
        exposures = gtx_vec_code(input$adv_exposures),
        approach = shQuote(input$adv_approach),
        distribution = if (identical(input$adv_approach, "survreg")) shQuote(input$adv_distribution) else NULL,
        direction = shQuote(input$select_direction),
        format = shQuote("flextable")
      )
    ), sep = "\n\n")
    updateTabsetPanel(session, "advanced_output_tab", selected = "Output")
  })

  observeEvent(input$advanced_tool, {
    tool <- input$advanced_tool %||% "selection"
    labels <- c(
      selection = "Model selection",
      comparison = "Model comparison",
      confounder = "Confounder assessment",
      interaction = "Interaction assessment",
      convergence = "Convergence diagnostics",
      collinearity = "Collinearity diagnostics"
    )
    rv$advanced <- NULL
    rv$advanced_text <- ""
    rv$advanced_code <- ""
    rv$advanced_title <- unname(labels[[tool]] %||% "Advanced analysis")
    rv$advanced_guidance <- "Complete the choices on the left and run this tool. Results from another advanced tool are cleared when you switch tools."
  }, ignoreInit = TRUE)

  observeEvent(input$compare_add_model, {
    rv$compare_count <- min(6L, rv$compare_count + 1L)
    if (rv$compare_count == 6L) {
      showNotification("A maximum of six candidate models keeps the comparison readable.", type = "message")
    }
  }, ignoreInit = TRUE)

  observeEvent(input$compare_remove_model, {
    rv$compare_count <- max(2L, rv$compare_count - 1L)
  }, ignoreInit = TRUE)

  observeEvent(input$run_compare, {
    data <- require_analysis_data()
    req(data)
    approach <- input$compare_approach %||% "logit"
    survival_comparison <- approach %in% c("cox", "survreg")
    if (survival_comparison) {
      req(input$compare_time, input$compare_event)
    } else {
      req(input$compare_outcome)
    }

    specifications <- lapply(seq_len(rv$compare_count), function(i) {
      exposures <- input[[paste0("compare_exposures_", i)]] %||% character(0)
      adjust <- input[[paste0("compare_adjust_", i)]] %||% character(0)
      int_a <- input[[paste0("compare_interaction_a_", i)]] %||% ""
      int_b <- input[[paste0("compare_interaction_b_", i)]] %||% ""
      list(
        name = trimws(input[[paste0("compare_name_", i)]] %||% paste("Model", i)),
        exposures = exposures,
        adjust = adjust,
        predictors = unique(c(exposures, adjust)),
        int_a = int_a,
        int_b = int_b,
        interaction = if (nzchar(int_a) && nzchar(int_b)) paste(int_a, int_b, sep = "*") else NULL
      )
    })

    model_names <- vapply(specifications, `[[`, character(1), "name")
    if (any(!nzchar(model_names))) {
      showNotification("Give every candidate model a name.", type = "warning")
      return(NULL)
    }
    if (anyDuplicated(model_names)) {
      showNotification("Candidate model names must be unique.", type = "warning")
      return(NULL)
    }
    if (any(vapply(specifications, function(x) !length(x$exposures), logical(1)))) {
      showNotification("Choose at least one reported exposure for every candidate model.", type = "warning")
      return(NULL)
    }
    overlap <- vapply(specifications, function(x) length(intersect(x$exposures, x$adjust)) > 0L, logical(1))
    if (any(overlap)) {
      showNotification("A variable cannot be both a reported exposure and an adjustment variable in the same model.", type = "warning")
      return(NULL)
    }
    incomplete_interaction <- vapply(specifications, function(x) xor(nzchar(x$int_a), nzchar(x$int_b)), logical(1))
    invalid_interaction <- vapply(specifications, function(x) {
      both <- nzchar(x$int_a) && nzchar(x$int_b)
      both && (identical(x$int_a, x$int_b) || !all(c(x$int_a, x$int_b) %in% x$predictors))
    }, logical(1))
    if (any(incomplete_interaction)) {
      showNotification("Choose both interaction terms, or leave both as None.", type = "warning")
      return(NULL)
    }
    if (any(invalid_interaction)) {
      showNotification("Interaction terms must be different variables already included in that candidate model.", type = "warning")
      return(NULL)
    }
    if (gtx_has_text(input$adv_primary_exposure) && any(!vapply(
      specifications,
      function(x) input$adv_primary_exposure %in% x$predictors,
      logical(1)
    ))) {
      showNotification("The primary exposure must be included in every candidate model.", type = "warning")
      return(NULL)
    }

    fit_one <- function(specification) {
      if (identical(approach, "cox")) {
        return(gtx_call(
          "cox_reg", data = data, time = input$compare_time, event = input$compare_event,
          exposures = specification$predictors, interaction = specification$interaction,
          multivariable = TRUE, format = "flextable", model_stats = TRUE
        ))
      }
      if (identical(approach, "survreg")) {
        return(gtx_call(
          "surv_reg", data = data, time = input$compare_time, event = input$compare_event,
          exposures = specification$predictors, interaction = specification$interaction,
          multivariable = TRUE, distribution = input$compare_distribution,
          format = "flextable", model_stats = TRUE
        ))
      }
      gtx_call(
        "multi_reg", data = data, outcome = input$compare_outcome,
        exposures = specification$predictors, interaction = specification$interaction,
        approach = approach, format = "flextable", model_stats = TRUE
      )
    }

    models <- tryCatch(
      withProgress(message = "Fitting candidate models", value = 0, {
        fitted <- lapply(seq_along(specifications), function(i) {
          incProgress(1 / length(specifications), detail = model_names[i])
          fit_one(specifications[[i]])
        })
        names(fitted) <- model_names
        fitted
      }),
      error = function(e) {
        showNotification(paste("A candidate model could not be fitted:", conditionMessage(e)), type = "error", duration = 10)
        NULL
      }
    )
    if (is.null(models)) return(NULL)

    compare_options <- list(model_names = model_names, format = "flextable")
    if (gtx_has_text(input$adv_primary_exposure)) compare_options$primary_exposure <- input$adv_primary_exposure
    res <- tryCatch(do.call(gtregression::compare_models, c(models, compare_options)), error = function(e) { showNotification(conditionMessage(e), type = "error"); NULL })
    if (is.null(res)) return(NULL)
    rv$advanced <- res
    rv$advanced_text <- ""
    rv$advanced_title <- "Model-comparison results"
    rv$advanced_guidance <- "Read the comparison-status panel first. Likelihood-based comparisons are strongest for nested models fitted to the same observations; the primary estimate helps assess robustness across candidate models."
    object_names <- make.unique(vapply(seq_along(specifications), function(i) {
      gtx_object_name(model_names[i], paste0("candidate_model_", i))
    }, character(1)))
    model_code <- vapply(seq_along(specifications), function(i) {
      specification <- specifications[[i]]
      fun <- if (identical(approach, "cox")) "cox_reg" else if (identical(approach, "survreg")) "surv_reg" else "multi_reg"
      args <- list(
        data = "analysis_data",
        outcome = if (!survival_comparison) gtx_null_code(input$compare_outcome) else NULL,
        time = if (survival_comparison) gtx_null_code(input$compare_time) else NULL,
        event = if (survival_comparison) gtx_null_code(input$compare_event) else NULL,
        exposures = gtx_vec_code(specification$predictors),
        interaction = if (!is.null(specification$interaction)) shQuote(specification$interaction) else NULL,
        multivariable = if (survival_comparison) "TRUE" else NULL,
        distribution = if (identical(approach, "survreg")) shQuote(input$compare_distribution) else NULL,
        approach = if (!survival_comparison) shQuote(approach) else NULL,
        format = shQuote("flextable"),
        model_stats = "TRUE"
      )
      paste0(
        "# ", model_names[i], ": reported exposures = ", paste(specification$exposures, collapse = ", "),
        if (length(specification$adjust)) paste0("; adjusted for = ", paste(specification$adjust, collapse = ", ")) else "",
        "\n", gtx_code_assign(object_names[i], fun, args)
      )
    }, character(1))
    model_lines <- paste0("  ", object_names, collapse = ",\n")
    rv$advanced_code <- paste0(
      analysis_code_prefix(), "\n\n",
      paste(model_code, collapse = "\n\n"), "\n\n",
      "comparison_table <- compare_models(\n",
      model_lines, ",\n",
      "  model_names = ", gtx_vec_code(model_names), ",\n",
      if (gtx_has_text(input$adv_primary_exposure)) paste0("  primary_exposure = ", shQuote(input$adv_primary_exposure), ",\n") else "",
      "  format = \"flextable\"\n",
      ")"
    )
    updateTabsetPanel(session, "advanced_output_tab", selected = "Output")
  })

  observeEvent(input$run_confounder, {
    data <- require_analysis_data()
    req(data, input$conf_exposure, input$conf_candidate)
    survival_advanced <- input$adv_approach %in% c("cox", "survreg")
    if (survival_advanced) {
      req(input$adv_time, input$adv_event)
    } else {
      req(input$adv_outcome)
    }
    res <- run_safely("Confounder assessment", gtx_capture(gtx_call(
      "identify_confounder",
      data = data,
      outcome = if (survival_advanced) NULL else input$adv_outcome,
      time = if (survival_advanced) input$adv_time else NULL,
      event = if (survival_advanced) input$adv_event else NULL,
      exposure = input$conf_exposure,
      potential_confounder = input$conf_candidate,
      approach = input$adv_approach,
      distribution = if (identical(input$adv_approach, "survreg")) input$adv_distribution else NULL,
      format = "flextable"
    )), on_error = clear_advanced_error)
    if (is.null(res)) return(NULL)
    rv$advanced <- res$value
    rv$advanced_text <- res$text
    rv$advanced_title <- "Confounder assessment"
    rv$advanced_guidance <- "This is a viewing and decision-support result, not a publication-ready claim that a variable is a confounder. Base the final decision on a prespecified DAG, temporal order, and subject-matter knowledge."
    rv$advanced_code <- paste(analysis_code_prefix(), gtx_code_assign(
      "confounder_check",
      "identify_confounder",
      list(
        data = "analysis_data",
        outcome = if (input$adv_approach %in% c("cox", "survreg")) NULL else gtx_null_code(input$adv_outcome),
        time = if (input$adv_approach %in% c("cox", "survreg")) gtx_null_code(input$adv_time) else NULL,
        event = if (input$adv_approach %in% c("cox", "survreg")) gtx_null_code(input$adv_event) else NULL,
        exposure = gtx_null_code(input$conf_exposure),
        potential_confounder = gtx_null_code(input$conf_candidate),
        approach = shQuote(input$adv_approach),
        distribution = if (identical(input$adv_approach, "survreg")) shQuote(input$adv_distribution) else NULL,
        format = shQuote("flextable")
      )
    ), sep = "\n\n")
    updateTabsetPanel(session, "advanced_output_tab", selected = "Output")
  })

  observeEvent(input$run_interaction, {
    data <- require_analysis_data()
    req(data, input$conf_exposure, input$conf_candidate)
    survival_advanced <- input$adv_approach %in% c("cox", "survreg")
    if (survival_advanced) {
      req(input$adv_time, input$adv_event)
    } else {
      req(input$adv_outcome)
    }
    res <- run_safely("Interaction assessment", gtx_capture(gtx_call(
      "interaction_models",
      data = data,
      outcome = if (survival_advanced) NULL else input$adv_outcome,
      time = if (survival_advanced) input$adv_time else NULL,
      event = if (survival_advanced) input$adv_event else NULL,
      exposure = input$conf_exposure,
      effect_modifier = input$conf_candidate,
      covariates = input$adv_covariates,
      approach = input$adv_approach,
      distribution = if (identical(input$adv_approach, "survreg")) input$adv_distribution else NULL,
      format = "flextable"
    )), on_error = clear_advanced_error)
    if (is.null(res)) return(NULL)
    rv$advanced <- res$value
    rv$advanced_text <- res$text
    rv$advanced_title <- "Interaction assessment"
    rv$advanced_guidance <- "Use the interaction test together with stratum-specific estimates and confidence intervals. Statistical interaction depends on the chosen effect scale."
    rv$advanced_code <- paste(analysis_code_prefix(), gtx_code_assign(
      "interaction_check",
      "interaction_models",
      list(
        data = "analysis_data",
        outcome = if (input$adv_approach %in% c("cox", "survreg")) NULL else gtx_null_code(input$adv_outcome),
        time = if (input$adv_approach %in% c("cox", "survreg")) gtx_null_code(input$adv_time) else NULL,
        event = if (input$adv_approach %in% c("cox", "survreg")) gtx_null_code(input$adv_event) else NULL,
        exposure = gtx_null_code(input$conf_exposure),
        effect_modifier = gtx_null_code(input$conf_candidate),
        covariates = gtx_nonempty_vec_code(input$adv_covariates %||% character(0)),
        approach = shQuote(input$adv_approach),
        distribution = if (identical(input$adv_approach, "survreg")) shQuote(input$adv_distribution) else NULL,
        format = shQuote("flextable")
      )
    ), sep = "\n\n")
    updateTabsetPanel(session, "advanced_output_tab", selected = "Output")
  })

  observeEvent(input$run_convergence, {
    data <- require_analysis_data()
    req(data, input$adv_outcome, input$adv_exposures)
    res <- run_safely(
      "Convergence diagnostics",
      gtx_capture(gtx_call("check_convergence", data = data, outcome = input$adv_outcome, exposures = input$adv_exposures, approach = input$adv_approach, multivariate = TRUE, multivariable = TRUE, format = "flextable")),
      on_error = clear_advanced_error
    )
    if (is.null(res)) return(NULL)
    rv$advanced <- res$value
    rv$advanced_text <- res$text
    rv$advanced_title <- "Convergence diagnostics"
    rv$advanced_guidance <- "Models flagged as non-convergent or unstable should not be interpreted until the data, specification, or modelling approach has been reviewed."
    rv$advanced_code <- paste(analysis_code_prefix(), gtx_code_assign(
      "convergence_check",
      "check_convergence",
      list(
        data = "analysis_data",
        outcome = gtx_null_code(input$adv_outcome),
        exposures = gtx_vec_code(input$adv_exposures),
        approach = shQuote(input$adv_approach),
        multivariable = "TRUE",
        format = shQuote("flextable")
      )
    ), sep = "\n\n")
    updateTabsetPanel(session, "advanced_output_tab", selected = "Output")
  })

  observeEvent(input$run_collinearity, {
    available <- Filter(Negate(is.null), list(
      uni = rv$uni,
      multi = rv$multi,
      cox_exposure = rv$cox_exposure,
      cox_multi = rv$cox_multi,
      survreg_exposure = rv$survreg_exposure,
      survreg_multi = rv$survreg_multi
    ))
    req(input$adv_collinearity_model, available[[input$adv_collinearity_model]])
    res <- run_safely(
      "Collinearity diagnostics",
      gtx_capture(gtx_call("check_collinearity", model = available[[input$adv_collinearity_model]], format = "flextable")),
      on_error = clear_advanced_error
    )
    if (is.null(res)) return(NULL)
    rv$advanced <- res$value
    rv$advanced_text <- res$text
    rv$advanced_title <- "Collinearity diagnostics"
    rv$advanced_guidance <- "Large collinearity measures indicate that predictors may be difficult to distinguish in the fitted model. Review coding, clinical overlap, and model purpose before removing variables."
    rv$advanced_code <- paste0(
      "collinearity_check <- check_collinearity(\n",
      "  model = ", input$adv_collinearity_model, "_result,\n",
      "  format = \"flextable\"\n",
      ")"
    )
    updateTabsetPanel(session, "advanced_output_tab", selected = "Output")
  })

  output$advanced_table <- gtx_render_table(function() rv$advanced)
  output$advanced_text <- renderText(rv$advanced_text)
  output$advanced_code <- renderText(rv$advanced_code)
  output$advanced_result_heading <- renderUI(tagList(h3(rv$advanced_title)))
  output$advanced_guidance <- renderUI(div(class = "gtx-help", strong("How to use this result: "), rv$advanced_guidance))
  output$advanced_downloads <- renderUI({
    if (is.null(rv$advanced)) return(NULL)
    tagListDownload("advanced", table = TRUE)
  })
  gtx_table_downloads(output, "advanced", reactive(rv$advanced))

  observeEvent(input$run_mediation, {
    data <- require_analysis_data()
    req(data, input$med_exposure, input$med_mediator, input$med_outcome)
    rv$mediation <- tryCatch(
      gtregression::mediation_analysis(
        data = data,
        exposure = input$med_exposure,
        mediator = input$med_mediator,
        outcome = input$med_outcome,
        covariates = input$med_covariates,
        outcome_approach = input$med_approach,
        sims = input$med_sims,
        seed = input$med_seed,
        format = "flextable"
      ),
      error = function(e) { showNotification(conditionMessage(e), type = "error"); NULL }
    )
  })

  output$med_table <- gtx_render_table(function() rv$mediation)
  output$med_plot <- renderPlot({
    req(rv$mediation)
    print(gtregression::plot_mediation(rv$mediation))
  })
  gtx_table_downloads(output, "med", reactive(rv$mediation))
  gtx_plot_downloads(output, "medplot", reactive({
    req(rv$mediation)
    gtregression::plot_mediation(rv$mediation)
  }))
  output$med_code <- renderText({
    paste0(
      analysis_code_prefix(),
      "\n\n",
      gtx_code_assign(
        "mediation_result",
        "mediation_analysis",
        list(
          data = "analysis_data",
          exposure = gtx_null_code(input$med_exposure),
          mediator = gtx_null_code(input$med_mediator),
          outcome = gtx_null_code(input$med_outcome),
          covariates = gtx_nonempty_vec_code(input$med_covariates %||% character(0)),
          outcome_approach = shQuote(input$med_approach),
          sims = as.character(input$med_sims),
          seed = as.character(input$med_seed),
          format = shQuote("flextable")
        )
      ),
      "\n\n",
      "mediation_plot <- plot_mediation(mediation_result)\n",
      "save_table(mediation_result$table, filename = \"mediation_table.docx\")\n",
      "save_plot(mediation_plot, filename = \"mediation_path.png\", width = 8, height = 6)"
    )
  })
}

shinyApp(ui, server)
