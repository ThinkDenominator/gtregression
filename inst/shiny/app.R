options(shiny.sanitize.errors = FALSE)

required_app_packages <- c("shiny", "gt", "flextable", "ggplot2")
missing_app_packages <- required_app_packages[
  !vapply(required_app_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_app_packages)) {
  stop(
    "GT Regression requires these packages: ",
    paste(missing_app_packages, collapse = ", "),
    ". Install them and run gtregression_app() again.",
    call. = FALSE
  )
}

library(shiny)

`%||%` <- function(a, b) if (!is.null(a)) a else b

gtx_ns <- asNamespace("gtregression")

if (!exists("gtx_launch_context", inherits = FALSE)) {
  gtx_launch_context <- list(mode = "web", data_env = NULL)
}
gtx_app_mode <- gtx_launch_context$mode %||% "web"
# "app" is the canonical package GUI launched by gtregression_app() (and by the
# RStudio Addin, which only calls gtregression_app()); "addin" is a legacy alias.
# "web" is this file sourced directly (for example by Shiny Server).
# gtx_addin_mode is the historical internal name for "running as the package
# GUI in a local R session"; it no longer means an Addin-specific interface.
gtx_addin_mode <- gtx_app_mode %in% c("app", "addin")
gtx_data_env <- gtx_launch_context$data_env

# The app is sourced by Shiny outside the package namespace. Bind the internal
# modules and helpers here so installed-package behaviour matches load_all().
mod_data_prep_ui <- get("mod_data_prep_ui", envir = gtx_ns)
mod_data_prep_server <- get("mod_data_prep_server", envir = gtx_ns)
gt_data_structure <- get("gt_data_structure", envir = gtx_ns)
gt_variable_detail <- get("gt_variable_detail", envir = gtx_ns)
gt_ds_num <- get("gt_ds_num", envir = gtx_ns)
gt_ds_pct <- get("gt_ds_pct", envir = gtx_ns)
gt_ds_html <- get("gt_ds_html", envir = gtx_ns)
gt_ds_text <- get("gt_ds_text", envir = gtx_ns)
gtx_reference_candidates <- get("gtx_reference_candidates", envir = gtx_ns)
gtx_relevel_predictors <- get("gtx_relevel_predictors", envir = gtx_ns)
gtx_reference_code <- get("gtx_reference_code", envir = gtx_ns)
gtx_discover_data_frames <- get("gtx_discover_data_frames", envir = gtx_ns)
gtx_get_data_frame <- get("gtx_get_data_frame", envir = gtx_ns)
gtx_object_reference <- get("gtx_object_reference", envir = gtx_ns)

# Error safety: every observer handler runs inside gtx_guard(), so an error is
# shown as a plain notification and never ends the session or opens the
# debugger. Download failures are explained the same way.
gtx_guard <- get("gtx_guard", envir = gtx_ns)
gtx_friendly_error <- get("gtx_friendly_error", envir = gtx_ns)
gtx_notify_error <- get("gtx_notify_error", envir = gtx_ns)
gtx_role_conflicts <- get("gtx_role_conflicts", envir = gtx_ns)
gtx_role_clashes <- get("gtx_role_clashes", envir = gtx_ns)
gtx_role_conflict_text <- get("gtx_role_conflict_text", envir = gtx_ns)
gtx_collect <- get("gtx_collect", envir = gtx_ns)
gtx_model_checks <- get("gtx_model_checks", envir = gtx_ns)
gtx_import_notes <- get("gtx_import_notes", envir = gtx_ns)
gtx_session_inputs <- get("gtx_session_inputs", envir = gtx_ns)
gtx_result_bytes <- get("gtx_result_bytes", envir = gtx_ns)
gtx_compact_result <- get("gtx_compact_result", envir = gtx_ns)
observeEvent <- get("gtx_safe_observe_event", envir = gtx_ns)
observe <- get("gtx_safe_observe", envir = gtx_ns)
downloadHandler <- get("gtx_safe_download_handler", envir = gtx_ns)

gtx_exported <- function(name) {
  name %in% getNamespaceExports("gtregression")
}

# Busy indicator, one-click re-run after a role fix, and locked choices
# (variables that already hold a single role such as the outcome).
gtx_helper_js <- HTML("
(function() {
  var busySince = null, timer = null, pill = null, locks = {};
  var root = document.documentElement;
  function getPill() {
    if (!pill) {
      pill = document.createElement('div');
      pill.className = 'gtx-busy';
      pill.setAttribute('role', 'status');
      pill.setAttribute('aria-live', 'polite');
      document.body.appendChild(pill);
    }
    return pill;
  }
  function tick() {
    if (busySince === null) return;
    var ms = Date.now() - busySince;
    if (ms < 900) return;
    var s = Math.floor(ms / 1000);
    var p = getPill();
    p.textContent = 'Working' + (s >= 2 ? ' \u00b7 ' + s + ' s' : '') + '\u2026';
    p.classList.add('is-on');
    root.classList.add('gtx-busy-on');
  }
  $(document).on('shiny:busy', function() {
    if (busySince !== null) return;
    busySince = Date.now();
    timer = setInterval(tick, 300);
  });
  $(document).on('shiny:idle', function() {
    busySince = null;
    clearInterval(timer);
    if (pill) pill.classList.remove('is-on');
    root.classList.remove('gtx-busy-on');
  });
  function whenIdle(fn) {
    var n = 0;
    (function wait() {
      if (!root.classList.contains('shiny-busy') || n > 300) fn();
      else { n += 1; setTimeout(wait, 150); }
    })();
  }
  function applyLocks() {
    Object.keys(locks).forEach(function(id) {
      var host = document.getElementById(id);
      if (!host) return;
      var lock = locks[id];
      Array.prototype.forEach.call(host.querySelectorAll('input[type=checkbox]'), function(b) {
        var i = lock.values.indexOf(b.value);
        var label = b.closest('label') || b.parentNode;
        if (i >= 0) {
          if (!b.checked) b.disabled = true;
          label.classList.add('gtx-locked');
          label.title = 'Used as ' + lock.notes[i];
        } else if (label.classList.contains('gtx-locked')) {
          b.disabled = false;
          label.classList.remove('gtx-locked');
          label.removeAttribute('title');
        }
      });
    });
  }
  $(document).on('shiny:connected', function() {
    Shiny.addCustomMessageHandler('gtx-click', function(id) {
      setTimeout(function() { whenIdle(function() { var b = document.getElementById(id); if (b) b.click(); }); }, 300);
    });
    Shiny.addCustomMessageHandler('gtx-lock', function(msg) {
      locks[msg.id] = { values: [].concat(msg.values || []), notes: [].concat(msg.notes || []) };
      applyLocks();
    });
  });
  $(document).on('shiny:value shiny:bound', function() { setTimeout(applyLocks, 0); });
})();
")

# Opening a saved session reads an R data file. That is safe for files the
# user made on their own computer; on a shared web server it is offered only
# on R versions where reading such files cannot run code.
gtx_session_restore_allowed <- function() {
  gtx_addin_mode || getRversion() >= "4.4.0"
}

gtx_session_save_ui <- function() {
  div(
    class = "gtx-session-save",
    h4("Results in this session"),
    uiOutput("history_summary"),
    h4("Save and resume"),
    div(
      class = "gtx-help",
      "Save your selections and the analysis data to a file, then open it later to continue where you left off. ",
      "Results are recreated when you run them again. The file contains your data, so store it as securely as the data itself."
    ),
    div(
      class = "gtx-session-save-actions",
      downloadButton("download_session_state", "Save session"),
      if (gtx_session_restore_allowed()) {
        fileInput("restore_session_file", "Open a saved session", accept = ".gtrsession", buttonLabel = "Choose file")
      } else {
        div(class = "gtx-help", "Opening saved sessions needs R 4.4.0 or later on this server.")
      }
    )
  )
}

gtx_has_formal <- function(fun, arg) {
  arg %in% names(formals(get(fun, envir = gtx_ns)))
}

gtx_call <- function(fun, ...) {
  args <- list(...)
  f <- get(fun, envir = gtx_ns)
  args <- args[intersect(names(args), names(formals(f)))]
  gtx_collect(do.call(f, args))
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

# ---- Selection continuity across preparation changes ----------------------
# Analysis inputs are rebuilt when the analysis dataset changes. A selection
# is kept when its variable still exists (following renames made in
# Prepare); it is cleared when the change removed it.
gtx_map_names <- function(values, events = list()) {
  for (event in events) values[values == event[["from"]]] <- event[["to"]]
  values
}

gtx_keep_selection <- function(values, choices, events = list()) {
  values <- as.character(values %||% character(0))
  values <- values[!is.na(values) & nzchar(values)]
  if (!length(values)) return(NULL)
  values <- unique(gtx_map_names(values, events))
  values <- values[values %in% choices]
  if (length(values)) values else NULL
}

# An interaction term such as "smoke*race" is kept only when every variable
# in it still exists (renamed variables are followed).
gtx_keep_interaction <- function(text, choices, events = list()) {
  text <- trimws(as.character(text %||% ""))
  if (!nzchar(text)) return("")
  pieces <- strsplit(text, "(?<=[*:])|(?=[*:])", perl = TRUE)[[1]]
  is_term <- !pieces %in% c("*", ":")
  terms <- trimws(pieces[is_term])
  mapped <- gtx_map_names(terms, events)
  if (!length(mapped) || !all(mapped %in% choices)) return("")
  pieces[is_term] <- mapped
  paste(pieces, collapse = "")
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

# Notes kept with a result: translated model warnings and the analysis
# messages (for example which outcome level was modelled).
gtx_notes_ui <- function(notes) {
  if (is.null(notes)) return(NULL)
  tagList(
    if (length(notes$warnings)) {
      div(
        class = "gtx-result-note gtx-note-warn", role = "note",
        strong("Check before reporting"),
        tags$ul(lapply(notes$warnings, tags$li))
      )
    },
    if (length(notes$messages)) {
      tags$details(
        class = "gtx-result-note gtx-note-info",
        tags$summary(if (length(notes$messages) == 1) "1 note from the analysis" else paste(length(notes$messages), "notes from the analysis")),
        tags$ul(lapply(notes$messages, tags$li))
      )
    }
  )
}

gtx_stale_ui <- function() {
  div(
    class = "gtx-result-note gtx-note-stale", role = "status",
    "The selections have changed since this result was made. Run again to update it."
  )
}

gtx_checks_ui <- function(checks) {
  if (!length(checks)) return(NULL)
  div(
    class = "gtx-checks",
    strong("Worth checking: "),
    tags$ul(lapply(checks, function(x) tags$li(class = "gtx-check-item", x)))
  )
}

gtx_render_table <- function(expr, stale = NULL) {
  renderUI({
    res <- expr()
    req(res)
    is_stale <- is.function(stale) && isTRUE(stale())
    table <- tryCatch({
      tb <- gtx_table(res)
      if (inherits(tb, "flextable")) {
        flextable::htmltools_value(tb)
      } else if (inherits(tb, "gt_tbl")) {
        HTML(gt::as_raw_html(tb, inline_css = TRUE))
      } else {
        tb
      }
    }, error = function(e) {
      div(
        class = "alert alert-danger",
        strong("This result could not be displayed. "),
        gtx_friendly_error(e)
      )
    })
    tagList(if (is_stale) gtx_stale_ui(), gtx_notes_ui(attr(res, "gtx_notes")), table)
  })
}

gtx_capture <- function(expr) {
  value <- NULL
  text <- utils::capture.output(
    withCallingHandlers(
      {
        value <- eval.parent(substitute(expr))
        notes <- attr(value, "gtx_notes")
        for (m in notes$messages) cat(m, "\n")
        for (w in notes$warnings) cat("Warning: ", w, "\n", sep = "")
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

gtx_code_panel <- function(title, id, copy_label = " Copy") {
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
        copy_label
      )
    ),
    verbatimTextOutput(id)
  )
}

# Presentation helper: in the Addin, group rarely changed controls in a
# collapsed disclosure; in web mode, return the controls unchanged.
gtw_details <- function(summary, ..., open = FALSE) {
  if (!gtx_addin_mode) return(tagList(...))
  tags$details(
    class = "gtw-details",
    open = if (isTRUE(open)) NA else NULL,
    tags$summary(summary),
    div(class = "gtw-details-body", ...)
  )
}

# Presentation helper: in the Addin, move substantive explanatory text into a
# collapsed 'About this analysis' disclosure; in web mode, return it unchanged.
gtw_about <- function(..., summary = "About this analysis") {
  if (!gtx_addin_mode) return(tagList(...))
  tags$details(
    class = "gtw-details gtw-about",
    tags$summary(summary),
    div(class = "gtw-details-body", ...)
  )
}

# Presentation helper: Addin-only control that hides the settings pane so the
# results pane can use the full width. It never reruns or changes an analysis.
gtw_focus_toggle <- function() {
  if (!gtx_addin_mode) return(NULL)
  tags$button(
    type = "button",
    class = "btn btn-default btn-xs gtw-focus-toggle",
    title = "Hide the settings pane to give results the full width (Esc to return)",
    onclick = "gtwToggleFocus()",
    "Focus results"
  )
}

# Reusable workbench UI components (D2) --------------------------------------
# Generic presentation components intended for reuse beyond GT Regression
# (for example a future GT Stats workspace). They are switched on for the Addin
# now; setting gtw_components_enabled to TRUE in web mode would enable them
# there without changing the components. They only read and write existing
# Shiny inputs and outputs: no statistical, validation, or code-generation
# behaviour depends on them.
gtw_components_enabled <- gtx_addin_mode

# Contextual help registry: id -> title, short popover text, optional longer
# text shown under "More". Existing guidance is preserved in `more`.
gtw_help_registry <- list(
  desc = list(
    title = "Descriptive table",
    short = "Summarises variables overall or by group. The table can be reused in merge_table(), forest_df() and reports.",
    more = "Use this first to understand the data and to create baseline tables for merge_table(), forest_df(), and reports."
  ),
  desc_by = list(
    title = "Group by",
    short = "Leave as None for one overall summary; otherwise the selected variable defines the comparison columns.",
    more = "Start by choosing Group by. Leave it as None for one overall summary; otherwise the selected variable defines the comparison columns."
  ),
  desc_statistic = list(
    title = "Numeric statistic override",
    short = "Named overrides, such as age = mean, for variables that need a different summary from the default."
  ),
  reg = list(
    title = "Regression tables",
    short = "Run univariable models first, then keep the important variables selected and run the multivariable model.",
    more = paste(
      "Run univariable analysis first. Then keep the important variables selected and run the multivariable model.",
      "Use Show reference categories when you plan to merge tables or create forest plots.",
      "Choose an outcome first, then exposures. Adjustment variables belong only in the multivariable model and should be",
      "selected using the study question, a DAG, and clinical knowledge."
    )
  ),
  reg_approach = list(
    title = "Approach",
    short = "Regression family for the outcome. Logistic and Firth logistic give odds ratios; log-binomial, Poisson and robust Poisson give risk or rate ratios; negative binomial gives rate ratios; linear gives mean differences."
  ),
  reg_adjust = list(
    title = "Adjust for",
    short = "Used only in the multivariable model. Choose adjustment variables using the study question, a DAG and clinical knowledge."
  ),
  reg_interaction = list(
    title = "Interaction term",
    short = "Optional term added to the model, written as var1*var2 (for example smoke*race)."
  ),
  reg_model_stats = list(
    title = "Store model statistics",
    short = "Enabled by default so the Model Stats tab is ready after fitting. These statistics are kept out of the publication table."
  ),
  reg_stratifier = list(
    title = "Stratifier",
    short = "Fits the selected models separately within each level of this variable."
  ),
  data_file = list(
    title = "Supported files",
    short = "CSV/TSV/TXT, Excel, RDS data frames, Stata (.dta), SPSS (.sav/.zsav/.por) and SAS (.sas7bdat/.xpt)."
  ),
  data_import = list(
    title = "Import options",
    short = "Delimited settings apply only to CSV/TSV/TXT files. The sheet field applies only to Excel; leave it blank for the first sheet."
  ),
  data_templates = list(
    title = "Quick-start templates",
    short = "Templates load labelled teaching data and preselect common variables. They are safe to overwrite by choosing another dataset."
  )
)

# Where help attaches: next to an input's label, or inside an element matched
# by a CSS selector. Help is used selectively, not beside every label.
gtw_help_targets <- list(
  list(help = "desc", selector = ".gtw-desc-settings > h3"),
  list(help = "desc_by", input = "desc_by"),
  list(help = "desc_statistic", input = "desc_statistic"),
  list(help = "reg", selector = ".gtw-reg-settings > h3"),
  list(help = "reg_approach", input = "reg_approach"),
  list(help = "reg_adjust", input = "reg_adjust"),
  list(help = "reg_interaction", input = "reg_interaction"),
  list(help = "reg_model_stats", input = "reg_model_stats"),
  list(help = "reg_stratifier", input = "reg_stratifier"),
  list(help = "data_file", input = "data_file"),
  list(help = "data_import", selector = ".gtw-options[data-gtw-options='data_import'] > summary .gtw-options-label"),
  list(help = "data_templates", selector = ".gtw-templates-title")
)

# Variable pickers: presentation layer over existing checkboxGroupInputs. The
# checkbox input stays authoritative; `conflicts` only annotates variables that
# are used in another role and mirrors the existing Select all exclusions.
gtw_picker_targets <- list(
  list(input = "desc_exposures", label = "Variables to summarise", conflicts = list(desc_by = "Group by")),
  list(input = "reg_exposures", label = "Exposures", conflicts = list(reg_outcome = "Outcome", reg_adjust = "Adjust for", reg_stratifier = "Stratifier")),
  list(input = "reg_adjust", label = "Adjust for", conflicts = list(reg_outcome = "Outcome", reg_exposures = "Exposures", reg_stratifier = "Stratifier"))
)

gtw_component_config <- function() {
  if (!gtw_components_enabled) return(NULL)
  targets <- gtw_help_targets
  registry <- gtw_help_registry
  pickers <- gtw_picker_targets
  if (isTRUE(get0("gta_shell_enabled", ifnotfound = FALSE))) {
    targets <- c(targets, list(
      list(help = "desc", selector = ".gta-describe .gta-result-title"),
      list(help = "reg", selector = ".gta-regression .gta-run-label"),
      list(help = "reg_references", selector = ".gta-refs-panel .gta-ref-help"),
      list(help = "surv_approach", selector = ".gta-survival .gta-surv-methods-help"),
      list(help = "surv_event_codes", input = "competing_event_of_interest"),
      list(help = "surv_multivariable", input = "surv_multivariable"),
      list(help = "more_method", selector = ".gta-more .gta-more-methods-help"),
      list(help = "reg_references", selector = "#gta-panel-more-refs .gta-ref-help"),
      list(help = "special_multivariable", input = "special_multivariable"),
      list(help = "trial", selector = ".gta-trials .gta-run-label"),
      list(help = "trial_event_type", input = "trial_event_type"),
      list(help = "structure_notes", selector = ".gta-structure-worth-help")
    ))
    pickers[[1]]$label <- "Variables"
  }
  config <- list(help = registry, helpTargets = targets, pickers = pickers)
  extra <- get0("gta_extra_picker_targets", ifnotfound = NULL)
  if (length(extra)) config$extraPickers <- extra
  tags$script(
    type = "application/json",
    id = "gtw-config",
    HTML(as.character(jsonlite::toJSON(config, auto_unbox = TRUE)))
  )
}

# Text that the component layer moves into contextual help. It is shown
# unchanged when components are off (ordinary web mode).
gtw_fallback_help <- function(...) {
  if (gtw_components_enabled) return(NULL)
  tagList(...)
}

# Options disclosure with a mandatory summary of every non-default setting it
# contains (the summary is computed from the live inputs by the component JS).
gtw_options <- function(..., id, label = "Options") {
  if (!gtw_components_enabled) return(tagList(...))
  tags$details(
    class = "gtw-options",
    `data-gtw-options` = id,
    tags$summary(
      icon("sliders", `aria-hidden` = "true"),
      span(class = "gtw-options-label", label),
      span(class = "gtw-options-state", `aria-live` = "polite", "defaults")
    ),
    div(class = "gtw-options-body", ...)
  )
}

gtw_options_group <- function(title, ...) {
  if (!gtw_components_enabled) return(tagList(...))
  div(class = "gtw-options-group", div(class = "gtw-options-group-title", title), ...)
}

# Methodological caution: always visible in concise form; the full existing
# text remains one click away under Details.
gtw_caution <- function(summary, details) {
  if (!gtw_components_enabled) return(details)
  div(
    class = "gtw-caution-box",
    role = "note",
    icon("triangle-exclamation", `aria-hidden` = "true"),
    div(
      class = "gtw-caution-text",
      summary,
      tags$details(class = "gtw-caution-details", tags$summary("Details"), details)
    )
  )
}

# Result toolbar: Copy R code (existing generated code), Export (the existing
# download buttons of the active result, shown as a menu) and Focus Results.
gtw_result_toolbar <- function() {
  if (!gtw_components_enabled) return(NULL)
  div(
    class = "gtw-toolbar",
    role = "toolbar",
    `aria-label` = "Result actions",
    tags$button(
      type = "button", class = "btn btn-default gtw-tb-btn gtw-tb-copy",
      `aria-label` = "Copy R code", `data-gtw-tip` = "Copy R code",
      icon("copy", `aria-hidden` = "true")
    ),
    tags$button(
      type = "button", class = "btn btn-default gtw-tb-btn gtw-tb-export",
      `aria-label` = "Export result", `data-gtw-tip` = "Export result",
      `aria-haspopup` = "menu", `aria-expanded` = "false",
      icon("download", `aria-hidden` = "true"),
      icon("caret-down", class = "gtw-tb-caret", `aria-hidden` = "true")
    ),
    tags$button(
      type = "button", class = "btn btn-default gtw-tb-btn gtw-tb-focus",
      `aria-label` = "Focus results", `data-gtw-tip` = "Focus results (Esc to return)",
      `aria-pressed` = "false", onclick = "gtwToggleFocus()",
      icon("up-right-and-down-left-from-center", `aria-hidden` = "true")
    )
  )
}

# Shared input definitions ----------------------------------------------------
# Each authoritative input is defined once and placed by whichever layout is
# active (D2 workbench, web, or the shared GT Analysis shell), so IDs, labels,
# choices and defaults cannot drift between presentations.
gtx_data_environment_controls <- function(...) {
  tagList(
    uiOutput("environment_dataset_input"),
    ...,
    actionButton("load_environment_data", "Load selected dataset", class = "btn-primary")
  )
}

gtx_data_example_controls <- function() {
  tagList(
    h4("Quick-start templates", class = "gtw-templates-title"),
    div(
      class = "gtx-template-grid",
      actionButton("template_birthwt", "Birthweight regression"),
      actionButton("template_lung", "Lung survival"),
      actionButton("template_diabetes", "Diabetes mediation"),
      actionButton("template_endometrial", "Firth logistic")
    ),
    gtw_fallback_help(gtx_inline_help("Templates load labelled teaching data and preselect common variables. They are safe to overwrite by choosing another dataset.")),
    tags$hr(),
    selectInput("dataset_name", "Built-in dataset", choices = c("", gtx_dataset_names())),
    actionButton("load_dataset", "Load built-in data", class = "btn-primary")
  )
}

gtx_data_file_controls <- function() {
  tagList(
    fileInput(
      "data_file",
      "Import a data file",
      accept = c(".csv", ".tsv", ".txt", ".xlsx", ".xls", ".rds", ".dta", ".sav", ".zsav", ".por", ".sas7bdat", ".xpt")
    ),
    gtw_fallback_help(gtx_inline_help("Supported: CSV/TSV/TXT, Excel, RDS data frames, Stata (.dta), SPSS (.sav/.zsav/.por), and SAS (.sas7bdat/.xpt).")),
    gtw_options(
      id = "data_import",
      label = "Import options",
      checkboxInput("csv_header", "Delimited file has header", TRUE),
      selectInput("csv_sep", "Delimited separator", choices = c("Automatic by extension" = "auto", "Comma" = ",", "Semicolon" = ";", "Tab" = "\t", "Whitespace" = ""), selected = "auto"),
      textInput("xlsx_sheet", "Excel sheet name/index", value = ""),
      gtw_fallback_help(gtx_inline_help("Delimited settings apply only to CSV/TSV/TXT files. The sheet field applies only to Excel; leave it blank for the first sheet."))
    ),
    actionButton("load_upload", "Load uploaded file", class = "btn-primary")
  )
}

gtx_reg_approach_input <- function() {
  selectInput("reg_approach", "Approach", choices = gtx_approaches, selected = "logit")
}

gtx_reg_table_option_inputs <- function() {
  tagList(
    checkboxInput("reg_show_ref", "Show reference categories", value = TRUE),
    checkboxInput("reg_model_stats", "Store model statistics", value = TRUE)
  )
}

gtx_reg_strata_controls <- function(run_label = "Run stratified regression") {
  tagList(
    uiOutput("reg_strata_inputs"),
    uiOutput("reg_strata_conflict"),
    checkboxInput("reg_strata_multi", "Stratified multivariable", value = FALSE),
    actionButton("run_strat", run_label, class = "btn-primary")
  )
}

gtx_surv_mode_input <- function() {
  radioButtons(
    "surv_mode",
    label = NULL,
    choices = c(
      "Cox regression" = "cox",
      "Parametric survival" = "survreg",
      "Competing risks" = "competing"
    ),
    selected = "cox",
    inline = TRUE
  )
}

gtx_surv_show_ref_input <- function() {
  checkboxInput("surv_show_ref", "Show reference categories", value = TRUE)
}

gtx_surv_multivariable_input <- function() {
  checkboxInput("surv_multivariable", "Single multivariable model", value = FALSE)
}

# More models inputs shared verbatim by the D2 pages and the D3 workspace.
gtx_special_mode_input <- function() {
  radioButtons(
    "special_mode",
    label = NULL,
    choices = c(
      "Ordered outcome" = "ordinal",
      "Unordered outcome" = "multinomial",
      "Clustered data" = "multilevel",
      "Matched case-control" = "matched"
    ),
    selected = "ordinal",
    inline = TRUE
  )
}

gtx_special_show_ref_input <- function() {
  checkboxInput("special_show_ref", "Show predictor reference categories", TRUE)
}

gtx_special_model_stats_input <- function() {
  checkboxInput("special_model_stats", "Store model statistics", TRUE)
}

gtx_med_approach_input <- function() {
  selectInput("med_approach", "Outcome model", choices = c("logit", "linear"), selected = "logit")
}

gtx_med_option_inputs <- function() {
  tagList(
    numericInput("med_sims", "Bootstrap replicates", value = 300, min = 50, step = 50),
    numericInput("med_seed", "Seed", value = 123, min = 1)
  )
}

gtx_km_plot_option_inputs <- function() {
  tagList(
    selectInput("km_theme", "KM theme", choices = c("classic", "minimal", "bw", "light", "none"), selected = "classic"),
    checkboxInput("km_y_percent", "Display y-axis as percent", TRUE),
    checkboxInput("km_grid", "Show grid", FALSE),
    checkboxInput("km_conf", "Show confidence interval", TRUE),
    checkboxInput("km_p", "Show log-rank p-value", TRUE),
    checkboxInput("km_risk", "Show risk table", TRUE),
    textInput("km_ylim", "Y limits", placeholder = "Optional: 50, 100"),
    textInput("km_xlim", "X limits", placeholder = "Optional: 0, 800")
  )
}

gtx_km_break_input <- function() {
  numericInput("km_break", "Time break interval", value = NA, min = 0)
}

gtx_cuminc_by_input <- function() {
  selectInput("cuminc_by", "Compare curves by", choices = c("None" = ""))
}

gtx_cuminc_plot_option_inputs <- function() {
  tagList(
    selectInput("cuminc_theme", "Plot theme", choices = c("classic", "minimal", "bw", "light"), selected = "classic"),
    checkboxInput("cuminc_y_percent", "Display y-axis as percent", TRUE),
    checkboxInput("cuminc_grid", "Show grid", FALSE),
    checkboxInput("cuminc_conf", "Show confidence interval", TRUE),
    checkboxInput("cuminc_p", "Show Gray's test p-value", TRUE),
    textInput("cuminc_ylim", "Y limits", placeholder = "Optional: 0, 50"),
    textInput("cuminc_xlim", "X limits", placeholder = "Optional: 0, 800"),
    numericInput("cuminc_break", "Time break interval", value = NA, min = 0)
  )
}

gtx_desc_option_inputs <- function() {
  tagList(
    selectInput("desc_percent", "Percent", choices = c("column", "row"), selected = "column"),
    selectInput("desc_overall", "Overall column", choices = c("no", "first", "last"), selected = "no"),
    selectInput("desc_missing", "Missing values", choices = c("ifany", "no"), selected = "ifany"),
    selectInput("desc_dich", "Binary display", choices = c("all_levels", "single_row"), selected = "all_levels"),
    textInput("desc_statistic", "Numeric statistic override", placeholder = 'Optional: age = mean, lwt = median'),
    gtw_fallback_help(gtx_inline_help("Use named overrides when a variable needs a different summary from the default."))
  )
}

gtx_data_source_panel <- function(source, ...) {
  content <- tagList(...)
  if (!gtx_addin_mode && identical(source, "environment")) return(NULL)
  if (!gtx_addin_mode) return(content)
  conditionalPanel(
    condition = sprintf("input.data_source == '%s'", source),
    content
  )
}

# Shared GT Analysis UI (D3) -------------------------------------------------
# One presentation architecture for every host (package GUI, web, future
# standalone). Hosts differ only through the capability profile below; the
# components themselves never branch on the host. Presentation only: every
# control is an existing authoritative Shiny input or output, placed in a
# different region. No statistical, validation or code-generation behaviour
# depends on this layer.
gta_host_profiles <- list(
  local = list(
    id = "local",
    sources = c("R Environment" = "environment", "Import file" = "file", "Example data" = "example"),
    default_source = "environment",
    close_app = TRUE,
    refresh_environment = TRUE,
    open_data_when_empty = TRUE,
    empty_data_text = "Choose a data frame from your R environment, import a file, or start from an example."
  ),
  web = list(
    id = "web",
    sources = c("Import file" = "file", "Example data" = "example"),
    default_source = "file",
    close_app = FALSE,
    refresh_environment = FALSE,
    open_data_when_empty = TRUE,
    empty_data_text = "Import a data file or start from an example."
  ),
  standalone = list(
    id = "standalone",
    sources = c("Import file" = "file", "Example data" = "example"),
    default_source = "file",
    close_app = FALSE,
    refresh_environment = FALSE,
    open_data_when_empty = TRUE,
    empty_data_text = "Import a data file or start from an example."
  )
)
gta_host <- gta_host_profiles[[if (gtx_addin_mode) "local" else "web"]]

# Feature switch. The package GUI uses the shared D3 shell. The D2 workbench is
# temporary migration scaffolding (equivalence testing only), reachable with
# options(gtregression.gui_layout = "d2"); the older option name
# gtregression.addin_layout is still honoured.
# Web mode keeps its current layout until the web host migration (M7).
gta_shell_enabled <- gtx_addin_mode &&
  gtw_components_enabled &&
  !identical(getOption("gtregression.gui_layout", getOption("gtregression.addin_layout", "shared")), "d2")

# Icon-only button: accessible name plus hover/focus tooltip.
gta_icon_button <- function(icon_name, label, ..., class = NULL) {
  tags$button(
    type = "button",
    class = paste(c("gta-icon-btn", class), collapse = " "),
    `aria-label` = label,
    `data-gtw-tip` = label,
    ...,
    icon(icon_name, `aria-hidden` = "true")
  )
}

gta_data_chip <- function() {
  tags$button(
    type = "button",
    id = "gta-data-chip",
    class = "gta-chip",
    `aria-haspopup` = "dialog",
    `aria-controls` = "gta-data",
    `aria-label` = "Data: open the Data workspace",
    icon("table", `aria-hidden` = "true"),
    uiOutput("analysis_status", inline = TRUE)
  )
}

gta_global_actions <- function() {
  div(
    class = "gta-global-actions",
    gta_icon_button("code", "Session code", onclick = "gtwOpenTab('Session Code')", `data-gta-tab` = "Session Code"),
    gta_icon_button("circle-question", "Help", onclick = "gtwOpenTab('Help')", `data-gta-tab` = "Help"),
    if (isTRUE(gta_host$close_app)) {
      actionButton(
        "close_app",
        label = span(class = "gtw-sr-only", "Close GT Regression"),
        icon = icon("power-off", `aria-hidden` = "true"),
        class = "gta-icon-btn gta-close",
        `aria-label` = "Close GT Regression",
        `data-gtw-tip` = "Close GT Regression"
      )
    }
  )
}

# Bottom panel: a temporary contextual surface above the bottom toolbar.
# Only one is open at a time. `summary` is a live one-line state.
gta_bottom_panel <- function(id, title, ..., summary = NULL, actions = NULL, class = NULL, body_class = NULL, attrs = list()) {
  panel <- div(
    id = id,
    class = paste(c("gta-panel", class), collapse = " "),
    role = "region",
    `aria-label` = title,
    tabindex = "-1",
    `data-gta-panel` = "bottom",
    div(
      class = "gta-panel-head",
      span(class = "gta-panel-title", title),
      summary,
      span(class = "gta-panel-actions", actions),
      gta_icon_button("xmark", paste("Close", title), class = "gta-panel-close", `data-gta-close` = id)
    ),
    div(class = paste(c("gta-panel-body", body_class), collapse = " "), ...)
  )
  if (length(attrs)) panel <- do.call(tagAppendAttributes, c(list(panel), attrs))
  panel
}

gta_panel_toggle <- function(panel_id, icon_name, label, badge = FALSE) {
  tags$button(
    type = "button",
    class = "gta-tb",
    `data-gta-toggle` = panel_id,
    `aria-controls` = panel_id,
    `aria-expanded` = "false",
    icon(icon_name, `aria-hidden` = "true"),
    span(class = "gta-tb-label", label),
    if (badge) span(class = "gta-badge", hidden = NA, `aria-hidden` = "true"),
    icon("caret-up", class = "gta-caret", `aria-hidden` = "true")
  )
}

gta_empty_state <- function(state, icon_name, title, text, action = NULL) {
  div(
    class = "gta-empty-state",
    `data-gta-for` = state,
    icon(icon_name, class = "gta-empty-icon", `aria-hidden` = "true"),
    div(class = "gta-empty-title", title),
    div(class = "gta-empty-text", text),
    action
  )
}

gta_source_panel <- function(source, ...) {
  if (!source %in% gta_host$sources) return(NULL)
  conditionalPanel(condition = sprintf("input.data_source == '%s'", source), ...)
}

# D3 Prepare: variable-first, action-driven preparation over the existing
# Data Prep module. Every control is an existing module input or output
# (namespace data_prep); the transformation engine is unchanged.
gta_prepare_workspace <- function(id = "data_prep") {
  ns <- NS(id)
  menu <- function(operation) tags$button(type = "button", class = "gta-hidden-menu", id = ns(paste0("menu_", operation)), class = "action-button", tabindex = "-1", `aria-hidden` = "true")
  action <- function(key, label, title, extra_class = NULL) {
    tags$button(
      type = "button", class = paste(c("gta-prep-action", extra_class), collapse = " "),
      `data-gta-prep-action` = key, `data-gta-prep-title` = title, label
    )
  }
  div(
    class = "gta-prep",
    tags$h3(class = "gtw-sr-only", "Data Prep"),
    # Operations are chosen through the module's existing menu inputs.
    div(class = "gta-hidden-input", lapply(c("label", "rename", "recode", "type", "missing", "calculation", "groups", "filter", "keep"), menu)),
    div(
      class = "gta-prep-head",
      uiOutput("prep_state", inline = TRUE),
      div(
        class = "gta-prep-head-tools",
        tags$button(type = "button", class = "gta-prep-tool action-button", id = ns("undo"), `aria-label` = "Undo last change", title = "Undo last change", icon("rotate-left", `aria-hidden` = "true"), span("Undo")),
        tags$button(type = "button", class = "gta-prep-tool action-button", id = ns("redo"), `aria-label` = "Redo", title = "Redo", icon("rotate-right", `aria-hidden` = "true"), span("Redo")),
        tags$button(type = "button", class = "gta-prep-tool gta-prep-reset action-button", id = ns("reset"), title = "Reset all changes (asks first)", "Reset"),
        span(class = "gta-prep-sep", `aria-hidden` = "true"),
        tags$button(type = "button", class = "gta-prep-tool", `data-gta-pop` = "gta-prep-changes", `aria-controls` = "gta-prep-changes", `aria-expanded` = "false", icon("list-ol", `aria-hidden` = "true"), span("Changes")),
        tags$button(type = "button", class = "gta-prep-tool", `data-gta-pop` = "gta-prep-code", `aria-controls` = "gta-prep-code", `aria-expanded` = "false", icon("code", `aria-hidden` = "true"), span("Code")),
        tags$button(type = "button", class = "gta-prep-tool", `data-gta-pop` = "gta-prep-export", `aria-controls` = "gta-prep-export", `aria-expanded` = "false", icon("download", `aria-hidden` = "true"), span("Export"))
      )
    ),
    div(
      class = "gta-prep-body",
      div(
        class = "gta-prep-side",
        div(
          class = "gta-prep-search",
          icon("magnifying-glass", `aria-hidden` = "true"),
          tags$input(type = "search", id = "gta-prep-search", placeholder = "Search variables", `aria-label` = "Search variables", autocomplete = "off")
        ),
        div(class = "gta-prep-list", uiOutput("prep_variables")),
        div(
          class = "gta-prep-focus", `aria-live` = "polite",
          div(class = "gta-prep-focus-empty", "Select a variable to change it."),
          div(
            class = "gta-prep-focus-body",
            div(
              class = "gta-prep-focus-head",
              span(class = "gta-prep-focus-name"),
              tags$button(type = "button", class = "gta-text-btn gta-prep-to-structure", title = "See this variable in Structure", "Structure")
            ),
            div(class = "gta-prep-focus-type"),
            div(
              class = "gta-prep-actions", role = "group", `aria-label` = "Change this variable",
              action("rename", "Rename", "Rename"),
              action("label", "Label", "Label"),
              action("recode", "Recode", "Recode", "gta-prep-needs-few"),
              action("type", "Change type", "Change type"),
              action("missing", "Missing values", "Missing values")
            ),
            div(
              class = "gta-prep-quick",
              span(class = "gta-prep-quick-label", "Quick start"),
              actionButton(ns("template_binary"), "Recode 0/1 as No/Yes", class = "gta-prep-quick-btn", `data-gta-prep-quick` = "zero-one", `data-gta-prep-sheet` = "recode"),
              actionButton(ns("template_missing"), "Treat 99/999 as missing", class = "gta-prep-quick-btn", `data-gta-prep-quick` = "codes", `data-gta-prep-sheet` = "missing"),
              actionButton(ns("template_age"), "Make age groups", class = "gta-prep-quick-btn", `data-gta-prep-quick` = "age", `data-gta-prep-sheet` = "groups")
            )
          )
        )
      ),
      div(
        class = "gta-prep-main",
        div(
          class = "gta-prep-main-head",
          span(class = "gta-prep-main-title", "Working data"),
          div(
            class = "gta-prep-dataset", role = "group", `aria-label` = "Change the dataset",
            action("calculation", tagList(icon("plus", `aria-hidden` = "true"), " Create variable"), "Create variable"),
            action("groups", "Create groups", "Create groups"),
            action("filter", "Filter rows", "Filter rows"),
            action("keep", "Keep variables", "Keep variables")
          )
        ),
        div(class = "gta-prep-preview", gtx_data_output("prep_preview"))
      ),
      # One focused action sheet: the chosen operation's existing fields,
      # its Before-you-apply preview, Cancel and Apply.
      div(
        id = "gta-prep-sheet", class = "gta-prep-sheet", role = "dialog", `aria-labelledby` = "gta-prep-sheet-title", `aria-hidden` = "true",
        div(
          class = "gta-prep-sheet-head",
          span(class = "gta-prep-sheet-title", id = "gta-prep-sheet-title", "Change"),
          tags$button(
            type = "button", class = "gta-icon-btn gta-prep-sheet-help", `aria-label` = "About this change",
            `data-gta-pop` = "gta-prep-hint", `aria-controls` = "gta-prep-hint", `aria-expanded` = "false",
            icon("circle-info", `aria-hidden` = "true")
          ),
          actionButton(ns("template_bmi"), "BMI-style starter", class = "gta-prep-quick-btn gta-prep-sheet-quick", `data-gta-prep-quick` = "bmi"),
          gta_icon_button("xmark", "Cancel", class = "gta-prep-sheet-close", `data-gta-prep-cancel` = NA)
        ),
        div(
          class = "gta-prep-sheet-body",
          uiOutput(ns("controls")),
          uiOutput(ns("value_guide")),
          uiOutput(ns("impact_preview"))
        ),
        div(
          class = "gta-prep-sheet-foot",
          tags$button(type = "button", class = "btn btn-default", `data-gta-prep-cancel` = NA, "Cancel"),
          actionButton(ns("apply"), "Apply", class = "btn-primary")
        )
      )
    ),
    div(
      id = "gta-prep-hint", class = "gta-pop gta-prep-pop gta-prep-hint-pop", role = "dialog", `aria-label` = "About this change",
      uiOutput(ns("operation_hint"))
    ),
    div(
      id = "gta-prep-changes", class = "gta-pop gta-prep-pop", role = "dialog", `aria-label` = "Changes",
      div(class = "gta-pop-title", "Changes"),
      uiOutput("prep_changes")
    ),
    div(
      id = "gta-prep-code", class = "gta-pop gta-prep-pop gta-prep-code-pop", role = "dialog", `aria-label` = "Preparation code",
      div(
        class = "gta-code-head",
        span(class = "gta-section-title", "Preparation code"),
        tags$button(
          type = "button", class = "btn btn-default btn-sm gtx-copy",
          onclick = "gtxCopyCode('prep_code', this)",
          icon("copy", `aria-hidden` = "true"), " Copy R code"
        ),
        downloadButton(ns("download_code"), "Download .R", class = "btn-default btn-sm")
      ),
      verbatimTextOutput("prep_code")
    ),
    div(
      id = "gta-prep-export", class = "gta-pop gta-prep-pop", role = "dialog", `aria-label` = "Export prepared data",
      div(class = "gta-pop-title", "Export prepared data"),
      div(
        class = "gta-prep-export-row",
        selectInput(ns("prepared_format"), NULL, choices = c("CSV" = "csv", "Excel (.xlsx)" = "xlsx", "R data (.rds)" = "rds", "Stata (.dta)" = "dta"), selected = "csv", width = "160px"),
        downloadButton(ns("download_prepared"), "Download", class = "btn-default btn-sm")
      )
    )
  )
}

# M1: layered Data workspace (Import | View | Prepare | Structure).
gta_data_workspace <- function() {
  div(
    id = "gta-data",
    class = "gta-modal",
    role = "dialog",
    `aria-modal` = "true",
    `aria-labelledby` = "gta-data-title",
    `aria-hidden` = "true",
    div(class = "gta-scrim", `data-gta-close` = "gta-data"),
    div(
      class = "gta-modal-card",
      div(class = "gta-modal-title", id = "gta-data-title", "Data"),
      gta_icon_button("xmark", "Close Data workspace", class = "gta-modal-close", `data-gta-close` = "gta-data"),
      tabsetPanel(
        tabPanel(
          "Import",
          div(
            class = "gta-import",
            div(
              class = "gta-source-list",
              radioButtons("data_source", "Source", choices = gta_host$sources, selected = gta_host$default_source)
            ),
            div(
              class = "gta-source-form",
              gta_source_panel(
                "environment",
                div(class = "gta-form-title", "Data frame in the R environment"),
                gtx_data_environment_controls(
                  if (isTRUE(gta_host$refresh_environment)) {
                    actionButton(
                      "refresh_environment", "Refresh list",
                      icon = icon("rotate", `aria-hidden` = "true"),
                      class = "btn-default btn-sm gta-refresh",
                      title = "Look again for data frames in the R environment"
                    )
                  }
                )
              ),
              gta_source_panel("file", div(class = "gta-form-title", "Import a file"), gtx_data_file_controls()),
              gta_source_panel("example", div(class = "gta-form-title", "Example data"), gtx_data_example_controls())
            )
          )
        ),
        tabPanel(
          "View",
          div(class = "gtx-help", "First 10 rows only. Use Structure to inspect the full dataset before modelling."),
          gtx_data_output("data_preview")
        ),
        tabPanel("Prepare", div(class = "gta-prepare", role = "region", `aria-label` = "Data Prep", gta_prepare_workspace("data_prep"))),
        tabPanel(
          "Structure",
          # Describes the current analysis dataset (original or prepared).
          div(
            class = "gta-structure",
            div(
              class = "gta-structure-head",
              uiOutput("structure_overview"),
              tags$button(
                type = "button", class = "gta-text-btn gta-structure-code-btn",
                `data-gta-pop` = "gta-structure-code", `aria-controls` = "gta-structure-code", `aria-expanded` = "false",
                icon("code", `aria-hidden` = "true"), " Import code"
              )
            ),
            div(
              class = "gta-structure-body",
              div(class = "gta-structure-table", uiOutput("structure_table")),
              div(
                class = "gta-structure-detail", role = "region", `aria-label` = "Variable detail", `aria-live` = "polite",
                uiOutput("structure_detail")
              )
            ),
            div(
              id = "gta-structure-code",
              class = "gta-pop gta-structure-code-pop",
              role = "dialog",
              `aria-label` = "Import code",
              div(
                class = "gta-code-head",
                span(class = "gta-section-title", "Import code"),
                tags$button(
                  type = "button", class = "btn btn-default btn-sm gtx-copy",
                  onclick = "gtxCopyCode('data_code', this)",
                  icon("copy", `aria-hidden` = "true"), " Copy R code"
                )
              ),
              verbatimTextOutput("data_code")
            )
          )
        )
      ),
      div(
        class = "gta-modal-foot",
        span(class = "gta-data-status", `aria-live` = "polite"),
        tags$button(type = "button", class = "btn btn-primary gta-data-done", `data-gta-close` = "gta-data", "Done")
      )
    )
  )
}

# M2: Describe workspace (analysis bar, results, bottom toolbar, panels).
gta_describe_workspace <- function() {
  div(
    class = "gta-workspace gtw-results gta-describe",
    `data-gta-workspace` = "describe",
    `data-gta-state` = "nodata",
    div(
      class = "gta-bar",
      role = "region",
      `aria-label` = "Descriptive table definition",
      div(
        class = "gta-bar-row",
        div(class = "gta-bar-slots", uiOutput("desc_inputs")),
        div(class = "gta-bar-run", actionButton("run_desc", "Create table", class = "btn-primary"))
      ),
      div(class = "gta-ready", `aria-live` = "polite", span(class = "gta-ready-mark", `aria-hidden` = "true"), span(class = "gta-ready-text"))
    ),
    div(
      class = "gta-results",
      role = "region",
      `aria-label` = "Descriptive table result",
      div(
        class = "gta-results-head",
        div(class = "gta-result-tabs", span(class = "gta-result-tab gta-result-title is-active", "Table")),
        tags$button(
          type = "button", class = "gta-icon-btn gtw-tb-btn gtw-tb-focus",
          `aria-label` = "Focus results", `data-gtw-tip` = "Focus results (Esc to return)",
          `aria-pressed` = "false", onclick = "gtwToggleFocus()",
          icon("up-right-and-down-left-from-center", `aria-hidden` = "true")
        )
      ),
      div(
        class = "gta-results-body",
        div(class = "gtx-card", tagListDownload("desc", table = TRUE), gtx_table_output("desc_table")),
        div(
          class = "gta-empty",
          gta_empty_state(
            "nodata", "table", "No data loaded", gta_host$empty_data_text,
            tags$button(type = "button", class = "btn btn-primary", `data-gta-open` = "gta-data", "Choose data")
          ),
          gta_empty_state(
            "empty", "list-check", "Describe your data",
            "Choose the variables to summarise. Group by is optional; leave it as None for one overall summary.",
            tags$button(type = "button", class = "btn btn-default", `data-gta-pick` = "desc_exposures", icon("plus", `aria-hidden` = "true"), " Choose variables")
          ),
          gta_empty_state(
            "ready", "table-list", "Ready to create the table",
            "Select Create table to summarise the chosen variables. The table will appear here.",
            tags$button(type = "button", class = "btn btn-default", `data-gta-click` = "run_desc", "Create table")
          )
        )
      )
    ),
    gta_bottom_panel(
      "gta-panel-desc-options", "Options",
      gtx_desc_option_inputs(),
      summary = span(class = "gtw-options-state", `aria-live` = "polite", "defaults"),
      class = "gtw-options gta-options-panel",
      body_class = "gtw-options-body",
      attrs = list(`data-gtw-options` = "desc_options")
    ),
    gta_bottom_panel(
      "gta-panel-desc-code", "R code",
      verbatimTextOutput("desc_code"),
      summary = span(class = "gta-panel-note", "Reflects the current settings, including any not yet run."),
      actions = tags$button(
        type = "button", class = "btn btn-default btn-sm gtx-copy",
        onclick = "gtxCopyCode('desc_code', this)",
        icon("copy", `aria-hidden` = "true"), " Copy R code"
      ),
      class = "gta-code-panel"
    ),
    div(
      class = "gta-toolbar",
      role = "toolbar",
      `aria-label` = "Descriptive table actions",
      div(class = "gta-toolbar-group", gta_panel_toggle("gta-panel-desc-options", "sliders", "Options", badge = TRUE)),
      div(
        class = "gta-toolbar-group gta-toolbar-end gtw-toolbar",
        tags$button(
          type = "button", class = "gta-tb gtw-tb-btn gtw-tb-export",
          `aria-label` = "Export result", `data-gtw-tip-base` = "Export result",
          `aria-haspopup` = "menu", `aria-expanded` = "false",
          icon("download", `aria-hidden` = "true"),
          span(class = "gta-tb-label", "Export"),
          icon("caret-up", class = "gta-caret", `aria-hidden` = "true")
        ),
        gta_panel_toggle("gta-panel-desc-code", "code", "Code")
      )
    )
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
/* Result notes, stale banner, pre-run checks, locked choices, busy pill */
.gtx-result-note { margin: 0 0 10px; padding: 8px 12px; border-radius: 6px; font-size: 13px; line-height: 1.45; }
.gtx-result-note ul { margin: 4px 0 0; padding-left: 18px; }
.gtx-note-warn { background: #FFF6E8; border: 1px solid #F0D9B5; color: #6B4208; }
.gtx-note-info { background: #F6F6F3; border: 1px solid #E1E2DD; color: #4A4C48; }
.gtx-note-info summary { cursor: pointer; font-weight: 600; }
.gtx-note-stale { background: #EEF3F8; border: 1px solid #C9D7E6; color: #23415F; }
.gtx-checks { margin-top: 6px; color: #6B4208; }
.gtx-checks ul { margin: 4px 0 0; padding-left: 18px; }
.gtx-locked { opacity: 0.55; }
.gtx-locked input { cursor: not-allowed; }
.gtx-busy { position: fixed; left: 50%; top: 64px; transform: translate(-50%, -8px); z-index: 2000;
  background: #1F2328; color: #F3F4F1; padding: 6px 14px; border-radius: 999px; font-size: 13px;
  box-shadow: 0 4px 14px rgba(0,0,0,0.18); opacity: 0; pointer-events: none; transition: opacity 0.15s, transform 0.15s; }
.gtx-busy.is-on { opacity: 1; transform: translate(-50%, 0); }
html.gtx-busy-on button[id^='run_'] { pointer-events: none; opacity: 0.6; }
.gtw-picker-row-locked { opacity: 0.55; }
.gtx-history-list { margin: 6px 0 10px; padding-left: 18px; font-size: 13px; }
.gtx-history-list li { margin: 2px 0; }
.gtx-history-meta { color: #6A6C67; }
.gtx-session-save h4 { margin-top: 18px; }

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
	  background: transparent;
	  border: 0;
	  border-left: 2px solid #C9CAC6;
	  border-radius: 0;
	  padding: 4px 0 4px 10px;
	  color: var(--gtx-muted);
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

# Addin workbench presentation --------------------------------------------
# Presentation only. These rules and scripts are emitted only in Addin mode and
# every selector is scoped beneath html.gt-addin-workbench, using the semantic
# gtw-* classes below. They move and restyle existing UI; they do not create,
# rename, or remove any Shiny input or output.
#   gtw-nav       navigation pane (the existing main_nav navbar, as a rail)
#   gtw-main      workbench area to the right of the navigation pane
#   gtw-status    compact dataset/application status line
#   gtw-actions   compact title-line actions (Help, Close)
#   gtw-workbench-row / gtw-settings / gtw-results   settings and results panes
#   gtw-code-strip  compact per-result R code presentation
#   gtw-pane-split  tab panes whose settings and results scroll independently
#   gtw-about / gtw-caution / gtw-session / gtw-help-page  D1 presentation
# Design tokens (--gtw-*) are defined once on html.gt-addin-workbench.
gtw_addin_css <- HTML("
/* ---- Design tokens (Addin only) ---------------------------------------- */
html.gt-addin-workbench {
  --gtw-fs-title: 16px;
  --gtw-fs-section: 13.5px;
  --gtw-fs-body: 13px;
  --gtw-fs-label: 12.5px;
  --gtw-fs-help: 11.5px;
  --gtw-fs-mono: 12px;
  --gtw-space-1: 4px;
  --gtw-space-2: 8px;
  --gtw-space-3: 12px;
  --gtw-space-4: 16px;
  --gtw-radius: 4px;
  --gtw-input-h: 28px;
  --gtw-btn-h: 26px;
  --gtw-btn-sm-h: 22px;
  --gtw-rail-w: 152px;
  --gtw-settings-w: 316px;
  --gtw-status-h: 33px;
  --gtw-ink: #1C1D1B;
  --gtw-text: #2B2C2A;
  --gtw-muted: #696B66;
  --gtw-line: #DDDED9;
  --gtw-line-strong: #C6C7C2;
  --gtw-surface: #FFFFFF;
  --gtw-canvas: #F3F3F0;
  --gtw-subtle: #F7F7F4;
  --gtw-accent: #386A5A;
  --gtw-accent-soft: #E8F0EC;
  --gtw-primary: #262626;
  --gtw-danger: #9F3A2B;
  --gtw-mono: Menlo, Consolas, 'Liberation Mono', monospace;
}

/* ---- Frame: rail, status bar, main area --------------------------------- */
html.gt-addin-workbench body {
  font-size: var(--gtw-fs-body);
  line-height: 1.45;
  color: var(--gtw-text);
  background: var(--gtw-canvas);
  padding-left: var(--gtw-rail-w);
  height: 100vh;
  overflow: hidden;
}
html.gt-addin-workbench .gtw-nav {
  position: fixed;
  top: 0;
  bottom: 0;
  left: 0;
  width: var(--gtw-rail-w);
  min-height: 0;
  margin: 0;
  border: 0;
  border-radius: 0;
  overflow-y: auto;
  z-index: 1030;
  background: #232323 !important;
}
html.gt-addin-workbench .gtw-nav > .container-fluid { padding: 0; }
html.gt-addin-workbench .gtw-nav .navbar-header { float: none; margin: 0; }
html.gt-addin-workbench .gtw-nav .navbar-brand {
  float: none;
  display: block;
  height: auto;
  margin: 0 !important;
  padding: 12px 12px 6px 14px;
  font-size: 15px;
  line-height: 1.2;
}
html.gt-addin-workbench .gtw-nav .navbar-brand .gtx-badge {
  font-size: 9px;
  padding: 1px 5px;
  margin-left: 3px;
  vertical-align: 2px;
}
html.gt-addin-workbench .gtw-nav .gtw-nav-list {
  float: none;
  margin: 0;
  padding: 0 0 12px;
  display: flex;
  flex-direction: column;
}
html.gt-addin-workbench .gtw-nav .gtw-nav-list > li { float: none; }
html.gt-addin-workbench .gtw-nav .gtw-nav-list > li > a {
  padding: 4px 10px 4px 14px;
  font-size: var(--gtw-fs-body);
  font-weight: 500;
  line-height: 1.35;
  border-left: 3px solid transparent;
  color: #D8D8D4 !important;
}
html.gt-addin-workbench .gtw-nav .gtw-nav-list > li > a:hover { background: #2E2E2D !important; }
html.gt-addin-workbench .gtw-nav .gtw-nav-list > li.active > a {
  background: #111 !important;
  color: #fff !important;
  border-left-color: #7FB3A0;
}
html.gt-addin-workbench .gtw-nav .gtw-nav-group {
  padding: 11px 10px 2px 14px;
  color: #92938F;
  font-size: 10px;
  font-weight: 700;
  letter-spacing: 0.07em;
  text-transform: uppercase;
}
html.gt-addin-workbench .gtw-nav .gtw-nav-sep {
  margin: 12px 12px 6px 14px;
  border-top: 1px solid #3A3A38;
}
html.gt-addin-workbench .gtw-main {
  max-width: none;
  height: 100vh;
  display: flex;
  flex-direction: column;
  padding: 0 var(--gtw-space-3);
}
html.gt-addin-workbench .gtw-main > .row:first-child { flex: 0 0 auto; margin: 0 calc(-1 * var(--gtw-space-3)); }
html.gt-addin-workbench .gtw-main > .tab-content { flex: 1 1 auto; min-height: 0; }
html.gt-addin-workbench .gtw-main > .tab-content > .tab-pane.active {
  height: 100%;
  overflow-y: auto;
  padding: var(--gtw-space-3) 0;
}
html.gt-addin-workbench .gtw-main .gtx-workflow-bar { display: none; }
html.gt-addin-workbench .gtw-status {
  height: var(--gtw-status-h);
  margin: 0;
  padding: 8px 110px 7px 14px;
  background: var(--gtw-surface);
  border-bottom: 1px solid var(--gtw-line);
  font-size: var(--gtw-fs-label);
  line-height: 1.35;
  color: #50524E;
  white-space: nowrap;
  overflow: hidden;
  text-overflow: ellipsis;
}
html.gt-addin-workbench .gtw-status .gtw-status-name { color: var(--gtw-ink); font-weight: 700; }
html.gt-addin-workbench .gtw-status .gtw-status-sep { color: #A4A5A1; margin: 0 6px; }
html.gt-addin-workbench .gtw-status .gtw-status-empty { color: var(--gtw-muted); }
html.gt-addin-workbench .gtw-actions {
  position: fixed;
  top: 4px;
  right: 10px;
  z-index: 1040;
  display: flex;
  gap: var(--gtw-space-1);
}
html.gt-addin-workbench .gtw-main .gtw-actions .btn {
  height: 24px;
  padding: 0 9px;
  font-size: 12px;
  font-weight: 600;
  line-height: 22px;
}
html.gt-addin-workbench .gtw-main .gtw-actions #close_app:hover,
html.gt-addin-workbench .gtw-main .gtw-actions #close_app:focus {
  color: #fff;
  background: var(--gtw-danger);
  border-color: var(--gtw-danger);
}

/* ---- Split panes: settings | results ------------------------------------ */
html.gt-addin-workbench .gtw-main > .tab-content > .tab-pane.active.gtw-pane-split {
  display: flex;
  flex-direction: column;
  overflow: hidden;
  padding-bottom: var(--gtw-space-3);
}
html.gt-addin-workbench .gtw-pane-split > * { flex: 0 0 auto; }
html.gt-addin-workbench .gtw-workbench-row {
  display: flex;
  align-items: flex-start;
  gap: var(--gtw-space-3);
  margin: 0;
}
html.gt-addin-workbench .gtw-pane-split > .gtw-workbench-row {
  flex: 1 1 auto;
  min-height: 0;
  align-items: stretch;
}
html.gt-addin-workbench .gtw-workbench-row > .gtw-settings {
  float: none;
  flex: 0 0 var(--gtw-settings-w);
  width: var(--gtw-settings-w);
  max-width: var(--gtw-settings-w);
  padding: 0;
}
html.gt-addin-workbench .gtw-workbench-row > .gtw-results {
  float: none;
  flex: 1 1 auto;
  width: auto;
  min-width: 0;
  padding: 0;
}
html.gt-addin-workbench .gtw-pane-split .gtw-settings,
html.gt-addin-workbench .gtw-pane-split .gtw-results {
  overflow-y: auto;
  background: var(--gtw-surface);
  border: 1px solid var(--gtw-line);
  border-radius: var(--gtw-radius);
}
html.gt-addin-workbench .gtw-pane-split .gtw-results { padding: 0 var(--gtw-space-3) var(--gtw-space-3); }
html.gt-addin-workbench .gtw-settings > .gtx-card {
  position: static;
  max-height: none;
  margin: 0;
  padding: var(--gtw-space-3) var(--gtw-space-3) 0;
  border: 0;
  border-radius: 0;
  background: transparent;
  box-shadow: none;
}
html.gt-addin-workbench .gtw-settings > .gtx-card > :last-child:not(.gtw-run-bar) { margin-bottom: var(--gtw-space-3); }
html.gt-addin-workbench.gtw-focus .gtw-workbench-row > .gtw-settings { display: none; }

/* ---- Typography inside the workbench ------------------------------------ */
html.gt-addin-workbench .gtw-main .tab-pane h1,
html.gt-addin-workbench .gtw-main .tab-pane h2,
html.gt-addin-workbench .gtw-main .tab-pane h3 {
  font-size: var(--gtw-fs-title);
  font-weight: 700;
  line-height: 1.3;
  margin: 0 0 var(--gtw-space-2);
  color: var(--gtw-ink);
}
html.gt-addin-workbench .gtw-main .tab-pane h4,
html.gt-addin-workbench .gtw-main .tab-pane h5 {
  font-size: var(--gtw-fs-section);
  font-weight: 650;
  line-height: 1.3;
  margin: var(--gtw-space-3) 0 var(--gtw-space-2);
  color: var(--gtw-ink);
}
html.gt-addin-workbench .gtw-main .tab-pane p,
html.gt-addin-workbench .gtw-main .tab-pane li { font-size: var(--gtw-fs-body); }
html.gt-addin-workbench .gtw-main .gtx-help,
html.gt-addin-workbench .gtw-main .gtx-field-help,
html.gt-addin-workbench .gtw-main .help-copy {
  font-size: var(--gtw-fs-help);
  line-height: 1.4;
  color: var(--gtw-muted);
}
html.gt-addin-workbench .gtw-main .gtx-help { margin-bottom: var(--gtw-space-2); }
html.gt-addin-workbench .gtw-main .gtx-field-help { margin: -4px 0 var(--gtw-space-2); }
html.gt-addin-workbench .gtw-main .gtx-step,
html.gt-addin-workbench .gtw-main .gtx-note,
html.gt-addin-workbench .gtw-main .gtx-warning,
html.gt-addin-workbench .gtw-main .gtx-preflight {
  font-size: 12px;
  line-height: 1.4;
  padding: 6px 9px;
  margin: 6px 0 10px;
  border-radius: 0 var(--gtw-radius) var(--gtw-radius) 0;
}
html.gt-addin-workbench .gtw-main .gtw-caution {
  border-left: 3px solid #B45309;
  background: #FFF7EC;
  color: #5E3A10;
}

/* ---- Cards: calm panels, no page-style chrome ---------------------------- */
html.gt-addin-workbench .gtw-main .gtx-card {
  padding: var(--gtw-space-3);
  margin-bottom: var(--gtw-space-3);
  border-color: var(--gtw-line);
  border-radius: var(--gtw-radius);
  box-shadow: none;
}
html.gt-addin-workbench .gtw-results .gtx-card {
  padding: var(--gtw-space-2) 0 0;
  margin-bottom: var(--gtw-space-2);
  border: 0;
  border-radius: 0;
  background: transparent;
}
html.gt-addin-workbench .gtw-main .gtx-tool-menu {
  padding: 6px var(--gtw-space-3) 0;
  margin-bottom: var(--gtw-space-2);
}
html.gt-addin-workbench .gtw-main .gtx-tool-menu h3 {
  font-size: 10.5px;
  font-weight: 700;
  letter-spacing: 0.06em;
  text-transform: uppercase;
  color: var(--gtw-muted);
  margin: 2px 0 0;
}
html.gt-addin-workbench .gtw-main .gtx-tool-menu .shiny-options-group { gap: 0 18px; }
html.gt-addin-workbench .gtw-main .gtx-tool-menu .radio-inline {
  padding: 5px 2px 7px 20px;
  font-size: var(--gtw-fs-body);
  font-weight: 600;
}

/* ---- Controls ---------------------------------------------------------- */
html.gt-addin-workbench .gtw-main .form-group { margin-bottom: 10px; }
html.gt-addin-workbench .gtw-main .form-group > label,
html.gt-addin-workbench .gtw-main .control-label {
  font-size: var(--gtw-fs-label);
  font-weight: 600;
  margin-bottom: 3px;
  color: var(--gtw-ink);
}
html.gt-addin-workbench .gtw-main .form-control,
html.gt-addin-workbench .gtw-main .selectize-input {
  min-height: var(--gtw-input-h);
  font-size: var(--gtw-fs-body);
  border-radius: var(--gtw-radius);
}
html.gt-addin-workbench .gtw-main input.form-control,
html.gt-addin-workbench .gtw-main select.form-control {
  height: var(--gtw-input-h);
  padding: 3px 8px;
}
html.gt-addin-workbench .gtw-main .selectize-input { padding: 4px 26px 3px 8px; line-height: 19px; }
html.gt-addin-workbench .gtw-main .selectize-input.has-items { padding-top: 4px; padding-bottom: 3px; }
html.gt-addin-workbench .gtw-main .selectize-dropdown { font-size: var(--gtw-fs-body); }
html.gt-addin-workbench .gtw-main .input-group .btn,
html.gt-addin-workbench .gtw-main .input-group .form-control { height: var(--gtw-input-h); }
html.gt-addin-workbench .gtw-main .input-group .btn { padding: 3px 10px; line-height: 20px; }
html.gt-addin-workbench .gtw-main .shiny-input-container:not(.shiny-input-container-inline) { width: 100%; }
html.gt-addin-workbench .gtw-main .radio,
html.gt-addin-workbench .gtw-main .checkbox { margin-top: 2px; margin-bottom: 2px; }
html.gt-addin-workbench .gtw-main .radio label,
html.gt-addin-workbench .gtw-main .checkbox label {
  font-size: var(--gtw-fs-body);
  font-weight: 500;
  line-height: 1.35;
}
html.gt-addin-workbench .gtw-main .gtx-select-buttons { margin: 2px 0 var(--gtw-space-2); }
html.gt-addin-workbench .gtw-main .gtx-select-buttons .btn { min-height: 20px; height: 20px; padding: 0 7px; font-size: 11px; line-height: 18px; font-weight: 500; }

/* Variable lists: two readable columns; long names wrap, never truncate. */
html.gt-addin-workbench .gtw-main #desc_exposures .shiny-options-group,
html.gt-addin-workbench .gtw-main #reg_exposures .shiny-options-group,
html.gt-addin-workbench .gtw-main #reg_adjust .shiny-options-group,
html.gt-addin-workbench .gtw-main #special_exposures .shiny-options-group,
html.gt-addin-workbench .gtw-main #special_adjust .shiny-options-group,
html.gt-addin-workbench .gtw-main #surv_exposures .shiny-options-group,
html.gt-addin-workbench .gtw-main #surv_adjust .shiny-options-group,
html.gt-addin-workbench .gtw-main #med_covariates .shiny-options-group,
html.gt-addin-workbench .gtw-main #adv_exposures .shiny-options-group,
html.gt-addin-workbench .gtw-main #adv_covariates .shiny-options-group,
html.gt-addin-workbench .gtw-main #conf_exposures .shiny-options-group {
  column-count: 2;
  column-gap: 12px;
}
html.gt-addin-workbench .gtw-main .shiny-options-group .checkbox,
html.gt-addin-workbench .gtw-main .shiny-options-group .checkbox + .checkbox {
  break-inside: avoid;
  margin: 0 0 2px;
}
html.gt-addin-workbench .gtw-main .shiny-input-checkboxgroup > .control-label { margin-bottom: 4px; }
html.gt-addin-workbench .gtw-main .shiny-options-group .checkbox label,
html.gt-addin-workbench .gtw-main .shiny-options-group .radio label {
  white-space: normal;
  overflow-wrap: anywhere;
}

/* ---- Buttons: primary / secondary / exit -------------------------------- */
html.gt-addin-workbench .gtw-main .btn {
  min-height: var(--gtw-btn-h);
  padding: 3px 11px;
  font-size: var(--gtw-fs-label);
  font-weight: 600;
  line-height: 18px;
  border-radius: var(--gtw-radius);
}
html.gt-addin-workbench .gtw-main .btn-default {
  background: var(--gtw-surface);
  border-color: var(--gtw-line-strong);
  color: #30312F;
}
html.gt-addin-workbench .gtw-main .btn-default:hover,
html.gt-addin-workbench .gtw-main .btn-default:focus { background: var(--gtw-subtle); border-color: #8E908B; }
html.gt-addin-workbench .gtw-main .btn-primary,
html.gt-addin-workbench .gtw-main .btn-info {
  background: var(--gtw-primary);
  border-color: var(--gtw-primary);
  color: #fff;
}
html.gt-addin-workbench .gtw-main .btn-primary:hover,
html.gt-addin-workbench .gtw-main .btn-primary:focus { background: #111; border-color: #111; }
html.gt-addin-workbench .gtw-main .btn:focus-visible { outline: 2px solid var(--gtw-accent); outline-offset: 1px; }
html.gt-addin-workbench .gtw-settings .gtx-template-grid .btn { font-size: 12px; padding: 4px 6px; font-weight: 500; }

/* ---- Results: tabs, toolbar, tables, empty states ------------------------ */
html.gt-addin-workbench .gtw-results { position: relative; }
html.gt-addin-workbench .gtw-results > .tabbable > .nav-tabs {
  position: sticky;
  top: 0;
  z-index: 5;
  margin: 0 calc(-1 * var(--gtw-space-3)) var(--gtw-space-1);
  padding: 6px 118px 0 var(--gtw-space-3);
  background: var(--gtw-surface);
  border-bottom-color: var(--gtw-line);
}
html.gt-addin-workbench .gtw-results .nav-tabs > li > a {
  padding: 5px 11px;
  font-size: var(--gtw-fs-body);
  font-weight: 600;
  border-radius: var(--gtw-radius) var(--gtw-radius) 0 0;
}
html.gt-addin-workbench .gtw-results .tab-content .nav-tabs > li > a { padding: 4px 10px; font-size: var(--gtw-fs-label); }
html.gt-addin-workbench .gtw-results .download-strip {
  display: flex;
  justify-content: flex-end;
  flex-wrap: wrap;
  gap: var(--gtw-space-1);
  margin-bottom: var(--gtw-space-2);
}
html.gt-addin-workbench .gtw-results .download-strip .btn {
  min-height: var(--gtw-btn-sm-h);
  height: var(--gtw-btn-sm-h);
  margin: 0;
  padding: 0 8px;
  font-size: 11.5px;
  line-height: 20px;
  font-weight: 600;
}
html.gt-addin-workbench .gtw-results .download-strip .btn .fa,
html.gt-addin-workbench .gtw-results .download-strip .btn .fas,
html.gt-addin-workbench .gtw-results .download-strip .btn svg { display: none; }
html.gt-addin-workbench .gtw-results .gtx-card:has(> .download-strip ~ .shiny-html-output:empty) > .download-strip { display: none; }
html.gt-addin-workbench .gtw-results .gtx-card > .download-strip ~ .shiny-html-output:empty::before {
  content: 'Run the analysis to see results here.';
  display: block;
  padding: var(--gtw-space-2) 2px;
  color: var(--gtw-muted);
  font-size: var(--gtw-fs-label);
}
html.gt-addin-workbench .gtw-reg-results .gtx-card > .download-strip ~ .shiny-html-output:empty::before {
  content: 'Configure the model and select Run.';
}
html.gt-addin-workbench .gtw-results table.dataTable thead th { padding: 5px 20px 5px 8px; }
html.gt-addin-workbench .gtw-results table.dataTable tbody td { padding: 4px 8px; }
html.gt-addin-workbench .gtw-results table.dataTable thead th,
html.gt-addin-workbench .gtw-results table.dataTable tbody td {
  font-size: var(--gtw-fs-label);
  white-space: nowrap;
}
html.gt-addin-workbench .gtw-results .dataTables_wrapper { font-size: var(--gtw-fs-label); }
html.gt-addin-workbench .gtw-main pre {
  font-family: var(--gtw-mono);
  font-size: var(--gtw-fs-mono);
  line-height: 1.5;
  padding: 8px 10px;
  border-color: var(--gtw-line);
  border-radius: var(--gtw-radius);
  background: var(--gtw-subtle);
}
html.gt-addin-workbench .gtw-main .gtx-code-head h4 { margin: 0; }

/* ---- Focus Results ------------------------------------------------------ */
html.gt-addin-workbench .gtw-focus-toggle {
  position: sticky;
  top: 6px;
  float: right;
  z-index: 6;
  margin: 6px 0 -30px;
  height: var(--gtw-btn-sm-h);
  min-height: var(--gtw-btn-sm-h) !important;
  padding: 0 8px !important;
  font-size: 11.5px !important;
  line-height: 20px !important;
}

/* ---- Pinned run area ----------------------------------------------------- */
html.gt-addin-workbench .gtw-run-bar {
  position: sticky;
  bottom: 0;
  z-index: 2;
  margin: var(--gtw-space-2) calc(-1 * var(--gtw-space-3)) 0;
  padding: var(--gtw-space-2) var(--gtw-space-3) 10px;
  background: var(--gtw-surface);
  border-top: 1px solid var(--gtw-line);
}
html.gt-addin-workbench .gtw-run-bar .gtx-preflight { margin: 0 0 6px; }
html.gt-addin-workbench .gtw-run-bar .gtx-run-group { margin-top: 0; }
html.gt-addin-workbench .gtw-run-bar .btn { width: 100%; }
html.gt-addin-workbench .gtw-run-bar .gtx-run-group .btn { padding: 4px 6px; }

/* ---- Regression specifics (D0 order preserved) --------------------------- */
html.gt-addin-workbench .gtw-reg-settings { display: flex; flex-direction: column; }
html.gt-addin-workbench .gtw-reg-settings > * { order: 5; }
html.gt-addin-workbench .gtw-reg-settings > h3 { order: 1; }
html.gt-addin-workbench .gtw-reg-settings > .gtw-reg-guidance { order: 2; }
html.gt-addin-workbench .gtw-reg-settings > #reg_inputs { order: 3; }
html.gt-addin-workbench .gtw-reg-settings > .gtw-reg-approach { order: 4; }
html.gt-addin-workbench .gtw-reg-settings #reg_reference_inputs:not(:empty) {
  margin: 2px 0 10px;
  padding: 2px 10px 2px;
  background: var(--gtw-subtle);
  border: 1px solid var(--gtw-line);
  border-radius: var(--gtw-radius);
}
html.gt-addin-workbench .gtw-reg-settings #reg_reference_inputs hr { display: none; }
html.gt-addin-workbench .gtw-reg-settings #reg_reference_inputs h4 { margin: 8px 0 2px; }

/* ---- Code strips ---------------------------------------------------------- */
html.gt-addin-workbench .gtw-code-strip {
  position: sticky;
  bottom: 0;
  z-index: 4;
  margin: var(--gtw-space-2) calc(-1 * var(--gtw-space-3)) 0;
}
html.gt-addin-workbench .gtw-results .gtw-code-strip > .gtx-card {
  padding: 6px var(--gtw-space-3) 8px;
  margin: 0;
  background: var(--gtw-subtle);
  border-top: 1px solid var(--gtw-line-strong);
}
html.gt-addin-workbench .gtw-code-strip .gtx-code-head { margin: 0; gap: var(--gtw-space-2); }
html.gt-addin-workbench .gtw-main .gtw-code-strip .gtx-code-head h4 {
  flex: 0 0 auto;
  margin: 0;
  font-size: 10.5px;
  font-weight: 700;
  letter-spacing: 0.06em;
  text-transform: uppercase;
  color: var(--gtw-muted);
}
html.gt-addin-workbench .gtw-code-strip .gtw-code-summary {
  flex: 1 1 auto;
  min-width: 0;
  overflow: hidden;
  text-overflow: ellipsis;
  white-space: nowrap;
  font-family: var(--gtw-mono);
  font-size: var(--gtw-fs-mono);
  color: #2F302E;
}
html.gt-addin-workbench .gtw-code-strip .gtw-code-summary::before { content: 'R> '; color: #7A7C77; }
html.gt-addin-workbench .gtw-main .gtw-code-strip .btn {
  min-height: var(--gtw-btn-sm-h);
  height: var(--gtw-btn-sm-h);
  padding: 0 8px;
  font-size: 11.5px;
  line-height: 20px;
}
html.gt-addin-workbench .gtw-main .gtw-code-strip pre {
  max-height: 0;
  margin: 0;
  padding: 0 10px;
  border-width: 0;
  overflow: hidden;
}
html.gt-addin-workbench .gtw-main .gtw-code-strip.gtw-open pre {
  max-height: 45vh;
  margin-top: 6px;
  padding: 8px 10px;
  border-width: 1px;
  overflow: auto;
  background: var(--gtw-surface);
}
html.gt-addin-workbench .gtw-code-strip .gtw-code-note { display: none; margin-top: 4px; font-size: var(--gtw-fs-help); color: var(--gtw-muted); }
html.gt-addin-workbench .gtw-code-strip.gtw-open .gtw-code-note { display: block; }

/* ---- Disclosures: options and About this analysis ------------------------ */
html.gt-addin-workbench .gtw-details { margin: 2px 0 10px; }
html.gt-addin-workbench .gtw-details > summary {
  display: list-item;
  cursor: pointer;
  font-size: var(--gtw-fs-label);
  font-weight: 600;
  color: #454743;
  padding: 3px 0;
}
html.gt-addin-workbench .gtw-details > summary:focus-visible { outline: 2px solid var(--gtw-accent); outline-offset: 1px; }
html.gt-addin-workbench .gtw-details-body {
  padding: 6px 0 0 12px;
  border-left: 2px solid var(--gtw-line);
  margin-left: 4px;
}
html.gt-addin-workbench .gtw-about { margin: -2px 0 10px; }
html.gt-addin-workbench .gtw-about > summary {
  font-size: var(--gtw-fs-help);
  font-weight: 600;
  color: var(--gtw-muted);
}
html.gt-addin-workbench .gtw-about .gtw-details-body { color: var(--gtw-text); }
html.gt-addin-workbench .gtw-tool-about {
  padding: 4px var(--gtw-space-3);
  margin-bottom: var(--gtw-space-2);
  background: var(--gtw-surface);
  border: 1px solid var(--gtw-line);
  border-radius: var(--gtw-radius);
}
html.gt-addin-workbench .gtw-tool-about .gtw-about { margin: 0; }
html.gt-addin-workbench .gtw-about .gtx-step-row { grid-template-columns: repeat(4, minmax(0, 1fr)); gap: 6px; margin-top: 6px; }
html.gt-addin-workbench .gtw-about .gtx-step-row .gtx-step { min-height: 0; margin: 0; }

/* ---- Session Code workspace ---------------------------------------------- */
html.gt-addin-workbench .gtw-session { padding: var(--gtw-space-3) var(--gtw-space-4); margin: 0; }
html.gt-addin-workbench .gtw-session-head {
  display: flex;
  align-items: center;
  gap: var(--gtw-space-3);
  padding-bottom: var(--gtw-space-2);
  margin-bottom: var(--gtw-space-2);
  border-bottom: 1px solid var(--gtw-line);
}
html.gt-addin-workbench .gtw-main .gtw-session-head h3 { margin: 0; }
html.gt-addin-workbench .gtw-session-sub { flex: 1 1 auto; color: var(--gtw-muted); font-size: var(--gtw-fs-label); }
html.gt-addin-workbench .gtw-session .gtx-card { padding: 0; margin: 0; border: 0; }
html.gt-addin-workbench .gtw-main .gtw-session .gtx-code-head { margin-bottom: 6px; }
html.gt-addin-workbench .gtw-main .gtw-session .gtx-code-head h4 {
  font-size: 10.5px;
  letter-spacing: 0.06em;
  text-transform: uppercase;
  color: var(--gtw-muted);
}
html.gt-addin-workbench .gtw-main .gtw-session pre {
  min-height: 240px;
  max-height: calc(100vh - 215px);
  overflow: auto;
  margin: 0;
  background: var(--gtw-surface);
}

/* ---- Help ------------------------------------------------------------------ */
html.gt-addin-workbench .gtw-help-page { max-width: 860px; padding: var(--gtw-space-3) var(--gtw-space-4); }
html.gt-addin-workbench .gtw-main .gtw-help-page h3 { margin-top: var(--gtw-space-4); }
html.gt-addin-workbench .gtw-main .gtw-help-page ol,
html.gt-addin-workbench .gtw-main .gtw-help-page ul { padding-left: 20px; }

/* ---- Narrower windows ------------------------------------------------------ */
@media (max-width: 1050px) {
  html.gt-addin-workbench { --gtw-settings-w: 280px; --gtw-rail-w: 140px; }
}
")

gtw_addin_js <- HTML("
document.documentElement.classList.add('gt-addin-workbench');
function gtwOpenTab(name) {
  var links = document.querySelectorAll('#main_nav a[data-value]');
  for (var i = 0; i < links.length; i++) {
    if (links[i].getAttribute('data-value') === name) { links[i].click(); return; }
  }
}
function gtwBuildShell() {
  var list = document.getElementById('main_nav');
  if (!list) return;
  var nav = list.closest('nav');
  if (nav) nav.classList.add('gtw-nav');
  list.classList.add('gtw-nav-list');
  if (nav && nav.nextElementSibling) nav.nextElementSibling.classList.add('gtw-main');
  // The shared GT Analysis shell builds its own task navigation.
  if (document.documentElement.classList.contains('gta-shell')) return;
  var groups = [
    ['Data', ['Data', 'data_prep']],
    ['Describe', ['Descriptive']],
    ['Model', ['Regression', 'Special Models', 'Survival', 'Mediation']],
    ['Output', ['Visualise & Export']],
    ['Tools', ['Advanced']],
    [null, ['Session Code', 'Help']]
  ];
  var items = {};
  Array.prototype.forEach.call(list.children, function(li) {
    var a = li.querySelector('a[data-value]');
    if (a) items[a.getAttribute('data-value')] = li;
    li.style.order = 900;
  });
  var order = 0;
  groups.forEach(function(group) {
    var label = document.createElement('li');
    label.setAttribute('aria-hidden', 'true');
    if (group[0]) {
      label.className = 'gtw-nav-group';
      label.textContent = group[0];
    } else {
      label.className = 'gtw-nav-sep';
    }
    label.style.order = order++;
    list.appendChild(label);
    group[1].forEach(function(name) {
      if (items[name]) items[name].style.order = order++;
    });
  });
}
// After a regression Run, bring the matching result tab forward only when
// the server confirms success with its existing 'table ready' notification.
// Errors, warnings, or no confirmation leave the current tab untouched.
var gtwPendingRun = null;
var gtwRunTabs = {
  run_uni: ['Univariate', 'Univariable table ready'],
  run_multi: ['Multivariable', 'Multivariable table ready'],
  run_strat: ['Stratified', 'Stratified table ready']
};
function gtwSelectResultTab(name) {
  var links = document.querySelectorAll('.gtw-reg-results .nav-tabs a[data-value]');
  for (var i = 0; i < links.length; i++) {
    if (links[i].getAttribute('data-value') === name) { window.jQuery(links[i]).tab('show'); return; }
  }
}
function gtwWatchRuns() {
  var $ = window.jQuery;
  if (!$) return;
  $(document).on('click', '#run_uni, #run_multi, #run_strat', function() {
    gtwPendingRun = { id: this.id, at: Date.now() };
  });
  $(document).on('shiny:message', function(event) {
    var note = event.message && event.message.notification;
    if (!gtwPendingRun || !note || note.type !== 'show' || !note.message) return;
    if (Date.now() - gtwPendingRun.at > 120000) { gtwPendingRun = null; return; }
    var target = gtwRunTabs[gtwPendingRun.id];
    var kind = note.message.type;
    var text = $('<div>').html(note.message.html || '').text();
    if (kind === 'message' && target && text.indexOf(target[1]) === 0) {
      gtwPendingRun = null;
      gtwSelectResultTab(target[0]);
    } else if (kind === 'error' || kind === 'warning') {
      gtwPendingRun = null;
    }
  });
}
// Focus Results: presentation only. Hides the settings pane; analysis state
// and results are untouched.
function gtwSetFocus(on) {
  document.documentElement.classList.toggle('gtw-focus', on);
  var buttons = document.querySelectorAll('.gtw-focus-toggle');
  for (var i = 0; i < buttons.length; i++) {
    buttons[i].textContent = on ? 'Back to settings' : 'Focus results';
  }
  var toolButtons = document.querySelectorAll('.gtw-tb-focus');
  for (var j = 0; j < toolButtons.length; j++) {
    var label = on ? 'Back to settings' : 'Focus results';
    toolButtons[j].setAttribute('aria-pressed', on ? 'true' : 'false');
    toolButtons[j].setAttribute('aria-label', label);
    toolButtons[j].setAttribute('data-gtw-tip', on ? label + ' (Esc)' : label + ' (Esc to return)');
    var glyph = toolButtons[j].querySelector('i');
    if (glyph) glyph.className = on ? 'fas fa-down-left-and-up-right-to-center' : 'fas fa-up-right-and-down-left-from-center';
  }
  window.dispatchEvent(new Event('resize'));
}
function gtwToggleFocus() {
  gtwSetFocus(!document.documentElement.classList.contains('gtw-focus'));
}
// Compact per-result R code strips: summarise the existing generated code on
// one line; the full code (and Copy) is unchanged.
function gtwSummariseCode(strip) {
  var pre = strip.querySelector('pre');
  var summary = strip.querySelector('.gtw-code-summary');
  if (!pre || !summary) return;
  var text = pre.textContent || '';
  var at = text.search(/^[A-Za-z.][A-Za-z0-9._]*\\s*<-\\s*[A-Za-z.][A-Za-z0-9._:]*\\(/m);
  var call = (at >= 0 ? text.slice(at) : text).replace(/\\s+/g, ' ').trim();
  summary.textContent = call || 'Complete the required inputs to generate R code.';
}
function gtwBuildCodeStrips() {
  var strips = document.querySelectorAll('.gtw-code-strip');
  Array.prototype.forEach.call(strips, function(strip) {
    var head = strip.querySelector('.gtx-code-head');
    if (!head || head.querySelector('.gtw-code-summary')) return;
    strip.title = 'R code for the current settings. It updates as inputs change.';
    var summary = document.createElement('span');
    summary.className = 'gtw-code-summary';
    var expand = document.createElement('button');
    expand.type = 'button';
    expand.className = 'btn btn-default btn-sm gtw-code-expand';
    expand.textContent = 'Expand';
    expand.addEventListener('click', function() {
      var open = strip.classList.toggle('gtw-open');
      expand.textContent = open ? 'Collapse' : 'Expand';
    });
    var h4 = head.querySelector('h4');
    head.insertBefore(summary, h4 ? h4.nextSibling : head.firstChild);
    head.insertBefore(expand, summary.nextSibling);
    var note = document.createElement('div');
    note.className = 'gtw-code-note';
    note.textContent = 'Reflects the current settings, including any not yet run.';
    strip.firstElementChild.appendChild(note);
    gtwSummariseCode(strip);
  });
  if (window.jQuery) {
    window.jQuery(document).on('shiny:value', function(event) {
      var strip = event.target && event.target.closest && event.target.closest('.gtw-code-strip');
      if (strip) setTimeout(function() { gtwSummariseCode(strip); }, 0);
    });
  }
}
// Mark tab panes containing a settings/results row so both panes can scroll
// independently. Panes without such a row scroll as a single page.
function gtwMarkSplitPanes() {
  var panes = document.querySelectorAll('.gtw-main > .tab-content > .tab-pane');
  Array.prototype.forEach.call(panes, function(pane) {
    for (var i = 0; i < pane.children.length; i++) {
      if (pane.children[i].classList.contains('gtw-workbench-row')) {
        pane.classList.add('gtw-pane-split');
        return;
      }
    }
  });
}
// Full-name hover text for checkbox/radio labels (presentation only).
function gtwTitleLabels(root) {
  var labels = (root || document).querySelectorAll('.gtw-main .checkbox label, .gtw-main .radio label');
  Array.prototype.forEach.call(labels, function(label) {
    if (!label.title) label.title = (label.textContent || '').trim();
  });
}
function gtwWatchLabels() {
  gtwTitleLabels(document);
  if (!window.MutationObserver) return;
  var pending = false;
  new MutationObserver(function() {
    if (pending) return;
    pending = true;
    setTimeout(function() { pending = false; gtwTitleLabels(document); }, 150);
  }).observe(document.body, { childList: true, subtree: true });
}
function gtwInit() {
  gtwBuildShell();
  gtwMarkSplitPanes();
  gtwWatchLabels();
  gtwWatchRuns();
  gtwBuildCodeStrips();
  document.addEventListener('keydown', function(event) {
    if (event.key === 'Escape' && document.documentElement.classList.contains('gtw-focus')) gtwSetFocus(false);
  });
}
if (document.readyState === 'loading') {
  document.addEventListener('DOMContentLoaded', gtwInit);
} else {
  gtwInit();
}
")

# Reusable workbench components: styles and behaviour (D2). Emitted only when
# gtw_components_enabled is TRUE; scoped beneath html.gtw-components so the
# same components could later be enabled in web mode.
gtw_components_css <- HTML("
/* ---- Reusable workbench components (scoped to html.gtw-components) ------ */
html.gtw-components {
  --gtwc-ink: #1C1D1B;
  --gtwc-text: #2B2C2A;
  --gtwc-muted: #696B66;
  --gtwc-line: #DDDED9;
  --gtwc-line-strong: #C6C7C2;
  --gtwc-surface: #FFFFFF;
  --gtwc-subtle: #F7F7F4;
  --gtwc-accent: #386A5A;
  --gtwc-accent-soft: #E8F0EC;
  --gtwc-radius: 4px;
  --gtwc-shadow: 0 6px 20px rgba(20, 20, 20, 0.14), 0 1px 3px rgba(20, 20, 20, 0.08);
}
html.gtw-components .gtw-sr-only {
  position: absolute;
  width: 1px;
  height: 1px;
  padding: 0;
  margin: -1px;
  overflow: hidden;
  clip: rect(0, 0, 0, 0);
  border: 0;
}

/* Tooltip for icon-only controls */
html.gtw-components .gtw-tip {
  position: fixed;
  z-index: 3000;
  max-width: 260px;
  padding: 4px 8px;
  font-size: 11.5px;
  line-height: 1.35;
  color: #fff;
  background: #262626;
  border-radius: var(--gtwc-radius);
  pointer-events: none;
}

/* Contextual help trigger and popover */
html.gtw-components .gtw-help-btn {
  display: inline-flex;
  align-items: center;
  justify-content: center;
  width: 20px;
  height: 20px;
  margin: 0 0 0 4px;
  padding: 0;
  border: 0;
  border-radius: 50%;
  background: transparent;
  color: #7C7E79;
  font-size: 12px;
  line-height: 1;
  vertical-align: -3px;
  cursor: pointer;
}
html.gtw-components .gtw-help-btn:hover,
html.gtw-components .gtw-help-btn[aria-expanded='true'] { color: var(--gtwc-accent); background: var(--gtwc-accent-soft); }
html.gtw-components .gtw-help-btn:focus-visible,
html.gtw-components .gtw-pop button:focus-visible,
html.gtw-components .gtw-tb-btn:focus-visible,
html.gtw-components .gtw-picker button:focus-visible,
html.gtw-components .gtw-options > summary:focus-visible,
html.gtw-components .gtw-caution-details > summary:focus-visible {
  outline: 2px solid var(--gtwc-accent);
  outline-offset: 1px;
}
html.gtw-components .gtw-pop {
  position: fixed;
  z-index: 2500;
  width: 280px;
  padding: 10px 12px;
  font-size: 12.5px;
  font-weight: 400;
  line-height: 1.45;
  text-transform: none;
  letter-spacing: 0;
  color: var(--gtwc-text);
  background: var(--gtwc-surface);
  border: 1px solid var(--gtwc-line-strong);
  border-radius: var(--gtwc-radius);
  box-shadow: var(--gtwc-shadow);
  white-space: normal;
}
html.gtw-components .gtw-pop[hidden] { display: none; }
html.gtw-components .gtw-pop-title { display: block; margin-bottom: 3px; font-weight: 700; color: var(--gtwc-ink); }
html.gtw-components .gtw-pop-more {
  margin-top: 6px;
  padding: 0;
  border: 0;
  background: none;
  color: var(--gtwc-accent);
  font-size: 12px;
  font-weight: 600;
  cursor: pointer;
}
html.gtw-components .gtw-pop-long { margin-top: 6px; padding-top: 6px; border-top: 1px solid var(--gtwc-line); color: var(--gtwc-muted); }
html.gtw-components .gtw-pop-long[hidden] { display: none; }

/* Methodological caution */
html.gtw-components .gtw-caution-box {
  display: flex;
  gap: 8px;
  align-items: flex-start;
  margin: 4px 0 12px;
  padding: 7px 10px;
  font-size: 12px;
  line-height: 1.4;
  color: #5E3A10;
  background: #FFF7EC;
  border-left: 3px solid #B45309;
  border-radius: 0 var(--gtwc-radius) var(--gtwc-radius) 0;
}
html.gtw-components .gtw-caution-box > .fa,
html.gtw-components .gtw-caution-box > .fas,
html.gtw-components .gtw-caution-box > svg { margin-top: 2px; color: #B45309; }
html.gtw-components .gtw-caution-details { margin-top: 3px; }
html.gtw-components .gtw-caution-details > summary {
  display: list-item;
  cursor: pointer;
  font-weight: 600;
  color: #7A4A12;
}
html.gtw-components .gtw-caution-details .gtx-note {
  margin: 4px 0 0;
  padding: 0;
  border: 0;
  background: none;
  color: inherit;
  font-size: 12px;
}

/* Options disclosure with mandatory changed-settings summary */
html.gtw-components .gtw-options {
  margin: 6px 0 10px;
  border: 1px solid var(--gtwc-line);
  border-radius: var(--gtwc-radius);
  background: var(--gtwc-surface);
}
html.gtw-components .gtw-options > summary {
  display: flex;
  align-items: baseline;
  gap: 6px;
  padding: 6px 10px;
  cursor: pointer;
  list-style: none;
  font-size: 12.5px;
  line-height: 1.35;
}
html.gtw-components .gtw-options > summary::-webkit-details-marker { display: none; }
html.gtw-components .gtw-options > summary::after {
  content: '';
  flex: 0 0 auto;
  margin-left: auto;
  width: 6px;
  height: 6px;
  border-right: 1.5px solid #6B6D68;
  border-bottom: 1.5px solid #6B6D68;
  transform: rotate(-45deg);
  align-self: center;
}
html.gtw-components .gtw-options[open] > summary::after { transform: rotate(45deg); }
html.gtw-components .gtw-options > summary > .fa,
html.gtw-components .gtw-options > summary > .fas,
html.gtw-components .gtw-options > summary > svg { color: #6B6D68; }
html.gtw-components .gtw-options-label { flex: 0 0 auto; font-weight: 650; color: var(--gtwc-ink); }
html.gtw-components .gtw-options-state { flex: 1 1 auto; min-width: 0; color: var(--gtwc-muted); overflow-wrap: anywhere; }
html.gtw-components .gtw-options.gtw-options-changed .gtw-options-state { color: #7A4A12; font-weight: 600; }
html.gtw-components .gtw-options-body { padding: 4px 10px 2px; border-top: 1px solid var(--gtwc-line); }
html.gtw-components .gtw-options-group { padding: 6px 0 2px; }
html.gtw-components .gtw-options-group + .gtw-options-group { border-top: 1px dashed var(--gtwc-line); }
html.gtw-components .gtw-options-group-title {
  margin: 2px 0 6px;
  font-size: 10.5px;
  font-weight: 700;
  letter-spacing: 0.06em;
  text-transform: uppercase;
  color: var(--gtwc-muted);
}

/* Result toolbar */
html.gtw-components .gtw-toolbar {
  position: sticky;
  top: 5px;
  float: right;
  z-index: 6;
  display: flex;
  gap: 4px;
  margin: 5px 0 -30px;
}
html.gtw-components .gtw-toolbar .gtw-tb-btn {
  display: inline-flex;
  align-items: center;
  justify-content: center;
  gap: 3px;
  min-width: 26px;
  min-height: 24px !important;
  height: 24px;
  padding: 0 6px !important;
  font-size: 12px !important;
  line-height: 1 !important;
}
html.gtw-components .gtw-toolbar .gtw-tb-caret { font-size: 9px; }
html.gtw-components .gtw-toolbar .gtw-tb-btn[aria-disabled='true'] { opacity: 0.45; cursor: default; }
html.gtw-components .gtw-toolbar .gtw-tb-focus[aria-pressed='true'] { background: var(--gtwc-accent-soft); border-color: var(--gtwc-accent); }
html.gtw-components .gtw-has-toolbar .download-strip:not(.gtw-export-menu) {
  position: absolute !important;
  width: 1px;
  height: 1px;
  margin: 0 !important;
  overflow: hidden;
  visibility: hidden;
  pointer-events: none;
}
html.gtw-components .gtw-has-toolbar .download-strip.gtw-export-menu {
  position: fixed;
  z-index: 2600;
  display: flex !important;
  flex-direction: column;
  align-items: stretch;
  gap: 2px;
  min-width: 130px;
  margin: 0;
  padding: 4px;
  background: var(--gtwc-surface);
  border: 1px solid var(--gtwc-line-strong);
  border-radius: var(--gtwc-radius);
  box-shadow: var(--gtwc-shadow);
}
html.gtw-components .gtw-has-toolbar .download-strip.gtw-export-menu .btn {
  display: flex !important;
  justify-content: flex-start;
  height: 26px !important;
  min-height: 26px !important;
  margin: 0;
  padding: 0 10px !important;
  border: 0;
  background: transparent;
  font-size: 12.5px !important;
  line-height: 26px !important;
  text-align: left;
}
html.gtw-components .gtw-has-toolbar .download-strip.gtw-export-menu .btn:hover,
html.gtw-components .gtw-has-toolbar .download-strip.gtw-export-menu .btn:focus { background: var(--gtwc-accent-soft); }
html.gtw-components .gtw-export-menu-title {
  padding: 3px 10px 4px;
  font-size: 10.5px;
  font-weight: 700;
  letter-spacing: 0.06em;
  text-transform: uppercase;
  color: var(--gtwc-muted);
}

/* Variable picker (presentation layer over an existing checkbox group) */
html.gtw-components .gtw-picker-host > .shiny-options-group { display: none !important; }
html.gtw-components .gtw-picker-host + .gtx-select-buttons { display: none !important; }
html.gtw-components .gtw-picker {
  display: flex;
  align-items: flex-start;
  gap: 4px;
  min-height: 30px;
  padding: 3px 3px 3px 4px;
  background: var(--gtwc-surface);
  border: 1px solid var(--gtwc-line-strong);
  border-radius: var(--gtwc-radius);
  cursor: pointer;
}
html.gtw-components .gtw-picker:focus-within { border-color: var(--gtwc-accent); }
html.gtw-components .gtw-picker-chips { display: flex; flex-wrap: wrap; gap: 3px; flex: 1 1 auto; min-width: 0; }
html.gtw-components .gtw-picker-empty { padding: 2px 4px; color: var(--gtwc-muted); font-size: 12.5px; line-height: 20px; }
html.gtw-components .gtw-chip {
  display: inline-flex;
  align-items: center;
  max-width: 100%;
  height: 22px;
  padding: 0 2px 0 7px;
  font-size: 12px;
  color: var(--gtwc-ink);
  background: var(--gtwc-accent-soft);
  border: 1px solid #C9DCD3;
  border-radius: 11px;
  cursor: default;
}
html.gtw-components .gtw-chip-text { overflow: hidden; text-overflow: ellipsis; white-space: nowrap; }
html.gtw-components .gtw-chip button {
  display: inline-flex;
  align-items: center;
  justify-content: center;
  width: 18px;
  height: 18px;
  margin-left: 2px;
  padding: 0;
  border: 0;
  border-radius: 50%;
  background: transparent;
  color: #50524E;
  font-size: 11px;
  cursor: pointer;
}
html.gtw-components .gtw-chip button:hover { background: #D4E4DC; color: var(--gtwc-ink); }
html.gtw-components .gtw-chip-more {
  height: 22px;
  padding: 0 8px;
  font-size: 12px;
  border: 1px dashed var(--gtwc-line-strong);
  border-radius: 11px;
  background: var(--gtwc-surface);
  color: var(--gtwc-muted);
  cursor: pointer;
}
html.gtw-components .gtw-picker-add {
  flex: 0 0 auto;
  width: 24px;
  height: 22px;
  padding: 0;
  border: 1px solid var(--gtwc-line-strong);
  border-radius: var(--gtwc-radius);
  background: var(--gtwc-subtle);
  color: #30312F;
  font-size: 11px;
  cursor: pointer;
}
html.gtw-components .gtw-picker-add:hover { background: var(--gtwc-accent-soft); border-color: var(--gtwc-accent); }
html.gtw-components .gtw-picker-pop {
  position: fixed;
  z-index: 2500;
  display: flex;
  flex-direction: column;
  width: 300px;
  max-height: 380px;
  font-size: 13px;
  background: var(--gtwc-surface);
  border: 1px solid var(--gtwc-line-strong);
  border-radius: var(--gtwc-radius);
  box-shadow: var(--gtwc-shadow);
}
html.gtw-components .gtw-picker-pop[hidden] { display: none; }
html.gtw-components .gtw-picker-head {
  display: flex;
  align-items: center;
  justify-content: space-between;
  padding: 8px 10px 4px;
  font-weight: 700;
  color: var(--gtwc-ink);
}
html.gtw-components .gtw-picker-head button,
html.gtw-components .gtw-picker-actions button,
html.gtw-components .gtw-picker-foot button {
  height: 24px;
  padding: 0 8px;
  font-size: 12px;
  border: 1px solid var(--gtwc-line-strong);
  border-radius: var(--gtwc-radius);
  background: var(--gtwc-surface);
  cursor: pointer;
}
html.gtw-components .gtw-picker-head button { width: 24px; padding: 0; border: 0; }
html.gtw-components .gtw-picker-search { padding: 2px 10px 6px; }
html.gtw-components .gtw-picker-search input {
  width: 100%;
  height: 28px;
  padding: 3px 8px;
  font-size: 13px;
  border: 1px solid var(--gtwc-line-strong);
  border-radius: var(--gtwc-radius);
}
html.gtw-components .gtw-picker-search input:focus { outline: none; border-color: var(--gtwc-accent); box-shadow: 0 0 0 2px var(--gtwc-accent-soft); }
html.gtw-components .gtw-picker-actions { display: flex; gap: 4px; padding: 0 10px 6px; }
html.gtw-components .gtw-picker-list {
  flex: 1 1 auto;
  min-height: 60px;
  overflow-y: auto;
  padding: 2px 4px;
  border-top: 1px solid var(--gtwc-line);
  border-bottom: 1px solid var(--gtwc-line);
}
html.gtw-components .gtw-picker-row {
  display: flex;
  align-items: flex-start;
  gap: 7px;
  margin: 0;
  padding: 4px 6px;
  font-weight: 400;
  border-radius: 3px;
  cursor: pointer;
}
html.gtw-components .gtw-picker-row:hover,
html.gtw-components .gtw-picker-row:focus-within { background: var(--gtwc-subtle); }
html.gtw-components .gtw-picker-row input { margin: 2px 0 0; flex: 0 0 auto; }
html.gtw-components .gtw-picker-row-name { flex: 1 1 auto; min-width: 0; overflow-wrap: anywhere; color: var(--gtwc-ink); }
html.gtw-components .gtw-picker-row-note { flex: 0 0 auto; font-size: 11px; color: #7A4A12; }
html.gtw-components .gtw-picker-row[hidden] { display: none; }
html.gtw-components .gtw-picker-none { padding: 8px; color: var(--gtwc-muted); font-size: 12px; }
html.gtw-components .gtw-picker-foot {
  display: flex;
  align-items: center;
  justify-content: space-between;
  padding: 6px 10px 8px;
  font-size: 12px;
  color: var(--gtwc-muted);
}
html.gtw-components .gtw-picker-foot .gtw-picker-done { background: #262626; border-color: #262626; color: #fff; font-weight: 600; }
")

gtw_components_js <- HTML("
// Reusable workbench components (presentation only). Reads and writes existing
// Shiny inputs; never creates analytical state of its own.
function gtwComponentsBoot() {
  var cfgEl = document.getElementById('gtw-config');
  if (!cfgEl) return;
  var cfg;
  try { cfg = JSON.parse(cfgEl.textContent); } catch (e) { return; }
  if (cfg.extraPickers) cfg.pickers = (cfg.pickers || []).concat(cfg.extraPickers);
  document.documentElement.classList.add('gtw-components');
  var uid = 0;
  function nextId(prefix) { uid += 1; return prefix + '-' + uid; }
  function el(tag, cls, text) {
    var n = document.createElement(tag);
    if (cls) n.className = cls;
    if (text !== undefined && text !== null) n.textContent = text;
    return n;
  }
  function icon(name) {
    var i = el('i', 'fas fa-' + name);
    i.setAttribute('aria-hidden', 'true');
    return i;
  }
  function placeBelow(node, anchor, alignRight) {
    var r = anchor.getBoundingClientRect();
    var w = node.offsetWidth || 280;
    var h = node.offsetHeight || 200;
    var left = alignRight ? r.right - w : r.left;
    left = Math.max(8, Math.min(left, window.innerWidth - w - 8));
    var top = r.bottom + 4;
    if (top + h > window.innerHeight - 8) top = Math.max(8, r.top - h - 4);
    node.style.left = left + 'px';
    node.style.top = top + 'px';
  }

  // ---- Tooltips for icon-only controls ([data-gtw-tip]) -------------------
  var tip = el('div', 'gtw-tip');
  tip.setAttribute('role', 'tooltip');
  tip.id = 'gtw-tip';
  tip.hidden = true;
  document.body.appendChild(tip);
  function showTip(target) {
    var text = target.getAttribute('data-gtw-tip');
    if (!text) return;
    tip.textContent = text;
    tip.hidden = false;
    placeBelow(tip, target, true);
  }
  function hideTip() { tip.hidden = true; }
  document.addEventListener('mouseover', function(e) {
    var t = e.target.closest && e.target.closest('[data-gtw-tip]');
    if (t) showTip(t);
  });
  document.addEventListener('mouseout', function(e) {
    var t = e.target.closest && e.target.closest('[data-gtw-tip]');
    if (t) hideTip();
  });
  document.addEventListener('focusin', function(e) {
    var t = e.target.closest && e.target.closest('[data-gtw-tip]');
    if (t) showTip(t); else hideTip();
  });
  document.addEventListener('focusout', hideTip);

  // ---- Contextual help ----------------------------------------------------
  var openHelp = null;
  function closeHelp(returnFocus) {
    if (!openHelp) return;
    openHelp.pop.hidden = true;
    openHelp.btn.setAttribute('aria-expanded', 'false');
    if (returnFocus) openHelp.btn.focus();
    openHelp = null;
  }
  function buildHelpButton(helpId) {
    var entry = cfg.help[helpId];
    if (!entry) return null;
    var wrap = el('span', 'gtw-help');
    wrap.setAttribute('data-gtw-help', helpId);
    var btn = el('button', 'gtw-help-btn');
    btn.type = 'button';
    btn.setAttribute('aria-label', 'Help: ' + entry.title);
    btn.setAttribute('aria-expanded', 'false');
    btn.appendChild(icon('circle-info'));
    var pop = el('div', 'gtw-pop');
    pop.id = nextId('gtw-help');
    pop.setAttribute('role', 'dialog');
    pop.setAttribute('aria-label', entry.title);
    pop.hidden = true;
    btn.setAttribute('aria-controls', pop.id);
    pop.appendChild(el('span', 'gtw-pop-title', entry.title));
    pop.appendChild(el('span', 'gtw-pop-short', entry.short));
    if (entry.more && entry.more !== entry.short) {
      var more = el('button', 'gtw-pop-more', 'More');
      more.type = 'button';
      var longText = el('div', 'gtw-pop-long', entry.more);
      longText.hidden = true;
      more.setAttribute('aria-expanded', 'false');
      more.addEventListener('click', function(ev) {
        ev.stopPropagation();
        longText.hidden = !longText.hidden;
        more.textContent = longText.hidden ? 'More' : 'Less';
        more.setAttribute('aria-expanded', longText.hidden ? 'false' : 'true');
        placeBelow(pop, btn, false);
      });
      pop.appendChild(document.createElement('br'));
      pop.appendChild(more);
      pop.appendChild(longText);
    }
    var hoverTimer = null;
    function open(pinned) {
      if (openHelp && openHelp.btn !== btn) closeHelp(false);
      pop.hidden = false;
      placeBelow(pop, btn, false);
      btn.setAttribute('aria-expanded', 'true');
      openHelp = { btn: btn, pop: pop, pinned: pinned };
    }
    btn.addEventListener('click', function(ev) {
      ev.preventDefault();
      ev.stopPropagation();
      if (openHelp && openHelp.btn === btn && openHelp.pinned) { closeHelp(false); return; }
      open(true);
    });
    btn.addEventListener('mouseenter', function() {
      clearTimeout(hoverTimer);
      hoverTimer = setTimeout(function() { if (!openHelp || openHelp.btn !== btn) open(false); }, 250);
    });
    btn.addEventListener('mouseleave', function() {
      clearTimeout(hoverTimer);
      if (openHelp && openHelp.btn === btn && !openHelp.pinned) closeHelp(false);
    });
    btn.addEventListener('focus', function() { if (!openHelp || openHelp.btn !== btn) open(false); });
    btn.addEventListener('blur', function() {
      setTimeout(function() {
        if (openHelp && openHelp.btn === btn && !openHelp.pinned && !pop.contains(document.activeElement)) closeHelp(false);
      }, 0);
    });
    pop.addEventListener('click', function(ev) { ev.stopPropagation(); });
    wrap.appendChild(btn);
    wrap.appendChild(pop);
    return wrap;
  }
  function helpAnchor(target) {
    if (target.selector) return { node: document.querySelector(target.selector), mode: 'append' };
    var input = document.getElementById(target.input);
    if (!input) return { node: null };
    if (input.type === 'checkbox') {
      var lab = input.closest('label');
      return { node: lab, mode: 'after' };
    }
    var label = document.getElementById(target.input + '-label') || document.querySelector('label[for=' + JSON.stringify(target.input) + ']');
    return { node: label, mode: 'after' };
  }
  function attachHelp() {
    (cfg.helpTargets || []).forEach(function(target) {
      var a = helpAnchor(target);
      if (!a.node) return;
      var existing = a.mode === 'append' ? a.node.querySelector('.gtw-help') : (a.node.nextElementSibling && a.node.nextElementSibling.classList.contains('gtw-help') ? a.node.nextElementSibling : null);
      if (existing) return;
      var w = buildHelpButton(target.help);
      if (!w) return;
      if (a.mode === 'append') a.node.appendChild(w);
      else a.node.parentNode.insertBefore(w, a.node.nextSibling);
    });
  }

  // ---- Options disclosure: summary of every non-default setting -----------
  var defaults = new WeakMap();
  function optionInputs(details) {
    var body = details.querySelector('.gtw-options-body');
    if (!body) return [];
    return Array.prototype.filter.call(body.querySelectorAll('select[id], input[id]'), function(n) {
      if (n.type === 'file' || n.type === 'hidden' || n.type === 'button' || n.type === 'submit') return false;
      if (n.classList.contains('selectize-input')) return false;
      return n.id && !n.closest('.selectize-control');
    });
  }
  function readValue(n) {
    if (n.type === 'checkbox') return n.checked ? 'on' : 'off';
    if (n.tagName === 'SELECT') {
      return Array.prototype.filter.call(n.options, function(o) { return o.selected; }).map(function(o) { return o.value; }).join('|');
    }
    return n.value || '';
  }
  function displayValue(n) {
    if (n.type === 'checkbox') return n.checked ? 'on' : 'off';
    if (n.tagName === 'SELECT') {
      var texts = Array.prototype.filter.call(n.options, function(o) { return o.selected; }).map(function(o) { return o.textContent.trim() || o.value; });
      if (n.selectize) {
        var v = n.selectize.getValue();
        var vals = Array.isArray(v) ? v : [v];
        texts = vals.map(function(x) { var o = n.selectize.options[x]; return o ? (o.label || o.text || x) : x; });
      }
      return texts.join(', ') || 'none';
    }
    return (n.value || '').trim() ? String.fromCharCode(8220) + n.value.trim() + String.fromCharCode(8221) : 'blank';
  }
  function labelFor(n) {
    if (n.type === 'checkbox') {
      var lab = n.closest('label');
      var sp = lab && lab.querySelector('span');
      return (sp ? sp.textContent : (lab ? lab.textContent : n.id)).trim();
    }
    var l = document.getElementById(n.id + '-label') || document.querySelector('label[for=' + JSON.stringify(n.id) + ']');
    return l ? l.firstChild && l.firstChild.nodeType === 3 ? l.firstChild.textContent.trim() : l.textContent.trim() : n.id;
  }
  function updateOptions(details) {
    var parts = [];
    optionInputs(details).forEach(function(n) {
      if (!defaults.has(n)) defaults.set(n, readValue(n));
      if (readValue(n) !== defaults.get(n)) parts.push(labelFor(n) + ': ' + displayValue(n));
    });
    var state = details.querySelector('.gtw-options-state');
    var text = parts.length ? parts.join(' ' + String.fromCharCode(183) + ' ') : 'defaults';
    if (state && state.textContent !== text) state.textContent = text;
    details.classList.toggle('gtw-options-changed', parts.length > 0);
    details.setAttribute('data-gtw-changed', String(parts.length));
  }
  function updateAllOptions() {
    Array.prototype.forEach.call(document.querySelectorAll('.gtw-options'), updateOptions);
  }

  // ---- Result toolbar -----------------------------------------------------
  function activeResult(bar) {
    var root = bar.closest('.gtw-results') || bar.parentNode;
    var host = root.querySelector('[data-gtw-result-root]') || root;
    var pane = host.querySelector(':scope > .tabbable > .tab-content > .tab-pane.active');
    return { root: root, pane: pane || host };
  }
  function resultState(bar) {
    var r = activeResult(bar);
    var pre = r.pane.querySelector('.gtw-code-strip pre[id], pre.shiny-text-output[id]');
    var strips = Array.prototype.slice.call(r.pane.querySelectorAll('.download-strip'));
    var strip = strips.filter(function(x) { return x.offsetParent !== null; })[0] || strips[0] || null;
    var output = strip && strip.parentNode.querySelector('.shiny-html-output, .shiny-plot-output');
    var hasResult = !!(output && (output.children.length > 0 || (output.textContent || '').trim()));
    return { pre: pre, strip: strip, hasCode: !!(pre && (pre.textContent || '').trim()), hasExport: !!(strip && hasResult) };
  }
  function setDisabled(btn, disabled, reason) {
    if (!btn) return;
    btn.setAttribute('aria-disabled', disabled ? 'true' : 'false');
    var base = btn.getAttribute('aria-label');
    btn.setAttribute('data-gtw-tip', disabled && reason ? base + ' (' + reason + ')' : btn.getAttribute('data-gtw-tip-base') || base);
  }
  function updateToolbars() {
    Array.prototype.forEach.call(document.querySelectorAll('.gtw-toolbar'), function(bar) {
      var root = bar.closest('.gtw-results');
      if (root) root.classList.add('gtw-has-toolbar');
      var s = resultState(bar);
      setDisabled(bar.querySelector('.gtw-tb-copy'), !s.hasCode, 'no R code yet');
      setDisabled(bar.querySelector('.gtw-tb-export'), !s.hasExport, 'run the analysis first');
    });
  }
  var openMenu = null;
  function closeMenu(returnFocus) {
    if (!openMenu) return;
    openMenu.strip.classList.remove('gtw-export-menu');
    openMenu.strip.removeAttribute('role');
    openMenu.strip.style.left = '';
    openMenu.strip.style.top = '';
    var title = openMenu.strip.querySelector('.gtw-export-menu-title');
    if (title) title.remove();
    Array.prototype.forEach.call(openMenu.strip.querySelectorAll('a, button'), function(a) { a.removeAttribute('role'); });
    openMenu.btn.setAttribute('aria-expanded', 'false');
    if (returnFocus) openMenu.btn.focus();
    openMenu = null;
  }
  function openExport(btn) {
    var bar = btn.closest('.gtw-toolbar');
    var s = resultState(bar);
    if (!s.hasExport) return;
    closeMenu(false);
    var strip = s.strip;
    var title = el('div', 'gtw-export-menu-title', 'Export as');
    title.setAttribute('aria-hidden', 'true');
    strip.insertBefore(title, strip.firstChild);
    strip.classList.add('gtw-export-menu');
    strip.setAttribute('role', 'menu');
    strip.setAttribute('aria-label', 'Export result');
    var items = strip.querySelectorAll('a, button');
    Array.prototype.forEach.call(items, function(a) { a.setAttribute('role', 'menuitem'); });
    placeBelow(strip, btn, true);
    btn.setAttribute('aria-expanded', 'true');
    openMenu = { strip: strip, btn: btn };
    if (items.length) items[0].focus();
  }
  function copyCode(btn) {
    var s = resultState(btn.closest('.gtw-toolbar'));
    if (!s.hasCode) return;
    var text = s.pre.textContent;
    var done = function() {
      btn.setAttribute('data-gtw-tip', 'R code copied');
      showTip(btn);
      setTimeout(function() { btn.setAttribute('data-gtw-tip', 'Copy R code'); }, 1400);
    };
    if (navigator.clipboard && navigator.clipboard.writeText) {
      navigator.clipboard.writeText(text).then(done).catch(function() { window.prompt('Copy this R code:', text); });
    } else {
      window.prompt('Copy this R code:', text);
    }
  }
  document.addEventListener('click', function(e) {
    var b = e.target.closest && e.target.closest('.gtw-tb-btn');
    if (b) {
      if (b.getAttribute('aria-disabled') === 'true') { e.preventDefault(); return; }
      if (b.classList.contains('gtw-tb-copy')) copyCode(b);
      if (b.classList.contains('gtw-tb-export')) {
        if (openMenu && openMenu.btn === b) closeMenu(false); else openExport(b);
        e.stopPropagation();
      }
      return;
    }
    if (openMenu) {
      if (openMenu.strip.contains(e.target)) {
        var item = e.target.closest('a, button');
        if (item && openMenu.strip.contains(item)) setTimeout(function() { closeMenu(false); }, 0);
      } else closeMenu(false);
    }
    if (openHelp && !openHelp.pop.contains(e.target)) closeHelp(false);
  });
  document.addEventListener('keydown', function(e) {
    if (e.key === 'Escape') {
      if (openMenu) { closeMenu(true); e.stopPropagation(); return; }
      if (openHelp) { closeHelp(true); e.stopPropagation(); return; }
    }
    if (openMenu && (e.key === 'ArrowDown' || e.key === 'ArrowUp')) {
      var items = Array.prototype.slice.call(openMenu.strip.querySelectorAll('a, button'));
      var i = items.indexOf(document.activeElement);
      i = e.key === 'ArrowDown' ? Math.min(items.length - 1, i + 1) : Math.max(0, i - 1);
      if (items[i]) items[i].focus();
      e.preventDefault();
    }
  }, true);
  window.addEventListener('resize', function() { closeMenu(false); closeHelp(false); });

  // ---- Variable picker ----------------------------------------------------
  var pickerState = {};
  var openPicker = null;
  function sourceBoxes(id) {
    var host = document.getElementById(id);
    return host ? Array.prototype.slice.call(host.querySelectorAll('input[type=checkbox]')) : [];
  }
  function boxLabel(box) {
    var sp = box.parentNode && box.parentNode.querySelector('span');
    return (sp ? sp.textContent : box.value).trim();
  }
  function setBox(box, checked) {
    if (box.checked === checked) return;
    box.checked = checked;
    box.dispatchEvent(new Event('change', { bubbles: true }));
  }
  function conflictNote(target, value) {
    var notes = [];
    Object.keys(target.conflicts || {}).forEach(function(otherId) {
      var other = document.getElementById(otherId);
      if (!other) return;
      var used = false;
      if (other.tagName === 'SELECT') used = readValue(other).split('|').indexOf(value) >= 0;
      else used = Array.prototype.some.call(other.querySelectorAll('input[type=checkbox]'), function(b) { return b.checked && b.value === value; });
      if (used) notes.push(target.conflicts[otherId]);
    });
    return notes.length ? 'used as ' + notes.join(', ') : '';
  }
  function renderChips(target) {
    var host = document.getElementById(target.input);
    if (!host || !host._gtwPicker) return;
    var p = host._gtwPicker;
    var boxes = sourceBoxes(target.input).filter(function(b) { return b.checked; });
    var expanded = !!(pickerState[target.input] && pickerState[target.input].expanded);
    var sig = boxes.map(function(b) { return b.value; }).join('|') + '#' + expanded + '#' + sourceBoxes(target.input).length;
    if (p.sig === sig && p.chips.childNodes.length) return;
    p.sig = sig;
    var chips = p.chips;
    chips.textContent = '';
    if (!boxes.length) chips.appendChild(el('span', 'gtw-picker-empty', 'None selected'));
    var limit = pickerState[target.input] && pickerState[target.input].expanded ? boxes.length : 8;
    boxes.slice(0, limit).forEach(function(b) {
      var name = boxLabel(b);
      var chip = el('span', 'gtw-chip');
      chip.title = name;
      var shown = name.length > 28 ? name.slice(0, 13) + String.fromCharCode(8230) + name.slice(-13) : name;
      chip.appendChild(el('span', 'gtw-chip-text', shown));
      var x = el('button');
      x.type = 'button';
      x.setAttribute('aria-label', 'Remove ' + name);
      x.appendChild(icon('xmark'));
      x.addEventListener('click', function(ev) { ev.stopPropagation(); setBox(b, false); p.add.focus(); });
      chip.appendChild(x);
      chips.appendChild(chip);
    });
    if (boxes.length > limit) {
      var more = el('button', 'gtw-chip-more', '+' + (boxes.length - limit) + ' more');
      more.type = 'button';
      more.addEventListener('click', function(ev) {
        ev.stopPropagation();
        pickerState[target.input] = { expanded: true };
        renderChips(target);
      });
      chips.appendChild(more);
    }
    p.live.textContent = boxes.length + ' selected';
  }
  function closePicker(returnFocus) {
    if (!openPicker) return;
    var op = openPicker;
    openPicker = null;
    op.pop.remove();
    op.add.setAttribute('aria-expanded', 'false');
    if (returnFocus && document.body.contains(op.add)) op.add.focus();
  }
  function openPickerFor(target) {
    var host = document.getElementById(target.input);
    if (!host || !host._gtwPicker) return;
    closePicker(false);
    var p = host._gtwPicker;
    var pop = el('div', 'gtw-picker-pop');
    pop.setAttribute('role', 'dialog');
    pop.setAttribute('aria-label', 'Select ' + target.label);
    var head = el('div', 'gtw-picker-head');
    head.appendChild(el('span', null, target.label));
    var x = el('button');
    x.type = 'button';
    x.setAttribute('aria-label', 'Close');
    x.appendChild(icon('xmark'));
    head.appendChild(x);
    pop.appendChild(head);
    var sw = el('div', 'gtw-picker-search');
    var search = el('input');
    search.type = 'search';
    search.placeholder = 'Search variables';
    search.setAttribute('aria-label', 'Search variables');
    sw.appendChild(search);
    pop.appendChild(sw);
    var actions = el('div', 'gtw-picker-actions');
    var all = el('button', null, 'Select all shown');
    all.type = 'button';
    var clear = el('button', null, 'Clear');
    clear.type = 'button';
    actions.appendChild(all);
    actions.appendChild(clear);
    pop.appendChild(actions);
    var list = el('div', 'gtw-picker-list');
    list.setAttribute('role', 'group');
    list.setAttribute('aria-label', target.label);
    var none = el('div', 'gtw-picker-none', 'No matching variables');
    none.hidden = true;
    var rows = sourceBoxes(target.input).map(function(b) {
      var row = el('label', 'gtw-picker-row');
      var cb = el('input');
      cb.type = 'checkbox';
      cb.checked = b.checked;
      var name = boxLabel(b);
      var note = conflictNote(target, b.value);
      row.appendChild(cb);
      row.appendChild(el('span', 'gtw-picker-row-name', name));
      if (note) row.appendChild(el('span', 'gtw-picker-row-note', note));
      row.title = name + (note ? ' (' + note + ')' : '');
      if (b.disabled && !b.checked) { cb.disabled = true; row.classList.add('gtw-picker-row-locked'); }
      cb.addEventListener('change', function() { setBox(b, cb.checked); count(); });
      list.appendChild(row);
      return { row: row, cb: cb, box: b, name: name.toLowerCase(), conflict: !!note || cb.disabled };
    });
    list.appendChild(none);
    pop.appendChild(list);
    var foot = el('div', 'gtw-picker-foot');
    var countText = el('span');
    countText.setAttribute('aria-live', 'polite');
    var done = el('button', 'gtw-picker-done', 'Done');
    done.type = 'button';
    foot.appendChild(countText);
    foot.appendChild(done);
    pop.appendChild(foot);
    function count() { countText.textContent = rows.filter(function(r) { return r.cb.checked; }).length + ' selected'; }
    function filter() {
      var q = search.value.trim().toLowerCase();
      var shown = 0;
      rows.forEach(function(r) { r.row.hidden = q && r.name.indexOf(q) < 0; if (!r.row.hidden) shown += 1; });
      none.hidden = shown > 0;
    }
    search.addEventListener('input', filter);
    search.addEventListener('keydown', function(ev) {
      if (ev.key === 'ArrowDown') {
        var first = rows.filter(function(r) { return !r.row.hidden; })[0];
        if (first) first.cb.focus();
        ev.preventDefault();
      }
      if (ev.key === 'Enter') ev.preventDefault();
    });
    list.addEventListener('keydown', function(ev) {
      if (ev.key !== 'ArrowDown' && ev.key !== 'ArrowUp') return;
      var visible = rows.filter(function(r) { return !r.row.hidden; }).map(function(r) { return r.cb; });
      var i = visible.indexOf(document.activeElement);
      if (ev.key === 'ArrowUp' && i <= 0) { search.focus(); ev.preventDefault(); return; }
      var nextBox = visible[ev.key === 'ArrowDown' ? Math.min(visible.length - 1, i + 1) : i - 1];
      if (nextBox) nextBox.focus();
      ev.preventDefault();
    });
    all.addEventListener('click', function() {
      rows.forEach(function(r) { if (!r.row.hidden && !r.conflict && !r.cb.checked) { r.cb.checked = true; setBox(r.box, true); } });
      count();
    });
    clear.addEventListener('click', function() {
      rows.forEach(function(r) { if (r.cb.checked) { r.cb.checked = false; setBox(r.box, false); } });
      count();
    });
    x.addEventListener('click', function() { closePicker(true); });
    done.addEventListener('click', function() { closePicker(true); });
    pop.addEventListener('keydown', function(ev) {
      if (ev.key === 'Escape') { ev.stopPropagation(); closePicker(true); }
      if (ev.key === 'Tab') {
        var f = Array.prototype.filter.call(pop.querySelectorAll('button, input'), function(n) { return !n.closest('[hidden]'); });
        if (!f.length) return;
        if (ev.shiftKey && document.activeElement === f[0]) { f[f.length - 1].focus(); ev.preventDefault(); }
        else if (!ev.shiftKey && document.activeElement === f[f.length - 1]) { f[0].focus(); ev.preventDefault(); }
      }
    });
    pop.addEventListener('click', function(ev) { ev.stopPropagation(); });
    document.body.appendChild(pop);
    placeBelow(pop, p.field, false);
    count();
    p.add.setAttribute('aria-expanded', 'true');
    openPicker = { pop: pop, add: p.add, target: target };
    search.focus();
  }
  function attachPickers() {
    (cfg.pickers || []).forEach(function(target) {
      var host = document.getElementById(target.input);
      if (!host || !host.classList.contains('shiny-input-checkboxgroup')) return;
      if (host._gtwPicker && host.contains(host._gtwPicker.field)) { renderChips(target); return; }
      var label = document.getElementById(target.input + '-label');
      var field = el('div', 'gtw-picker');
      field.setAttribute('role', 'group');
      if (label) field.setAttribute('aria-labelledby', label.id);
      var chips = el('div', 'gtw-picker-chips');
      var add = el('button', 'gtw-picker-add');
      add.type = 'button';
      add.setAttribute('aria-label', 'Choose ' + target.label);
      add.setAttribute('data-gtw-tip', 'Choose ' + target.label);
      add.setAttribute('aria-haspopup', 'dialog');
      add.setAttribute('aria-expanded', 'false');
      add.appendChild(icon('plus'));
      var live = el('span', 'gtw-sr-only');
      live.setAttribute('aria-live', 'polite');
      field.appendChild(chips);
      field.appendChild(add);
      field.appendChild(live);
      field.addEventListener('click', function(ev) {
        if (ev.target.closest('.gtw-chip button, .gtw-chip-more')) return;
        if (openPicker && openPicker.target.input === target.input) closePicker(false); else openPickerFor(target);
      });
      if (label) {
        var ref = label.nextElementSibling && label.nextElementSibling.classList.contains('gtw-help') ? label.nextElementSibling.nextSibling : label.nextSibling;
        label.parentNode.insertBefore(field, ref);
      } else host.insertBefore(field, host.firstChild);
      host.classList.add('gtw-picker-host');
      host._gtwPicker = { field: field, chips: chips, add: add, live: live };
      renderChips(target);
    });
  }
  document.addEventListener('change', function(e) {
    var host = e.target.closest && e.target.closest('.gtw-picker-host');
    if (host) {
      (cfg.pickers || []).forEach(function(t) { if (t.input === host.id) renderChips(t); });
    }
    if (e.target.closest && e.target.closest('.gtw-options')) updateAllOptions();
  });
  document.addEventListener('input', function(e) {
    if (e.target.closest && e.target.closest('.gtw-options')) updateAllOptions();
  });
  document.addEventListener('click', function(e) {
    if (openPicker && !openPicker.pop.contains(e.target) && !(e.target.closest && e.target.closest('.gtw-picker'))) closePicker(false);
  });
  window.addEventListener('resize', function() { closePicker(false); });
  document.addEventListener('scroll', function(e) {
    if (openPicker && !openPicker.pop.contains(e.target)) closePicker(false);
    if (openHelp) placeBelow(openHelp.pop, openHelp.btn, false);
    if (openMenu) placeBelow(openMenu.strip, openMenu.btn, true);
  }, true);

  // ---- Refresh loop -------------------------------------------------------
  function refresh() {
    attachHelp();
    attachPickers();
    updateAllOptions();
    updateToolbars();
  }
  var pending = false;
  function schedule() {
    if (pending) return;
    pending = true;
    setTimeout(function() { pending = false; refresh(); }, 120);
  }
  function init() {
    refresh();
    if (window.MutationObserver) new MutationObserver(schedule).observe(document.body, { childList: true, subtree: true });
    if (window.jQuery) {
      window.jQuery(document).on('shiny:value shiny:inputchanged shown.bs.tab', schedule);
    }
  }
  init();
  window.gtwComponents = { refresh: refresh };
}
if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', gtwComponentsBoot); else gtwComponentsBoot();
")

# M3: Regression workspace. Same spatial pattern as Describe: definition in the
# analysis bar, results across the full width, secondary settings in bottom
# panels. Every control is an existing input; the server is unchanged.
gta_regression_workspace <- function() {
  code_block <- function(tab, id) {
    div(class = "gta-code-for", `data-gta-tab` = tab, verbatimTextOutput(id))
  }
  div(
    class = "gta-workspace gtw-results gtw-reg-results gta-regression",
    `data-gta-workspace` = "regression",
    `data-gta-show` = "nodata",
    div(
      class = "gta-bar",
      role = "region",
      `aria-label` = "Regression model definition",
      div(
        class = "gta-bar-grid gta-reg-grid",
        uiOutput("reg_inputs"),
        div(class = "gta-slot gta-reg-approach", gtx_reg_approach_input()),
        div(
          class = "gta-slot gta-reg-run",
          span(class = "gta-slot-label gta-run-label", id = "gta-reg-run-label", "Run"),
          div(
            class = "gta-btn-group",
            role = "group",
            `aria-labelledby` = "gta-reg-run-label",
            actionButton("run_uni", "Univariable", class = "btn-primary", `aria-label` = "Run univariable models"),
            actionButton("run_multi", "Multivariable", class = "btn-primary", `aria-label` = "Run multivariable model")
          )
        ),
        div(
          class = "gta-ready",
          `aria-live` = "polite",
          span(class = "gta-ready-mark", `aria-hidden` = "true"),
          span(class = "gta-ready-text"),
          tags$button(
            type = "button", class = "gta-text-btn gta-ready-details",
            `data-gta-pop` = "gta-reg-preflight", `aria-controls` = "gta-reg-preflight", `aria-expanded` = "false",
            "Details"
          )
        )
      )
    ),
    div(
      class = "gta-results",
      role = "region",
      `aria-label` = "Regression results",
      `data-gtw-result-root` = NA,
      tabsetPanel(
        id = "reg_output_tab",
        tabPanel("Univariable", value = "Univariate", div(class = "gtx-card", tagListDownload("uni", table = TRUE), gtx_table_output("uni_table"))),
        tabPanel("Multivariable", div(class = "gtx-card", tagListDownload("multi", table = TRUE), gtx_table_output("multi_table"))),
        tabPanel("Stratified", div(class = "gtx-card", tagListDownload("strat", table = TRUE), gtx_table_output("strat_table"))),
        tabPanel("Model Stats", div(class = "gtx-card", gtx_data_output("reg_stats")))
      ),
      tags$button(
        type = "button", class = "gta-icon-btn gtw-tb-btn gtw-tb-focus gta-results-focus",
        `aria-label` = "Focus results", `data-gtw-tip` = "Focus results (Esc to return)",
        `aria-pressed` = "false", onclick = "gtwToggleFocus()",
        icon("up-right-and-down-left-from-center", `aria-hidden` = "true")
      ),
      div(
        class = "gta-empty",
        gta_empty_state(
          "nodata", "table", "No data loaded", gta_host$empty_data_text,
          tags$button(type = "button", class = "btn btn-primary", `data-gta-open` = "gta-data", "Choose data")
        ),
        gta_empty_state(
          "no-outcome", "chart-line", "Fit a regression model",
          "Choose the outcome, then at least one exposure.",
          tags$button(type = "button", class = "btn btn-default", `data-gta-focus` = "reg_outcome", "Choose outcome")
        ),
        gta_empty_state(
          "no-exposure", "list-check", "Choose exposures",
          "Add at least one exposure to fit the model.",
          tags$button(type = "button", class = "btn btn-default", `data-gta-pick` = "reg_exposures", icon("plus", `aria-hidden` = "true"), " Choose exposures")
        ),
        gta_empty_state(
          "ready-uni", "table-list", "Ready for univariable models",
          "Each exposure is fitted in its own model.",
          tags$button(type = "button", class = "btn btn-default", `data-gta-click` = "run_uni", "Run univariable")
        ),
        gta_empty_state(
          "ready-multi", "table-list", "Ready for the multivariable model",
          "Exposures and Adjust for variables are fitted together in one model.",
          tags$button(type = "button", class = "btn btn-default", `data-gta-click` = "run_multi", "Run multivariable")
        ),
        gta_empty_state(
          "ready-strat", "layer-group", "No stratified results yet",
          "Choose a stratifier, then run the stratified analysis.",
          tags$button(type = "button", class = "btn btn-default", `data-gta-toggle` = "gta-panel-reg-strata", "Open Stratify")
        ),
        gta_empty_state(
          "ready-stats", "chart-simple", "No model statistics yet",
          "Model statistics appear here after a model is fitted."
        )
      )
    ),
    div(
      id = "gta-reg-preflight",
      class = "gta-pop",
      role = "dialog",
      `aria-label` = "Model check",
      uiOutput("reg_preflight")
    ),
    gta_bottom_panel(
      "gta-panel-reg-options", "Options",
      gtx_reg_table_option_inputs(),
      summary = span(class = "gtw-options-state", `aria-live` = "polite", "defaults"),
      class = "gtw-options gta-options-panel",
      body_class = "gtw-options-body",
      attrs = list(`data-gtw-options` = "reg_options")
    ),
    gta_bottom_panel(
      "gta-panel-reg-refs", "Reference levels",
      uiOutput("reg_reference_inputs"),
      div(class = "gta-panel-empty", "Reference levels appear here for categorical exposures and adjustment variables."),
      summary = tagList(
        span(class = "gta-ref-help"),
        span(class = "gtw-options-state", `aria-live` = "polite", "defaults")
      ),
      class = "gtw-options gta-options-panel gta-refs-panel",
      body_class = "gtw-options-body",
      attrs = list(`data-gtw-options` = "reg_references")
    ),
    gta_bottom_panel(
      "gta-panel-reg-strata", "Stratify",
      gtx_reg_strata_controls("Run stratified"),
      summary = span(class = "gtw-options-state", `aria-live` = "polite", "defaults"),
      class = "gtw-options gta-options-panel gta-strata-panel",
      body_class = "gtw-options-body",
      attrs = list(`data-gtw-options` = "reg_strata")
    ),
    gta_bottom_panel(
      "gta-panel-reg-code", "R code",
      code_block("Univariate", "uni_code"),
      code_block("Multivariable", "multi_code"),
      code_block("Stratified", "strat_code"),
      div(class = "gta-code-for gta-code-none", `data-gta-tab` = "Model Stats", "Model Stats has no separate R code. The statistics come from the fitted univariable or multivariable models."),
      summary = span(class = "gta-panel-note gta-code-context", "Univariable table"),
      actions = tags$button(
        type = "button", class = "btn btn-default btn-sm gtx-copy gta-copy-active",
        icon("copy", `aria-hidden` = "true"), " Copy R code"
      ),
      class = "gta-code-panel"
    ),
    div(
      class = "gta-toolbar",
      role = "toolbar",
      `aria-label` = "Regression actions",
      div(
        class = "gta-toolbar-group",
        gta_panel_toggle("gta-panel-reg-options", "sliders", "Options", badge = TRUE),
        gta_panel_toggle("gta-panel-reg-refs", "arrow-right-arrow-left", "References", badge = TRUE),
        gta_panel_toggle("gta-panel-reg-strata", "layer-group", "Stratify", badge = TRUE)
      ),
      div(
        class = "gta-toolbar-group gta-toolbar-end gtw-toolbar",
        tags$button(
          type = "button", class = "gta-tb gtw-tb-btn gtw-tb-export",
          `aria-label` = "Export result", `data-gtw-tip-base` = "Export result",
          `aria-haspopup` = "menu", `aria-expanded` = "false",
          icon("download", `aria-hidden` = "true"),
          span(class = "gta-tb-label", "Export"),
          icon("caret-up", class = "gta-caret", `aria-hidden` = "true")
        ),
        gta_panel_toggle("gta-panel-reg-code", "code", "Code")
      )
    )
  )
}

# Contextual help used only by the shared shell (the explanation moves out of
# the permanent UI into help, per the shared UI-copy rule).
if (gta_shell_enabled) {
  gta_extra_picker_targets <- c(list(
    list(input = "surv_exposures", label = "Exposures", conflicts = list(surv_time = "Time", surv_event = "Event", surv_stratifier = "Stratifier", surv_adjust = "Adjust for")),
    list(input = "surv_adjust", label = "Adjust for", conflicts = list(surv_time = "Time", surv_event = "Event", surv_stratifier = "Stratifier", surv_exposures = "Exposures")),
    list(input = "special_exposures", label = "Exposures", conflicts = list(special_outcome = "Outcome", special_group = "Grouping variable", special_matched_set = "Matched-set identifier", special_adjust = "Adjust for")),
    list(input = "special_adjust", label = "Adjust for", conflicts = list(special_outcome = "Outcome", special_group = "Grouping variable", special_matched_set = "Matched-set identifier", special_exposures = "Exposures")),
    list(input = "med_covariates", label = "Covariates", conflicts = list(med_exposure = "Exposure", med_mediator = "Mediator", med_outcome = "Outcome")),
    list(input = "trial_adjust", label = "Adjust for", conflicts = list(trial_outcome = "Outcome", trial_exposure = "Treatment group"))
  ))
  gtw_help_registry$surv_approach <- list(
    title = "Survival method",
    short = "Kaplan-Meier describes observed survival, optionally by group. Cox PH estimates hazard ratios without specifying the baseline hazard distribution; parametric survival estimates time ratios with a selected distribution; competing risks (Fine-Gray) estimates subdistribution hazard ratios for one event while retaining competing events."
  )
  gtw_help_registry$surv_event_codes <- list(
    title = "Event definition",
    short = "Choose the code that means censored and the code for the event of interest. At least one other observed code must represent a competing event."
  )
  gtw_help_registry$surv_multivariable <- list(
    title = "Single multivariable model",
    short = "Off: each exposure is fitted in its own model (adjusted for the Adjust for variables when any are chosen). On: one multivariable model is fitted with all exposures."
  )
  gtw_help_registry$more_method <- list(
    title = "Model method",
    short = "Ordinal: an outcome with three or more ordered categories (set it as an ordered factor in Data Prep first). Multinomial: three or more unordered categories; the first level is the outcome reference. Multilevel: a random-intercept model for observations grouped within clusters such as clinics or centres. Conditional logistic: matched case-control data, where each case and its controls share a matched-set identifier. Mediation: direct and indirect effects of an exposure through a mediator; read Causal assumptions before interpreting."
  )
  gtw_help_registry$special_multivariable <- list(
    title = "Single multivariable model",
    short = "Off: each exposure is fitted in its own model, with the Adjust for variables. On: one model is fitted with all exposures and the Adjust for variables."
  )
  gtw_help_registry$structure_notes <- list(
    title = "Worth a look",
    short = "Objective features of the data, not judgements. Duplicate rows: identical to an earlier row in every variable. Possible identifier: every non-missing value is different (whole numbers or text), as in a record ID. Constant: one distinct value, or none. Over 20% missing: more than a fifth of the values are missing. Numeric 0/1: a numeric variable that only takes the values 0 and 1. Select one to see that variable."
  )
  gtw_help_registry$trial <- list(
    title = "Risk difference",
    short = "The absolute difference in risk between two groups: comparison minus reference, in percentage points. Without adjustment the interval and test are Miettinen-Nurminen score based. With Adjust for variables the risks are standardised from a logistic model over everyone analysed, with a Wald interval from the influence-function standard error."
  )
  gtw_help_registry$trial_event_type <- list(
    title = "Number needed to treat",
    short = "Reported only when you say whether the event is harmful (for example death) or beneficial (for example cure), because that decides whether a difference is a benefit (NNTB) or a harm (NNTH). It describes this comparison over this follow-up and is causal only when the design supports it."
  )
  gtw_help_registry$reg_references <- list(
    title = "Reference categories",
    short = "The selected category is the baseline. Every reported category is interpreted relative to this baseline."
  )
}

# Survival workspace (D3). The visible method selector drives the existing
# surv_mode input for the three model workflows; Kaplan-Meier is a presentation state
# that leaves surv_mode on a model mode, exactly as the D2 layout requires.
# Every control is an existing authoritative input; the server is unchanged.
gta_surv_approaches <- list(
  list(id = "km", label = HTML("Kaplan&ndash;Meier"), aria = "Kaplan-Meier", mode = NA),
  list(id = "cox", label = "Cox PH", aria = "Cox PH", mode = "cox"),
  list(id = "survreg", label = "Parametric", aria = "Parametric survival", mode = "survreg"),
  list(id = "competing", label = "Competing risks", aria = "Competing risks", mode = "competing")
)

gta_survival_workspace <- function() {
  run_button <- function(id, label, aria, approaches) {
    tags$span(
      class = "gta-run-for", `data-gta-for` = approaches,
      actionButton(id, label, class = "btn-primary", `aria-label` = aria)
    )
  }
  method_nav <- div(
    class = "gta-surv-methods",
    div(
      class = "gta-seg", role = "radiogroup", id = "gta-surv-methods",
      `aria-label` = "Survival method",
      lapply(gta_surv_approaches, function(a) {
        on <- identical(a$id, "km")
        tags$button(
          type = "button", class = "gta-seg-btn", role = "radio",
          id = paste0("gta-surv-method-", a$id),
          `aria-checked` = if (on) "true" else "false",
          tabindex = if (on) "0" else "-1",
          `aria-label` = a$aria,
          `data-gta-approach` = a$id, `data-gta-mode` = if (is.na(a$mode)) "" else a$mode,
          a$label
        )
      })
    ),
    span(class = "gta-surv-methods-help")
  )
  model_tab <- function() {
    tabPanel(
      "Model result", value = "Model Result",
      conditionalPanel(
        "input.surv_mode == 'cox'",
        tabsetPanel(
          id = "cox_result_tab",
          tabPanel("Exposure models", div(class = "gtx-card", tagListDownload("cox_exposure", table = TRUE), gtx_table_output("cox_exposure_table"))),
          tabPanel("Multivariable model", div(class = "gtx-card", tagListDownload("cox_multi", table = TRUE), gtx_table_output("cox_multi_table")))
        )
      ),
      conditionalPanel(
        "input.surv_mode == 'survreg'",
        tabsetPanel(
          id = "survreg_result_tab",
          tabPanel("Exposure models", div(class = "gtx-card", tagListDownload("survreg_exposure", table = TRUE), gtx_table_output("survreg_exposure_table"))),
          tabPanel("Multivariable model", div(class = "gtx-card", tagListDownload("survreg_multi", table = TRUE), gtx_table_output("survreg_multi_table")))
        )
      ),
      conditionalPanel(
        "input.surv_mode == 'competing'",
        div(class = "gtx-card", tagListDownload("competing", table = TRUE), gtx_table_output("competing_table"))
      )
    )
  }
  div(
    class = "gta-workspace gtw-results gta-survival",
    `data-gta-workspace` = "survival",
    `data-gta-approach` = "km",
    `data-gta-mode` = "cox",
    `data-gta-show` = "nodata",
    div(class = "gta-hidden-input", gtx_surv_mode_input()),
    div(
      class = "gta-bar",
      role = "region",
      `aria-label` = "Survival analysis definition",
      method_nav,
      div(
        class = "gta-bar-grid gta-surv-grid",
        uiOutput("surv_inputs"),
        div(class = "gta-slot gta-surv-km-by", uiOutput("km_group_input")),
        div(class = "gta-surv-multi", `data-gta-on` = "false", gtx_surv_multivariable_input()),
        div(
          class = "gta-slot gta-surv-run",
          span(class = "gta-slot-label gta-run-label", id = "gta-surv-run-label", "Run"),
          div(
            class = "gta-btn-group", role = "group", `aria-labelledby` = "gta-surv-run-label",
            run_button("run_km", "Run", "Draw the Kaplan-Meier plot", "km"),
            run_button("run_cox", "Run", "Run Cox regression", "cox"),
            run_button("run_survreg", "Run", "Run parametric survival regression", "survreg"),
            run_button("run_competing", "Fine-Gray", "Run Fine-Gray regression", "competing"),
            run_button("run_cuminc", "Cumulative incidence", "Draw cumulative incidence", "competing")
          )
        ),
        div(
          class = "gta-ready",
          `aria-live` = "polite",
          span(class = "gta-ready-mark", `aria-hidden` = "true"),
          span(class = "gta-ready-text"),
          tags$button(
            type = "button", class = "gta-text-btn gta-ready-details",
            `data-gta-pop` = "gta-surv-preflight", `aria-controls` = "gta-surv-preflight", `aria-expanded` = "false",
            "Details"
          )
        )
      )
    ),
    div(
      class = "gta-results",
      role = "region",
      `aria-label` = "Survival results",
      `data-gtw-result-root` = NA,
      tabsetPanel(
        id = "surv_output_tab",
        model_tab(),
        tabPanel("KM plot", value = "KM Plot", div(class = "gtx-card", tagListDownload("km", table = FALSE, plot = TRUE), plotOutput("km_plot", height = "calc(100vh - 330px)"))),
        tabPanel("Cumulative incidence", value = "Cumulative Incidence", div(class = "gtx-card", tagListDownload("cuminc", table = FALSE, plot = TRUE), plotOutput("cuminc_plot", height = "calc(100vh - 330px)"))),
        tabPanel("Survival tables", value = "Survival Tables", div(class = "gtx-card", gtx_table_output("logrank_table")), div(class = "gtx-card", gtx_table_output("surv_summary_table")))
      ),
      tags$button(
        type = "button", class = "gta-icon-btn gtw-tb-btn gtw-tb-focus gta-results-focus",
        `aria-label` = "Focus results", `data-gtw-tip` = "Focus results (Esc to return)",
        `aria-pressed` = "false", onclick = "gtwToggleFocus()",
        icon("up-right-and-down-left-from-center", `aria-hidden` = "true")
      ),
      div(
        class = "gta-empty",
        gta_empty_state(
          "nodata", "table", "No data loaded", gta_host$empty_data_text,
          tags$button(type = "button", class = "btn btn-primary", `data-gta-open` = "gta-data", "Choose data")
        ),
        gta_empty_state(
          "need-time", "clock", "Choose time",
          "Survival analyses need the follow-up time and the event variable.",
          tags$button(type = "button", class = "btn btn-default", `data-gta-focus` = "surv_time", "Choose time")
        ),
        gta_empty_state(
          "need-event", "clock", "Choose event",
          "Choose the variable that records the event or censoring status.",
          tags$button(type = "button", class = "btn btn-default", `data-gta-focus` = "surv_event", "Choose event")
        ),
        gta_empty_state(
          "need-exposure", "list-check", "Choose exposures",
          "Add at least one exposure to fit the model.",
          tags$button(type = "button", class = "btn btn-default", `data-gta-pick` = "surv_exposures", icon("plus", `aria-hidden` = "true"), " Choose exposures")
        ),
        gta_empty_state(
          "ready-km", "chart-line", HTML("Ready for the Kaplan&ndash;Meier plot"),
          "Group by is optional; leave it as None for one overall curve.",
          tags$button(type = "button", class = "btn btn-default", `data-gta-click` = "run_km", "Draw plot")
        ),
        gta_empty_state(
          "ready-cox", "table-list", "Ready for Cox PH regression",
          "Fits separate exposure models, or one model when Single multivariable model is on.",
          tags$button(type = "button", class = "btn btn-default", `data-gta-click` = "run_cox", "Run Cox PH")
        ),
        gta_empty_state(
          "ready-survreg", "table-list", "Ready for parametric survival regression",
          "Uses the distribution chosen in the analysis bar.",
          tags$button(type = "button", class = "btn btn-default", `data-gta-click` = "run_survreg", "Run parametric model")
        ),
        gta_empty_state(
          "ready-competing", "table-list", "Ready for Fine-Gray regression",
          "Check the censoring code and event of interest in the analysis bar before running.",
          tags$button(type = "button", class = "btn btn-default", `data-gta-click` = "run_competing", "Run Fine-Gray")
        ),
        gta_empty_state(
          "ready-cuminc", "chart-area", "Ready to draw cumulative incidence",
          "Curves use the censoring code and event of interest shown in the analysis bar.",
          tags$button(type = "button", class = "btn btn-default", `data-gta-click` = "run_cuminc", "Draw cumulative incidence")
        )
      )
    ),
    div(
      id = "gta-surv-preflight",
      class = "gta-pop",
      role = "dialog",
      `aria-label` = "Model check",
      uiOutput("surv_preflight")
    ),
    gta_bottom_panel(
      "gta-panel-surv-options", "Options",
      gtx_surv_show_ref_input(),
      summary = span(class = "gtw-options-state", `aria-live` = "polite", "defaults"),
      class = "gtw-options gta-options-panel",
      body_class = "gtw-options-body",
      attrs = list(`data-gtw-options` = "surv_options")
    ),
    gta_bottom_panel(
      "gta-panel-km-plot", "Plot options",
      gtx_km_plot_option_inputs(),
      gtx_km_break_input(),
      summary = span(class = "gtw-options-state", `aria-live` = "polite", "defaults"),
      class = "gtw-options gta-options-panel",
      body_class = "gtw-options-body",
      attrs = list(`data-gtw-options` = "km_plot_options")
    ),
    gta_bottom_panel(
      "gta-panel-cuminc-plot", "Plot options",
      gtx_cuminc_by_input(),
      gtx_cuminc_plot_option_inputs(),
      summary = span(class = "gtw-options-state", `aria-live` = "polite", "defaults"),
      class = "gtw-options gta-options-panel",
      body_class = "gtw-options-body",
      attrs = list(`data-gtw-options` = "cuminc_plot_options")
    ),
    gta_bottom_panel(
      "gta-panel-surv-code", "R code",
      verbatimTextOutput("surv_code"),
      summary = span(class = "gta-panel-note", "Reflects the current settings, including any not yet run."),
      actions = tags$button(
        type = "button", class = "btn btn-default btn-sm gtx-copy",
        onclick = "gtxCopyCode('surv_code', this)",
        icon("copy", `aria-hidden` = "true"), " Copy R code"
      ),
      class = "gta-code-panel"
    ),
    div(
      class = "gta-toolbar",
      role = "toolbar",
      `aria-label` = "Survival actions",
      div(
        class = "gta-toolbar-group",
        span(class = "gta-tool-for", `data-gta-for` = "cox survreg competing", gta_panel_toggle("gta-panel-surv-options", "sliders", "Options", badge = TRUE)),
        span(class = "gta-tool-for", `data-gta-for` = "km", gta_panel_toggle("gta-panel-km-plot", "palette", "Plot options", badge = TRUE)),
        span(class = "gta-tool-for", `data-gta-for` = "competing", gta_panel_toggle("gta-panel-cuminc-plot", "palette", "Plot options", badge = TRUE))
      ),
      div(
        class = "gta-toolbar-group gta-toolbar-end gtw-toolbar",
        tags$button(
          type = "button", class = "gta-tb gtw-tb-btn gtw-tb-export",
          `aria-label` = "Export result", `data-gtw-tip-base` = "Export result",
          `aria-haspopup` = "menu", `aria-expanded` = "false",
          icon("download", `aria-hidden` = "true"),
          span(class = "gta-tb-label", "Export"),
          icon("caret-up", class = "gta-caret", `aria-hidden` = "true")
        ),
        gta_panel_toggle("gta-panel-surv-code", "code", "Code")
      )
    )
  )
}

# More models workspace (D3). Five visible methods: the four special designs
# drive the existing special_mode input; Mediation is the fifth method and
# uses its own existing inputs and outputs. Every control is an existing
# authoritative input and the server is unchanged.
gta_more_methods <- list(
  list(id = "ordinal", label = "Ordinal", aria = "Ordinal regression", mode = "ordinal"),
  list(id = "multinomial", label = "Multinomial", aria = "Multinomial regression", mode = "multinomial"),
  list(id = "multilevel", label = "Multilevel", aria = "Multilevel regression", mode = "multilevel"),
  list(id = "matched", label = "Conditional logistic", aria = "Conditional logistic regression", mode = "matched"),
  list(id = "mediation", label = "Mediation", aria = "Mediation analysis", mode = NA)
)

gta_more_workspace <- function() {
  designs <- "ordinal multinomial multilevel matched"
  method_nav <- div(
    class = "gta-surv-methods gta-more-methods",
    div(
      class = "gta-seg", role = "radiogroup", id = "gta-more-methods",
      `aria-label` = "Model method",
      lapply(gta_more_methods, function(m) {
        on <- identical(m$id, "ordinal")
        tags$button(
          type = "button", class = "gta-seg-btn", role = "radio",
          id = paste0("gta-more-method-", m$id),
          `aria-checked` = if (on) "true" else "false",
          tabindex = if (on) "0" else "-1",
          `aria-label` = m$aria,
          `data-gta-method` = m$id, `data-gta-mode` = if (is.na(m$mode)) "" else m$mode,
          m$label
        )
      })
    ),
    span(class = "gta-more-methods-help")
  )
  for_methods <- function(methods, ...) span(class = "gta-run-for", `data-gta-for` = methods, ...)
  code_block <- function(methods, id) {
    div(class = "gta-code-for", `data-gta-for` = methods, verbatimTextOutput(id))
  }
  div(
    class = "gta-workspace gtw-results gta-more",
    `data-gta-workspace` = "more",
    `data-gta-method` = "ordinal",
    `data-gta-show` = "nodata",
    div(class = "gta-hidden-input", gtx_special_mode_input()),
    div(
      class = "gta-bar",
      role = "region",
      `aria-label` = "Model definition",
      method_nav,
      div(
        class = "gta-bar-grid gta-more-grid",
        uiOutput("special_inputs"),
        uiOutput("med_inputs"),
        div(class = "gta-slot gta-more-medmodel", gtx_med_approach_input()),
        div(
          class = "gta-more-causal",
          tags$button(
            type = "button", class = "gta-text-btn gta-causal-btn",
            `data-gta-pop` = "gta-med-causal", `aria-controls` = "gta-med-causal", `aria-expanded` = "false",
            "Causal assumptions",
            icon("circle-info", `aria-hidden` = "true")
          )
        ),
        div(
          class = "gta-slot gta-more-run",
          span(class = "gta-slot-label gta-run-label", id = "gta-more-run-label", "Run"),
          div(
            class = "gta-btn-group", role = "group", `aria-labelledby` = "gta-more-run-label",
            for_methods(designs, actionButton("run_special", "Run", class = "btn-primary", `aria-label` = "Run the selected model")),
            for_methods("mediation", actionButton("run_mediation", "Run", class = "btn-primary", `aria-label` = "Run mediation analysis"))
          )
        ),
        div(
          class = "gta-ready",
          `aria-live` = "polite",
          span(class = "gta-ready-mark", `aria-hidden` = "true"),
          span(class = "gta-ready-text"),
          tags$button(
            type = "button", class = "gta-text-btn gta-ready-details gta-run-for", `data-gta-for` = designs,
            `data-gta-pop` = "gta-more-preflight", `aria-controls` = "gta-more-preflight", `aria-expanded` = "false",
            "Details"
          )
        )
      )
    ),
    div(
      class = "gta-results",
      role = "region",
      `aria-label` = "Model results",
      `data-gtw-result-root` = NA,
      tabsetPanel(
        id = "special_output_tab",
        tabPanel("Exposure models", value = "Exposure Models", div(class = "gtx-card", tagListDownload("special_exposure", table = TRUE), gtx_table_output("special_exposure_table"))),
        tabPanel("Multivariable model", value = "Multivariable Model", div(class = "gtx-card", tagListDownload("special_multi", table = TRUE), gtx_table_output("special_multi_table"))),
        tabPanel("Model stats", value = "Model Stats", uiOutput("special_stats_panel")),
        tabPanel("Assumption check", value = "Assumption Check", uiOutput("special_assumptions_panel")),
        tabPanel("Mediation table", value = "Table", div(class = "gtx-card", tagListDownload("med", table = TRUE), gtx_table_output("med_table"))),
        tabPanel("Diagram", value = "Diagram", div(class = "gtx-card", tagListDownload("medplot", table = FALSE, plot = TRUE), plotOutput("med_plot", height = "calc(100vh - 340px)")))
      ),
      tags$button(
        type = "button", class = "gta-icon-btn gtw-tb-btn gtw-tb-focus gta-results-focus",
        `aria-label` = "Focus results", `data-gtw-tip` = "Focus results (Esc to return)",
        `aria-pressed` = "false", onclick = "gtwToggleFocus()",
        icon("up-right-and-down-left-from-center", `aria-hidden` = "true")
      ),
      div(
        class = "gta-empty",
        gta_empty_state(
          "nodata", "table", "No data loaded", gta_host$empty_data_text,
          tags$button(type = "button", class = "btn btn-primary", `data-gta-open` = "gta-data", "Choose data")
        ),
        # Text and target of these two states follow the selected method.
        gta_empty_state(
          "need", "list-check", "Choose outcome", "",
          tags$button(type = "button", class = "btn btn-default gta-more-need-btn", `data-gta-focus` = "special_outcome", "Choose outcome")
        ),
        gta_empty_state(
          "ready", "table-list", "Ready", "",
          tags$button(type = "button", class = "btn btn-default gta-more-ready-btn", `data-gta-click` = "run_special", "Run")
        )
      )
    ),
    div(
      id = "gta-more-preflight",
      class = "gta-pop",
      role = "dialog",
      `aria-label` = "Model check",
      uiOutput("special_preflight")
    ),
    div(
      id = "gta-med-causal",
      class = "gta-pop gta-causal-pop",
      role = "dialog",
      `aria-labelledby` = "gta-med-causal-title",
      div(class = "gta-pop-title", id = "gta-med-causal-title", "Causal assumptions"),
      # The D2 caution text, verbatim, followed by the other assumptions
      # stated in the mediation_analysis() documentation.
      p(
        strong("Screening aid only: "),
        "a causal interpretation assumes no unmeasured exposure-mediator, mediator-outcome, or exposure-outcome confounding."
      ),
      p("Specify the causal question before running this analysis. The result is a screening aid and depends on no unmeasured exposure-mediator, mediator-outcome, or exposure-outcome confounding assumptions."),
      p("It also assumes:"),
      tags$ul(
        tags$li("the exposure comes before the mediator, and the mediator before the outcome;"),
        tags$li("the mediator and outcome models are suitably specified (the mediator model is linear).")
      ),
      p("Use a DAG and subject-matter knowledge before interpreting the estimates causally. With a logistic outcome model the effects are differences in predicted probability, not odds ratios.")
    ),
    gta_bottom_panel(
      "gta-panel-more-options", "Options",
      gtx_special_show_ref_input(),
      gtx_special_model_stats_input(),
      summary = span(class = "gtw-options-state", `aria-live` = "polite", "defaults"),
      class = "gtw-options gta-options-panel",
      body_class = "gtw-options-body",
      attrs = list(`data-gtw-options` = "special_options")
    ),
    gta_bottom_panel(
      "gta-panel-more-refs", "Reference levels",
      uiOutput("special_reference_inputs"),
      div(class = "gta-panel-empty", "Reference levels appear here for categorical exposures and adjustment variables."),
      summary = tagList(
        span(class = "gta-ref-help"),
        span(class = "gtw-options-state", `aria-live` = "polite", "defaults")
      ),
      class = "gtw-options gta-options-panel gta-refs-panel",
      body_class = "gtw-options-body",
      attrs = list(`data-gtw-options` = "special_references")
    ),
    gta_bottom_panel(
      "gta-panel-med-options", "Options",
      gtx_med_option_inputs(),
      summary = span(class = "gtw-options-state", `aria-live` = "polite", "defaults"),
      class = "gtw-options gta-options-panel",
      body_class = "gtw-options-body",
      attrs = list(`data-gtw-options` = "med_options")
    ),
    gta_bottom_panel(
      "gta-panel-more-code", "R code",
      code_block(designs, "special_code"),
      code_block("mediation", "med_code"),
      summary = span(class = "gta-panel-note", "Reflects the current settings, including any not yet run."),
      actions = tags$button(
        type = "button", class = "btn btn-default btn-sm gtx-copy gta-copy-active",
        icon("copy", `aria-hidden` = "true"), " Copy R code"
      ),
      class = "gta-code-panel"
    ),
    div(
      class = "gta-toolbar",
      role = "toolbar",
      `aria-label` = "More models actions",
      div(
        class = "gta-toolbar-group",
        span(class = "gta-tool-for", `data-gta-for` = designs, gta_panel_toggle("gta-panel-more-options", "sliders", "Options", badge = TRUE)),
        span(class = "gta-tool-for", `data-gta-for` = designs, gta_panel_toggle("gta-panel-more-refs", "arrow-right-arrow-left", "References", badge = TRUE)),
        span(class = "gta-tool-for", `data-gta-for` = "mediation", gta_panel_toggle("gta-panel-med-options", "sliders", "Options", badge = TRUE))
      ),
      div(
        class = "gta-toolbar-group gta-toolbar-end gtw-toolbar",
        tags$button(
          type = "button", class = "gta-tb gtw-tb-btn gtw-tb-export",
          `aria-label` = "Export result", `data-gtw-tip-base` = "Export result",
          `aria-haspopup` = "menu", `aria-expanded` = "false",
          icon("download", `aria-hidden` = "true"),
          span(class = "gta-tb-label", "Export"),
          icon("caret-up", class = "gta-caret", `aria-hidden` = "true")
        ),
        gta_panel_toggle("gta-panel-more-code", "code", "Code")
      )
    )
  )
}

# Trials workspace (D3). Absolute treatment effects from risk_difference():
# its own analysis family, not a regression approach. The workspace exists in
# the shared shell only; the D2 scaffolding and web mode are unchanged.
gtx_trial_event_type_input <- function() {
  selectInput(
    "trial_event_type", "Number needed to treat",
    choices = c("Not reported" = "", "Event is harmful" = "harmful", "Event is beneficial" = "beneficial"),
    selected = ""
  )
}

gtx_trial_option_inputs <- function() {
  numericInput("trial_conf_level", "Confidence level", value = 0.95, min = 0.5, max = 0.999, step = 0.01)
}

# Observed values of a candidate binary variable, as the text keys that
# risk_difference() itself compares, in its default order. Used only to offer
# the Event and Reference group choices; risk_difference() stays authoritative.
gtx_trial_values <- function(x, name = "variable", role = c("outcome", "exposure")) {
  role <- match.arg(role)
  x <- tryCatch(get(".rd_unlabel", envir = gtx_ns)(x, name), error = function(e) x)
  key <- get(".rd_key", envir = gtx_ns)
  if (is.factor(x)) {
    values <- levels(droplevels(x))
    if (identical(role, "outcome") && length(values) == 1L && nlevels(x) == 2L) values <- levels(x)
    kind <- "text"
  } else if (is.logical(x)) {
    values <- if (identical(role, "outcome")) c("FALSE", "TRUE") else as.character(sort(unique(x[!is.na(x)])))
    kind <- "logical"
  } else if (is.numeric(x)) {
    values <- key(sort(unique(x[!is.na(x)])))
    kind <- "numeric"
  } else if (is.character(x)) {
    values <- sort(unique(x[!is.na(x)]))
    kind <- "text"
  } else {
    return(list(values = character(0), kind = "unsupported", n = NA_integer_))
  }
  list(values = values, kind = kind, n = length(values))
}

# R literal for an event or reference value in generated code.
gtx_trial_literal <- function(value, kind) {
  if (!gtx_has_text(value)) return(NULL)
  if (kind %in% c("numeric", "logical")) return(value)
  paste0("\"", gsub("([\\\\\"])", "\\\\\\1", value), "\"")
}

gta_trials_workspace <- function() {
  div(
    class = "gta-workspace gtw-results gta-trials",
    `data-gta-workspace` = "trials",
    `data-gta-show` = "nodata",
    div(
      class = "gta-bar",
      role = "region",
      `aria-label` = "Risk difference definition",
      div(
        class = "gta-bar-grid gta-trial-grid",
        uiOutput("trial_inputs"),
        div(class = "gta-slot gta-trial-event", uiOutput("trial_event_input")),
        div(class = "gta-slot gta-trial-ref", uiOutput("trial_reference_input")),
        div(class = "gta-slot gta-trial-nnt", gtx_trial_event_type_input()),
        div(
          class = "gta-slot gta-trial-run",
          span(class = "gta-slot-label gta-run-label", id = "gta-trial-run-label", "Run"),
          div(
            class = "gta-btn-group", role = "group", `aria-labelledby` = "gta-trial-run-label",
            actionButton("run_trial", "Risk difference", class = "btn-primary", `aria-label` = "Run risk difference")
          )
        ),
        div(
          class = "gta-ready",
          `aria-live` = "polite",
          span(class = "gta-ready-mark", `aria-hidden` = "true"),
          span(class = "gta-ready-text"),
          tags$button(
            type = "button", class = "gta-text-btn gta-ready-details",
            `data-gta-pop` = "gta-trial-preflight", `aria-controls` = "gta-trial-preflight", `aria-expanded` = "false",
            "Details"
          )
        )
      )
    ),
    div(
      class = "gta-results",
      role = "region",
      `aria-label` = "Risk difference result",
      div(
        class = "gta-results-head",
        div(class = "gta-result-tabs", span(class = "gta-result-tab gta-result-title is-active", "Risk difference")),
        tags$button(
          type = "button", class = "gta-icon-btn gtw-tb-btn gtw-tb-focus",
          `aria-label` = "Focus results", `data-gtw-tip` = "Focus results (Esc to return)",
          `aria-pressed` = "false", onclick = "gtwToggleFocus()",
          icon("up-right-and-down-left-from-center", `aria-hidden` = "true")
        )
      ),
      div(
        class = "gta-results-body",
        div(class = "gtx-card", tagListDownload("risk_difference", table = TRUE), gtx_table_output("trial_table")),
        div(
          class = "gta-empty",
          gta_empty_state(
            "nodata", "table", "No data loaded", gta_host$empty_data_text,
            tags$button(type = "button", class = "btn btn-primary", `data-gta-open` = "gta-data", "Choose data")
          ),
          gta_empty_state(
            "need", "scale-balanced", "Choose outcome", "",
            tags$button(type = "button", class = "btn btn-default gta-trial-need-btn", `data-gta-focus` = "trial_outcome", "Choose outcome")
          ),
          gta_empty_state(
            "ready", "table-list", "Ready for the risk difference",
            "Risks in each group, their difference and its confidence interval will appear here.",
            tags$button(type = "button", class = "btn btn-default", `data-gta-click` = "run_trial", "Run risk difference")
          )
        )
      )
    ),
    div(
      id = "gta-trial-preflight",
      class = "gta-pop",
      role = "dialog",
      `aria-label` = "Analysis check",
      uiOutput("trial_preflight")
    ),
    gta_bottom_panel(
      "gta-panel-trial-options", "Options",
      gtx_trial_option_inputs(),
      summary = span(class = "gtw-options-state", `aria-live` = "polite", "defaults"),
      class = "gtw-options gta-options-panel",
      body_class = "gtw-options-body",
      attrs = list(`data-gtw-options` = "trial_options")
    ),
    gta_bottom_panel(
      "gta-panel-trial-code", "R code",
      verbatimTextOutput("trial_code"),
      summary = span(class = "gta-panel-note", "Reflects the current settings, including any not yet run."),
      actions = tags$button(
        type = "button", class = "btn btn-default btn-sm gtx-copy",
        onclick = "gtxCopyCode('trial_code', this)",
        icon("copy", `aria-hidden` = "true"), " Copy R code"
      ),
      class = "gta-code-panel"
    ),
    div(
      class = "gta-toolbar",
      role = "toolbar",
      `aria-label` = "Risk difference actions",
      div(class = "gta-toolbar-group", gta_panel_toggle("gta-panel-trial-options", "sliders", "Options", badge = TRUE)),
      div(
        class = "gta-toolbar-group gta-toolbar-end gtw-toolbar",
        tags$button(
          type = "button", class = "gta-tb gtw-tb-btn gtw-tb-export",
          `aria-label` = "Export result", `data-gtw-tip-base` = "Export result",
          `aria-haspopup` = "menu", `aria-expanded` = "false",
          icon("download", `aria-hidden` = "true"),
          span(class = "gta-tb-label", "Export"),
          icon("caret-up", class = "gta-caret", `aria-hidden` = "true")
        ),
        gta_panel_toggle("gta-panel-trial-code", "code", "Code")
      )
    )
  )
}

# Task-based navigation (tab values are the existing main_nav values).
gta_nav_config <- list(
  primary = list(
    list(value = "Descriptive", label = "Describe"),
    list(value = "Regression", label = "Regression"),
    list(value = "Survival", label = "Survival"),
    list(value = "Trials", label = "Trials"),
    list(value = "Special Models", label = "More models"),
    list(value = "Advanced", label = "Model tools")
  ),
  menus = list(),
  trailing = list(list(value = "Visualise & Export", label = "Visualise & export")),
  hidden = list("Session Code", "Help")
)

gta_config <- function() {
  config <- list(
    host = gta_host$id,
    openDataWhenEmpty = isTRUE(gta_host$open_data_when_empty),
    nav = gta_nav_config
  )
  tags$script(
    type = "application/json",
    id = "gta-config",
    HTML(as.character(jsonlite::toJSON(config, auto_unbox = TRUE)))
  )
}

# Shared GT Analysis shell: styles (D3.0 design system) and behaviour.
# Emitted only when gta_shell_enabled is TRUE; scoped beneath html.gta-shell.
gta_css <- HTML("
/* ==== GT Analysis shared shell (D3). Scoped beneath html.gta-shell. ==== */
/* ---- D3.0 design tokens ------------------------------------------------ */
html.gta-shell {
  --gta-ink: #1B1C1A;
  --gta-text: #2C2D2B;
  --gta-muted: #6A6C67;
  --gta-faint: #9A9C97;
  --gta-line: #E1E2DD;
  --gta-line-strong: #C8C9C4;
  --gta-surface: #FFFFFF;
  --gta-subtle: #F6F6F3;
  --gta-chrome: #1F2328;
  --gta-chrome-text: #E7E8E5;
  --gta-chrome-muted: #AEB2AC;
  --gta-chrome-accent: #8FC2B1;
  --gta-primary: #2F5D50;
  --gta-primary-hover: #264C41;
  --gta-primary-soft: #E6EFEB;
  --gta-primary-line: #C7DCD3;
  --gta-caution-ink: #9A5B0B;
  --gta-caution-bg: #FFF6E8;
  --gta-error-ink: #A1362A;
  --gta-error-bg: #FBEEEC;
  --gta-s1: 4px;
  --gta-s2: 8px;
  --gta-s3: 12px;
  --gta-s4: 16px;
  --gta-s5: 24px;
  --gta-radius: 4px;
  --gta-radius-chip: 11px;
  --gta-shadow: 0 8px 24px rgba(0, 0, 0, 0.16), 0 1px 3px rgba(0, 0, 0, 0.08);
  --gta-top-h: 44px;
  --gta-toolbar-h: 40px;
  --gta-results-head-h: 34px;
  --gta-control-h: 28px;
  --gta-chip-h: 22px;
  --gta-font: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, 'Helvetica Neue', Arial, sans-serif;
  --gta-mono: Menlo, Consolas, 'Liberation Mono', monospace;
  /* Existing D0/D2 tokens resolve to the D3 language. */
  --gtw-accent: var(--gta-primary);
  --gtw-accent-soft: var(--gta-primary-soft);
  --gtw-primary: var(--gta-primary);
  --gtw-line: var(--gta-line);
  --gtw-line-strong: var(--gta-line-strong);
  --gtw-subtle: var(--gta-subtle);
  --gtw-canvas: var(--gta-surface);
  --gtw-input-h: var(--gta-control-h);
  --gtw-btn-h: var(--gta-control-h);
  --gtwc-accent: var(--gta-primary);
  --gtwc-accent-soft: var(--gta-primary-soft);
  --gtwc-line: var(--gta-line);
  --gtwc-line-strong: var(--gta-line-strong);
  --gtwc-shadow: var(--gta-shadow);
}
html.gta-shell body {
  padding-left: 0 !important;
  height: 100vh;
  overflow: hidden;
  font-family: var(--gta-font);
  font-size: 13px;
  color: var(--gta-text);
  background: var(--gta-surface);
}
html.gta-shell .gtw-sr-only {
  position: absolute; width: 1px; height: 1px; padding: 0; margin: -1px;
  overflow: hidden; clip: rect(0, 0, 0, 0); border: 0;
}

/* ---- Region 1: top bar -------------------------------------------------- */
html.gta-shell .gtw-nav {
  position: fixed;
  top: 0;
  left: 0;
  right: 0;
  bottom: auto;
  width: auto;
  height: var(--gta-top-h);
  min-height: 0;
  margin: 0;
  overflow: visible;
  border: 0;
  border-radius: 0;
  background: var(--gta-chrome) !important;
  z-index: 1030;
}
html.gta-shell .gtw-nav > .container-fluid {
  display: flex;
  align-items: stretch;
  height: 100%;
  padding: 0 112px 0 10px;
}
html.gta-shell .gtw-nav .navbar-header {
  float: none;
  display: flex;
  align-items: center;
  flex: 0 1 auto;
  min-width: 0;
  margin: 0 12px 0 0 !important;
}
html.gta-shell .gtw-nav .navbar-brand {
  float: none;
  display: flex;
  align-items: center;
  min-width: 0;
  height: auto;
  margin: 0 !important;
  padding: 0;
}
html.gta-shell #main_nav {
  float: none;
  display: flex;
  flex-direction: row;
  align-items: stretch;
  flex: 0 0 auto;
  margin: 0;
  padding: 0;
  border: 0;
}
html.gta-shell #main_nav > li { float: none; display: flex; position: relative; }
html.gta-shell #main_nav > li.gta-hidden,
html.gta-shell #main_nav > li.gta-in-menu { display: none; }
html.gta-shell #main_nav > li > a,
html.gta-shell .gta-menu-btn {
  display: flex;
  align-items: center;
  gap: 6px;
  height: var(--gta-top-h);
  margin: 0;
  padding: 0 12px;
  font-family: var(--gta-font);
  font-size: 13px;
  font-weight: 500;
  line-height: 1;
  white-space: nowrap;
  color: var(--gta-chrome-muted) !important;
  background: transparent !important;
  border: 0;
  border-bottom: 2px solid transparent;
  border-radius: 0;
  cursor: pointer;
}
html.gta-shell #main_nav > li > a:hover,
html.gta-shell .gta-menu-btn:hover { color: #FFFFFF !important; }
html.gta-shell #main_nav > li.active > a,
html.gta-shell .gta-menu-btn.is-active {
  color: #FFFFFF !important;
  border-bottom-color: var(--gta-chrome-accent);
}
html.gta-shell .gta-menu-btn .fa,
html.gta-shell .gta-menu-btn .fas,
html.gta-shell .gta-menu-btn svg { font-size: 10px; opacity: 0.8; }
html.gta-shell .gta-menu-list {
  position: absolute;
  top: calc(var(--gta-top-h) - 2px);
  left: 0;
  z-index: 1060;
  min-width: 190px;
  padding: 4px;
  background: var(--gta-surface);
  border: 1px solid var(--gta-line-strong);
  border-radius: var(--gta-radius);
  box-shadow: var(--gta-shadow);
}
html.gta-shell .gta-menu-list[hidden] { display: none; }
html.gta-shell .gta-menu-item {
  display: block;
  width: 100%;
  height: 30px;
  padding: 0 10px;
  font-family: var(--gta-font);
  font-size: 13px;
  text-align: left;
  color: var(--gta-text);
  background: transparent;
  border: 0;
  border-radius: 3px;
  cursor: pointer;
}
html.gta-shell .gta-menu-item:hover,
html.gta-shell .gta-menu-item:focus { background: var(--gta-primary-soft); outline: none; }
html.gta-shell .gta-menu-item[aria-current='page'] { font-weight: 600; color: var(--gta-primary); }

/* Data chip (persistent dataset state; opens the Data workspace) */
html.gta-shell .gta-chip {
  display: inline-flex;
  align-items: center;
  gap: 8px;
  height: 28px;
  max-width: 360px;
  min-width: 0;
  padding: 0 10px;
  font-family: var(--gta-font);
  font-size: 13px;
  line-height: 1;
  white-space: nowrap;
  color: var(--gta-chrome-text);
  background: rgba(255, 255, 255, 0.06);
  border: 1px solid rgba(255, 255, 255, 0.22);
  border-radius: var(--gta-radius);
  cursor: pointer;
}
html.gta-shell .gta-chip:hover { background: rgba(255, 255, 255, 0.13); }
html.gta-shell .gta-chip > .fa,
html.gta-shell .gta-chip > .fas,
html.gta-shell .gta-chip > svg { flex: 0 0 auto; font-size: 12px; color: var(--gta-chrome-accent); }
html.gta-shell .gta-chip #analysis_status { display: inline-flex; min-width: 0; overflow: hidden; }
html.gta-shell .gta-chip .gtw-status {
  display: block;
  height: auto;
  margin: 0;
  padding: 0;
  font-size: 13px;
  line-height: 1.2;
  color: inherit;
  background: none;
  border: 0;
  overflow: hidden;
  text-overflow: ellipsis;
  white-space: nowrap;
}
html.gta-shell .gta-chip .gtw-status-name { color: #FFFFFF; font-weight: 600; }
html.gta-shell .gta-chip .gtw-status-sep { margin: 0 5px; color: #7E837D; }
html.gta-shell .gta-chip .gtw-status-empty { color: #FFFFFF; font-weight: 600; }
html.gta-shell .gta-chip.gta-chip-empty { background: var(--gta-primary); border-color: var(--gta-primary); }
html.gta-shell .gta-chip.gta-chip-empty:hover { background: var(--gta-primary-hover); }
html.gta-shell .gta-chip.gta-chip-empty > .fa,
html.gta-shell .gta-chip.gta-chip-empty > .fas,
html.gta-shell .gta-chip.gta-chip-empty > svg { color: #FFFFFF; }

/* Global actions (Session code, Help, Close) */
html.gta-shell .gta-global-actions {
  position: fixed;
  top: 8px;
  right: 10px;
  z-index: 1040;
  display: flex;
  gap: 4px;
}
html.gta-shell .gta-icon-btn,
html.gta-shell .gtw-main .gta-icon-btn.btn {
  display: inline-flex;
  align-items: center;
  justify-content: center;
  flex: 0 0 auto;
  width: 28px;
  height: 28px;
  min-height: 28px;
  margin: 0;
  padding: 0;
  font-size: 14px;
  line-height: 1;
  color: var(--gta-muted);
  background: transparent;
  border: 0;
  border-radius: var(--gta-radius);
  box-shadow: none;
  cursor: pointer;
}
html.gta-shell .gta-icon-btn:hover,
html.gta-shell .gtw-main .gta-icon-btn.btn:hover { color: var(--gta-ink); background: var(--gta-subtle); }
html.gta-shell .gta-global-actions .gta-icon-btn,
html.gta-shell .gtw-main .gta-global-actions .gta-icon-btn.btn { color: var(--gta-chrome-text); background: transparent; }
html.gta-shell .gta-global-actions .gta-icon-btn:hover,
html.gta-shell .gta-global-actions .gta-icon-btn[aria-current='page'],
html.gta-shell .gtw-main .gta-global-actions .gta-icon-btn.btn:hover { color: #FFFFFF; background: rgba(255, 255, 255, 0.13); }
html.gta-shell .gtw-main .gta-global-actions .gta-close.btn:hover,
html.gta-shell .gtw-main .gta-global-actions .gta-close.btn:focus-visible { color: #FFFFFF; background: var(--gta-error-ink); }

/* Focus: always visible */
html.gta-shell .gta-icon-btn:focus-visible,
html.gta-shell .gta-tb:focus-visible,
html.gta-shell .gta-menu-btn:focus-visible,
html.gta-shell .gta-chip:focus-visible,
html.gta-shell #main_nav > li > a:focus-visible,
html.gta-shell .gta-workspace .btn:focus-visible,
html.gta-shell .gta-modal .btn:focus-visible,
html.gta-shell .gta-modal .nav-tabs > li > a:focus-visible {
  outline: 2px solid var(--gta-primary);
  outline-offset: 1px;
}
html.gta-shell .gta-modal .nav-tabs > li > a:focus-visible { outline-offset: -4px; }
html.gta-shell .gtw-nav .gta-chip:focus-visible,
html.gta-shell .gtw-nav .gta-menu-btn:focus-visible,
html.gta-shell #main_nav > li > a:focus-visible,
html.gta-shell .gta-global-actions .gta-icon-btn:focus-visible { outline-color: var(--gta-chrome-accent); outline-offset: -2px; }

/* ---- Workspace frame ------------------------------------------------------ */
html.gta-shell .gtw-main {
  position: absolute;
  top: var(--gta-top-h);
  left: 0;
  right: 0;
  bottom: 0;
  height: auto;
  padding: 0;
  display: flex;
  flex-direction: column;
}
html.gta-shell .gtw-main > .row:first-child { flex: 0 0 auto; height: 0; margin: 0; }
html.gta-shell .gtw-main > .tab-content { flex: 1 1 auto; min-height: 0; }
html.gta-shell .gtw-main > .tab-content > .tab-pane.active { height: 100%; overflow-y: auto; padding: var(--gta-s3); }
html.gta-shell .gtw-main > .tab-content > .tab-pane.active.gta-pane { padding: 0; overflow: hidden; }
html.gta-shell .gtw-main .btn { min-height: var(--gta-control-h); font-size: 13px; }
html.gta-shell .gtw-main .btn-primary,
html.gta-shell .gtw-main .btn-info { background: var(--gta-primary); border-color: var(--gta-primary); color: #FFFFFF; }
html.gta-shell .gtw-main .btn-primary:hover,
html.gta-shell .gtw-main .btn-primary:focus { background: var(--gta-primary-hover); border-color: var(--gta-primary-hover); }

html.gta-shell .gta-workspace {
  position: relative;
  display: flex;
  flex-direction: column;
  height: 100%;
  overflow: hidden;
  background: var(--gta-surface);
  border: 0;
  border-radius: 0;
}

/* ---- Region 2: analysis bar ------------------------------------------- */
html.gta-shell .gta-bar {
  flex: 0 0 auto;
  max-height: 124px;
  padding: 8px var(--gta-s3) 6px;
  border-bottom: 1px solid var(--gta-line);
  background: var(--gta-surface);
}
html.gta-shell .gta-bar-row { display: flex; align-items: flex-end; gap: var(--gta-s4); }
html.gta-shell .gta-bar-slots { flex: 1 1 auto; min-width: 0; }
html.gta-shell .gta-bar-slots > .shiny-html-output { display: flex; align-items: flex-start; gap: var(--gta-s4); min-height: 49px; }
html.gta-shell .gta-bar-slots > .shiny-html-output:empty::before {
  content: 'Group by and Variables appear here once data is loaded.';
  align-self: center;
  font-size: 13px;
  color: var(--gta-faint);
}
html.gta-shell .gta-bar-slots .form-group { margin: 0; }
html.gta-shell .gta-bar-slots .shiny-input-container:not(.shiny-input-checkboxgroup) { flex: 0 0 200px; width: 200px; }
html.gta-shell .gta-bar-slots .shiny-input-checkboxgroup { flex: 1 1 auto; min-width: 200px; width: auto; }
html.gta-shell .gta-bar .control-label,
html.gta-shell .gta-bar .form-group > label {
  display: inline-block;
  margin: 0 0 3px;
  font-size: 12px;
  font-weight: 600;
  line-height: 16px;
  color: var(--gta-muted);
}
html.gta-shell .gta-bar .gtw-picker { min-height: var(--gta-control-h); padding: 2px 2px 2px 3px; }
html.gta-shell .gta-bar .gtw-picker-chips { max-height: 50px; overflow-y: auto; }
html.gta-shell .gta-bar .gtw-picker-empty { color: var(--gta-faint); }
html.gta-shell .gta-bar-run { flex: 0 0 auto; padding-bottom: 1px; }
html.gta-shell .gta-bar-run .btn { height: var(--gta-control-h); padding: 0 14px; font-weight: 600; }
html.gta-shell .gta-ready {
  display: flex;
  align-items: center;
  gap: 6px;
  min-width: 0;
  margin-top: 6px;
  font-size: 12px;
  line-height: 17px;
  color: var(--gta-muted);
  white-space: nowrap;
}
html.gta-shell .gta-ready-text { min-width: 0; overflow: hidden; text-overflow: ellipsis; }
html.gta-shell .gta-ready-mark { flex: 0 0 auto; font-weight: 700; }
html.gta-shell .gta-ready[data-ready='true'] .gta-ready-mark::before { content: '\\2713'; color: var(--gta-primary); }
html.gta-shell .gta-ready[data-ready='true'] .gta-ready-text { color: var(--gta-text); }
html.gta-shell .gta-ready[data-checks='true'] .gta-ready-mark::before { content: '!'; color: var(--gta-caution-ink); }
html.gta-shell .gta-ready[data-checks='true'] .gta-ready-text { color: var(--gta-caution-ink); }
html.gta-shell .gta-pop .gtx-checks { margin-top: 8px; }
html.gta-shell.gtw-focus .gta-bar { display: none; }

/* ---- Region 3: results workspace ------------------------------------------ */
html.gta-shell .gta-results { position: relative; flex: 1 1 auto; min-height: 0; display: flex; flex-direction: column; }
html.gta-shell .gta-results-head {
  flex: 0 0 var(--gta-results-head-h);
  display: flex;
  align-items: stretch;
  justify-content: space-between;
  padding: 0 8px 0 var(--gta-s3);
  border-bottom: 1px solid var(--gta-line);
}
html.gta-shell .gta-result-tabs { display: flex; align-items: stretch; gap: 4px; }
html.gta-shell .gta-result-tab {
  display: flex;
  align-items: center;
  padding: 0 4px;
  font-size: 13px;
  font-weight: 500;
  color: var(--gta-muted);
  border-bottom: 2px solid transparent;
}
html.gta-shell .gta-result-tab.is-active { color: var(--gta-ink); border-bottom-color: var(--gta-primary); }
html.gta-shell .gta-result-tab .gtw-help { display: inline-flex; }
html.gta-shell .gta-results-head > .gta-icon-btn { align-self: center; }
html.gta-shell .gta-results-head > .gtw-tb-focus[aria-pressed='true'] { color: var(--gta-primary); background: var(--gta-primary-soft); }
html.gta-shell .gta-results-body { position: relative; flex: 1 1 auto; min-height: 0; overflow: auto; padding: 4px var(--gta-s3) var(--gta-s3); }
html.gta-shell .gta-results-body > .gtx-card { margin: 0; padding: 0; }
html.gta-shell .gta-describe .gtx-card > .download-strip ~ .shiny-html-output:empty::before { content: none; display: none; }

/* Empty, ready and no-data states */
html.gta-shell .gta-empty {
  position: absolute;
  inset: 0;
  display: flex;
  align-items: center;
  justify-content: center;
  padding: var(--gta-s5);
  background: var(--gta-surface);
}
html.gta-shell .gta-empty-state { display: none; max-width: 420px; text-align: center; }
html.gta-shell [data-gta-state='nodata'] .gta-empty-state[data-gta-for='nodata'],
html.gta-shell [data-gta-state='empty'] .gta-empty-state[data-gta-for='empty'],
html.gta-shell [data-gta-state='ready'] .gta-empty-state[data-gta-for='ready'] { display: block; }
html.gta-shell [data-gta-state='results'] .gta-empty { display: none; }
html.gta-shell .gta-empty-icon { display: block; margin: 0 auto 10px; font-size: 24px; color: var(--gta-faint); }
html.gta-shell .gta-empty-title { margin-bottom: 4px; font-size: 14px; font-weight: 600; color: var(--gta-ink); }
html.gta-shell .gta-empty-text { margin-bottom: 14px; font-size: 13px; line-height: 18px; color: var(--gta-muted); }
html.gta-shell .gta-empty .btn { height: var(--gta-control-h); padding: 0 14px; }

/* ---- Region 4: bottom toolbar --------------------------------------------- */
html.gta-shell .gta-toolbar {
  position: relative;
  z-index: 20;
  flex: 0 0 var(--gta-toolbar-h);
  display: flex;
  align-items: center;
  justify-content: space-between;
  gap: var(--gta-s2);
  padding: 0 var(--gta-s2);
  background: var(--gta-surface);
  border-top: 1px solid var(--gta-line);
}
html.gta-shell .gta-toolbar-group { display: flex; align-items: center; gap: 2px; }
html.gta-shell .gta-toolbar .gtw-toolbar { position: static; float: none; margin: 0; gap: 2px; }
html.gta-shell .gta-tb,
html.gta-shell .gta-toolbar .gtw-toolbar .gta-tb.gtw-tb-btn {
  display: inline-flex;
  align-items: center;
  gap: 6px;
  height: var(--gta-control-h);
  min-height: var(--gta-control-h) !important;
  padding: 0 10px !important;
  font-family: var(--gta-font);
  font-size: 13px !important;
  font-weight: 500;
  line-height: 1 !important;
  color: var(--gta-text);
  background: transparent;
  border: 1px solid transparent;
  border-radius: var(--gta-radius);
  opacity: 1;
  cursor: pointer;
}
html.gta-shell .gta-tb:hover { background: var(--gta-subtle); }
html.gta-shell .gta-tb > .fa,
html.gta-shell .gta-tb > .fas,
html.gta-shell .gta-tb > svg { font-size: 12px; color: var(--gta-muted); }
html.gta-shell .gta-tb[aria-expanded='true'] { color: var(--gta-primary); background: var(--gta-primary-soft); }
html.gta-shell .gta-tb[aria-expanded='true'] > .fa,
html.gta-shell .gta-tb[aria-expanded='true'] > .fas,
html.gta-shell .gta-tb[aria-expanded='true'] > svg { color: var(--gta-primary); }
html.gta-shell .gta-tb[aria-disabled='true'],
html.gta-shell .gta-toolbar .gtw-toolbar .gta-tb[aria-disabled='true'] { color: var(--gta-faint); background: transparent; opacity: 1; cursor: default; }
html.gta-shell .gta-tb[aria-disabled='true'] > .fa,
html.gta-shell .gta-tb[aria-disabled='true'] > .fas,
html.gta-shell .gta-tb[aria-disabled='true'] > svg { color: var(--gta-faint); }
html.gta-shell .gta-tb .gta-caret { font-size: 9px !important; transition: transform 0.12s; }
html.gta-shell .gta-tb[aria-expanded='true'] .gta-caret { transform: rotate(180deg); }
html.gta-shell .gta-badge {
  min-width: 18px;
  height: 18px;
  padding: 0 5px;
  font-size: 11px;
  font-weight: 600;
  line-height: 18px;
  text-align: center;
  color: #FFFFFF;
  background: var(--gta-primary);
  border-radius: 9px;
}
html.gta-shell .gta-badge[hidden] { display: none; }

/* ---- Bottom panels (one open at a time) ----------------------------------- */
html.gta-shell .gta-panel,
html.gta-shell .gta-panel.gtw-options {
  position: absolute;
  left: 0;
  right: 0;
  bottom: var(--gta-toolbar-h);
  z-index: 15;
  display: flex;
  flex-direction: column;
  max-height: 45%;
  margin: 0;
  background: var(--gta-surface);
  border: 0;
  border-top: 1px solid var(--gta-line-strong);
  border-radius: 0;
  box-shadow: 0 -8px 24px rgba(0, 0, 0, 0.10);
  visibility: hidden;
  opacity: 0;
  transform: translateY(8px);
  transition: opacity 0.12s, transform 0.12s, visibility 0s linear 0.12s;
}
html.gta-shell .gta-panel:focus { outline: none; }
html.gta-shell .gta-panel:focus-visible .gta-panel-title { text-decoration: underline; text-underline-offset: 3px; }
html.gta-shell .gta-panel.is-open { visibility: visible; opacity: 1; transform: none; transition: opacity 0.12s, transform 0.12s; }
@media (max-height: 760px) {
  html.gta-shell .gta-panel,
  html.gta-shell .gta-panel.gtw-options { max-height: 55%; }
}
html.gta-shell .gta-panel-head {
  flex: 0 0 36px;
  display: flex;
  align-items: center;
  gap: 10px;
  min-width: 0;
  padding: 0 6px 0 var(--gta-s3);
  border-bottom: 1px solid var(--gta-line);
}
html.gta-shell .gta-panel-title { flex: 0 0 auto; font-size: 13px; font-weight: 650; color: var(--gta-ink); }
html.gta-shell .gta-panel-head .gtw-options-state,
html.gta-shell .gta-panel-note {
  flex: 1 1 auto;
  min-width: 0;
  overflow: hidden;
  text-overflow: ellipsis;
  white-space: nowrap;
  font-size: 12px;
  color: var(--gta-muted);
}
html.gta-shell .gta-panel-actions { flex: 0 0 auto; display: flex; gap: 6px; }
html.gta-shell .gta-panel-actions .btn { height: 24px; min-height: 24px; padding: 0 8px; font-size: 12px; line-height: 22px; }
html.gta-shell .gta-panel-body,
html.gta-shell .gta-panel.gtw-options .gtw-options-body {
  flex: 1 1 auto;
  min-height: 0;
  overflow: auto;
  padding: 10px var(--gta-s3) var(--gta-s3);
  border: 0;
}
html.gta-shell .gta-panel.gta-fit .gta-panel-body { overflow: visible; }
html.gta-shell .gta-options-panel .gta-panel-body {
  display: grid;
  grid-template-columns: repeat(auto-fill, minmax(190px, 1fr));
  gap: 8px var(--gta-s4);
  align-items: start;
}
html.gta-shell .gta-options-panel .gta-panel-body .form-group { margin: 0; }
html.gta-shell .gta-panel .selectize-dropdown { top: auto !important; bottom: 100% !important; margin: 0 0 2px; }
html.gta-shell .gta-code-panel pre { margin: 0; background: var(--gta-subtle); border-color: var(--gta-line); }

/* ---- Layered Data workspace (modal) ---------------------------------------- */
html.gta-shell .gta-modal {
  position: fixed;
  inset: 0;
  z-index: 1500;
  display: flex;
  align-items: center;
  justify-content: center;
  visibility: hidden;
  opacity: 0;
  transition: opacity 0.12s, visibility 0s linear 0.12s;
}
html.gta-shell .gta-modal.is-open { visibility: visible; opacity: 1; transition: opacity 0.12s; }
html.gta-shell .gta-scrim { position: absolute; inset: 0; background: rgba(0, 0, 0, 0.35); }
html.gta-shell .gta-modal-card {
  position: relative;
  display: flex;
  flex-direction: column;
  width: min(1120px, calc(100vw - 32px));
  height: min(720px, calc(100vh - 32px));
  overflow: hidden;
  background: var(--gta-surface);
  border-radius: 6px;
  box-shadow: var(--gta-shadow);
}
html.gta-shell .gta-modal-title {
  position: absolute;
  top: 0;
  left: var(--gta-s5);
  z-index: 2;
  display: flex;
  align-items: center;
  height: 52px;
  font-size: 16px;
  font-weight: 650;
  color: var(--gta-ink);
}
html.gta-shell .gta-modal-close { position: absolute; top: 12px; right: 12px; z-index: 2; }
html.gta-shell .gta-modal-card > .tabbable { flex: 1 1 auto; min-height: 0; display: flex; flex-direction: column; }
html.gta-shell .gta-modal-card > .tabbable > .nav-tabs {
  flex: 0 0 52px;
  display: flex;
  align-items: stretch;
  margin: 0;
  padding: 0 56px 0 96px;
  border-bottom: 1px solid var(--gta-line);
}
html.gta-shell .gta-modal-card > .tabbable > .nav-tabs > li { float: none; display: flex; margin: 0; }
html.gta-shell .gta-modal-card > .tabbable > .nav-tabs > li > a {
  display: flex;
  align-items: center;
  margin: 0;
  padding: 0 12px;
  font-size: 13px;
  font-weight: 500;
  color: var(--gta-muted);
  background: transparent !important;
  border: 0 !important;
  border-bottom: 2px solid transparent !important;
  border-radius: 0;
}
html.gta-shell .gta-modal-card > .tabbable > .nav-tabs > li > a:hover { color: var(--gta-ink); }
html.gta-shell .gta-modal-card > .tabbable > .nav-tabs > li.active > a { color: var(--gta-ink); border-bottom-color: var(--gta-primary) !important; }
html.gta-shell .gta-modal-card > .tabbable > .tab-content { flex: 1 1 auto; min-height: 0; overflow: hidden; }
html.gta-shell .gta-modal-card > .tabbable > .tab-content > .tab-pane { height: 100%; overflow: auto; padding: var(--gta-s4) var(--gta-s5); }
html.gta-shell .gta-modal-foot {
  flex: 0 0 52px;
  display: flex;
  align-items: center;
  justify-content: space-between;
  gap: var(--gta-s3);
  padding: 0 var(--gta-s4) 0 var(--gta-s5);
  border-top: 1px solid var(--gta-line);
}
html.gta-shell .gta-data-status { min-width: 0; overflow: hidden; text-overflow: ellipsis; white-space: nowrap; font-size: 12px; color: var(--gta-muted); }
html.gta-shell .gta-data-status.is-loaded { color: var(--gta-text); }
html.gta-shell .gta-data-status.is-loaded::before { content: '\\2713  '; color: var(--gta-primary); font-weight: 700; }
html.gta-shell .gta-modal-foot .btn { height: var(--gta-control-h); padding: 0 16px; font-weight: 600; }

/* Import: source list | source form */
html.gta-shell .gta-import {
  display: grid;
  grid-template-columns: 200px minmax(0, 1fr);
  min-height: calc(100% + 32px);
  margin: calc(-1 * var(--gta-s4)) calc(-1 * var(--gta-s5));
}
html.gta-shell .gta-source-list { padding: var(--gta-s4) var(--gta-s3) var(--gta-s4) 0; border-right: 1px solid var(--gta-line); }
html.gta-shell .gta-source-list .form-group { margin: 0; }
html.gta-shell .gta-source-list .form-group > label.control-label {
  display: block;
  margin: 0 0 6px var(--gta-s5);
  font-size: 11px;
  font-weight: 700;
  letter-spacing: 0.06em;
  text-transform: uppercase;
  color: var(--gta-muted);
}
html.gta-shell .gta-source-list .radio { position: relative; margin: 0; }
html.gta-shell .gta-source-list .radio label {
  display: block;
  padding: 7px 10px 7px 21px;
  font-size: 13px;
  font-weight: 500;
  color: var(--gta-text);
  border-left: 3px solid transparent;
  border-radius: 0 var(--gta-radius) var(--gta-radius) 0;
  cursor: pointer;
}
html.gta-shell .gta-source-list .radio label:hover { background: var(--gta-subtle); }
html.gta-shell .gta-source-list .radio input[type='radio'] { position: absolute; opacity: 0; margin: 0; }
html.gta-shell .gta-source-list .radio label:has(input:checked) { color: var(--gta-ink); font-weight: 600; background: var(--gta-primary-soft); border-left-color: var(--gta-primary); }
html.gta-shell .gta-source-list .radio label:has(input:focus-visible) { outline: 2px solid var(--gta-primary); outline-offset: -2px; }
html.gta-shell .gta-source-form { max-width: 680px; padding: var(--gta-s4) var(--gta-s5); }
html.gta-shell .gta-form-title { margin: 0 0 var(--gta-s3); font-size: 14px; font-weight: 600; color: var(--gta-ink); }
html.gta-shell .gta-source-form .gta-refresh { margin-right: 6px; }
html.gta-shell .gta-source-form .gtw-options { margin: 4px 0 12px; }
html.gta-shell .gta-source-form .gtx-template-grid { margin-bottom: var(--gta-s3); }
html.gta-shell .gta-source-form hr { margin: var(--gta-s4) 0; border-color: var(--gta-line); }
html.gta-shell .gta-modal .gtx-card { border-color: var(--gta-line); box-shadow: none; }
html.gta-shell .gta-code-block { margin-top: var(--gta-s4); }
html.gta-shell .gta-code-head { display: flex; align-items: center; justify-content: space-between; margin-bottom: 6px; }
html.gta-shell .gta-section-title { font-size: 11px; font-weight: 700; letter-spacing: 0.06em; text-transform: uppercase; color: var(--gta-muted); }
html.gta-shell .gta-code-head .btn { height: 24px; min-height: 24px; padding: 0 8px; font-size: 12px; line-height: 22px; }

/* ---- M3: shared slot primitives ------------------------------------------ */
html.gta-shell .gta-slot { min-width: 0; }
html.gta-shell .gta-slot .form-group { margin: 0; }
html.gta-shell .gta-slot-label {
  display: block;
  margin: 0 0 3px;
  font-size: 12px;
  font-weight: 600;
  line-height: 16px;
  color: var(--gta-muted);
}
html.gta-shell .gta-slot-label .gtw-help { display: inline-flex; }
html.gta-shell .gta-btn-group { display: inline-flex; }
html.gta-shell .gta-btn-group .btn { height: var(--gta-control-h); padding: 0 12px; font-weight: 600; white-space: nowrap; }
html.gta-shell .gta-btn-group .btn + .btn { margin-left: 1px; }
html.gta-shell .gta-btn-group .btn:first-child { border-top-right-radius: 0; border-bottom-right-radius: 0; }
html.gta-shell .gta-btn-group .btn:last-child { border-top-left-radius: 0; border-bottom-left-radius: 0; }
html.gta-shell .gta-text-btn {
  flex: 0 0 auto;
  padding: 0 2px;
  font-size: 12px;
  font-weight: 600;
  color: var(--gta-primary);
  background: none;
  border: 0;
  cursor: pointer;
}
html.gta-shell .gta-text-btn:hover { text-decoration: underline; }
html.gta-shell .gta-text-btn:focus-visible { outline: 2px solid var(--gta-primary); outline-offset: 1px; }
html.gta-shell .gta-ready .gta-ready-details { display: none; }
html.gta-shell .gta-ready[data-ready='true'] .gta-ready-details { display: inline; }

/* Popover (non-modal, light dismiss); kept laid out so outputs stay bound */
html.gta-shell .gta-pop {
  position: fixed;
  top: 0;
  left: 0;
  z-index: 2500;
  width: 340px;
  max-height: 60vh;
  overflow: auto;
  padding: 10px 12px;
  font-size: 12.5px;
  line-height: 1.45;
  background: var(--gta-surface);
  border: 1px solid var(--gta-line-strong);
  border-radius: var(--gta-radius);
  box-shadow: var(--gta-shadow);
  visibility: hidden;
  opacity: 0;
}
html.gta-shell .gta-pop.is-open { visibility: visible; opacity: 1; }
html.gta-shell .gta-pop .gtx-preflight { margin: 0; padding: 0; border: 0; background: none; font-size: 12.5px; }

/* ---- M3: Regression analysis bar ----------------------------------------- */
html.gta-shell .gta-reg-grid {
  display: grid;
  grid-template-columns: minmax(130px, 170px) minmax(170px, 1fr) minmax(150px, 1fr) minmax(120px, 160px) auto;
  column-gap: var(--gta-s4);
  row-gap: 5px;
  align-items: start;
}
html.gta-shell .gta-reg-grid > #reg_inputs { display: contents; }
html.gta-shell .gta-reg-grid > #reg_inputs:empty::before {
  content: 'Outcome, Exposures and Adjust for appear here once data is loaded.';
  grid-column: 1 / 4;
  grid-row: 1;
  align-self: center;
  font-size: 13px;
  color: var(--gta-faint);
}
html.gta-shell .gta-reg-grid > #reg_inputs > .form-group { width: auto; min-width: 0; margin: 0; }
html.gta-shell .gta-reg-grid > #reg_inputs > .form-group:nth-child(1) { grid-column: 1; grid-row: 1; }
html.gta-shell .gta-reg-grid > #reg_inputs > .form-group:nth-child(2) { grid-column: 2; grid-row: 1; }
html.gta-shell .gta-reg-grid > #reg_inputs > .form-group:nth-child(4) { grid-column: 3; grid-row: 1; }
html.gta-shell .gta-reg-grid > #reg_inputs > .gtx-select-buttons { display: none; }
/* Interaction (inline until D-3): quiet, on the ready-line row */
html.gta-shell .gta-reg-grid > #reg_inputs > .form-group:nth-child(6) {
  grid-column: 4 / 6;
  grid-row: 2;
  justify-self: end;
  display: flex;
  align-items: center;
  gap: 4px;
  max-width: 100%;
}
html.gta-shell .gta-reg-grid > #reg_inputs > .form-group:nth-child(6) > label {
  margin: 0;
  font-size: 12px;
  font-weight: 500;
  color: var(--gta-muted);
  white-space: nowrap;
}
html.gta-shell .gta-reg-grid > #reg_inputs > .form-group:nth-child(6) input.form-control {
  width: 150px;
  height: 22px;
  min-height: 22px;
  padding: 0 6px;
  font-size: 12px;
  border-color: var(--gta-line);
  box-shadow: none;
}
html.gta-shell .gta-reg-grid > #reg_inputs > .form-group:nth-child(6) input.form-control:not(:placeholder-shown) { border-color: var(--gta-primary-line); background: var(--gta-primary-soft); }
html.gta-shell .gta-reg-grid .gta-reg-approach { grid-column: 4; grid-row: 1; }
html.gta-shell .gta-reg-grid .gta-reg-run { grid-column: 5; grid-row: 1; }
html.gta-shell .gta-reg-grid > .gta-ready { grid-column: 1 / 4; grid-row: 2; margin-top: 0; min-height: 22px; }
html.gta-shell .gta-bar .gta-reg-grid .control-label { max-width: 100%; }

/* ---- M3: results with real result tabs ---------------------------------- */
html.gta-shell .gta-results > .tabbable { flex: 1 1 auto; min-height: 0; display: flex; flex-direction: column; }
html.gta-shell .gta-results > .tabbable > .nav-tabs {
  flex: 0 0 var(--gta-results-head-h);
  display: flex;
  align-items: stretch;
  gap: 4px;
  margin: 0;
  padding: 0 48px 0 var(--gta-s3);
  border-bottom: 1px solid var(--gta-line);
}
html.gta-shell .gta-results > .tabbable > .nav-tabs > li { float: none; display: flex; margin: 0; }
html.gta-shell .gta-results > .tabbable > .nav-tabs > li > a {
  display: flex;
  align-items: center;
  margin: 0;
  padding: 0 8px;
  font-size: 13px;
  font-weight: 500;
  color: var(--gta-muted);
  background: transparent !important;
  border: 0 !important;
  border-bottom: 2px solid transparent !important;
  border-radius: 0;
}
html.gta-shell .gta-results > .tabbable > .nav-tabs > li > a:hover { color: var(--gta-ink); }
html.gta-shell .gta-results > .tabbable > .nav-tabs > li.active > a { color: var(--gta-ink); border-bottom-color: var(--gta-primary) !important; }
html.gta-shell .gta-results > .tabbable > .nav-tabs > li > a:focus-visible { outline: 2px solid var(--gta-primary); outline-offset: -3px; }
html.gta-shell .gta-results > .tabbable > .tab-content { flex: 1 1 auto; min-height: 0; overflow: auto; padding: 4px var(--gta-s3) var(--gta-s3); }
html.gta-shell .gta-results > .tabbable > .tab-content .gtx-card { margin: 0; padding: 0; }
html.gta-shell .gta-results > .gta-results-focus { position: absolute; top: 3px; right: 8px; z-index: 3; }
html.gta-shell .gta-results > .gta-results-focus[aria-pressed='true'] { color: var(--gta-primary); background: var(--gta-primary-soft); }
html.gta-shell .gta-results > .gta-empty { top: var(--gta-results-head-h); }
html.gta-shell .gta-workspace .gtx-card > .download-strip ~ .shiny-html-output:empty::before { content: none; display: none; }
html.gta-shell [data-gta-show] .gta-empty-state { display: none; }
html.gta-shell [data-gta-show='nodata'] .gta-empty-state[data-gta-for='nodata'],
html.gta-shell [data-gta-show='no-outcome'] .gta-empty-state[data-gta-for='no-outcome'],
html.gta-shell [data-gta-show='no-exposure'] .gta-empty-state[data-gta-for='no-exposure'],
html.gta-shell [data-gta-show='ready-uni'] .gta-empty-state[data-gta-for='ready-uni'],
html.gta-shell [data-gta-show='ready-multi'] .gta-empty-state[data-gta-for='ready-multi'],
html.gta-shell [data-gta-show='ready-strat'] .gta-empty-state[data-gta-for='ready-strat'],
html.gta-shell [data-gta-show='ready-stats'] .gta-empty-state[data-gta-for='ready-stats'] { display: block; }
html.gta-shell [data-gta-show='results'] .gta-empty { display: none; }

/* ---- M3: Regression panels ------------------------------------------------ */
html.gta-shell .gta-refs-panel #reg_reference_inputs { display: contents; }
html.gta-shell .gta-refs-panel #reg_reference_inputs > hr,
html.gta-shell .gta-refs-panel #reg_reference_inputs > h4,
html.gta-shell .gta-refs-panel #reg_reference_inputs > .gtx-help { display: none; }
html.gta-shell .gta-panel-empty { grid-column: 1 / -1; font-size: 13px; color: var(--gta-muted); }
html.gta-shell .gta-refs-panel #reg_reference_inputs:not(:empty) ~ .gta-panel-empty { display: none; }
html.gta-shell .gta-ref-help { flex: 0 0 auto; display: inline-flex; }
html.gta-shell .gta-ref-help .gtw-help-btn { margin: 0; }
html.gta-shell .gta-strata-panel .gta-panel-body { align-items: end; }
html.gta-shell .gta-strata-panel #reg_strata_inputs { display: contents; }
html.gta-shell .gta-strata-panel .checkbox { margin: 0 0 4px; }
html.gta-shell .gta-strata-panel #reg_strata_conflict { grid-column: 1 / -1; }
html.gta-shell .gtx-role-alert { margin: 0; padding: 10px 12px; font-size: 13px; }
html.gta-shell .gtx-role-alert p { margin: 0 0 6px; }
html.gta-shell .gta-strata-panel #run_strat { justify-self: start; height: var(--gta-control-h); padding: 0 14px; font-weight: 600; }
html.gta-shell .gta-options-panel .checkbox { margin: 2px 0; }
html.gta-shell .gta-code-for[hidden] { display: none; }
html.gta-shell .gta-code-none { font-size: 13px; color: var(--gta-muted); }

/* ---- Survival workspace ---------------------------------------------------- */
html.gta-shell .gta-hidden-input { display: none; }

/* Method selector: the four Survival methods are always visible (navigation,
   not configuration). A quiet segmented control, distinct from result tabs. */
html.gta-shell .gta-surv-methods { display: flex; align-items: center; gap: 4px; margin: 0 0 7px; }
html.gta-shell .gta-seg {
  display: inline-flex;
  align-items: stretch;
  padding: 2px;
  background: var(--gta-subtle);
  border: 1px solid var(--gta-line);
  border-radius: calc(var(--gta-radius) + 2px);
}
html.gta-shell .gta-seg-btn {
  height: 26px;
  padding: 0 14px;
  font-family: var(--gta-font);
  font-size: 13px;
  font-weight: 500;
  line-height: 24px;
  white-space: nowrap;
  color: var(--gta-muted);
  background: transparent;
  border: 1px solid transparent;
  border-radius: var(--gta-radius);
  cursor: pointer;
}
html.gta-shell .gta-seg-btn:hover { color: var(--gta-ink); }
html.gta-shell .gta-seg-btn[aria-checked='true'] {
  font-weight: 600;
  color: var(--gta-primary);
  background: var(--gta-surface);
  border-color: var(--gta-primary-line);
  box-shadow: 0 1px 2px rgba(0, 0, 0, 0.06);
}
html.gta-shell .gta-seg-btn:focus-visible { outline: 2px solid var(--gta-primary); outline-offset: 1px; }
html.gta-shell .gta-surv-methods-help .gtw-help { margin-left: 2px; }

/* Analysis bar grid: the method decides which fields are shown */
html.gta-shell .gta-surv-grid {
  display: grid;
  grid-template-columns: minmax(110px, 160px) minmax(110px, 160px) minmax(160px, 1fr) minmax(150px, 1fr) auto;
  column-gap: var(--gta-s4);
  row-gap: 5px;
  align-items: start;
}
html.gta-shell [data-gta-approach='survreg'] .gta-surv-grid {
  grid-template-columns: minmax(100px, 150px) minmax(100px, 150px) minmax(150px, 1fr) minmax(140px, 1fr) minmax(120px, 150px) auto;
}
html.gta-shell [data-gta-approach='km'] .gta-surv-grid {
  grid-template-columns: minmax(130px, 170px) minmax(130px, 170px) minmax(150px, 220px) 1fr auto;
}
html.gta-shell .gta-surv-grid > #surv_inputs { display: contents; }
html.gta-shell .gta-surv-grid > #surv_inputs:empty::before {
  content: 'Time, Event and the other fields appear here once data is loaded.';
  grid-column: 1 / 5;
  grid-row: 1;
  align-self: center;
  font-size: 13px;
  color: var(--gta-faint);
}
html.gta-shell .gta-surv-grid > #surv_inputs > .form-group { width: auto; min-width: 0; margin: 0; }
html.gta-shell .gta-surv-grid > #surv_inputs > .form-group:nth-child(1) { grid-column: 1; grid-row: 1; }
html.gta-shell .gta-surv-grid > #surv_inputs > .form-group:nth-child(2) { grid-column: 2; grid-row: 1; }
html.gta-shell .gta-surv-grid > #surv_inputs > .form-group:nth-child(3) { grid-column: 3; grid-row: 1; }
html.gta-shell .gta-surv-grid > #surv_inputs > .form-group:nth-child(5) { grid-column: 4; grid-row: 1; }
html.gta-shell .gta-surv-grid > #surv_inputs > .gtx-select-buttons { display: none; }
/* Parametric distribution: model-defining, stays in the first row */
html.gta-shell .gta-surv-grid > #surv_inputs > div:nth-child(9) { grid-column: 5; grid-row: 1; min-width: 0; }
html.gta-shell .gta-surv-grid > #surv_inputs > div:nth-child(9) .form-group { margin: 0; }
html.gta-shell .gta-surv-grid .gta-surv-run { grid-column: -2; grid-row: 1; }
html.gta-shell .gta-surv-grid .gta-surv-km-by { grid-column: 3; grid-row: 1; }
html.gta-shell .gta-surv-grid .gta-surv-km-by .form-group { margin: 0; }
/* Second row: quiet inline definition details */
html.gta-shell .gta-surv-grid > #surv_inputs > div:nth-child(7) { grid-column: 1 / 3; grid-row: 2; }
html.gta-shell .gta-surv-grid > #surv_inputs > #competing_event_codes { grid-column: 1 / 4; grid-row: 2; display: flex; align-items: center; gap: 14px; flex-wrap: wrap; min-width: 0; }
html.gta-shell .gta-surv-grid .gta-surv-multi { grid-column: 3; grid-row: 2; align-self: center; }
html.gta-shell [data-gta-approach='competing'] .gta-surv-grid .gta-surv-multi { grid-column: 4; }
html.gta-shell .gta-surv-grid > #surv_inputs > div:nth-child(7) .form-group,
html.gta-shell .gta-surv-grid > #surv_inputs > #competing_event_codes .form-group {
  display: flex;
  align-items: center;
  gap: 6px;
  margin: 0;
  width: auto;
}
html.gta-shell .gta-surv-grid > #surv_inputs > div:nth-child(7) .form-group > label,
html.gta-shell .gta-surv-grid > #surv_inputs > #competing_event_codes .form-group > label {
  margin: 0;
  font-size: 12px;
  font-weight: 500;
  color: var(--gta-muted);
  white-space: nowrap;
}
html.gta-shell .gta-surv-grid > #surv_inputs > div:nth-child(7) .selectize-control,
html.gta-shell .gta-surv-grid > #surv_inputs > #competing_event_codes .selectize-control { min-width: 110px; }
html.gta-shell .gta-surv-grid > #surv_inputs > div:nth-child(7) .selectize-input,
html.gta-shell .gta-surv-grid > #surv_inputs > #competing_event_codes .selectize-input { min-height: 22px; padding: 1px 22px 1px 6px; font-size: 12px; line-height: 18px; }
html.gta-shell .gta-surv-grid > #surv_inputs > #competing_event_codes > .gtx-help { margin: 0; font-size: 12px; color: var(--gta-faint); }
html.gta-shell .gta-surv-grid > #surv_inputs > #competing_event_codes .form-group ~ .gtx-help { display: none; }
html.gta-shell .gta-surv-multi .checkbox { margin: 0; }
html.gta-shell .gta-surv-multi .checkbox label {
  display: inline-flex;
  align-items: center;
  gap: 6px;
  height: 26px;
  padding: 0 10px 0 8px;
  font-size: 13px;
  font-weight: 600;
  color: var(--gta-ink);
  white-space: nowrap;
  background: var(--gta-surface);
  border: 1px solid var(--gta-line-strong);
  border-radius: var(--gta-radius);
  cursor: pointer;
}
html.gta-shell .gta-surv-multi .checkbox label input[type='checkbox'] { position: static; margin: 0; accent-color: var(--gta-primary); }
html.gta-shell .gta-surv-multi .checkbox label:hover { border-color: #8E908B; }
html.gta-shell .gta-surv-multi[data-gta-on='true'] .checkbox label { color: var(--gta-primary); background: var(--gta-primary-soft); border-color: var(--gta-primary); }
html.gta-shell .gta-surv-grid > .gta-ready { grid-column: 1 / -1; grid-row: 3; margin-top: 0; }
/* Approach visibility */
html.gta-shell [data-gta-approach='km'] .gta-surv-grid > #surv_inputs > .form-group:nth-child(3),
html.gta-shell [data-gta-approach='km'] .gta-surv-grid > #surv_inputs > .form-group:nth-child(5),
html.gta-shell [data-gta-approach='km'] .gta-surv-grid > #surv_inputs > div:nth-child(7),
html.gta-shell [data-gta-approach='km'] .gta-surv-grid .gta-surv-multi { display: none !important; }
/* Group by (km_by) keeps D2's rendering behaviour: in D2 it is rendered
   whenever surv_mode is not 'competing', so outside the Kaplan-Meier approach
   it stays laid out (hidden from view) and is removed only in competing mode. */
html.gta-shell .gta-survival:not([data-gta-approach='km']):not([data-gta-mode='competing']) .gta-surv-grid .gta-surv-km-by {
  position: absolute;
  left: -10000px;
  top: 0;
  width: 1px;
  height: 1px;
  overflow: hidden;
  visibility: hidden;
}
html.gta-shell .gta-survival[data-gta-mode='competing'] .gta-surv-grid .gta-surv-km-by { display: none !important; }
html.gta-shell .gta-survival .gta-run-for,
html.gta-shell .gta-survival .gta-tool-for { display: none; }
html.gta-shell [data-gta-approach='km'] [data-gta-for~='km'],
html.gta-shell [data-gta-approach='cox'] [data-gta-for~='cox'],
html.gta-shell [data-gta-approach='survreg'] [data-gta-for~='survreg'],
html.gta-shell [data-gta-approach='competing'] [data-gta-for~='competing'] { display: inline-flex; }
html.gta-shell .gta-survival .gta-btn-group .gta-run-for .btn { border-radius: 0; }
html.gta-shell .gta-survival .gta-btn-group .gta-run-for:first-child .btn,
html.gta-shell [data-gta-approach='competing'] .gta-btn-group .gta-run-for[data-gta-for='competing'] .btn#run_competing { border-top-left-radius: var(--gta-radius); border-bottom-left-radius: var(--gta-radius); }
html.gta-shell .gta-survival .gta-btn-group .btn { border-radius: var(--gta-radius); }
html.gta-shell [data-gta-approach='competing'] .gta-btn-group #run_competing { border-top-right-radius: 0; border-bottom-right-radius: 0; }
html.gta-shell [data-gta-approach='competing'] .gta-btn-group #run_cuminc { margin-left: 1px; border-top-left-radius: 0; border-bottom-left-radius: 0; }
/* Result tabs per approach */
html.gta-shell .gta-survival .gta-results > .tabbable > .nav-tabs > li[data-gta-tabfor] { display: none; }
html.gta-shell [data-gta-approach='km'] .gta-results > .tabbable > .nav-tabs > li[data-gta-tabfor~='km'],
html.gta-shell [data-gta-approach='cox'] .gta-results > .tabbable > .nav-tabs > li[data-gta-tabfor~='cox'],
html.gta-shell [data-gta-approach='survreg'] .gta-results > .tabbable > .nav-tabs > li[data-gta-tabfor~='survreg'],
html.gta-shell [data-gta-approach='competing'] .gta-results > .tabbable > .nav-tabs > li[data-gta-tabfor~='competing'] { display: flex; }
/* Nested result tabs (Exposure models / Multivariable model) */
html.gta-shell .gta-survival .tab-content .tabbable > .nav-tabs {
  display: flex;
  gap: 2px;
  margin: 2px 0 6px;
  border-bottom: 1px solid var(--gta-line);
}
html.gta-shell .gta-survival .tab-content .tabbable > .nav-tabs > li { float: none; margin: 0; }
html.gta-shell .gta-survival .tab-content .tabbable > .nav-tabs > li > a {
  margin: 0;
  padding: 4px 8px;
  font-size: 12px;
  font-weight: 500;
  color: var(--gta-muted);
  background: transparent !important;
  border: 0 !important;
  border-bottom: 2px solid transparent !important;
  border-radius: 0;
}
html.gta-shell .gta-survival .tab-content .tabbable > .nav-tabs > li.active > a { color: var(--gta-ink); border-bottom-color: var(--gta-primary-line) !important; }
html.gta-shell .gta-survival .shiny-plot-output { min-height: 360px; }

/* Survival: states, narrow widths, row-two details */
html.gta-shell [data-gta-show='need-time'] .gta-empty-state[data-gta-for='need-time'],
html.gta-shell [data-gta-show='need-event'] .gta-empty-state[data-gta-for='need-event'],
html.gta-shell [data-gta-show='need-exposure'] .gta-empty-state[data-gta-for='need-exposure'],
html.gta-shell [data-gta-show='ready-km'] .gta-empty-state[data-gta-for='ready-km'],
html.gta-shell [data-gta-show='ready-cox'] .gta-empty-state[data-gta-for='ready-cox'],
html.gta-shell [data-gta-show='ready-survreg'] .gta-empty-state[data-gta-for='ready-survreg'],
html.gta-shell [data-gta-show='ready-competing'] .gta-empty-state[data-gta-for='ready-competing'],
html.gta-shell [data-gta-show='ready-cuminc'] .gta-empty-state[data-gta-for='ready-cuminc'] { display: block; }
html.gta-shell .gta-survival .gta-bar { max-height: none; }
html.gta-shell .gta-surv-multi .checkbox { display: flex; align-items: center; gap: 4px; white-space: nowrap; }
html.gta-shell .gta-surv-multi { min-width: 0; white-space: nowrap; }
html.gta-shell .gta-surv-grid > #surv_inputs > #competing_event_codes .selectize-control { min-width: 150px; }
@media (max-width: 1120px) {
  html.gta-shell .gta-surv-grid,
  html.gta-shell [data-gta-approach='km'] .gta-surv-grid,
  html.gta-shell [data-gta-approach='survreg'] .gta-surv-grid { column-gap: 12px; }
  html.gta-shell .gta-surv-grid { grid-template-columns: minmax(100px, 140px) minmax(100px, 140px) minmax(130px, 1fr) minmax(120px, 1fr) auto; }
  html.gta-shell [data-gta-approach='survreg'] .gta-surv-grid { grid-template-columns: minmax(90px, 124px) minmax(90px, 124px) minmax(120px, 1fr) minmax(110px, 1fr) minmax(110px, 130px) auto; }
  html.gta-shell .gta-seg-btn { padding: 0 12px; }
  html.gta-shell .gta-survival .gta-btn-group .btn { padding: 0 10px; }
}

html.gta-shell .gta-surv-multi .checkbox label { flex: 0 0 auto; }
html.gta-shell [data-gta-approach='competing'] .gta-surv-grid > #surv_inputs > #competing_event_codes { grid-column: 1 / 4; flex-wrap: nowrap; }
html.gta-shell [data-gta-approach='competing'] .gta-surv-grid .gta-surv-multi { grid-column: 4 / -1; }

/* ---- More models workspace ------------------------------------------------ */
/* Bar grid: the method decides which fields are shown. Server-built fields are
   tagged by role (data-gta-slot) so placement does not depend on their order. */
html.gta-shell .gta-more-grid {
  display: grid;
  grid-template-columns: minmax(130px, 190px) minmax(170px, 1fr) minmax(150px, 1fr) auto;
  column-gap: var(--gta-s4);
  row-gap: 5px;
  align-items: start;
}
html.gta-shell [data-gta-method='matched'] .gta-more-grid { grid-template-columns: minmax(120px, 170px) minmax(160px, 1fr) minmax(140px, 1fr) minmax(130px, 170px) auto; }
html.gta-shell [data-gta-method='multilevel'] .gta-more-grid { grid-template-columns: minmax(110px, 160px) minmax(150px, 1fr) minmax(130px, 1fr) minmax(120px, 160px) minmax(130px, 170px) auto; }
html.gta-shell [data-gta-method='mediation'] .gta-more-grid { grid-template-columns: minmax(110px, 170px) minmax(110px, 170px) minmax(110px, 170px) minmax(150px, 1fr) minmax(100px, 130px) auto; }
html.gta-shell .gta-more-grid > #special_inputs,
html.gta-shell .gta-more-grid > #med_inputs { display: contents; }
html.gta-shell .gta-more-grid > #special_inputs:empty::before {
  content: 'Outcome, Exposures and the other fields appear here once data is loaded.';
  grid-column: 1 / 4;
  grid-row: 1;
  align-self: center;
  font-size: 13px;
  color: var(--gta-faint);
}
html.gta-shell [data-gta-method='mediation'] .gta-more-grid > #special_inputs:empty::before { content: none; }
html.gta-shell .gta-more-grid [data-gta-slot] { min-width: 0; margin: 0; width: auto; }
html.gta-shell .gta-more-grid [data-gta-slot='btns'] { display: none; }
html.gta-shell .gta-more-grid .selectize-input > .item { overflow: hidden; text-overflow: ellipsis; white-space: nowrap; max-width: 100%; }
html.gta-shell .gta-more-grid .selectize-input { white-space: nowrap; }
html.gta-shell .gta-more-grid [data-gta-slot='outcome'] { grid-column: 1; grid-row: 1; }
html.gta-shell .gta-more-grid [data-gta-slot='exposures'] { grid-column: 2; grid-row: 1; }
html.gta-shell .gta-more-grid [data-gta-slot='adjust'] { grid-column: 3; grid-row: 1; }
html.gta-shell .gta-more-grid [data-gta-slot='matched'] { grid-column: 4; grid-row: 1; }
html.gta-shell .gta-more-grid [data-gta-slot='group'] { grid-column: 4; grid-row: 1; }
html.gta-shell .gta-more-grid [data-gta-slot='cluster'] { grid-column: 5; grid-row: 1; }
html.gta-shell .gta-more-grid [data-gta-slot='m-outcome'] { grid-column: 1; grid-row: 1; }
html.gta-shell .gta-more-grid [data-gta-slot='m-exposure'] { grid-column: 2; grid-row: 1; }
html.gta-shell .gta-more-grid [data-gta-slot='m-mediator'] { grid-column: 3; grid-row: 1; }
html.gta-shell .gta-more-grid [data-gta-slot='m-cov'] { grid-column: 4; grid-row: 1; }
html.gta-shell .gta-more-grid .gta-more-medmodel { grid-column: 5; grid-row: 1; min-width: 0; }
html.gta-shell .gta-more-grid .gta-more-medmodel .form-group { margin: 0; width: auto; }
html.gta-shell .gta-more-grid .gta-more-run { grid-column: -2; grid-row: 1; }
html.gta-shell .gta-more-grid > .gta-ready { grid-column: 1 / -1; grid-row: 3; margin-top: 0; }
/* Row two: analysis-defining toggles (and the causal-assumptions link). */
html.gta-shell .gta-more .gta-bar { max-height: none; }
html.gta-shell .gta-more-grid [data-gta-slot='multi'] { grid-column: 1 / -1; grid-row: 2; align-self: center; justify-self: start; }
html.gta-shell .gta-more-grid [data-gta-slot='po'] { grid-column: 1 / -1; grid-row: 2; align-self: center; justify-self: start; margin-left: 250px; }
html.gta-shell [data-gta-method='multinomial'] .gta-more-grid [data-gta-slot='po'],
html.gta-shell [data-gta-method='multilevel'] .gta-more-grid [data-gta-slot='po'],
html.gta-shell [data-gta-method='matched'] .gta-more-grid [data-gta-slot='po'] { margin-left: 0; }
html.gta-shell .gta-more-grid .gta-more-causal { grid-column: 1 / -1; grid-row: 2; align-self: center; }
html.gta-shell .gta-more-grid [data-gta-slot='multi'] .checkbox,
html.gta-shell .gta-more-grid [data-gta-slot='po'] .checkbox { display: flex; align-items: center; gap: 4px; margin: 0; white-space: nowrap; }
html.gta-shell .gta-more-grid [data-gta-slot='multi'] .checkbox label {
  display: inline-flex;
  flex: 0 0 auto;
  align-items: center;
  gap: 6px;
  height: 26px;
  padding: 0 10px 0 8px;
  font-size: 13px;
  font-weight: 600;
  color: var(--gta-ink);
  white-space: nowrap;
  background: var(--gta-surface);
  border: 1px solid var(--gta-line-strong);
  border-radius: var(--gta-radius);
  cursor: pointer;
}
html.gta-shell .gta-more-grid [data-gta-slot='multi'] .checkbox label input[type='checkbox'] { position: static; margin: 0; accent-color: var(--gta-primary); }
html.gta-shell .gta-more-grid [data-gta-slot='multi'] .checkbox label:hover { border-color: #8E908B; }
html.gta-shell .gta-more-grid [data-gta-slot='multi'][data-gta-on='true'] .checkbox label { color: var(--gta-primary); background: var(--gta-primary-soft); border-color: var(--gta-primary); }
html.gta-shell .gta-more-grid [data-gta-slot='po'] .checkbox label { display: inline-flex; flex: 0 0 auto; align-items: center; gap: 6px; padding: 0; font-size: 12px; font-weight: 500; color: var(--gta-muted); }
html.gta-shell .gta-more-grid [data-gta-slot='po'] .checkbox label input[type='checkbox'] { position: static; margin: 0; accent-color: var(--gta-primary); }
html.gta-shell .gta-causal-btn { display: inline-flex; align-items: center; gap: 5px; padding: 0; font-size: 12.5px; }
html.gta-shell .gta-causal-btn svg,
html.gta-shell .gta-causal-btn .fa,
html.gta-shell .gta-causal-btn .fas { font-size: 12px; }
html.gta-shell .gta-causal-pop { width: 380px; }
html.gta-shell .gta-pop-title { margin: 0 0 6px; font-size: 13px; font-weight: 700; color: var(--gta-ink); }
html.gta-shell .gta-causal-pop p { margin: 0 0 6px; }
html.gta-shell .gta-causal-pop ul { margin: 0 0 6px; padding-left: 18px; }
/* Method visibility */
html.gta-shell .gta-more:not([data-gta-method='mediation']) .gta-more-grid [data-gta-slot^='m-'],
html.gta-shell .gta-more:not([data-gta-method='mediation']) .gta-more-grid .gta-more-medmodel,
html.gta-shell .gta-more:not([data-gta-method='mediation']) .gta-more-grid .gta-more-causal,
html.gta-shell .gta-more[data-gta-method='mediation'] .gta-more-grid > #special_inputs > * { display: none !important; }
html.gta-shell .gta-more .gta-run-for,
html.gta-shell .gta-more .gta-tool-for { display: none; }
html.gta-shell [data-gta-method='ordinal'] .gta-run-for[data-gta-for~='ordinal'],
html.gta-shell [data-gta-method='multinomial'] .gta-run-for[data-gta-for~='multinomial'],
html.gta-shell [data-gta-method='multilevel'] .gta-run-for[data-gta-for~='multilevel'],
html.gta-shell [data-gta-method='matched'] .gta-run-for[data-gta-for~='matched'],
html.gta-shell [data-gta-method='mediation'] .gta-run-for[data-gta-for~='mediation'],
html.gta-shell [data-gta-method='ordinal'] .gta-tool-for[data-gta-for~='ordinal'],
html.gta-shell [data-gta-method='multinomial'] .gta-tool-for[data-gta-for~='multinomial'],
html.gta-shell [data-gta-method='multilevel'] .gta-tool-for[data-gta-for~='multilevel'],
html.gta-shell [data-gta-method='matched'] .gta-tool-for[data-gta-for~='matched'],
html.gta-shell [data-gta-method='mediation'] .gta-tool-for[data-gta-for~='mediation'] { display: inline-flex; }
/* Result tabs per method */
html.gta-shell .gta-more .gta-results > .tabbable > .nav-tabs > li[data-gta-tabfor] { display: none; }
html.gta-shell [data-gta-method='ordinal'] .gta-results > .tabbable > .nav-tabs > li[data-gta-tabfor~='ordinal'],
html.gta-shell [data-gta-method='multinomial'] .gta-results > .tabbable > .nav-tabs > li[data-gta-tabfor~='multinomial'],
html.gta-shell [data-gta-method='multilevel'] .gta-results > .tabbable > .nav-tabs > li[data-gta-tabfor~='multilevel'],
html.gta-shell [data-gta-method='matched'] .gta-results > .tabbable > .nav-tabs > li[data-gta-tabfor~='matched'],
html.gta-shell [data-gta-method='mediation'] .gta-results > .tabbable > .nav-tabs > li[data-gta-tabfor~='mediation'] { display: flex; }
html.gta-shell .gta-more .shiny-plot-output { min-height: 340px; }
html.gta-shell .gta-more[data-gta-method='mediation'] .gta-ready .gta-ready-details { display: none; }
/* Empty states */
html.gta-shell [data-gta-show='need'] .gta-empty-state[data-gta-for='need'],
html.gta-shell [data-gta-show='ready'] .gta-empty-state[data-gta-for='ready'] { display: block; }
html.gta-shell .gta-more .gta-empty-text:empty { display: none; }
/* References panel (same treatment as Regression) */
html.gta-shell .gta-refs-panel #special_reference_inputs { display: contents; }
html.gta-shell .gta-refs-panel #special_reference_inputs > hr,
html.gta-shell .gta-refs-panel #special_reference_inputs > h4,
html.gta-shell .gta-refs-panel #special_reference_inputs > .gtx-help { display: none; }
html.gta-shell .gta-refs-panel #special_reference_inputs:not(:empty) ~ .gta-panel-empty { display: none; }
@media (max-width: 1120px) {
  html.gta-shell .gta-more-grid { column-gap: 12px; grid-template-columns: minmax(110px, 160px) minmax(140px, 1fr) minmax(120px, 1fr) auto; }
  html.gta-shell [data-gta-method='matched'] .gta-more-grid { grid-template-columns: minmax(100px, 140px) minmax(130px, 1fr) minmax(110px, 1fr) minmax(110px, 140px) auto; }
  html.gta-shell [data-gta-method='multilevel'] .gta-more-grid { grid-template-columns: minmax(96px, 130px) minmax(120px, 1fr) minmax(100px, 1fr) minmax(100px, 130px) minmax(110px, 140px) auto; }
  html.gta-shell [data-gta-method='mediation'] .gta-more-grid { grid-template-columns: minmax(96px, 140px) minmax(96px, 140px) minmax(96px, 140px) minmax(120px, 1fr) minmax(90px, 110px) auto; }
}

/* ---- Trials: risk difference analysis bar --------------------------------- */
html.gta-shell .gta-trial-grid {
  display: grid;
  grid-template-columns: minmax(110px, 170px) minmax(90px, 140px) minmax(110px, 170px) minmax(90px, 140px) minmax(140px, 1fr) auto;
  column-gap: var(--gta-s4);
  row-gap: 5px;
  align-items: start;
}
html.gta-shell .gta-trial-grid > #trial_inputs { display: contents; }
html.gta-shell .gta-trial-grid > #trial_inputs:empty::before {
  content: 'Outcome, Treatment group and Adjust for appear here once data is loaded.';
  grid-column: 1 / 6;
  grid-row: 1;
  align-self: center;
  font-size: 13px;
  color: var(--gta-faint);
}
html.gta-shell .gta-trial-grid > #trial_inputs > .form-group { width: auto; min-width: 0; margin: 0; }
html.gta-shell .gta-trial-grid > #trial_inputs > .form-group:nth-child(1) { grid-column: 1; grid-row: 1; }
html.gta-shell .gta-trial-grid > #trial_inputs > .form-group:nth-child(2) { grid-column: 3; grid-row: 1; }
html.gta-shell .gta-trial-grid > #trial_inputs > .form-group:nth-child(3) { grid-column: 5; grid-row: 1; }
html.gta-shell .gta-trial-grid .gta-trial-event { grid-column: 2; grid-row: 1; }
html.gta-shell .gta-trial-grid .gta-trial-ref { grid-column: 4; grid-row: 1; }
html.gta-shell .gta-trial-grid .gta-trial-run { grid-column: 6; grid-row: 1; }
html.gta-shell .gta-trial-grid .gta-trial-run .btn { border-radius: var(--gta-radius); }
html.gta-shell .gta-trial-grid .selectize-input { white-space: nowrap; }
html.gta-shell .gta-trial-grid .selectize-input > .item { overflow: hidden; text-overflow: ellipsis; white-space: nowrap; max-width: 100%; }
html.gta-shell .gta-trial-grid .gta-trial-problem .gta-trial-problem-text {
  display: flex;
  align-items: center;
  min-height: var(--gta-control-h);
  font-size: 12px;
  line-height: 15px;
  color: #7A4A12;
}
/* Second row: quiet inline definition detail (same treatment as Survival) */
html.gta-shell .gta-trial-grid .gta-trial-nnt { grid-column: 1 / 4; grid-row: 2; }
html.gta-shell .gta-trial-grid .gta-trial-nnt .form-group { display: flex; align-items: center; gap: 6px; margin: 0; width: auto; }
html.gta-shell .gta-trial-grid .gta-trial-nnt .form-group > label { margin: 0; font-size: 12px; font-weight: 500; color: var(--gta-muted); white-space: nowrap; }
html.gta-shell .gta-trial-grid .gta-trial-nnt .form-group > div { flex: 0 0 180px; }
html.gta-shell .gta-trial-grid .gta-trial-nnt .selectize-control { min-width: 150px; }
html.gta-shell .gta-trial-grid .gta-trial-nnt .selectize-input { height: 22px; min-height: 22px; padding: 1px 22px 1px 6px; font-size: 12px; line-height: 18px; overflow: hidden; }
html.gta-shell .gta-trial-grid .gta-trial-nnt .selectize-input > .item { line-height: 18px; }
html.gta-shell .gta-trial-grid > .gta-ready { grid-column: 1 / -1; grid-row: 3; margin-top: 0; }
html.gta-shell .gta-trials .gta-bar { max-height: none; }
html.gta-shell .gta-trials .gta-empty-text:empty { display: none; }
@media (max-width: 1120px) {
  html.gta-shell .gta-trial-grid { column-gap: 12px; grid-template-columns: minmax(96px, 150px) minmax(80px, 120px) minmax(96px, 150px) minmax(80px, 120px) minmax(120px, 1fr) auto; }
}

/* ---- Data > Structure ------------------------------------------------------ */
html.gta-shell .gta-modal-card > .tabbable > .tab-content > .tab-pane[data-value='Structure'] { overflow: hidden; }
html.gta-shell .gta-structure { display: flex; flex-direction: column; height: 100%; min-height: 0; }
html.gta-shell .gta-structure-head { flex: 0 0 auto; display: flex; align-items: flex-start; justify-content: space-between; gap: var(--gta-s4); padding-bottom: 10px; border-bottom: 1px solid var(--gta-line); }
html.gta-shell .gta-structure-head > .shiny-html-output { flex: 1 1 auto; min-width: 0; }
html.gta-shell .gta-structure-source { font-size: 12px; color: var(--gta-muted); }
html.gta-shell .gta-structure-head > #structure_overview:empty::before { content: 'Load data to see its structure.'; font-size: 13px; color: var(--gta-faint); }
html.gta-shell .gta-structure-summary { margin-top: 2px; font-size: 15px; font-weight: 600; color: var(--gta-ink); font-variant-numeric: tabular-nums; }
html.gta-shell .gta-structure-types { margin-top: 2px; font-size: 13px; color: var(--gta-text); }
html.gta-shell .gta-structure-dot { margin: 0 5px; color: var(--gta-faint); }
html.gta-shell .gta-structure-worth { display: flex; flex-wrap: wrap; align-items: center; margin-top: 6px; font-size: 12.5px; color: var(--gta-text); }
html.gta-shell .gta-structure-worth-label { font-weight: 600; color: var(--gta-ink); }
html.gta-shell .gta-structure-worth-help .gtw-help { margin-left: 6px; }
html.gta-shell .gta-structure-note { padding: 0; font: inherit; color: var(--gta-primary); background: none; border: 0; border-bottom: 1px dotted var(--gta-primary-line); cursor: pointer; }
html.gta-shell .gta-structure-note:hover { border-bottom-color: var(--gta-primary); }
html.gta-shell .gta-structure-note:focus-visible { outline: 2px solid var(--gta-primary); outline-offset: 1px; }
html.gta-shell .gta-structure-code-btn { display: inline-flex; align-items: center; gap: 4px; white-space: nowrap; }
html.gta-shell .gta-structure-code-pop { width: 520px; }
html.gta-shell .gta-structure-code-pop pre { max-height: 300px; margin: 6px 0 0; }
html.gta-shell .gta-structure-body { flex: 1 1 auto; min-height: 0; display: grid; grid-template-columns: minmax(0, 1fr) 300px; gap: var(--gta-s4); padding-top: 10px; }
html.gta-shell .gta-structure-table { min-height: 0; overflow: auto; border: 1px solid var(--gta-line); border-radius: var(--gta-radius); }
html.gta-shell .gta-structure-grid { width: 100%; table-layout: fixed; border-collapse: separate; border-spacing: 0; font-size: 13px; }
html.gta-shell .gta-structure-grid th {
  position: sticky; top: 0; z-index: 1;
  padding: 6px 10px; font-size: 12px; font-weight: 600; text-align: left; color: var(--gta-muted);
  background: var(--gta-subtle); border-bottom: 1px solid var(--gta-line);
}
html.gta-shell .gta-structure-grid th:nth-child(1) { width: 30%; }
html.gta-shell .gta-structure-grid th:nth-child(2) { width: 12%; }
html.gta-shell .gta-structure-grid th:nth-child(3) { width: 13%; }
html.gta-shell .gta-structure-grid th:nth-child(4) { width: 10%; }
html.gta-shell .gta-structure-grid td {
  padding: 5px 10px; vertical-align: top; color: var(--gta-text);
  white-space: nowrap; overflow: hidden; text-overflow: ellipsis; border-bottom: 1px solid var(--gta-line);
}
html.gta-shell .gta-structure-grid tbody tr { cursor: pointer; }
html.gta-shell .gta-structure-grid tbody tr:hover td { background: var(--gta-subtle); }
html.gta-shell .gta-structure-grid tbody tr[aria-selected='true'] td { background: var(--gta-primary-soft); }
html.gta-shell .gta-structure-grid tbody tr[aria-selected='true'] td:first-child { box-shadow: inset 3px 0 0 var(--gta-primary); }
html.gta-shell .gta-structure-grid tbody tr:focus { outline: none; }
html.gta-shell .gta-structure-grid tbody tr:focus-visible td { box-shadow: inset 0 2px 0 var(--gta-primary), inset 0 -2px 0 var(--gta-primary); }
html.gta-shell .gta-structure-grid tbody tr:focus-visible td:first-child { box-shadow: inset 2px 0 0 var(--gta-primary), inset 0 2px 0 var(--gta-primary), inset 0 -2px 0 var(--gta-primary); }
html.gta-shell .gta-structure-grid tbody tr:focus-visible td:last-child { box-shadow: inset -2px 0 0 var(--gta-primary), inset 0 2px 0 var(--gta-primary), inset 0 -2px 0 var(--gta-primary); }
html.gta-shell .gta-structure-grid .gta-num,
html.gta-shell .gta-structure-levels .gta-num { text-align: right; font-variant-numeric: tabular-nums; }
html.gta-shell .gta-structure-varname,
html.gta-shell .gta-structure-label,
html.gta-shell .gta-structure-tags { display: block; overflow: hidden; text-overflow: ellipsis; }
html.gta-shell .gta-structure-varname { font-weight: 600; color: var(--gta-ink); }
html.gta-shell .gta-structure-label { font-size: 12px; color: var(--gta-muted); }
html.gta-shell .gta-structure-tags { font-size: 11.5px; color: var(--gta-caution-ink); }
html.gta-shell .gta-structure-pct { font-size: 11.5px; color: var(--gta-muted); }
html.gta-shell .gta-structure-zero { color: var(--gta-faint); }
html.gta-shell .gta-structure-detail { min-height: 0; overflow: auto; padding: 2px 2px 8px; }
html.gta-shell .gta-structure-detail-empty { padding: 24px 8px; font-size: 13px; text-align: center; color: var(--gta-faint); }
html.gta-shell .gta-structure-detail-title { overflow: hidden; font-size: 15px; font-weight: 600; color: var(--gta-ink); text-overflow: ellipsis; white-space: nowrap; }
html.gta-shell .gta-structure-detail-type { margin-top: 1px; font-size: 12.5px; color: var(--gta-muted); }
html.gta-shell .gta-structure-stats { display: grid; grid-template-columns: auto 1fr; gap: 3px 12px; margin: 10px 0 0; font-size: 13px; }
html.gta-shell .gta-structure-stats dt { font-weight: 500; color: var(--gta-muted); }
html.gta-shell .gta-structure-stats dd { margin: 0; text-align: right; color: var(--gta-ink); font-variant-numeric: tabular-nums; }
html.gta-shell .gta-structure-hist { margin-top: 12px; }
html.gta-shell .gta-structure-hist svg { display: block; width: 100%; height: 72px; }
html.gta-shell .gta-structure-hist rect { fill: var(--gta-primary); opacity: 0.55; }
html.gta-shell .gta-structure-hist rect:hover { opacity: 0.85; }
html.gta-shell .gta-structure-hist-axis { display: flex; justify-content: space-between; margin-top: 2px; font-size: 11px; color: var(--gta-faint); border-top: 1px solid var(--gta-line); padding-top: 2px; }
html.gta-shell .gta-structure-levels { width: 100%; margin-top: 12px; table-layout: fixed; font-size: 12.5px; border-collapse: collapse; }
html.gta-shell .gta-structure-levels th { padding: 3px 4px; font-size: 11.5px; font-weight: 600; text-align: left; color: var(--gta-muted); border-bottom: 1px solid var(--gta-line); }
html.gta-shell .gta-structure-levels th:nth-child(2), html.gta-shell .gta-structure-levels th:nth-child(3) { width: 22%; text-align: right; }
html.gta-shell .gta-structure-levels td { padding: 3px 4px; overflow: hidden; text-overflow: ellipsis; white-space: nowrap; border-bottom: 1px solid var(--gta-line); }
html.gta-shell .gta-structure-levels tfoot td { color: var(--gta-muted); border-bottom: 0; }
html.gta-shell .gta-structure-actions { margin-top: 14px; }
@media (max-width: 1120px) {
  html.gta-shell .gta-structure-body { grid-template-columns: minmax(0, 1fr) 250px; gap: 12px; }
}

/* ---- Data > Prepare (variable-first) ---------------------------------------- */
html.gta-shell .gta-modal-card > .tabbable > .tab-content > .tab-pane[data-value='Prepare'] { overflow: hidden; }
html.gta-shell .gta-prepare { height: 100%; }
html.gta-shell .gta-hidden-menu { display: none; }
html.gta-shell .gta-prep { display: flex; flex-direction: column; height: 100%; min-height: 0; }
html.gta-shell .gta-prep-head { flex: 0 0 auto; display: flex; align-items: center; justify-content: space-between; gap: var(--gta-s4); padding-bottom: 8px; border-bottom: 1px solid var(--gta-line); }
html.gta-shell .gta-prep-state { font-size: 13px; font-weight: 600; color: var(--gta-ink); white-space: nowrap; }
html.gta-shell .gta-prep-head-tools { display: flex; align-items: center; gap: 2px; }
html.gta-shell .gta-prep-tool {
  display: inline-flex; align-items: center; gap: 5px; height: 28px; padding: 0 9px;
  font-family: var(--gta-font); font-size: 12.5px; font-weight: 500; color: var(--gta-text);
  background: transparent; border: 1px solid transparent; border-radius: var(--gta-radius); cursor: pointer;
}
html.gta-shell .gta-prep-tool:hover:not([disabled]) { background: var(--gta-subtle); color: var(--gta-ink); }
html.gta-shell .gta-prep-tool[disabled] { color: var(--gta-faint); cursor: default; }
html.gta-shell .gta-prep-tool:focus-visible { outline: 2px solid var(--gta-primary); outline-offset: 1px; }
html.gta-shell .gta-prep-tool[aria-expanded='true'] { background: var(--gta-primary-soft); color: var(--gta-primary); }
html.gta-shell .gta-prep-reset { color: var(--gta-muted); }
html.gta-shell .gta-prep-sep { width: 1px; height: 18px; margin: 0 6px; background: var(--gta-line); }
html.gta-shell .gta-prep-body { position: relative; flex: 1 1 auto; min-height: 0; display: grid; grid-template-columns: 260px minmax(0, 1fr); gap: var(--gta-s4); padding-top: 10px; overflow: hidden; }
/* Variables column */
html.gta-shell .gta-prep-side { display: flex; flex-direction: column; min-height: 0; border: 1px solid var(--gta-line); border-radius: var(--gta-radius); }
html.gta-shell .gta-prep-search { flex: 0 0 auto; display: flex; align-items: center; gap: 6px; padding: 6px 8px; border-bottom: 1px solid var(--gta-line); color: var(--gta-faint); }
html.gta-shell .gta-prep-search input { flex: 1 1 auto; min-width: 0; height: 26px; padding: 0 4px; font-size: 13px; color: var(--gta-ink); border: 0; outline: none; background: transparent; }
html.gta-shell .gta-prep-list { flex: 1 1 auto; min-height: 0; overflow: auto; }
html.gta-shell .gta-prep-var { display: flex; align-items: baseline; justify-content: space-between; gap: 8px; padding: 5px 10px; font-size: 13px; cursor: pointer; border-bottom: 1px solid var(--gta-line); }
html.gta-shell .gta-prep-var[hidden] { display: none; }
html.gta-shell .gta-prep-var:hover { background: var(--gta-subtle); }
html.gta-shell .gta-prep-var[aria-selected='true'] { background: var(--gta-primary-soft); box-shadow: inset 3px 0 0 var(--gta-primary); }
html.gta-shell .gta-prep-var:focus { outline: none; }
html.gta-shell .gta-prep-var:focus-visible { box-shadow: inset 0 0 0 2px var(--gta-primary); }
html.gta-shell .gta-prep-var-name { min-width: 0; overflow: hidden; font-weight: 600; color: var(--gta-ink); text-overflow: ellipsis; white-space: nowrap; }
html.gta-shell .gta-prep-var-type { flex: 0 0 auto; font-size: 12px; color: var(--gta-muted); white-space: nowrap; }
html.gta-shell .gta-prep-focus { flex: 0 0 auto; padding: 8px 10px 10px; border-top: 1px solid var(--gta-line-strong); background: var(--gta-subtle); }
html.gta-shell .gta-prep-focus-empty { font-size: 12.5px; color: var(--gta-faint); }
html.gta-shell .gta-prep-focus-body { display: none; }
html.gta-shell .gta-prep-focus.has-variable .gta-prep-focus-empty { display: none; }
html.gta-shell .gta-prep-focus.has-variable .gta-prep-focus-body { display: block; }
html.gta-shell .gta-prep-focus-head { display: flex; align-items: baseline; justify-content: space-between; gap: 8px; }
html.gta-shell .gta-prep-focus-name { min-width: 0; overflow: hidden; font-size: 14px; font-weight: 600; color: var(--gta-ink); text-overflow: ellipsis; white-space: nowrap; }
html.gta-shell .gta-prep-focus-type { margin-top: 1px; font-size: 12px; color: var(--gta-muted); }
html.gta-shell .gta-prep-actions { display: flex; flex-wrap: wrap; gap: 4px; margin-top: 8px; }
html.gta-shell .gta-prep-action {
  height: 26px; padding: 0 9px; font-family: var(--gta-font); font-size: 12.5px; font-weight: 500; color: var(--gta-ink);
  background: var(--gta-surface); border: 1px solid var(--gta-line-strong); border-radius: var(--gta-radius); cursor: pointer; white-space: nowrap;
}
html.gta-shell .gta-prep-action:hover { border-color: var(--gta-primary); color: var(--gta-primary); }
html.gta-shell .gta-prep-action[hidden] { display: none; }
html.gta-shell .gta-prep-action:focus-visible { outline: 2px solid var(--gta-primary); outline-offset: 1px; }
html.gta-shell .gta-prep-action.is-active { color: var(--gta-primary); background: var(--gta-primary-soft); border-color: var(--gta-primary); }
html.gta-shell .gta-prep-quick { display: flex; flex-wrap: wrap; align-items: center; gap: 2px 8px; margin-top: 8px; font-size: 12px; }
html.gta-shell .gta-prep-quick[hidden] { display: none; }
html.gta-shell .gta-prep-quick-label { font-weight: 600; color: var(--gta-muted); }
html.gta-shell .gta-prep-quick-btn.btn,
html.gta-shell .gta-prep-quick-btn {
  height: auto; min-height: 0; padding: 0; font-size: 12px; font-weight: 600; line-height: 18px; color: var(--gta-primary);
  background: none; border: 0; border-bottom: 1px dotted var(--gta-primary-line); border-radius: 0; box-shadow: none;
}
html.gta-shell .gta-prep-quick-btn[hidden] { display: none; }
/* Main column */
html.gta-shell .gta-prep-main { display: flex; flex-direction: column; min-width: 0; min-height: 0; }
html.gta-shell .gta-prep-main-head { flex: 0 0 auto; display: flex; flex-wrap: wrap; align-items: center; justify-content: space-between; gap: 6px 12px; margin-bottom: 6px; }
html.gta-shell .gta-prep-main-title { font-size: 12px; font-weight: 600; color: var(--gta-muted); text-transform: uppercase; letter-spacing: 0.04em; }
html.gta-shell .gta-prep-dataset { display: flex; flex-wrap: wrap; gap: 4px; }
html.gta-shell .gta-prep-preview { flex: 1 1 auto; min-height: 0; overflow: auto; }
html.gta-shell .gta-prep-preview .dataTables_wrapper { font-size: 13px; }
/* Action sheet */
html.gta-shell .gta-prep-sheet {
  position: absolute; top: 10px; right: 0; bottom: 0; z-index: 20; width: min(440px, 62%);
  display: flex; flex-direction: column; background: var(--gta-surface);
  border: 1px solid var(--gta-line-strong); border-radius: var(--gta-radius); box-shadow: var(--gta-shadow);
  transform: translateX(calc(100% + 24px)); visibility: hidden; transition: transform 0.14s ease-out, visibility 0s linear 0.14s;
}
html.gta-shell .gta-prep-sheet.is-open { transform: none; visibility: visible; transition: transform 0.14s ease-out; }
html.gta-shell .gta-prep-sheet-head { flex: 0 0 auto; display: flex; align-items: center; gap: 10px; padding: 8px 8px 8px 14px; border-bottom: 1px solid var(--gta-line); }
html.gta-shell .gta-prep-sheet-title { flex: 1 1 auto; min-width: 0; overflow: hidden; font-size: 15px; font-weight: 600; color: var(--gta-ink); text-overflow: ellipsis; white-space: nowrap; }
html.gta-shell .gta-prep-sheet-quick[hidden] { display: none; }
html.gta-shell .gta-prep-sheet-body { flex: 1 1 auto; min-height: 0; overflow: auto; padding: 10px 14px; font-size: 13px; }
html.gta-shell .gta-prep-sheet-foot { flex: 0 0 auto; display: flex; justify-content: flex-end; gap: 8px; padding: 10px 14px; border-top: 1px solid var(--gta-line); }
html.gta-shell .gta-prep-sheet-foot .btn { height: var(--gta-control-h); padding: 0 16px; font-weight: 600; }
html.gta-shell .gta-prep-sheet .gta-prep-varfield { display: none; }
html.gta-shell .gta-prep-sheet .form-group { margin-bottom: 10px; }
html.gta-shell .gta-prep-sheet .control-label { font-size: 12px; font-weight: 600; color: var(--gta-muted); }
html.gta-shell .gta-prep-sheet .help-copy { margin: 0 0 8px; font-size: 12px; line-height: 1.45; color: var(--gta-muted); }
/* Explanations live behind the sheet's info button; only the rule-defining
   notes for groups and filters stay visible. The value list is shown where
   the fields do not already list the values. */
html.gta-shell .gta-prep-sheet:not([data-action=groups]):not([data-action=filter]) .help-copy,
html.gta-shell .gta-prep-sheet .gtx-recode-map > .help-copy,
html.gta-shell .gta-prep-sheet .gtx-value-guide > .help-copy { display: none; }
html.gta-shell .gta-prep-sheet[data-action=recode] #data_prep-value_guide,
html.gta-shell .gta-prep-sheet[data-action=label] #data_prep-value_guide,
html.gta-shell .gta-prep-sheet[data-action=keep] #data_prep-value_guide,
html.gta-shell .gta-prep-sheet[data-action=rename] #data_prep-value_guide { display: none; }
html.gta-shell .gta-prep-sheet code { padding: 0 4px; font-size: 12px; color: var(--gta-ink); background: var(--gta-subtle); border-radius: 3px; }
html.gta-shell .gta-prep-hint-pop .gtx-step { border: 0; padding: 0; background: none; }
html.gta-shell .gta-prep-hint-pop .help-copy { margin: 4px 0 0; font-size: 12.5px; color: var(--gta-text); }
html.gta-shell .gta-prep-sheet #data_prep-controls > strong { display: none; }
html.gta-shell .gta-prep-sheet .gtx-step { margin: 0 0 10px; padding: 8px 10px; border: 1px solid var(--gta-line); border-radius: var(--gta-radius); }
html.gta-shell .gta-prep-sheet .gtx-step > strong { display: block; margin-bottom: 4px; font-size: 12px; color: var(--gta-ink); }
html.gta-shell .gta-prep-sheet .gtx-recode-map { margin: 4px 0 10px; }
html.gta-shell .gta-prep-sheet .gtx-recode-map h4,
html.gta-shell .gta-prep-sheet .gtx-value-guide h5 { margin: 0 0 4px; font-size: 12px; font-weight: 600; color: var(--gta-ink); }
html.gta-shell .gta-prep-sheet .gtx-impact-table { width: 100%; font-size: 12.5px; border-collapse: collapse; }
html.gta-shell .gta-prep-sheet .gtx-impact-table th { padding: 4px 6px; font-size: 11.5px; font-weight: 600; text-align: left; color: var(--gta-muted); border-bottom: 1px solid var(--gta-line); }
html.gta-shell .gta-prep-sheet .gtx-impact-table td { padding: 3px 6px; border-bottom: 1px solid var(--gta-line); vertical-align: middle; }
html.gta-shell .gta-prep-sheet .gtx-impact-table .form-group { margin: 0; }
html.gta-shell .gta-prep-sheet .gtx-impact-table .form-control { height: 26px; padding: 2px 6px; font-size: 12.5px; }
html.gta-shell .gta-prep-sheet .gtx-value-guide { margin: 4px 0 10px; padding: 8px 10px; background: var(--gta-subtle); border-radius: var(--gta-radius); }
html.gta-shell .gta-prep-sheet .gtx-value-guide pre { max-height: 90px; margin: 4px 0 0; padding: 6px; font-size: 11.5px; white-space: pre-wrap; }
html.gta-shell .gta-prep-sheet .gtx-value-guide .copy-code { float: right; }
html.gta-shell .gta-prep-sheet .gtx-note { margin: 4px 0 0; padding: 8px 10px; font-size: 12.5px; background: var(--gta-primary-soft); border: 0; border-left: 3px solid var(--gta-primary); border-radius: var(--gta-radius); }
html.gta-shell .gta-prep-sheet .gtx-note .gtx-impact-table { margin-top: 6px; background: var(--gta-surface); }
/* Task dialog: operations that build, map, order or combine (Recode, Change
   type, Create variable, Create groups, Filter) open the same sheet as a
   centred dialog. Head and Cancel/Apply stay fixed; only variable-length
   regions scroll, inside their own bounds. Rename, Label, Missing values and
   Keep variables remain the compact side sheet. */
html.gta-shell .gta-prep-sheet.gta-prep-dialog {
  position: fixed; top: 50%; left: 50%; right: auto; bottom: auto; z-index: 1550;
  width: min(940px, calc(100vw - 48px)); max-height: calc(100vh - 56px);
  transform: translate(-50%, -50%); background: transparent;
  transition: visibility 0s linear 0s;
}
html.gta-shell .gta-prep-sheet.gta-prep-dialog.is-open { transform: translate(-50%, -50%); transition: none; }
html.gta-shell .gta-prep-sheet.gta-prep-dialog::before { content: ''; position: fixed; top: -100vh; left: -100vw; width: 300vw; height: 300vh; z-index: -1; background: rgba(31, 35, 40, 0.38); }
html.gta-shell .gta-prep-dialog .gta-prep-sheet-head,
html.gta-shell .gta-prep-dialog .gta-prep-sheet-body,
html.gta-shell .gta-prep-dialog .gta-prep-sheet-foot { background: var(--gta-surface); }
html.gta-shell .gta-prep-dialog .gta-prep-sheet-head { padding: 10px 10px 10px 18px; border-radius: var(--gta-radius) var(--gta-radius) 0 0; }
html.gta-shell .gta-prep-dialog .gta-prep-sheet-foot { padding: 10px 18px; border-radius: 0 0 var(--gta-radius) var(--gta-radius); }
html.gta-shell .gta-prep-dialog .gta-prep-sheet-title { font-size: 16px; }
html.gta-shell .gta-prep-dialog .gta-prep-sheet-body {
  display: grid; grid-template-columns: minmax(0, 1.55fr) minmax(250px, 1fr); grid-template-rows: auto minmax(0, 1fr);
  column-gap: 18px; align-items: start; min-height: 300px; padding: 14px 18px; overflow: hidden;
}
html.gta-shell .gta-prep-dialog #data_prep-controls { grid-column: 1; grid-row: 1 / span 2; min-width: 0; }
html.gta-shell .gta-prep-dialog #data_prep-value_guide { grid-column: 2; grid-row: 1; min-width: 0; }
html.gta-shell .gta-prep-dialog #data_prep-impact_preview { grid-column: 2; grid-row: 2; min-width: 0; max-height: calc(100vh - 300px); overflow: auto; }
html.gta-shell .gta-prep-dialog[data-action=recode] #data_prep-impact_preview { grid-row: 1 / span 2; max-height: calc(100vh - 190px); }
html.gta-shell .gta-prep-dialog #data_prep-impact_preview:empty::before { content: 'The result of this change is shown here before you apply it.'; display: block; padding: 8px 10px; font-size: 12.5px; color: var(--gta-muted); background: var(--gta-subtle); border-radius: var(--gta-radius); }
html.gta-shell .gta-prep-dialog .gtx-note { margin-top: 0; }
html.gta-shell .gta-prep-dialog .gtx-value-guide { margin-top: 0; }
html.gta-shell .gta-prep-sheet.gta-prep-dialog[data-action] .gtx-value-guide > .help-copy { display: block; margin: 0; }
html.gta-shell .gta-prep-dialog .selectize-dropdown-content { max-height: 168px; }
html.gta-shell .gta-prep-dialog .gtx-step { display: grid; column-gap: 10px; align-items: end; margin-bottom: 8px; padding: 8px 10px; }
html.gta-shell .gta-prep-dialog .gtx-step > strong { grid-column: 1; grid-row: 1 / span 2; align-self: center; margin: 0; }
html.gta-shell .gta-prep-dialog .gtx-step .form-group { margin-bottom: 0; min-width: 0; }
html.gta-shell .gta-prep-dialog .gtx-step .control-label { margin-bottom: 2px; font-size: 11.5px; line-height: 1.25; }
/* Create groups: source and count side by side; rules scroll in their own
   region; the new variable's name stays below it, always visible. */
html.gta-shell .gta-prep-dialog[data-action=groups] #data_prep-controls { display: grid; grid-template-columns: minmax(0, 1fr) minmax(0, 1.25fr); column-gap: 14px; }
html.gta-shell .gta-prep-dialog[data-action=groups] #data_prep-controls > * { grid-column: 1 / -1; }
html.gta-shell .gta-prep-dialog[data-action=groups] #data_prep-controls > .form-group:first-of-type { grid-column: 1; }
html.gta-shell .gta-prep-dialog[data-action=groups] #data_prep-controls > #data_prep-group_count { grid-column: 2; }
html.gta-shell .gta-prep-dialog #data_prep-group_rules { margin-bottom: 8px; }
@media (max-height: 780px) {
  html.gta-shell .gta-prep-dialog #data_prep-group_rules { max-height: calc(100vh - 318px); min-height: 96px; overflow: auto; padding-right: 2px; }
}
html.gta-shell .gta-prep-dialog #data_prep-group_rules .gtx-step { grid-template-columns: 84px minmax(0, 1.7fr) minmax(0, 1.05fr) minmax(0, 0.85fr) minmax(0, 1.3fr); }
html.gta-shell .gta-prep-dialog #data_prep-group_rules .gtx-step > :nth-child(2) { grid-column: 2; }
html.gta-shell .gta-prep-dialog #data_prep-group_rules .gtx-step > :nth-child(3) { grid-column: 3; }
html.gta-shell .gta-prep-dialog #data_prep-group_rules .gtx-step > :nth-child(4) { grid-column: 4; }
html.gta-shell .gta-prep-dialog #data_prep-group_rules .gtx-step > .form-group:last-child { grid-column: 5; }
/* Filter: one row per condition; the combination rule sits above condition 2. */
html.gta-shell .gta-prep-dialog #data_prep-filter_conditions .gtx-step { grid-template-columns: 78px minmax(0, 1.2fr) minmax(0, 1.2fr) minmax(0, 1fr); }
html.gta-shell .gta-prep-dialog #data_prep-filter_conditions #data_prep-connector { grid-column: 2 / -1; margin-bottom: 6px; }
html.gta-shell .gta-prep-dialog #data_prep-filter_conditions > .form-group { margin: 0 0 6px; }
/* Create variable: calculation fields in two columns. */
html.gta-shell .gta-prep-dialog #data_prep-calculation_controls { display: grid; grid-template-columns: minmax(0, 1fr) minmax(0, 1fr); column-gap: 14px; }
html.gta-shell .gta-prep-dialog #data_prep-calculation_mode .radio { margin: 2px 0; }
/* Recode and Change type: the value list scrolls in place under a fixed header. */
html.gta-shell .gta-prep-dialog .gtx-recode-map { max-height: calc(100vh - 270px); overflow: auto; margin-top: 0; }
html.gta-shell .gta-prep-dialog .gtx-recode-map .gtx-impact-table th,
html.gta-shell .gta-prep-dialog #data_prep-impact_preview .gtx-impact-table th { position: sticky; top: 0; z-index: 1; background: var(--gta-surface); }
html.gta-shell .gta-prep-dialog[data-action=recode] #data_prep-controls > .help-copy { display: block; }
@media (max-width: 1120px), (max-height: 740px) {
  html.gta-shell .gta-prep-sheet.gta-prep-dialog { width: calc(100vw - 32px); max-height: calc(100vh - 24px); }
  html.gta-shell .gta-prep-dialog .gta-prep-sheet-body { grid-template-columns: minmax(0, 1.5fr) minmax(220px, 1fr); column-gap: 14px; padding: 10px 14px; }
  html.gta-shell .gta-prep-dialog .gtx-recode-map { max-height: calc(100vh - 220px); }
  html.gta-shell .gta-prep-dialog #data_prep-impact_preview { max-height: calc(100vh - 250px); }
  html.gta-shell .gta-prep-dialog[data-action=recode] #data_prep-impact_preview { max-height: calc(100vh - 150px); }
}
/* Changes / Code / Export popovers */
html.gta-shell .gta-prep-pop { width: 460px; }
html.gta-shell .gta-prep-code-pop { width: 560px; }
html.gta-shell .gta-prep-code-pop pre { max-height: 340px; margin: 6px 0 0; }
html.gta-shell .gta-prep-code-pop .gta-code-head { gap: 6px; }
html.gta-shell .gta-prep-changes { margin: 6px 0 0; padding-left: 20px; font-size: 13px; }
html.gta-shell .gta-prep-changes li { padding: 3px 0; border-bottom: 1px solid var(--gta-line); }
html.gta-shell .gta-prep-changes-empty { margin-top: 6px; font-size: 13px; color: var(--gta-muted); }
html.gta-shell .gta-prep-export-row { display: flex; align-items: center; gap: 8px; margin-top: 8px; }
html.gta-shell .gta-prep-export-row .form-group { margin: 0; }
@media (max-width: 1120px) {
  html.gta-shell .gta-prep-body { grid-template-columns: 230px minmax(0, 1fr); gap: 12px; }
}

/* Shiny confirmation dialogs (e.g. Prepare > Reset all) open above the Data workspace. */
html.gta-shell .modal-backdrop { z-index: 1600; }
html.gta-shell .modal { z-index: 1610; }

/* Notifications sit above the bottom toolbar. */
html.gta-shell #shiny-notification-panel { bottom: 48px; }

/* ---- Narrow windows (1000 x 700) ------------------------------------------ */
@media (max-width: 1120px) {
  html.gta-shell .gta-chip { max-width: 260px; }
  html.gta-shell .gta-chip [data-gta-part='origin'],
  html.gta-shell .gta-chip [data-gta-part='origin'] + .gtw-status-sep,
  html.gta-shell .gta-chip .gta-sep-origin { display: none; }
  html.gta-shell #main_nav > li > a,
  html.gta-shell .gta-menu-btn { padding: 0 10px; }
  html.gta-shell .gta-bar-slots .shiny-input-container:not(.shiny-input-checkboxgroup) { flex-basis: 170px; width: 170px; }
}
")

gta_js <- HTML("
// GT Analysis shared shell (D3). Presentation only: it moves focus, shows and
// hides layers, and reads existing inputs to describe state. It never sets an
// analysis input except through the existing D2 components.
function gtaBoot() {
  var root = document.documentElement;
  if (!root.classList.contains('gta-shell')) return;
  var cfg = {};
  try { cfg = JSON.parse(document.getElementById('gta-config').textContent); } catch (e) { cfg = {}; }
  var $ = window.jQuery;
  var DOT = ' ' + String.fromCharCode(183) + ' ';
  function el(tag, cls, text) {
    var n = document.createElement(tag);
    if (cls) n.className = cls;
    if (text !== undefined && text !== null) n.textContent = text;
    return n;
  }
  function icon(name) {
    var i = el('i', 'fas fa-' + name);
    i.setAttribute('aria-hidden', 'true');
    return i;
  }
  function visible(n) {
    if (!n || n.disabled) return false;
    if (n.offsetParent === null && getComputedStyle(n).position !== 'fixed') return false;
    return getComputedStyle(n).visibility !== 'hidden';
  }
  function focusables(node) {
    return Array.prototype.filter.call(
      node.querySelectorAll('a[href], button, input, select, textarea, [tabindex]:not([tabindex=\"-1\"])'),
      function(n) { return n.type !== 'hidden' && visible(n); }
    );
  }

  // ---- Task navigation ---------------------------------------------------
  var list = document.getElementById('main_nav');
  function tabLink(value) {
    return list ? list.querySelector('a[data-value=' + JSON.stringify(value) + ']') : null;
  }
  function openTab(value) {
    var a = tabLink(value);
    if (a) a.click();
  }
  var menus = [];
  function closeMenus(except) {
    menus.forEach(function(m) {
      if (m === except) return;
      m.pop.hidden = true;
      m.btn.setAttribute('aria-expanded', 'false');
    });
  }
  function buildNav() {
    if (!list) return;
    var nav = cfg.nav || {};
    var order = 0;
    function place(item) {
      var a = tabLink(item.value);
      if (!a) return;
      if (item.label) a.textContent = item.label;
      a.parentNode.style.order = order++;
    }
    Array.prototype.forEach.call(list.children, function(li) { li.style.order = 500; });
    (nav.primary || []).forEach(place);
    (nav.menus || []).forEach(function(menu) {
      var li = el('li', 'gta-menu');
      li.style.order = order++;
      var btn = el('button', 'gta-menu-btn');
      btn.type = 'button';
      btn.setAttribute('aria-haspopup', 'menu');
      btn.setAttribute('aria-expanded', 'false');
      btn.appendChild(document.createTextNode(menu.label));
      btn.appendChild(icon('chevron-down'));
      var pop = el('div', 'gta-menu-list');
      pop.setAttribute('role', 'menu');
      pop.setAttribute('aria-label', menu.label);
      pop.hidden = true;
      pop.id = 'gta-menu-' + menus.length;
      btn.setAttribute('aria-controls', pop.id);
      var items = [];
      (menu.items || []).forEach(function(item) {
        var a = tabLink(item.value);
        if (!a) return;
        a.parentNode.classList.add('gta-in-menu');
        var b = el('button', 'gta-menu-item', item.label || item.value);
        b.type = 'button';
        b.setAttribute('role', 'menuitem');
        b.setAttribute('data-gta-value', item.value);
        b.addEventListener('click', function() {
          closeMenus();
          openTab(item.value);
          btn.focus();
        });
        pop.appendChild(b);
        items.push(b);
      });
      var entry = { btn: btn, pop: pop, items: items, values: (menu.items || []).map(function(i) { return i.value; }) };
      btn.addEventListener('click', function(ev) {
        ev.stopPropagation();
        var open = pop.hidden;
        closeMenus();
        pop.hidden = !open;
        btn.setAttribute('aria-expanded', open ? 'true' : 'false');
        if (open && items[0]) items[0].focus();
      });
      btn.addEventListener('keydown', function(ev) {
        if (ev.key === 'ArrowDown') { ev.preventDefault(); btn.click(); }
      });
      pop.addEventListener('keydown', function(ev) {
        var i = items.indexOf(document.activeElement);
        if (ev.key === 'ArrowDown' || ev.key === 'ArrowUp') {
          ev.preventDefault();
          i = ev.key === 'ArrowDown' ? Math.min(items.length - 1, i + 1) : Math.max(0, i - 1);
          if (items[i]) items[i].focus();
        } else if (ev.key === 'Escape') {
          ev.stopPropagation();
          closeMenus();
          btn.focus();
        } else if (ev.key === 'Tab') {
          closeMenus();
        }
      });
      li.appendChild(btn);
      li.appendChild(pop);
      list.appendChild(li);
      menus.push(entry);
    });
    (nav.trailing || []).forEach(place);
    // Keep DOM (keyboard) order identical to the visual order.
    Array.prototype.slice.call(list.children)
      .sort(function(a, b) { return (parseInt(a.style.order, 10) || 0) - (parseInt(b.style.order, 10) || 0); })
      .forEach(function(li) { list.appendChild(li); });
    (nav.hidden || []).forEach(function(value) {
      var a = tabLink(value);
      if (a) a.parentNode.classList.add('gta-hidden');
    });
    document.addEventListener('click', function(ev) {
      if (!(ev.target.closest && ev.target.closest('.gta-menu'))) closeMenus();
    });
  }
  function activeTab() {
    var a = list && list.querySelector('li.active > a[data-value]');
    return a ? a.getAttribute('data-value') : null;
  }
  function updateNavState() {
    var value = activeTab();
    menus.forEach(function(m) {
      var inside = m.values.indexOf(value) >= 0;
      m.btn.classList.toggle('is-active', inside);
      m.items.forEach(function(b) {
        if (b.getAttribute('data-gta-value') === value) b.setAttribute('aria-current', 'page');
        else b.removeAttribute('aria-current');
      });
    });
    Array.prototype.forEach.call(document.querySelectorAll('[data-gta-tab]'), function(b) {
      if (b.getAttribute('data-gta-tab') === value) b.setAttribute('aria-current', 'page');
      else b.removeAttribute('aria-current');
    });
  }
  function markPanes() {
    Array.prototype.forEach.call(document.querySelectorAll('.gtw-main > .tab-content > .tab-pane'), function(pane) {
      if (pane.querySelector(':scope > .gta-workspace')) pane.classList.add('gta-pane');
    });
  }

  // ---- Layers: modal workspace and bottom panels --------------------------
  var modalReturn = null;
  var panelReturn = null;
  function isOpen(node) { return !!node && node.classList.contains('is-open'); }
  function openModal(id, opener) {
    var m = document.getElementById(id);
    if (!m || isOpen(m)) return;
    closePanels(false);
    closeMenus();
    modalReturn = opener || document.activeElement;
    m.classList.add('is-open');
    m.setAttribute('aria-hidden', 'false');
    setTimeout(function() {
      var tab = m.querySelector('.nav-tabs > li.active > a');
      (tab || focusables(m)[0] || m).focus();
      if ($) $(window).trigger('resize');
    }, 40);
  }
  function closeModal(id, returnFocus) {
    var m = document.getElementById(id);
    if (!m || !isOpen(m)) return;
    m.classList.remove('is-open');
    m.setAttribute('aria-hidden', 'true');
    var back = modalReturn && document.body.contains(modalReturn) && visible(modalReturn) ? modalReturn : document.getElementById('gta-data-chip');
    modalReturn = null;
    if (returnFocus !== false && back) back.focus();
  }
  function openModalNode() { return document.querySelector('.gta-modal.is-open'); }
  function togglesFor(id) { return document.querySelectorAll('[data-gta-toggle=' + JSON.stringify(id) + ']'); }
  function fitPanel(p) {
    var body = p.querySelector('.gta-panel-body');
    if (!body) return;
    p.classList.remove('gta-fit');
    if (body.scrollHeight <= body.clientHeight + 1) p.classList.add('gta-fit');
  }
  function openPanel(id, opener) {
    var p = document.getElementById(id);
    if (!p) return;
    closePanels(false);
    p.classList.add('is-open');
    Array.prototype.forEach.call(togglesFor(id), function(t) { t.setAttribute('aria-expanded', 'true'); });
    panelReturn = opener || null;
    fitPanel(p);
    // Focus the panel itself (not its first field, which could open a
    // dropdown); Tab then moves through the panel's controls.
    setTimeout(function() { p.focus(); }, 30);
  }
  function closePanels(returnFocus) {
    var any = false;
    Array.prototype.forEach.call(document.querySelectorAll('.gta-panel.is-open'), function(p) {
      any = true;
      p.classList.remove('is-open');
      Array.prototype.forEach.call(togglesFor(p.id), function(t) { t.setAttribute('aria-expanded', 'false'); });
    });
    if (any && returnFocus && panelReturn && document.body.contains(panelReturn)) panelReturn.focus();
    if (any) panelReturn = null;
    return any;
  }
  // Popovers ([data-gta-pop] -> element id); kept laid out while closed.
  var openPop = null;
  function placePop(pop, anchor) {
    var r = anchor.getBoundingClientRect();
    var w = pop.offsetWidth || 320;
    var h = pop.offsetHeight || 160;
    var left = Math.max(8, Math.min(r.left, window.innerWidth - w - 8));
    var top = r.bottom + 4;
    if (top + h > window.innerHeight - 8) top = Math.max(8, r.top - h - 4);
    pop.style.left = left + 'px';
    pop.style.top = top + 'px';
  }
  function closePop(returnFocus) {
    if (!openPop) return false;
    openPop.pop.classList.remove('is-open');
    openPop.btn.setAttribute('aria-expanded', 'false');
    if (returnFocus) openPop.btn.focus();
    openPop = null;
    return true;
  }
  function togglePop(btn) {
    var pop = document.getElementById(btn.getAttribute('data-gta-pop'));
    if (!pop) return;
    if (openPop && openPop.pop === pop) { closePop(false); return; }
    closePop(false);
    pop.classList.add('is-open');
    placePop(pop, btn);
    btn.setAttribute('aria-expanded', 'true');
    openPop = { pop: pop, btn: btn };
  }
  document.addEventListener('click', function(ev) {
    if (openPop && !openPop.pop.contains(ev.target) && !openPop.btn.contains(ev.target)) closePop(false);
  });
  window.addEventListener('resize', function() { closePop(false); });
  document.addEventListener('click', function(ev) {
    var t = ev.target.closest && ev.target.closest('[data-gta-toggle], [data-gta-close], [data-gta-open], [data-gta-pick], [data-gta-click], [data-gta-pop], [data-gta-focus], .gta-copy-active, #gta-data-chip');
    if (!t) return;
    if (t.hasAttribute('data-gta-pop')) { togglePop(t); return; }
    if (t.hasAttribute('data-gta-focus')) {
      var sel = document.getElementById(t.getAttribute('data-gta-focus'));
      if (sel && sel.selectize) sel.selectize.focus(); else if (sel) sel.focus();
      return;
    }
    if (t.classList.contains('gta-copy-active')) {
      var panel = t.closest('.gta-panel');
      var pre = panel && Array.prototype.filter.call(panel.querySelectorAll('.gta-code-for pre[id]'), function(p) { return !p.closest('[hidden]'); })[0];
      if (pre) window.gtxCopyCode(pre.id, t);
      return;
    }
    if (t.id === 'gta-data-chip') { openModal('gta-data', t); return; }
    if (t.hasAttribute('data-gta-toggle')) {
      var id = t.getAttribute('data-gta-toggle');
      if (isOpen(document.getElementById(id))) closePanels(true); else openPanel(id, t);
      return;
    }
    if (t.hasAttribute('data-gta-close')) {
      var target = document.getElementById(t.getAttribute('data-gta-close'));
      if (target && target.classList.contains('gta-modal')) closeModal(target.id);
      else closePanels(true);
      return;
    }
    if (t.hasAttribute('data-gta-open')) { openModal(t.getAttribute('data-gta-open'), t); return; }
    if (t.hasAttribute('data-gta-pick')) {
      var add = document.querySelector('#' + t.getAttribute('data-gta-pick') + ' .gtw-picker-add');
      if (add) { ev.stopPropagation(); add.click(); }
      return;
    }
    if (t.hasAttribute('data-gta-click')) {
      var b = document.getElementById(t.getAttribute('data-gta-click'));
      if (b) b.click();
    }
  });
  var escFromSelectize = false;
  document.addEventListener('keydown', function(ev) {
    if (ev.key !== 'Escape') return;
    var ctl = ev.target && ev.target.closest ? ev.target.closest('.selectize-control') : null;
    escFromSelectize = !!(ctl && ctl.querySelector('.selectize-input.dropdown-active'));
  }, true);
  document.addEventListener('keydown', function(ev) {
    var m = openModalNode();
    if (ev.key === 'Escape') {
      if (escFromSelectize) return;
      if (document.querySelector('.gtw-picker-pop, .gtw-export-menu')) return;
      if (menus.some(function(x) { return !x.pop.hidden; })) { closeMenus(); return; }
      if (closePop(true)) return;
      if (m) { closeModal(m.id); return; }
      if (closePanels(true)) return;
    }
    if (ev.key === 'Tab' && m) {
      var f = focusables(m.querySelector('.gta-modal-card'));
      if (!f.length) return;
      var first = f[0];
      var last = f[f.length - 1];
      if (!m.contains(document.activeElement)) { first.focus(); ev.preventDefault(); return; }
      if (ev.shiftKey && document.activeElement === first) { last.focus(); ev.preventDefault(); }
      else if (!ev.shiftKey && document.activeElement === last) { first.focus(); ev.preventDefault(); }
    }
  });
  window.addEventListener('resize', function() {
    Array.prototype.forEach.call(document.querySelectorAll('.gta-panel.is-open'), fitPanel);
  });

  // ---- Data chip and Data workspace status -------------------------------
  var autoOpened = false;
  var dataRows = null;
  function middle(text, max) {
    return text.length > max ? text.slice(0, Math.ceil(max / 2) - 1) + String.fromCharCode(8230) + text.slice(-(Math.floor(max / 2) - 1)) : text;
  }
  function updateChip() {
    var chip = document.getElementById('gta-data-chip');
    if (!chip) return null;
    var status = chip.querySelector('.gtw-status');
    var empty = !status || !!chip.querySelector('.gtw-status-empty');
    chip.classList.toggle('gta-chip-empty', empty);
    var parts = status ? Array.prototype.filter.call(status.children, function(s) { return !s.classList.contains('gtw-status-sep'); }) : [];
    if (parts.length >= 4) {
      parts[2].setAttribute('data-gta-part', 'origin');
    }
    dataRows = null;
    if (!empty && parts[1]) {
      var m = (parts[1].textContent || '').replace(/,/g, '').match(/^\\s*(\\d+)/);
      if (m) dataRows = parseInt(m[1], 10);
    }
    var name = status && status.querySelector('.gtw-status-name');
    if (name) {
      var full = name.getAttribute('data-full') || name.textContent;
      name.setAttribute('data-full', full);
      name.title = full;
      name.textContent = middle(full, 30);
    }
    var text = empty ? 'No dataset loaded' : parts.map(function(s) { return s.getAttribute('data-full') || s.textContent; }).join(DOT);
    chip.setAttribute('aria-label', 'Data: ' + text + '. Open the Data workspace');
    chip.title = empty ? 'Choose data' : text + ' (open the Data workspace)';
    var foot = document.querySelector('.gta-data-status');
    if (foot) {
      foot.textContent = empty ? 'No dataset loaded yet.' : text;
      foot.classList.toggle('is-loaded', !empty);
    }
    return empty;
  }

  // ---- Describe: state and ready line -----------------------------------
  function checked(id) {
    return Array.prototype.filter.call(document.querySelectorAll('#' + id + ' input[type=checkbox]'), function(b) { return b.checked; });
  }
  function describeState() {
    if (!document.getElementById('desc_by')) return 'nodata';
    var out = document.getElementById('desc_table');
    if (out && (out.classList.contains('shiny-output-error') || out.children.length || (out.textContent || '').trim())) return 'results';
    return checked('desc_exposures').length ? 'ready' : 'empty';
  }
  function selectText(id) {
    var s = document.getElementById(id);
    if (!s) return '';
    if (s.selectize) {
      var v = s.selectize.getValue();
      var o = s.selectize.options[v];
      return v ? (o ? (o.label || o.text || v) : v) : '';
    }
    return s.value || '';
  }
  // Ready lines follow the shared UI-copy rule: UI = action/state, contextual
  // help = explanation, Help = detail. Only meaningful non-default state is
  // named; obvious zero/default conditions are left out.
  function changedCount(panelId) {
    var p = document.getElementById(panelId);
    return p ? parseInt(p.getAttribute('data-gtw-changed') || '0', 10) : 0;
  }
  function nText() { return dataRows === null ? null : 'N=' + dataRows.toLocaleString('en-GB'); }
  function setReady(ws, ok, parts) {
    var ready = ws.querySelector('.gta-ready');
    var text = ready.querySelector('.gta-ready-text');
    var nChecks = ok ? ws.querySelectorAll('.gtx-check-item').length : 0;
    if (nChecks) parts = parts.concat([nChecks + (nChecks === 1 ? ' thing to check' : ' things to check')]);
    var line = ok ? ['Ready'].concat(parts).filter(Boolean).join(DOT) : parts[0];
    ready.setAttribute('data-ready', ok ? 'true' : 'false');
    ready.setAttribute('data-checks', nChecks ? 'true' : 'false');
    if (text.textContent !== line) text.textContent = line;
    ready.title = line;
  }
  function plural(n, one, many) { return n + ' ' + (n === 1 ? one : many); }
  function updateDescribe() {
    var ws = document.querySelector('.gta-describe');
    if (!ws) return;
    var state = describeState();
    ws.setAttribute('data-gta-state', state);
    var n = checked('desc_exposures').length;
    if (state === 'nodata') { setReady(ws, false, ['Load data to begin.']); return; }
    if (!n) { setReady(ws, false, ['Choose at least one variable.']); return; }
    var by = selectText('desc_by');
    setReady(ws, true, [
      plural(n, 'variable', 'variables'),
      by ? 'by ' + by : null,
      changedCount('gta-panel-desc-options') ? 'options changed' : null,
      nText()
    ]);
  }
  var regTabKeys = { 'Univariate': 'uni', 'Multivariable': 'multi', 'Stratified': 'strat', 'Model Stats': 'stats' };
  var regTabNames = { 'Univariate': 'Univariable table', 'Multivariable': 'Multivariable table', 'Stratified': 'Stratified table', 'Model Stats': 'Model Stats' };
  function paneHasResult(pane) {
    if (!pane) return false;
    var outs = pane.querySelectorAll('.shiny-html-output, .html-widget-output, .shiny-plot-output');
    return Array.prototype.some.call(outs, function(o) {
      if (o.offsetParent === null) return false;
      if (o.classList.contains('shiny-plot-output')) return !!o.querySelector('img');
      if (o.classList.contains('shiny-output-error')) return true;
      if (o.classList.contains('html-widget-output')) return !!o.querySelector('table');
      return o.children.length > 0 || !!(o.textContent || '').trim();
    });
  }
  function updateRegression() {
    var ws = document.querySelector('.gta-regression');
    if (!ws) return;
    var pane = ws.querySelector('.gta-results > .tabbable > .tab-content > .tab-pane.active');
    var tab = pane ? pane.getAttribute('data-value') : 'Univariate';
    var hasData = !!document.getElementById('reg_outcome');
    var outcome = hasData ? selectText('reg_outcome') : '';
    var nExp = checked('reg_exposures').length;
    var show;
    if (paneHasResult(pane)) show = 'results';
    else if (!hasData) show = 'nodata';
    else if (tab === 'Model Stats') show = 'ready-stats';
    else if (tab === 'Stratified' && outcome && nExp) show = 'ready-strat';
    else if (!outcome) show = 'no-outcome';
    else if (!nExp) show = 'no-exposure';
    else show = 'ready-' + (regTabKeys[tab] || 'uni');
    ws.setAttribute('data-gta-show', show);
    // Ready line
    if (!hasData) setReady(ws, false, ['Load data to begin.']);
    else if (!outcome) setReady(ws, false, ['Choose an outcome.']);
    else if (!nExp) setReady(ws, false, ['Choose at least one exposure.']);
    else {
      var nAdj = checked('reg_adjust').length;
      var inter = document.getElementById('reg_interaction');
      var interText = inter && inter.value.trim() ? 'interaction ' + inter.value.trim() : null;
      setReady(ws, true, [
        selectText('reg_approach'),
        plural(nExp, 'exposure', 'exposures'),
        nAdj ? 'adjusted for ' + nAdj : null,
        interText,
        changedCount('gta-panel-reg-refs') ? 'references changed' : null,
        changedCount('gta-panel-reg-options') ? 'options changed' : null,
        nText()
      ]);
    }
    // Code panel follows the active result tab
    var ctx = ws.querySelector('.gta-code-context');
    if (ctx) ctx.textContent = regTabNames[tab] || '';
    Array.prototype.forEach.call(ws.querySelectorAll('.gta-code-for'), function(b) {
      var on = b.getAttribute('data-gta-tab') === tab;
      if (b.hidden === on) {
        b.hidden = !on;
        if (on && $) $(b).trigger('shown');
      }
    });
    var copy = ws.querySelector('.gta-copy-active');
    if (copy) copy.disabled = tab === 'Model Stats';
  }
  // ---- Survival: method selector, state and ready line -------------------
  var survTabFor = { 'Model Result': 'cox survreg competing', 'KM Plot': 'km', 'Cumulative Incidence': 'competing', 'Survival Tables': 'km' };
  function survWs() { return document.querySelector('.gta-survival'); }
  function survTabs(ws) { return ws.querySelectorAll('.gta-results > .tabbable > .nav-tabs > li'); }
  function survMarkTabs() {
    var ws = survWs();
    if (!ws) return;
    Array.prototype.forEach.call(survTabs(ws), function(li) {
      var a = li.querySelector('a[data-value]');
      if (a && survTabFor[a.getAttribute('data-value')]) li.setAttribute('data-gta-tabfor', survTabFor[a.getAttribute('data-value')]);
    });
  }
  function survEnsureVisibleTab(ws) {
    var lis = Array.prototype.slice.call(survTabs(ws));
    var active = lis.filter(function(li) { return li.classList.contains('active'); })[0];
    if (active && getComputedStyle(active).display !== 'none') return;
    var first = lis.filter(function(li) { return getComputedStyle(li).display !== 'none'; })[0];
    var a = first && first.querySelector('a[data-value]');
    if (a && $) $(a).tab('show');
  }
  function survMode() {
    var r = document.querySelector('input[name=surv_mode]:checked');
    return r ? r.value : 'cox';
  }
  function survSetMode(mode) {
    var r = document.querySelector('input[name=surv_mode][value=' + JSON.stringify(mode) + ']');
    if (r && !r.checked) {
      r.checked = true;
      if ($) $(r).trigger('change');
    }
  }
  function survSetApproach(id) {
    var ws = survWs();
    if (!ws) return;
    ws.setAttribute('data-gta-approach', id);
    var item = ws.querySelector('.gta-seg-btn[data-gta-approach=' + JSON.stringify(id) + ']');
    Array.prototype.forEach.call(ws.querySelectorAll('.gta-surv-methods .gta-seg-btn'), function(b) {
      var on = b === item;
      b.setAttribute('aria-checked', on ? 'true' : 'false');
      b.setAttribute('tabindex', on ? '0' : '-1');
    });
    var mode = item ? item.getAttribute('data-gta-mode') : '';
    if (mode) survSetMode(mode);
    else if (survMode() === 'competing') survSetMode('cox');
    ws.setAttribute('data-gta-mode', survMode());
    closePanels(false);
    closePop(false);
    setTimeout(function() { survEnsureVisibleTab(ws); schedule(); }, 0);
  }
  // Radio-group keyboard model: Tab reaches the selected method; arrows,
  // Home and End move and select; Enter/Space select the focused method.
  function survBindMethods() {
    var ws = survWs();
    if (!ws) return;
    var items = Array.prototype.slice.call(ws.querySelectorAll('.gta-surv-methods .gta-seg-btn'));
    items.forEach(function(b) {
      b.addEventListener('click', function() { survSetApproach(b.getAttribute('data-gta-approach')); });
      b.addEventListener('keydown', function(ev) {
        var i = items.indexOf(b), j = null;
        if (ev.key === 'ArrowRight' || ev.key === 'ArrowDown') j = (i + 1) % items.length;
        else if (ev.key === 'ArrowLeft' || ev.key === 'ArrowUp') j = (i - 1 + items.length) % items.length;
        else if (ev.key === 'Home') j = 0;
        else if (ev.key === 'End') j = items.length - 1;
        if (j === null) return;
        ev.preventDefault();
        items[j].focus();
        survSetApproach(items[j].getAttribute('data-gta-approach'));
      });
    });
  }
  function capital(x) { return x ? x.charAt(0).toUpperCase() + x.slice(1) : x; }
  function updateSurvival() {
    var ws = survWs();
    if (!ws) return;
    var approach = ws.getAttribute('data-gta-approach') || 'km';
    if (ws.getAttribute('data-gta-mode') !== survMode()) ws.setAttribute('data-gta-mode', survMode());
    var pane = ws.querySelector('.gta-results > .tabbable > .tab-content > .tab-pane.active');
    var tab = pane ? pane.getAttribute('data-value') : '';
    var hasData = !!document.getElementById('surv_time');
    var time = hasData ? selectText('surv_time') : '';
    var event = hasData ? selectText('surv_event') : '';
    var nExp = checked('surv_exposures').length;
    var nAdj = checked('surv_adjust').length;
    var multi = !!(document.getElementById('surv_multivariable') || {}).checked;
    var multiBox = ws.querySelector('.gta-surv-multi');
    if (multiBox && multiBox.getAttribute('data-gta-on') !== String(multi)) multiBox.setAttribute('data-gta-on', String(multi));
    var censor = selectText('competing_censor_code');
    var eoi = selectText('competing_event_of_interest');
    var show;
    if (paneHasResult(pane)) show = 'results';
    else if (!hasData) show = 'nodata';
    else if (!time) show = 'need-time';
    else if (!event) show = 'need-event';
    else if (tab === 'KM Plot' || tab === 'Survival Tables') show = 'ready-km';
    else if (tab === 'Cumulative Incidence') show = 'ready-cuminc';
    else if (!nExp) show = 'need-exposure';
    else show = 'ready-' + approach;
    ws.setAttribute('data-gta-show', show);
    if (!hasData) { setReady(ws, false, ['Load data to begin.']); return; }
    if (!time) { setReady(ws, false, ['Choose time.']); return; }
    if (!event) { setReady(ws, false, ['Choose event.']); return; }
    if (approach === 'km') {
      var by = selectText('km_by');
      setReady(ws, true, [
        'Kaplan\\u2013Meier',
        by ? 'by ' + by : 'overall curve',
        by ? null : 'choose Group by to compare',
        changedCount('gta-panel-km-plot') ? 'plot options changed' : null,
        nText()
      ]);
      return;
    }
    if (approach === 'competing' && !censor) { setReady(ws, false, ['Choose the censoring code.']); return; }
    if (approach === 'competing' && !eoi) { setReady(ws, false, ['Choose the event of interest.']); return; }
    if (!nExp) { setReady(ws, false, ['Choose exposures.']); return; }
    if (multi && nAdj && approach !== 'competing') { setReady(ws, false, ['Single multivariable model uses Exposures only: clear Adjust for, or turn it off.']); return; }
    var strat = approach === 'competing' ? '' : selectText('surv_stratifier');
    var label = approach === 'cox' ? 'Cox PH' : approach === 'survreg' ? 'Parametric survival' : 'Competing risks';
    setReady(ws, true, [
      label,
      approach === 'survreg' ? capital(selectText('surv_dist') || 'weibull') : null,
      approach === 'competing' ? 'event ' + eoi : null,
      approach === 'competing' ? 'censoring ' + censor : null,
      plural(nExp, 'exposure', 'exposures'),
      multi ? 'single multivariable model' : (nAdj ? 'adjusted for ' + nAdj : null),
      strat ? 'stratified by ' + strat : null,
      changedCount('gta-panel-surv-options') ? 'options changed' : null,
      approach === 'competing' && changedCount('gta-panel-cuminc-plot') ? 'plot options changed' : null,
      nText()
    ]);
  }
  // ---- More models: method selector, field roles, state and ready line ----
  var moreTabFor = {
    'Exposure Models': 'ordinal multinomial matched',
    'Multivariable Model': 'ordinal multinomial multilevel matched',
    'Model Stats': 'ordinal multinomial multilevel matched',
    'Assumption Check': 'ordinal',
    'Table': 'mediation',
    'Diagram': 'mediation'
  };
  var moreNames = { ordinal: 'Ordinal', multinomial: 'Multinomial', multilevel: 'Multilevel', matched: 'Conditional logistic', mediation: 'Mediation' };
  var moreReadyTitles = {
    ordinal: 'Ready for ordinal regression',
    multinomial: 'Ready for multinomial regression',
    multilevel: 'Ready for multilevel regression',
    matched: 'Ready for conditional logistic regression',
    mediation: 'Ready for mediation analysis'
  };
  var moreSlots = [
    ['special_outcome', 'outcome'], ['special_exposures', 'exposures'], ['special_adjust', 'adjust'],
    ['special_group', 'group'], ['special_cluster_approach', 'cluster'], ['special_matched_set', 'matched'],
    ['special_multivariable', 'multi'], ['special_assumption_test', 'po'],
    ['med_outcome', 'm-outcome'], ['med_exposure', 'm-exposure'], ['med_mediator', 'm-mediator'], ['med_covariates', 'm-cov']
  ];
  function moreWs() { return document.querySelector('.gta-more'); }
  function moreTabs(ws) { return ws.querySelectorAll('.gta-results > .tabbable > .nav-tabs > li'); }
  function moreMarkTabs() {
    var ws = moreWs();
    if (!ws) return;
    Array.prototype.forEach.call(moreTabs(ws), function(li) {
      var a = li.querySelector('a[data-value]');
      if (a && moreTabFor[a.getAttribute('data-value')]) li.setAttribute('data-gta-tabfor', moreTabFor[a.getAttribute('data-value')]);
    });
  }
  // Server-built fields are tagged by role so the bar can place them; the
  // tags are re-applied after every re-render of the server output.
  function moreTagSlots() {
    ['special_inputs', 'med_inputs'].forEach(function(id) {
      var box = document.getElementById(id);
      if (!box) return;
      Array.prototype.forEach.call(box.children, function(c) {
        if (c.hasAttribute('data-gta-slot')) return;
        var slot = null;
        if (c.classList.contains('gtx-select-buttons')) slot = 'btns';
        else moreSlots.some(function(m) {
          if (c.id === m[0] || c.querySelector('#' + m[0])) { slot = m[1]; return true; }
          return false;
        });
        if (slot) c.setAttribute('data-gta-slot', slot);
      });
    });
    // Same field name as Regression and Survival (presentation only).
    var expLabel = document.getElementById('special_exposures-label');
    if (expLabel && expLabel.textContent === 'Reported exposures') expLabel.textContent = 'Exposures';
  }
  function moreEnsureVisibleTab(ws) {
    var lis = Array.prototype.slice.call(moreTabs(ws));
    var active = lis.filter(function(li) { return li.classList.contains('active'); })[0];
    if (active && getComputedStyle(active).display !== 'none') return;
    var first = lis.filter(function(li) { return getComputedStyle(li).display !== 'none'; })[0];
    var a = first && first.querySelector('a[data-value]');
    if (a && $) $(a).tab('show');
  }
  function moreSetMode(mode) {
    var r = document.querySelector('input[name=special_mode][value=' + JSON.stringify(mode) + ']');
    if (r && !r.checked) {
      r.checked = true;
      if ($) $(r).trigger('change');
    }
  }
  function moreSetMethod(id) {
    var ws = moreWs();
    if (!ws) return;
    ws.setAttribute('data-gta-method', id);
    var item = ws.querySelector('.gta-more-methods .gta-seg-btn[data-gta-method=' + JSON.stringify(id) + ']');
    Array.prototype.forEach.call(ws.querySelectorAll('.gta-more-methods .gta-seg-btn'), function(b) {
      var on = b === item;
      b.setAttribute('aria-checked', on ? 'true' : 'false');
      b.setAttribute('tabindex', on ? '0' : '-1');
    });
    var mode = item ? item.getAttribute('data-gta-mode') : '';
    if (mode) moreSetMode(mode);
    closePanels(false);
    closePop(false);
    setTimeout(function() { moreEnsureVisibleTab(ws); schedule(); }, 0);
  }
  function moreBindMethods() {
    var ws = moreWs();
    if (!ws) return;
    var items = Array.prototype.slice.call(ws.querySelectorAll('.gta-more-methods .gta-seg-btn'));
    items.forEach(function(b) {
      b.addEventListener('click', function() { moreSetMethod(b.getAttribute('data-gta-method')); });
      b.addEventListener('keydown', function(ev) {
        var i = items.indexOf(b), j = null;
        if (ev.key === 'ArrowRight' || ev.key === 'ArrowDown') j = (i + 1) % items.length;
        else if (ev.key === 'ArrowLeft' || ev.key === 'ArrowUp') j = (i - 1 + items.length) % items.length;
        else if (ev.key === 'Home') j = 0;
        else if (ev.key === 'End') j = items.length - 1;
        if (j === null) return;
        ev.preventDefault();
        items[j].focus();
        moreSetMethod(items[j].getAttribute('data-gta-method'));
      });
    });
  }
  function selectValue(id) {
    var s = document.getElementById(id);
    if (!s) return '';
    if (s.selectize) return s.selectize.getValue() || '';
    return s.value || '';
  }
  // The next genuinely required input for the method, or null when complete.
  function moreNeed(method) {
    if (method === 'mediation') {
      if (!selectText('med_outcome')) return { text: 'Choose outcome.', title: 'Choose outcome', body: 'The outcome is the final variable in the exposure \\u2192 mediator \\u2192 outcome path.', focus: 'med_outcome' };
      if (!selectText('med_exposure')) return { text: 'Choose exposure.', title: 'Choose exposure', body: '', focus: 'med_exposure' };
      if (!selectText('med_mediator')) return { text: 'Choose mediator.', title: 'Choose mediator', body: 'The variable through which part of the exposure effect may act.', focus: 'med_mediator' };
      return null;
    }
    if (!selectText('special_outcome')) return { text: 'Choose outcome.', title: 'Choose outcome', body: '', focus: 'special_outcome' };
    if (!checked('special_exposures').length) return { text: 'Choose exposures.', title: 'Choose exposures', body: 'Add at least one exposure to fit the model.', pick: 'special_exposures' };
    if (method === 'multilevel' && !selectText('special_group')) return { text: 'Choose grouping variable.', title: 'Choose grouping variable', body: 'Observations are grouped within the levels of this variable.', focus: 'special_group' };
    if (method === 'matched' && !selectText('special_matched_set')) return { text: 'Choose matched set.', title: 'Choose matched set', body: 'Each case and its matched controls share this identifier.', focus: 'special_matched_set' };
    return null;
  }
  function updateMore() {
    var ws = moreWs();
    if (!ws) return;
    moreTagSlots();
    var method = ws.getAttribute('data-gta-method') || 'ordinal';
    var pane = ws.querySelector('.gta-results > .tabbable > .tab-content > .tab-pane.active');
    var hasData = !!document.getElementById(method === 'mediation' ? 'med_outcome' : 'special_outcome');
    var multi = !!(document.getElementById('special_multivariable') || {}).checked;
    var multiSlot = ws.querySelector('[data-gta-slot=multi]');
    if (multiSlot && multiSlot.getAttribute('data-gta-on') !== String(multi)) multiSlot.setAttribute('data-gta-on', String(multi));
    Array.prototype.forEach.call(ws.querySelectorAll('.gta-code-panel .gta-code-for'), function(b) {
      var on = (' ' + b.getAttribute('data-gta-for') + ' ').indexOf(' ' + method + ' ') >= 0;
      if (b.hidden === on) b.hidden = !on;
    });
    var need = hasData ? moreNeed(method) : null;
    var show;
    if (paneHasResult(pane)) show = 'results';
    else if (!hasData) show = 'nodata';
    else if (need) show = 'need';
    else show = 'ready';
    ws.setAttribute('data-gta-show', show);
    var needState = ws.querySelector('.gta-empty-state[data-gta-for=need]');
    if (need && needState) {
      needState.querySelector('.gta-empty-title').textContent = need.title;
      needState.querySelector('.gta-empty-text').textContent = need.body;
      var nb = needState.querySelector('.gta-more-need-btn');
      nb.textContent = need.title;
      if (need.pick) { nb.removeAttribute('data-gta-focus'); nb.setAttribute('data-gta-pick', need.pick); }
      else { nb.removeAttribute('data-gta-pick'); nb.setAttribute('data-gta-focus', need.focus); }
    }
    var readyState = ws.querySelector('.gta-empty-state[data-gta-for=ready]');
    if (readyState) {
      readyState.querySelector('.gta-empty-title').textContent = moreReadyTitles[method];
      readyState.querySelector('.gta-more-ready-btn').setAttribute('data-gta-click', method === 'mediation' ? 'run_mediation' : 'run_special');
    }
    if (!hasData) { setReady(ws, false, ['Load data to begin.']); return; }
    if (need) { setReady(ws, false, [need.text]); return; }
    if (method === 'mediation') {
      var nCov = checked('med_covariates').length;
      setReady(ws, true, [
        moreNames.mediation,
        selectText('med_exposure') + ' \\u2192 ' + selectText('med_mediator') + ' \\u2192 ' + selectText('med_outcome'),
        selectValue('med_approach') === 'linear' ? 'linear outcome model' : 'logistic outcome model',
        nCov ? 'adjusted for ' + nCov : null,
        changedCount('gta-panel-med-options') ? 'options changed' : null,
        nText()
      ]);
      return;
    }
    var nExp = checked('special_exposures').length;
    var nAdj = checked('special_adjust').length;
    setReady(ws, true, [
      moreNames[method],
      method === 'multilevel' ? (selectValue('special_cluster_approach') === 'logit' ? 'logistic' : 'linear') : null,
      selectText('special_outcome'),
      method === 'multilevel' ? 'clustered by ' + selectText('special_group') : null,
      method === 'matched' ? 'matched by ' + selectText('special_matched_set') : null,
      plural(nExp, 'exposure', 'exposures'),
      nAdj ? 'adjusted for ' + nAdj : null,
      multi && method !== 'multilevel' ? 'single multivariable model' : null,
      changedCount('gta-panel-more-refs') ? 'references changed' : null,
      changedCount('gta-panel-more-options') ? 'options changed' : null,
      nText()
    ]);
  }
  // ---- Trials: risk difference state and ready line ----------------------
  // The next required input, or null when the definition is complete. A
  // variable that cannot be used (not binary, not two groups) is reported by
  // the server next to the field and repeated here.
  function trialProblem(ws, slot) {
    var p = ws.querySelector('.' + slot + ' [data-gta-problem]');
    return p ? p.getAttribute('data-gta-problem') : '';
  }
  function trialNeed(ws) {
    if (!selectValue('trial_outcome')) return { text: 'Choose outcome.', title: 'Choose outcome', body: 'A binary outcome: the event either happened or did not.', focus: 'trial_outcome' };
    var bad = trialProblem(ws, 'gta-trial-event');
    if (bad) return { text: bad, title: 'Choose a binary outcome', body: bad, focus: 'trial_outcome' };
    if (!document.getElementById('trial_event')) return { text: 'Reading outcome values\\u2026', title: 'Reading outcome values', body: '', focus: 'trial_outcome' };
    if (!selectValue('trial_exposure')) return { text: 'Choose treatment group.', title: 'Choose treatment group', body: 'The variable holding the two groups to compare, for example treatment and control.', focus: 'trial_exposure' };
    bad = trialProblem(ws, 'gta-trial-ref');
    if (bad) return { text: bad, title: 'Choose a two-group variable', body: bad, focus: 'trial_exposure' };
    if (!document.getElementById('trial_reference')) return { text: 'Reading groups\\u2026', title: 'Reading groups', body: '', focus: 'trial_exposure' };
    return null;
  }
  function updateTrials() {
    var ws = document.querySelector('.gta-trials');
    if (!ws) return;
    var hasData = !!document.getElementById('trial_outcome');
    var out = document.getElementById('trial_table');
    var hasResult = !!(out && (out.classList.contains('shiny-output-error') || out.children.length || (out.textContent || '').trim()));
    var need = hasData ? trialNeed(ws) : null;
    var show = hasResult ? 'results' : !hasData ? 'nodata' : need ? 'need' : 'ready';
    if (ws.getAttribute('data-gta-show') !== show) ws.setAttribute('data-gta-show', show);
    var needState = ws.querySelector('.gta-empty-state[data-gta-for=need]');
    if (need && needState) {
      var t = needState.querySelector('.gta-empty-title');
      var b = needState.querySelector('.gta-empty-text');
      var nb = needState.querySelector('.gta-trial-need-btn');
      if (t.textContent !== need.title) t.textContent = need.title;
      if (b.textContent !== need.body) b.textContent = need.body;
      var btnText = need.focus === 'trial_exposure' ? 'Choose treatment group' : 'Choose outcome';
      if (nb.textContent !== btnText) nb.textContent = btnText;
      if (nb.getAttribute('data-gta-focus') !== need.focus) nb.setAttribute('data-gta-focus', need.focus);
    }
    if (!hasData) { setReady(ws, false, ['Load data to begin.']); return; }
    if (need) { setReady(ws, false, [need.text]); return; }
    var ref = selectValue('trial_reference');
    var refSel = document.getElementById('trial_reference');
    var groups = refSel && refSel.selectize ? Object.keys(refSel.selectize.options) : [];
    var comparison = groups.filter(function(g) { return g !== ref; })[0] || '';
    var nAdj = checked('trial_adjust').length;
    var type = selectValue('trial_event_type');
    setReady(ws, true, [
      'Risk difference',
      selectText('trial_outcome') + ' = ' + selectValue('trial_event'),
      comparison + ' \\u2212 ' + ref,
      nAdj ? 'adjusted for ' + nAdj : 'unadjusted',
      type ? (type === 'harmful' ? 'NNT, harmful event' : 'NNT, beneficial event') : null,
      changedCount('gta-panel-trial-options') ? 'options changed' : null,
      nText()
    ]);
  }
  // ---- Data > Prepare: variable-first selection, actions and sheet -------
  var prepSelected = null;
  var prepSheetInvoker = null;
  var prepPendingApply = null;
  var prepSyncing = false;
  var prepVariableActions = { rename: 1, label: 1, recode: 1, type: 1, missing: 1 };
  function prepItems() { return Array.prototype.slice.call(document.querySelectorAll('.gta-prep-vars .gta-prep-var')); }
  function prepVisibleItems() { return prepItems().filter(function(i) { return !i.hidden; }); }
  function prepItem(name) { return prepItems().filter(function(i) { return i.getAttribute('data-variable') === name; })[0] || null; }
  function prepUpdateFocus(item) {
    var box = document.querySelector('.gta-prep-focus');
    if (!box) return;
    box.classList.toggle('has-variable', !!item);
    if (!item) return;
    var name = item.getAttribute('data-variable');
    var label = item.getAttribute('data-label') || '';
    box.querySelector('.gta-prep-focus-name').textContent = name;
    box.querySelector('.gta-prep-focus-name').title = name;
    var type = item.querySelector('.gta-prep-var-type').textContent;
    box.querySelector('.gta-prep-focus-type').textContent = label ? type + ' \\u00b7 ' + label : type;
    var distinct = parseInt(item.getAttribute('data-distinct') || '0', 10);
    var numeric = item.getAttribute('data-type') === 'Numeric';
    var zeroOne = item.getAttribute('data-zero-one') === 'true';
    // Recode lists every recorded value, so it is offered for variables with
    // a manageable number of distinct values.
    Array.prototype.forEach.call(box.querySelectorAll('.gta-prep-needs-few'), function(b) { b.hidden = !(distinct > 0 && distinct <= 50); });
    var quick = { 'zero-one': zeroOne, 'codes': numeric && item.getAttribute('data-codes') === 'true', 'age': numeric && !zeroOne && distinct > 10 && /age/i.test(name) };
    var any = false;
    Array.prototype.forEach.call(box.querySelectorAll('[data-gta-prep-quick]'), function(b) {
      var on = !!quick[b.getAttribute('data-gta-prep-quick')];
      b.hidden = !on;
      if (on) any = true;
    });
    box.querySelector('.gta-prep-quick').hidden = !any;
  }
  function prepSelect(name, focus) {
    var items = prepItems();
    var hit = name ? prepItem(name) : null;
    items.forEach(function(i) {
      var on = i === hit;
      i.setAttribute('aria-selected', on ? 'true' : 'false');
      i.tabIndex = on ? 0 : -1;
    });
    if (!hit) {
      var first = prepVisibleItems()[0];
      if (first) first.tabIndex = 0;
    }
    prepUpdateFocus(hit);
    if (hit) {
      if (focus) hit.focus();
      if (hit.scrollIntoView) hit.scrollIntoView({ block: 'nearest' });
    }
    var value = hit ? name : '';
    if (value !== prepSelected) {
      prepSelected = value;
      if (window.Shiny && Shiny.setInputValue) Shiny.setInputValue('data_prep-focus_variable', value);
      // Keep Structure on the same variable.
      if (value && !prepSyncing && typeof structureSelect === 'function') {
        prepSyncing = true;
        structureSelect(value, false);
        prepSyncing = false;
      }
    }
  }
  function prepTab(name) {
    var tab = document.querySelector('#gta-data a[data-value=' + name + ']');
    if (tab && $) $(tab).tab('show');
  }
  function prepOpenSheet(key, title, scope, invoker) {
    var sheet = document.getElementById('gta-prep-sheet');
    if (!sheet) return;
    prepSheetInvoker = invoker || null;
    sheet.setAttribute('data-scope', scope);
    sheet.setAttribute('data-action', key);
    sheet.classList.toggle('gta-prep-dialog', ['recode', 'type', 'calculation', 'groups', 'filter'].indexOf(key) !== -1);
    var name = scope === 'variable' && prepSelected ? prepSelected : '';
    document.getElementById('gta-prep-sheet-title').textContent = name ? title + ' \\u00b7 ' + name : title;
    var bmi = sheet.querySelector('[data-gta-prep-quick=bmi]');
    if (bmi) bmi.hidden = key !== 'calculation';
    Array.prototype.forEach.call(document.querySelectorAll('.gta-prep-action'), function(b) { b.classList.toggle('is-active', b.getAttribute('data-gta-prep-action') === key); });
    sheet.classList.add('is-open');
    sheet.setAttribute('aria-hidden', 'false');
    setTimeout(prepFocusSheet, 350);
  }
  function prepFocusSheet() {
    var sheet = document.getElementById('gta-prep-sheet');
    if (!sheet || !sheet.classList.contains('is-open')) return;
    if (sheet.contains(document.activeElement)) return;
    var f = Array.prototype.filter.call(sheet.querySelectorAll('.gta-prep-sheet-body input:not([type=hidden]), .gta-prep-sheet-body select, .gta-prep-sheet-body textarea, .gta-prep-sheet-body button'), function(n) { return n.offsetParent !== null && !n.disabled; })[0];
    if (f) f.focus(); else sheet.querySelector('.gta-prep-sheet-close').focus();
  }
  function prepCloseSheet(returnFocus) {
    var sheet = document.getElementById('gta-prep-sheet');
    if (!sheet || !sheet.classList.contains('is-open')) return false;
    sheet.classList.remove('is-open');
    sheet.setAttribute('aria-hidden', 'true');
    prepPendingApply = null;
    Array.prototype.forEach.call(document.querySelectorAll('.gta-prep-action.is-active'), function(b) { b.classList.remove('is-active'); });
    if (returnFocus && prepSheetInvoker && document.body.contains(prepSheetInvoker) && prepSheetInvoker.offsetParent !== null) prepSheetInvoker.focus();
    else if (returnFocus && prepSelected) { var it = prepItem(prepSelected); if (it) it.focus(); }
    return true;
  }
  function prepChanges() {
    var s = document.querySelector('.gta-prep-state');
    return s ? parseInt(s.getAttribute('data-changes') || '0', 10) : 0;
  }
  function prepSyncState() {
    var s = document.querySelector('.gta-prep-state');
    if (!s) return;
    var undo = document.getElementById('data_prep-undo');
    var redo = document.getElementById('data_prep-redo');
    var reset = document.getElementById('data_prep-reset');
    if (undo) undo.disabled = s.getAttribute('data-can-undo') !== 'true';
    if (redo) redo.disabled = s.getAttribute('data-can-redo') !== 'true';
    if (reset) reset.disabled = s.getAttribute('data-can-undo') !== 'true';
    if (prepPendingApply !== null && prepChanges() > prepPendingApply) prepCloseSheet(true);
  }
  function prepMarkVariableField() {
    var sel = document.getElementById('data_prep-variable');
    var fg = sel && sel.closest('.form-group');
    if (fg) fg.classList.add('gta-prep-varfield');
  }
  document.addEventListener('click', function(ev) {
    if (!ev.target.closest) return;
    var item = ev.target.closest('.gta-prep-vars .gta-prep-var');
    if (item) { prepSelect(item.getAttribute('data-variable'), true); return; }
    var act = ev.target.closest('[data-gta-prep-action]');
    if (act) {
      var key = act.getAttribute('data-gta-prep-action');
      var scope = prepVariableActions[key] ? 'variable' : 'dataset';
      if (scope === 'variable' && !prepSelected) return;
      var menu = document.getElementById('data_prep-menu_' + key);
      if (menu) menu.click();
      prepOpenSheet(key, act.getAttribute('data-gta-prep-title') || act.textContent.trim(), scope, act);
      return;
    }
    var quick = ev.target.closest('[data-gta-prep-quick]');
    if (quick && quick.getAttribute('data-gta-prep-sheet')) {
      var target = quick.getAttribute('data-gta-prep-sheet');
      var titles = { recode: 'Recode', missing: 'Missing values', groups: 'Create groups' };
      // The module fills the normal workflow; nothing is applied until Apply.
      setTimeout(function() { prepOpenSheet(target, titles[target] || 'Change', prepVariableActions[target] ? 'variable' : 'dataset', quick); }, 0);
      return;
    }
    if (ev.target.closest('[data-gta-prep-cancel]')) { prepCloseSheet(true); return; }
    if (ev.target.closest('#data_prep-apply')) { prepPendingApply = prepChanges(); return; }
    if (ev.target.closest('.gta-prep-to-structure')) {
      var name = prepSelected;
      prepTab('Structure');
      if (name && typeof structureSelect === 'function') setTimeout(function() { structureSelect(name, true); }, 300);
    }
  });
  document.addEventListener('keydown', function(ev) {
    var sheet = document.getElementById('gta-prep-sheet');
    if (ev.key === 'Escape' && sheet && sheet.classList.contains('is-open') && !escFromSelectize) {
      if (document.querySelector('.modal.in, .modal.show')) return;
      ev.stopPropagation();
      ev.preventDefault();
      prepCloseSheet(true);
      return;
    }
    var item = ev.target.closest && ev.target.closest('.gta-prep-vars .gta-prep-var');
    if (!item) return;
    var items = prepVisibleItems();
    var i = items.indexOf(item), j = null;
    if (ev.key === 'ArrowDown') j = Math.min(items.length - 1, i + 1);
    else if (ev.key === 'ArrowUp') j = Math.max(0, i - 1);
    else if (ev.key === 'Home') j = 0;
    else if (ev.key === 'End') j = items.length - 1;
    else if (ev.key === 'Enter' || ev.key === ' ') j = i;
    if (j === null || !items[j]) return;
    ev.preventDefault();
    prepSelect(items[j].getAttribute('data-variable'), true);
  }, true);
  document.addEventListener('input', function(ev) {
    if (!ev.target || ev.target.id !== 'gta-prep-search') return;
    var q = ev.target.value.trim().toLowerCase();
    prepItems().forEach(function(i) {
      var text = (i.getAttribute('data-variable') + ' ' + (i.getAttribute('data-label') || '')).toLowerCase();
      i.hidden = !!q && text.indexOf(q) < 0;
    });
  });
  $(document).on('shiny:value', function(ev) {
    if (ev.name === 'prep_variables') setTimeout(function() { prepSelect(prepSelected, false); }, 0);
    if (ev.name === 'prep_state') setTimeout(prepSyncState, 0);
    if (ev.name === 'data_prep-controls') setTimeout(function() { prepMarkVariableField(); prepFocusSheet(); }, 0);
  });
  // ---- Data > Structure: select a variable (mouse, keyboard, notes) -------
  var structureSelected = null;
  function structureRows() {
    return Array.prototype.slice.call(document.querySelectorAll('.gta-structure-grid tbody tr[data-variable]'));
  }
  function structureSelect(name, focus) {
    var rows = structureRows();
    var hit = null;
    rows.forEach(function(r) {
      var on = r.getAttribute('data-variable') === name;
      r.setAttribute('aria-selected', on ? 'true' : 'false');
      r.tabIndex = on ? 0 : -1;
      if (on) hit = r;
    });
    if (!hit && rows.length) rows[0].tabIndex = 0;
    if (hit) {
      if (focus) hit.focus();
      if (hit.scrollIntoView) hit.scrollIntoView({ block: 'nearest' });
    }
    if (name && name !== structureSelected) {
      structureSelected = name;
      if (window.Shiny && Shiny.setInputValue) Shiny.setInputValue('structure_variable', name);
      // Keep Prepare on the same variable.
      if (!prepSyncing) { prepSyncing = true; prepSelect(name, false); prepSyncing = false; }
    }
  }
  // Open the variable in Prepare, selected and ready for its actions.
  function structureOpenPrepare(name) {
    prepTab('Prepare');
    var tries = 0;
    (function select() {
      if (prepItem(name) || tries > 20) { prepSelect(name, true); return; }
      tries += 1;
      setTimeout(select, 100);
    })();
  }
  document.addEventListener('click', function(ev) {
    if (!ev.target.closest) return;
    var note = ev.target.closest('[data-gta-structure-select]');
    if (note) { structureSelect(note.getAttribute('data-gta-structure-select'), true); return; }
    var prep = ev.target.closest('[data-gta-structure-prepare]');
    if (prep) { structureOpenPrepare(prep.getAttribute('data-gta-structure-prepare')); return; }
    var row = ev.target.closest('.gta-structure-grid tbody tr[data-variable]');
    if (row) structureSelect(row.getAttribute('data-variable'), true);
  });
  document.addEventListener('keydown', function(ev) {
    var row = ev.target.closest && ev.target.closest('.gta-structure-grid tbody tr[data-variable]');
    if (!row) return;
    var rows = structureRows();
    var i = rows.indexOf(row), j = null;
    if (ev.key === 'ArrowDown') j = Math.min(rows.length - 1, i + 1);
    else if (ev.key === 'ArrowUp') j = Math.max(0, i - 1);
    else if (ev.key === 'Home') j = 0;
    else if (ev.key === 'End') j = rows.length - 1;
    else if (ev.key === 'PageDown') j = Math.min(rows.length - 1, i + 10);
    else if (ev.key === 'PageUp') j = Math.max(0, i - 10);
    else if (ev.key === 'Enter' || ev.key === ' ') j = i;
    if (j === null) return;
    ev.preventDefault();
    structureSelect(rows[j].getAttribute('data-variable'), true);
  });
  $(document).on('shiny:value', function(ev) {
    if (ev.name === 'structure_table') setTimeout(function() { structureSelect(structureSelected, false); }, 0);
  });
  function updateBadges() {
    Array.prototype.forEach.call(document.querySelectorAll('.gta-panel.gtw-options'), function(p) {
      var n = parseInt(p.getAttribute('data-gtw-changed') || '0', 10);
      Array.prototype.forEach.call(togglesFor(p.id), function(t) {
        var badge = t.querySelector('.gta-badge');
        if (!badge) return;
        badge.hidden = !n;
        badge.textContent = n ? String(n) : '';
        var label = t.querySelector('.gta-tb-label').textContent;
        t.setAttribute('aria-label', n ? label + ', ' + n + ' changed from default' : label);
      });
    });
  }

  // ---- Refresh loop ------------------------------------------------------
  function refresh() {
    if (window.gtwComponents) window.gtwComponents.refresh();
    var empty = updateChip();
    updateDescribe();
    updateRegression();
    updateSurvival();
    updateMore();
    updateTrials();
    updateBadges();
    updateNavState();
    if (empty === true && !autoOpened && cfg.openDataWhenEmpty && window.Shiny && window.Shiny.shinyapp && window.Shiny.shinyapp.isConnected()) {
      autoOpened = true;
      openModal('gta-data', document.getElementById('gta-data-chip'));
    }
    if (empty === false) autoOpened = true;
  }
  var pending = false;
  function schedule() {
    if (pending) return;
    pending = true;
    setTimeout(function() { pending = false; refresh(); }, 90);
  }
  buildNav();
  markPanes();
  survMarkTabs();
  survBindMethods();
  moreMarkTabs();
  moreBindMethods();
  if (moreWs()) setTimeout(function() { moreEnsureVisibleTab(moreWs()); }, 0);
  if (survWs()) setTimeout(function() { survEnsureVisibleTab(survWs()); }, 0);
  refresh();
  // Bootstrap's tab plugin gives previously active task links tabindex=-1;
  // keep every visible task in the Tab order so the top bar is predictable.
  function normaliseNavTabindex() {
    if (!list) return;
    Array.prototype.forEach.call(list.querySelectorAll('a[data-value]'), function(a) { a.removeAttribute('tabindex'); });
  }
  if ($) $(document).on('shown.bs.tab', '#main_nav a', function() { closePanels(false); closePop(false); setTimeout(normaliseNavTabindex, 0); });
  document.addEventListener('change', schedule);
  document.addEventListener('input', schedule);
  if ($) {
    $(document).on('shiny:value shiny:inputchanged shiny:error shown.bs.tab', schedule);
    $(document).on('shiny:value', function(ev) {
      if (ev.name === 'analysis_status') setTimeout(refresh, 0);
    });
  }
  if (window.MutationObserver) {
    Array.prototype.forEach.call(document.querySelectorAll('.gta-workspace'), function(ws) {
      new MutationObserver(schedule).observe(ws, { childList: true, subtree: true });
    });
  }
  window.gtaShell = { openModal: openModal, closeModal: closeModal, openPanel: openPanel, closePanels: closePanels, refresh: refresh };
}
if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', gtaBoot); else gtaBoot();
")

ui <- navbarPage(
  title = if (gta_shell_enabled) gta_data_chip() else tagList("gtregression", span(class = "gtx-badge", "App")),
  windowTitle = if (gta_shell_enabled) "GT Regression" else NA,
  id = "main_nav",
  header = tagList(
    tags$head(
      tags$style(css),
      tags$script(copy_js),
      tags$script(gtx_helper_js),
      if (gta_shell_enabled) tags$script(HTML("document.documentElement.classList.add('gta-shell');")),
      if (gtx_addin_mode) tagList(tags$style(gtw_addin_css), tags$script(gtw_addin_js)),
      if (gtw_components_enabled) tagList(tags$style(gtw_components_css), gtw_component_config(), tags$script(gtw_components_js)),
      if (gta_shell_enabled) tagList(tags$style(gta_css), gta_config(), tags$script(gta_js))
    ),
    if (gta_shell_enabled) {
      tagList(gta_global_actions(), gta_data_workspace())
    } else if (gtx_addin_mode) {
      div(
        class = "gtw-actions",
        tags$button(type = "button", class = "btn btn-default btn-sm", title = "Open Help", onclick = "gtwOpenTab('Help')", "?"),
        actionButton("close_app", "Close", icon = icon("power-off"), class = "btn-default btn-sm", title = "Close GT Regression")
      )
    } else {
      div(class = "gtx-close-app", actionButton("close_app", "Close app", icon = icon("power-off"), class = "btn-danger"))
    },
    uiOutput("workflow_guide"),
    if (!gta_shell_enabled) uiOutput("analysis_status")
  ),

  if (!gta_shell_enabled) tabPanel(
    "Data",
    fluidRow(
      class = "gtw-workbench-row",
      column(
        4,
        class = "gtw-settings",
        div(
          class = "gtx-card gtx-side",
          h3("Load Data"),
          div(class = "gtx-help", "Start with a built-in teaching dataset or import delimited text, Excel, RDS, Stata, SPSS, or SAS data."),
          if (!gtx_addin_mode) div(
            class = "gtx-step",
            "Recommended flow: load data, build descriptive tables, run univariable models, reselect variables, then run multivariable or survival models."
          ),
          if (gtx_addin_mode) radioButtons(
            "data_source",
            "Data source",
            choices = c(
              "R Environment" = "environment",
              "Import file" = "file",
              "Example data" = "example"
            ),
            selected = "environment"
          ),
          gtx_data_source_panel("environment", gtx_data_environment_controls()),
          gtx_data_source_panel("example", gtx_data_example_controls()),
          gtx_data_source_panel("file", gtx_data_file_controls())
        )
      ),
      column(
        8,
        class = "gtw-results",
        gtw_focus_toggle(),
        if (gtx_addin_mode) {
          tabsetPanel(
            tabPanel(
              "Preview",
              div(class = "gtx-card", div(class = "gtx-help", "First 10 rows only. Use Structure to inspect the full dataset before modelling."), gtx_data_output("data_preview"))
            ),
            tabPanel("Structure", div(class = "gtx-card", verbatimTextOutput("data_summary"))),
            tabPanel("Code", gtx_code_panel("Reusable Code", "data_code"))
          )
        } else {
          tagList(
            div(class = "gtx-card", h3("Preview"), div(class = "gtx-help", "First 10 rows only. Use Data Summary to inspect the full structure before modelling."), gtx_data_output("data_preview")),
            div(class = "gtx-card", h4("Data Summary"), verbatimTextOutput("data_summary")),
            gtx_code_panel("Reusable Code", "data_code")
          )
        }
      )
    )
  ),

  if (!gta_shell_enabled) tabPanel(
    "Data Prep",
    value = "data_prep",
    mod_data_prep_ui("data_prep")
  ),

  if (gta_shell_enabled) tabPanel("Descriptive", gta_describe_workspace()) else tabPanel(
    "Descriptive",
    fluidRow(
      class = "gtw-workbench-row",
      column(
        4,
        class = "gtw-settings",
        div(
          class = "gtx-card gtx-side gtw-desc-settings",
          h3("Descriptive Table"),
          gtw_fallback_help(
            div(class = "gtx-help", "Use this first to understand the data and to create baseline tables for merge_table(), forest_df(), and reports."),
            div(class = "gtx-note", "Start by choosing Group by. Leave it as None for one overall summary; otherwise the selected variable defines the comparison columns.")
          ),
          uiOutput("desc_inputs"),
          gtw_options(id = "desc_options", gtx_desc_option_inputs()),
          div(
            class = "gtw-run-bar",
            actionButton("run_desc", "Run descriptive table", class = "btn-primary")
          )
        )
      ),
      column(
        8,
        class = "gtw-results",
        if (gtw_components_enabled) gtw_result_toolbar() else gtw_focus_toggle(),
        if (gtx_addin_mode) {
          tabsetPanel(
            tabPanel(
              "Table",
              div(class = "gtx-card", tagListDownload("desc", table = TRUE), gtx_table_output("desc_table")),
              div(class = "gtw-code-strip", gtx_code_panel("Reusable Code", "desc_code"))
            )
          )
        } else {
          tagList(
            div(class = "gtx-card", tagListDownload("desc", table = TRUE), gtx_table_output("desc_table")),
            gtx_code_panel("Reusable Code", "desc_code")
          )
        }
      )
    )
  ),

  if (gta_shell_enabled) tabPanel("Regression", gta_regression_workspace()) else tabPanel(
    "Regression",
    fluidRow(
      class = "gtw-workbench-row",
      column(
        4,
        class = "gtw-settings",
        div(
          class = "gtx-card gtx-side gtw-reg-settings",
          h3("Regression Tables"),
          div(
            class = "gtw-reg-guidance",
            gtw_fallback_help(
              div(
                class = "gtx-step",
                "Run univariable analysis first. Then keep the important variables selected and run the multivariable model. Use Show reference categories when you plan to merge tables or create forest plots."
              ),
              div(class = "gtx-note", "Choose an outcome first, then exposures. Adjustment variables belong only in the multivariable model and should be selected using the study question, a DAG, and clinical knowledge.")
            )
          ),
          uiOutput("reg_inputs"),
          uiOutput("reg_reference_inputs"),
          div(
            class = "gtw-reg-approach",
            gtx_reg_approach_input()
          ),
          if (gtw_components_enabled) {
            gtw_options(
              id = "reg_options",
              gtw_options_group(
                "Table output",
                gtx_reg_table_option_inputs()
              ),
              gtw_options_group(
                "Stratified analysis",
                gtx_reg_strata_controls()
              )
            )
          } else {
            gtw_details(
              "Table options",
              gtx_reg_table_option_inputs(),
              gtw_fallback_help(gtx_inline_help("Enabled by default so the Model Stats tab is ready after fitting. These statistics are kept out of the publication table."))
            )
          },
          div(
            class = "gtw-run-bar",
            uiOutput("reg_preflight"),
            div(
              class = "gtx-run-group",
              actionButton("run_uni", "Run univariate", class = "btn-primary"),
              actionButton("run_multi", "Run multivariable", class = "btn-primary")
            )
          ),
          if (!gtw_components_enabled) tagList(
            if (!gtx_addin_mode) tags$hr(),
            gtw_details(
              "Stratified analysis",
              gtx_reg_strata_controls()
            )
          )
        )
      ),
      column(
        8,
        class = "gtw-results gtw-reg-results",
        if (gtw_components_enabled) gtw_result_toolbar() else gtw_focus_toggle(),
        tabsetPanel(
          id = "reg_output_tab",
          tabPanel("Univariate", div(class = "gtx-card", tagListDownload("uni", table = TRUE), gtx_table_output("uni_table")), div(class = "gtw-code-strip", gtx_code_panel("Code", "uni_code"))),
          tabPanel("Multivariable", div(class = "gtx-card", tagListDownload("multi", table = TRUE), gtx_table_output("multi_table")), div(class = "gtw-code-strip", gtx_code_panel("Code", "multi_code"))),
          tabPanel("Stratified", div(class = "gtx-card", tagListDownload("strat", table = TRUE), gtx_table_output("strat_table")), div(class = "gtw-code-strip", gtx_code_panel("Code", "strat_code"))),
          tabPanel("Model Stats", div(class = "gtx-card", gtx_data_output("reg_stats")))
        )
      )
    )
  ),

  if (gta_shell_enabled) tabPanel("Special Models", gta_more_workspace()) else tabPanel(
    "Special Models",
    div(
      class = "gtx-card gtx-tool-menu",
      h3("Choose a study design"),
      gtx_special_mode_input()
    ),
    fluidRow(
      class = "gtw-workbench-row",
      column(
        4,
        class = "gtw-settings",
        div(
          class = "gtx-card gtx-side",
          uiOutput("special_intro"),
          uiOutput("special_inputs"),
          uiOutput("special_reference_inputs"),
          gtx_special_show_ref_input(),
          gtx_special_model_stats_input(),
          uiOutput("special_preflight"),
          actionButton("run_special", "Run selected model", class = "btn-primary")
        )
      ),
      column(
        8,
        class = "gtw-results",
        gtw_focus_toggle(),
        tabsetPanel(
          id = "special_output_tab",
          tabPanel(
            "Exposure Models",
            div(
              class = "gtx-card",
              div(class = "gtx-help", "Separate exposure models are retained here when Single multivariable model is off."),
              tagListDownload("special_exposure", table = TRUE),
              gtx_table_output("special_exposure_table")
            )
          ),
          tabPanel(
            "Multivariable Model",
            div(
              class = "gtx-card",
              div(class = "gtx-help", "The joint model is retained here. Clustered models always use this result slot."),
              tagListDownload("special_multi", table = TRUE),
              gtx_table_output("special_multi_table")
            )
          ),
          tabPanel("Model Stats", uiOutput("special_stats_panel")),
          tabPanel(
            "Assumption Check",
            uiOutput("special_assumptions_panel")
          ),
          tabPanel("Code", gtx_code_panel("Code", "special_code"))
        )
      )
    )
  ),

  if (gta_shell_enabled) tabPanel("Trials", gta_trials_workspace()),

  if (gta_shell_enabled) tabPanel("Survival", gta_survival_workspace()) else tabPanel(
    "Survival",
    div(
      class = "gtx-card gtx-tool-menu",
      h3("Choose a survival model"),
      gtx_surv_mode_input()
    ),
    fluidRow(
      class = "gtw-workbench-row",
      column(
        4,
        class = "gtw-settings",
        div(
          class = "gtx-card gtx-side",
          uiOutput("surv_mode_intro"),
          gtw_about(div(class = "gtx-note", "Choose Cox for hazard ratios, parametric survival for time ratios, or competing risks when another event can prevent the event of interest.")),
          uiOutput("surv_inputs"),
          gtx_surv_show_ref_input(),
          gtx_surv_multivariable_input(),
          uiOutput("surv_preflight"),
          conditionalPanel(
            "input.surv_mode == 'cox'",
            actionButton("run_cox", "Run Cox regression", class = "btn-primary")
          ),
          conditionalPanel(
            "input.surv_mode == 'survreg'",
            actionButton("run_survreg", "Run parametric survival regression", class = "btn-primary")
          ),
          conditionalPanel(
            "input.surv_mode == 'competing'",
            actionButton("run_competing", "Run Fine-Gray regression", class = "btn-primary")
          ),
          conditionalPanel(
            "input.surv_mode != 'competing'",
            tags$hr(),
            h4("Kaplan-Meier curve"),
            div(class = "gtx-help", "This observed survival view is available for Cox and parametric workflows."),
            uiOutput("km_group_input"),
            gtx_km_plot_option_inputs(),
            tags$details(
              tags$summary("Additional KM options"),
              gtx_km_break_input()
            ),
            actionButton("run_km", "Draw KM plot", class = "btn-primary")
          ),
          conditionalPanel(
            "input.surv_mode == 'competing'",
            tags$hr(),
            h4("Cumulative incidence"),
            div(class = "gtx-help", "Shows the absolute probability of the selected event while retaining competing events."),
            gtx_cuminc_by_input(),
            gtx_cuminc_plot_option_inputs(),
            actionButton("run_cuminc", "Draw cumulative incidence", class = "btn-primary")
          )
        )
      ),
      column(
        8,
        class = "gtw-results",
        gtw_focus_toggle(),
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
            ),
            conditionalPanel(
              "input.surv_mode == 'competing'",
              div(class = "gtx-card", tagListDownload("competing", table = TRUE), gtx_table_output("competing_table"))
            )
          ),
          tabPanel("KM Plot", div(class = "gtx-card", tagListDownload("km", table = FALSE, plot = TRUE), plotOutput("km_plot", height = "650px"))),
          tabPanel("Cumulative Incidence", div(class = "gtx-card", tagListDownload("cuminc", table = FALSE, plot = TRUE), plotOutput("cuminc_plot", height = "650px"))),
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
          "Modify table" = "modify",
          "Regression plot" = "plot",
          "Forest plot" = "forest",
          "Model fit" = "fit"
        ),
        selected = "merge",
        inline = TRUE
      )
    ),
    fluidRow(
      class = "gtw-workbench-row",
      column(
        4,
        class = "gtw-settings",
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
            "input.visual_tool == 'modify'",
            h3("Modify Table"),
            div(class = "gtx-help", "Refine a completed table without changing the original result. Use one mapping per line for labels."),
            uiOutput("modify_source_controls"),
            textAreaInput("modify_variable_labels", "Variable labels", placeholder = "age = Maternal age\nsmoke = Smoking during pregnancy", rows = 3),
            gtx_inline_help("One `internal name = display label` per line."),
            div(class = "gtx-note", strong("Level-label format: "), "use the internal variable name and the exact current level shown in the table. For example: `smoke: Yes = Smoker` or `race: Other = Other ethnic group`. Do not use the display label on the left of `:`."),
            textAreaInput("modify_level_labels", "Level labels", placeholder = "smoke: Yes = Smoker", rows = 3),
            gtx_inline_help("One `variable: current level = new level` per line. The original level is case-sensitive."),
            textAreaInput("modify_header_labels", "Header labels", placeholder = "estimate = Adjusted OR\np.value = P value", rows = 3),
            gtx_inline_help("Use a visible header name or an alias such as `estimate`, `p.value`, or `N`."),
            textInput("modify_caption", "Caption", placeholder = "Optional table title"),
            checkboxInput("modify_bold_labels", "Bold characteristic labels", TRUE),
            checkboxInput("modify_bold_levels", "Bold category levels", FALSE),
            checkboxInput("modify_italic_labels", "Italicize characteristic labels", FALSE),
            checkboxInput("modify_italic_levels", "Italicize category levels", FALSE),
            checkboxInput("modify_remove_n", "Remove displayed N columns", FALSE),
            checkboxInput("modify_remove_n_obs", "Remove complete-case N note", FALSE),
            checkboxInput("modify_remove_abbreviations", "Remove abbreviations note", FALSE),
            checkboxInput("modify_remove_adjustment_note", "Remove automatic adjustment note", FALSE),
            textAreaInput("modify_caveat", "Additional note", placeholder = "Optional manuscript note", rows = 2),
            actionButton("run_modify_table", "Apply table changes", class = "btn-primary")
          ),
          conditionalPanel(
            "input.visual_tool == 'plot'",
            h3("Regression Plot"),
            uiOutput("plot_content_controls"),
            checkboxInput("plot_log_x", "Use a log x-axis", FALSE),
            gtx_inline_help("Linear scale is the default. Use log scale only when you need proportional spacing around the null value of 1."),
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
            gtx_inline_help("If x-axis labels overlap, enter fewer ticks such as 0.5, 1, 2, 4. Every tick must lie within x limits; use x limits as well when the interval range is very wide."),
            div(
              class = "gtx-help",
              "Combine descriptive, crude, and adjusted results when available. For overlapping labels, use fewer ticks, wider x limits, or a wider CI column."
            ),
            actionButton("run_forest", "Preview forest plot", class = "btn-primary")
          ),
          conditionalPanel(
            "input.visual_tool == 'fit'",
            h3("Model Fit"),
            div(class = "gtx-help", "Choose Auto for the recommended diagnostic panel. Calibration is most useful for multivariable binary-outcome models; bins only affects calibration."),
            uiOutput("fit_source_controls"),
            selectInput("fit_type", "Type", choices = c("Auto (recommended)" = "auto", "All diagnostics" = "all", "Residuals vs fitted" = "residual", "Normal Q-Q" = "qq", "Scale-location" = "scale_location", "Cook's distance" = "cooks", "Observed vs predicted" = "observed_predicted", "Calibration" = "calibration"), selected = "auto"),
            textInput("fit_model_name", "Model name for univariate objects", placeholder = "Optional, e.g. smoke"),
            conditionalPanel("input.fit_type == 'calibration'", numericInput("fit_bins", "Calibration bins", value = 10, min = 2)),
            numericInput("fit_base_size", "Base font size", value = 13, min = 6, step = 1),
            actionButton("run_fit_plot", "Draw model fit", class = "btn-primary")
          ),
          tags$hr(),
          gtw_about(
            summary = "About previews and downloads",
            div(
              class = "gtx-note",
              "Browser outputs are previews. Downloads are generated in temporary app files, then your browser chooses where to save them; the app never writes into a user folder itself. Copy the generated plot code into RStudio for final publication sizing."
            )
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
        class = "gtw-results",
        gtw_focus_toggle(),
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
          tabPanel(
            "Modified Table",
            div(class = "gtx-card", tagListDownload("modified", table = TRUE), gtx_table_output("modified_table"))
          ),
          tabPanel("plot_reg", div(class = "gtx-card", tagListDownload("plotreg", table = FALSE, plot = TRUE), plotOutput("plotreg_plot", height = "620px"))),
          tabPanel("forest_reg", div(class = "gtx-card", tagListDownload("forest", table = FALSE, plot = TRUE), plotOutput("forest_plot", height = "760px"))),
          tabPanel("Model fit", div(class = "gtx-card", tagListDownload("fitplot", table = FALSE, plot = TRUE), plotOutput("fit_plot", height = "680px"))),
          tabPanel("Code", gtx_code_panel("Code", "visual_code"))
        )
      )
    )
  ),

  tabPanel(
    "Advanced",
    if (gtx_addin_mode) {
      div(
        class = "gtw-tool-about",
        gtw_about(
          summary = "About the model building and diagnostic tools",
          p("Start with model selection when you want the app to build a candidate path from an outcome and predictor set. Use model comparison after fitting two or more models in the Regression or Survival tabs."),
          div(
            class = "gtx-step-row",
            span(class = "gtx-step", strong("1  Select"), " candidate predictors"),
            span(class = "gtx-step", strong("2  Fit"), " candidate models"),
            span(class = "gtx-step", strong("3  Compare"), " saved models"),
            span(class = "gtx-step", strong("4  Check"), " assumptions")
          )
        )
      )
    } else {
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
      )
    },
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
      class = "gtw-workbench-row",
      column(
        4,
        class = "gtw-settings",
        div(
          class = "gtx-card gtx-side",
          uiOutput("advanced_config")
        )
      ),
      column(
        8,
        class = "gtw-results",
        gtw_focus_toggle(),
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

  # In the shared shell Mediation is a method of the More models workspace.
  if (!gta_shell_enabled) tabPanel(
    "Mediation",
    fluidRow(
      class = "gtw-workbench-row",
      column(
        4,
        class = "gtw-settings",
        div(
          class = "gtx-card gtx-side",
          h3("Causal Mediation"),
          gtw_caution(
            summary = tagList(
              strong("Screening aid only: "),
              "a causal interpretation assumes no unmeasured exposure-mediator, mediator-outcome, or exposure-outcome confounding."
            ),
            details = div(class = "gtx-note", "Specify the causal question before running this analysis. The result is a screening aid and depends on no unmeasured exposure-mediator, mediator-outcome, or exposure-outcome confounding assumptions.")
          ),
          uiOutput("med_inputs"),
          gtx_med_approach_input(),
          gtx_med_option_inputs(),
          actionButton("run_mediation", "Run mediation analysis", class = "btn-primary")
        )
      ),
      column(
        8,
        class = "gtw-results",
        gtw_focus_toggle(),
        tabsetPanel(
          tabPanel("Table", div(class = "gtx-card", tagListDownload("med", table = TRUE), gtx_table_output("med_table"))),
          tabPanel("Diagram", div(class = "gtx-card", tagListDownload("medplot", table = FALSE, plot = TRUE), plotOutput("med_plot", height = "620px"))),
          tabPanel("Code", gtx_code_panel("Code", "med_code"))
        )
      )
    )
  ),

  tabPanel(
    "Session Code",
    fluidRow(
      column(
        12,
        if (gtx_addin_mode) {
          div(
            class = "gtx-card gtw-session",
            div(
              class = "gtw-session-head",
              h3("Session Code"),
              span(class = "gtw-session-sub", "Complete reproducible analysis"),
              downloadButton("download_session_script", "Download complete R script", class = "btn-primary")
            ),
            div(class = "gtx-help", "This script is updated automatically from the completed steps in this session. Copy one command from any Code panel, or copy/download the complete workflow here."),
            gtx_code_panel("Complete workflow", "full_workflow_code"),
            gtx_session_save_ui()
          )
        } else {
          div(
            class = "gtx-card",
            h3("Session Code"),
            div(class = "gtx-help", "This script is updated automatically from the completed steps in this session. Copy one command from any Code panel, or copy/download the complete workflow here."),
            downloadButton("download_session_script", "Download complete R script", class = "btn-primary"),
            tags$hr(),
            gtx_code_panel("Complete workflow", "full_workflow_code"),
            tags$hr(),
            gtx_session_save_ui()
          )
        }
      )
    )
  ),

  tabPanel(
    "Help",
    fluidRow(
      column(
        12,
        div(
          class = "gtx-card gtw-help-page",
          h2("GT Regression"),
          p("This app is a menu-driven companion to the R package. It is designed for beginners who want publication-ready tables and plots, while always showing reproducible R code."),
          tags$ol(
            tags$li("Load a dataset."),
            tags$li("Create descriptive tables."),
            tags$li("Run univariable analysis to screen variables."),
            tags$li("Reselect the variables you want, then run multivariable, Cox, or survival models."),
            tags$li("Use confounder, interaction, model comparison, diagnostics, or mediation tools when needed."),
            tags$li("Use the Code tabs to copy the exact R commands into RStudio for reproducible saving and editing.")
          ),
          p(if (gtx_addin_mode) {
            "Use the Close button in the top-right corner to stop the Shiny session cleanly and return to RStudio."
          } else {
            "Use the Close app button in the bottom-right corner to stop the Shiny session cleanly."
          }),
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
    data_origin = NULL, data_reference = NULL,
    desc = NULL, uni = NULL, multi = NULL, strat = NULL, special = NULL,
    special_exposure = NULL, special_multi = NULL,
    special_exposure_code = "", special_multi_code = "",
    cox = NULL, survreg = NULL, competing = NULL,
    cox_exposure = NULL, cox_multi = NULL,
    survreg_exposure = NULL, survreg_multi = NULL,
    km = NULL, cuminc = NULL, mediation = NULL,
    plotreg = NULL, forest = NULL, fitplot = NULL, merged = NULL, modified = NULL,
    trial = NULL, trial_code = "",
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

  # Selection continuity: each rebuilt input group remembers the imported
  # dataset (and any other context, e.g. the study design) it was last built
  # for. Within the same import, selections are kept where still valid.
  selection_memory <- new.env(parent = emptyenv())
  selection_context <- function(id, extra = "") {
    key <- paste(isolate(data_prep$source_id()), paste(extra, collapse = "|"))
    renames <- isolate(data_prep$renames())
    previous <- selection_memory[[id]]
    selection_memory[[id]] <- list(key = key, n = length(renames))
    if (is.null(previous) || !identical(previous$key, key)) return(list(keep = FALSE, events = list()))
    events <- if (length(renames) > previous$n) renames[seq.int(previous$n + 1L, length(renames))] else list()
    list(keep = TRUE, events = events)
  }
  # A variable cleared because a change removed it is remembered with the
  # number of changes at that point; undoing past that change (Undo or Reset)
  # selects it again when it is back. Redo clears it again in the same way.
  keep_sel <- function(ctx, name, choices, multiple = FALSE) {
    slot <- paste0("dropped:", name)
    if (!isTRUE(ctx$keep)) {
      selection_memory[[slot]] <- NULL
      return(NULL)
    }
    value <- isolate(input[[name]])
    depth <- isolate(data_prep$changes())
    kept <- gtx_keep_selection(value, choices, ctx$events)
    mapped <- gtx_map_names(as.character(value %||% character(0)), ctx$events)
    dropped <- setdiff(mapped[!is.na(mapped) & nzchar(mapped)], choices)
    record <- selection_memory[[slot]]
    if (!is.null(record)) {
      record$values <- gtx_map_names(record$values, ctx$events)
      if (depth < record$depth) {
        back <- record$values[record$values %in% choices]
        if (length(back)) {
          kept <- if (multiple) unique(c(kept, back)) else if (is.null(kept)) back[[1]] else kept
        }
        record <- NULL
      }
    }
    if (length(dropped)) record <- list(values = unique(c(record$values, dropped)), depth = depth)
    selection_memory[[slot]] <- record
    kept
  }
  keep_value <- function(ctx, value, choices = NULL) {
    if (!isTRUE(ctx$keep) || is.null(value)) return(NULL)
    if (!is.null(choices) && !all(value %in% choices)) return(NULL)
    value
  }
  # Reference-category inputs are indexed; remember which variable each index
  # held so a kept variable keeps its chosen reference level.
  keep_references <- function(ctx, prefix, candidates) {
    previous <- selection_memory[[paste0(prefix, "names")]] %||% character(0)
    old <- stats::setNames(lapply(seq_along(previous), function(i) isolate(input[[paste0(prefix, i)]])), gtx_map_names(previous, ctx$events))
    selection_memory[[paste0(prefix, "names")]] <- names(candidates)
    lapply(names(candidates), function(variable) {
      value <- if (isTRUE(ctx$keep)) old[[variable]] else NULL
      if (!is.null(value) && value %in% candidates[[variable]]) value else candidates[[variable]][[1]]
    })
  }

  # Variable-role checks shared by every workspace. A variable chosen for two
  # roles is explained in a dialog; when it can be fixed by removing it from a
  # list, one click removes it and runs the analysis again.
  role_fix <- reactiveVal(NULL)

  check_roles <- function(roles = list(), lists = list(), run_button = NULL, context = "this analysis") {
    clashes <- gtx_role_clashes(roles)
    conflicts <- gtx_role_conflicts(roles, lists)
    if (!length(clashes) && !nrow(conflicts)) return(TRUE)
    fixable <- !length(clashes) && nrow(conflicts) > 0
    role_fix(if (fixable) list(conflicts = conflicts, run = run_button) else NULL)
    fix_label <- if (nrow(conflicts) == 1) {
      paste0("Remove '", conflicts$variable, "' from ", conflicts$label, " and run")
    } else {
      "Remove these selections and run"
    }
    showModal(modalDialog(
      title = "Check the variable roles",
      easyClose = TRUE,
      p(paste0("Each variable can have only one role in ", context, ".")),
      tags$ul(lapply(c(clashes, gtx_role_conflict_text(conflicts)), tags$li)),
      if (fixable) {
        p("Remove the extra selection and the analysis will run with the remaining variables, or cancel to change the selections yourself.")
      } else {
        p("Choose a different variable for one of these roles, then run the analysis again.")
      },
      footer = tagList(
        modalButton(if (fixable) "Cancel" else "OK"),
        if (fixable) actionButton("gtx_role_fix", fix_label, class = "btn-primary")
      )
    ))
    FALSE
  }

  observeEvent(input$gtx_role_fix, {
    fix <- role_fix()
    removeModal()
    req(fix)
    role_fix(NULL)
    for (id in unique(fix$conflicts$id)) {
      drop <- fix$conflicts$variable[fix$conflicts$id == id]
      updateCheckboxGroupInput(session, id, selected = setdiff(input[[id]] %||% character(0), drop))
    }
    removed <- paste0("'", fix$conflicts$variable, "' from ", fix$conflicts$label, collapse = "; ")
    showNotification(paste0("Removed ", removed, "."), type = "message", duration = 6)
    if (!is.null(fix$run)) session$sendCustomMessage("gtx-click", fix$run)
  }, ignoreInit = TRUE)

  # Variables that hold a single role (outcome, time, event, group...) cannot
  # be ticked in that workspace's lists; one already ticked is removed with a
  # short explanation when it takes the role.
  adv_roles <- function() {
    if (isTRUE(input$adv_approach %in% c("cox", "survreg"))) {
      list(Time = input$adv_time, Event = input$adv_event)
    } else {
      list(Outcome = input$adv_outcome)
    }
  }
  role_lock_specs <- list(
    list(lists = c(desc_exposures = "Variables to summarise"), roles = function() list(`Group by` = input$desc_by)),
    list(lists = c(reg_exposures = "Exposures", reg_adjust = "Adjust for"), roles = function() list(Outcome = input$reg_outcome)),
    list(lists = c(special_exposures = "Exposures", special_adjust = "Adjust for"), roles = function() list(
      Outcome = input$special_outcome,
      `Grouping variable` = if (identical(input$special_mode, "multilevel")) input$special_group,
      `Matched-set identifier` = if (identical(input$special_mode, "matched")) input$special_matched_set
    )),
    list(lists = c(surv_exposures = "Exposures", surv_adjust = "Adjust for"), roles = function() list(Time = input$surv_time, Event = input$surv_event)),
    list(lists = c(med_covariates = "Covariates"), roles = function() list(Exposure = input$med_exposure, Mediator = input$med_mediator, Outcome = input$med_outcome)),
    list(lists = c(trial_adjust = "Adjust for"), roles = function() list(Outcome = input$trial_outcome, `Treatment group` = input$trial_exposure)),
    list(lists = c(adv_exposures = "Candidate predictors", adv_covariates = "Covariates", conf_exposures = "Exposures of interest"), roles = adv_roles)
  )
  lapply(role_lock_specs, function(spec) {
    observe({
      roles <- spec$roles()
      roles <- roles[vapply(roles, gtx_has_text, logical(1))]
      values <- vapply(roles, function(x) as.character(x[1]), character(1), USE.NAMES = FALSE)
      notes <- names(roles)
      for (id in names(spec$lists)) {
        session$sendCustomMessage("gtx-lock", list(id = id, values = as.list(values), notes = as.list(notes)))
        current <- isolate(input[[id]]) %||% character(0)
        clash <- intersect(current, values)
        if (length(clash)) {
          updateCheckboxGroupInput(session, id, selected = setdiff(current, clash))
          showNotification(
            paste0("'", clash, "' is now the ", tolower(notes[match(clash, values)]), ", so it was removed from ", spec$lists[[id]], ".", collapse = " "),
            type = "message", duration = 6
          )
        }
      }
    })
  })

  # Results remember the selections they were made with; a result whose
  # selections have since changed is marked as out of date.
  result_input_ids <- function(key) {
    ids <- names(input)
    by_prefix <- function(prefixes, drop = character(0)) {
      keep <- ids[Reduce(`|`, lapply(prefixes, function(p) startsWith(ids, p)))]
      keep <- keep[!grepl("_tab$|_all$|_clear$|_fix$|_preflight$", keep)]
      sort(setdiff(keep, drop))
    }
    reg_common <- c("reg_outcome", "reg_exposures", "reg_approach", "reg_interaction", "reg_show_ref", grep("^reg_reference_", ids, value = TRUE))
    switch(
      key,
      desc = by_prefix("desc_"),
      uni = sort(reg_common),
      multi = sort(c(reg_common, "reg_adjust")),
      strat = sort(c(reg_common, "reg_adjust", "reg_stratifier", "reg_strata_multi")),
      special_exposure = , special_multi = by_prefix("special_"),
      cox_exposure = , cox_multi = by_prefix(c("surv_", "cox_"), c("surv_mode", "surv_dist")),
      survreg_exposure = , survreg_multi = by_prefix("surv_", "surv_mode"),
      competing = by_prefix(c("surv_", "competing_"), c("surv_mode", "surv_dist", "surv_stratifier")),
      trial = by_prefix("trial_"),
      mediation = by_prefix("med_"),
      character(0)
    )
  }
  data_stamp <- reactive({
    d <- analysis_data()
    if (is.null(d)) return(NULL)
    if (exists("hash", envir = asNamespace("rlang"), inherits = FALSE)) rlang::hash(d) else paste(dim(d), collapse = "x")
  })
  input_signature <- function(key) {
    ids <- result_input_ids(key)
    list(values = lapply(stats::setNames(ids, ids), function(id) input[[id]]), data = data_stamp())
  }
  stale_keys <- c("desc", "uni", "multi", "strat", "special_exposure", "special_multi",
                  "cox_exposure", "cox_multi", "survreg_exposure", "survreg_multi",
                  "competing", "trial", "mediation")
  result_stamps <- reactiveValues()
  result_stale <- reactiveValues()
  lapply(stale_keys, function(key) {
    observeEvent(rv[[key]], {
      result_stamps[[key]] <- if (is.null(rv[[key]])) NULL else isolate(input_signature(key))
    }, ignoreNULL = FALSE)
    observe({
      stamp <- result_stamps[[key]]
      now_stale <- !is.null(stamp) && !is.null(rv[[key]]) && !identical(stamp, input_signature(key))
      if (!identical(isolate(result_stale[[key]]), now_stale)) result_stale[[key]] <- now_stale
    })
  })
  is_stale <- function(key) function() isTRUE(result_stale[[key]])

  # ---- Results kept in this session -----------------------------------------
  # Every table that is produced is kept, newest last, so earlier runs stay
  # available to Merge, Modify, plots and forest plots. The latest result of
  # each kind keeps its usual name (for example "Adjusted" / multi_result);
  # earlier ones are labelled with their run time. To keep memory in check the
  # list holds at most `history_max` results, and when fitted models exceed
  # `history_budget` the oldest results keep their table but release their
  # models (diagnostics then need a re-run).
  history_max <- getOption("gtregression.app_history_max", if (gtx_addin_mode) 30L else 15L)
  history_budget <- getOption("gtregression.app_history_mb", if (gtx_addin_mode) 500 else 200) * 1e6
  history_bases <- c(
    desc = "desc_result", uni = "uni_result", multi = "multi_result", strat = "stratified_result",
    special_exposure = "special_exposure_result", special_multi = "special_multi_result",
    cox_exposure = "cox_exposure_result", cox_multi = "cox_multi_result",
    survreg_exposure = "survreg_exposure_result", survreg_multi = "survreg_multi_result",
    competing = "competing_result", trial = "rd_result", mediation = "mediation_result"
  )
  history_short <- c(
    desc = "Descriptive", uni = "Crude", multi = "Adjusted", strat = "Stratified",
    special_exposure = "Special exposure models", special_multi = "Special multivariable model",
    cox_exposure = "Cox exposure models", cox_multi = "Cox multivariable model",
    survreg_exposure = "Parametric exposure models", survreg_multi = "Parametric multivariable model",
    competing = "Fine-Gray competing-risk model", trial = "Risk difference", mediation = "Mediation"
  )
  history_headers <- c(
    desc = "# 3. Descriptive table", uni = "# 4. Univariable regression",
    multi = "# 5. Multivariable regression", strat = "# 5. Stratified regression",
    special_exposure = "# 5a. Special exposure models", special_multi = "# 5b. Special multivariable model",
    cox_exposure = "# 6. Survival analysis: Cox exposure models", cox_multi = "# 6. Survival analysis: Cox multivariable model",
    survreg_exposure = "# 6. Survival analysis: parametric exposure models",
    survreg_multi = "# 6. Survival analysis: parametric multivariable model",
    competing = "# 6a. Competing-risk analysis", trial = "# 6b. Risk difference (Trials)",
    mediation = "# 9. Causal mediation"
  )
  history_mergeable <- setdiff(names(history_bases), c("trial", "mediation"))
  history_store <- reactiveVal(list())
  history_current <- reactiveValues()
  history_counter <- new.env(parent = emptyenv())
  history_counter$id <- 0L
  history_counter$runs <- list()

  # Code that recreates a result, written from the selections used to run it.
  history_call_code <- function(key) {
    base <- history_bases[[key]]
    reference_code <- function() {
      lines <- gtx_reference_code(selected_references(), reference_candidates(), data_name = "analysis_data")
      if (length(lines)) paste0(paste(lines, collapse = "\n"), "\n") else ""
    }
    survival_args <- function(multivariable) list(
      data = "analysis_data",
      time = gtx_null_code(input$surv_time),
      event = gtx_null_code(input$surv_event),
      exposures = gtx_vec_code(input$surv_exposures %||% character(0)),
      adjust_for = gtx_nonempty_vec_code(input$surv_adjust %||% character(0)),
      multivariable = gtx_bool_code(multivariable),
      stratifier = gtx_null_code(input$surv_stratifier)
    )
    switch(
      key,
      desc = gtx_code_assign(base, "descriptive_table", list(
        data = "analysis_data",
        exposures = gtx_vec_code(input$desc_exposures),
        by = gtx_null_code(input$desc_by),
        percent = shQuote(input$desc_percent),
        show_overall = shQuote(input$desc_overall),
        show_missing = shQuote(input$desc_missing),
        show_dichotomous = shQuote(input$desc_dich),
        statistic = gtx_statistic_code(input$desc_statistic),
        format = shQuote("flextable")
      )),
      uni = paste0(reference_code(), gtx_code_assign(base, "uni_reg", list(
        data = "analysis_data",
        outcome = gtx_null_code(input$reg_outcome),
        exposures = gtx_vec_code(input$reg_exposures),
        approach = shQuote(input$reg_approach),
        interaction = if (gtx_has_text(input$reg_interaction)) shQuote(input$reg_interaction) else NULL,
        show_ref = gtx_bool_code(input$reg_show_ref),
        model_stats = gtx_bool_code(input$reg_model_stats %||% FALSE),
        format = shQuote("flextable")
      ))),
      multi = paste0(reference_code(), gtx_code_assign(base, "multi_reg", list(
        data = "analysis_data",
        outcome = gtx_null_code(input$reg_outcome),
        exposures = gtx_vec_code(input$reg_exposures),
        adjust_for = gtx_nonempty_vec_code(input$reg_adjust %||% character(0)),
        approach = shQuote(input$reg_approach),
        interaction = if (gtx_has_text(input$reg_interaction)) shQuote(input$reg_interaction) else NULL,
        show_ref = gtx_bool_code(input$reg_show_ref),
        model_stats = gtx_bool_code(input$reg_model_stats %||% FALSE),
        format = shQuote("flextable")
      ))),
      strat = paste0(reference_code(), gtx_code_assign(
        base,
        if (isTRUE(input$reg_strata_multi)) "stratified_multi_reg" else "stratified_uni_reg",
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
      )),
      special_exposure = rv$special_exposure_code,
      special_multi = rv$special_multi_code,
      cox_exposure = , cox_multi = gtx_code_assign(base, "cox_reg", c(survival_args(key == "cox_multi"), list(
        show_ref = gtx_bool_code(input$surv_show_ref),
        model_stats = gtx_bool_code(input$reg_model_stats %||% TRUE),
        format = shQuote("flextable")
      ))),
      survreg_exposure = , survreg_multi = gtx_code_assign(base, "surv_reg", c(survival_args(key == "survreg_multi"), list(
        distribution = shQuote(input$surv_dist %||% "weibull"),
        show_ref = gtx_bool_code(input$surv_show_ref),
        model_stats = gtx_bool_code(input$reg_model_stats %||% TRUE),
        format = shQuote("flextable")
      ))),
      competing = gtx_code_assign(base, "competing_risk_reg", list(
        data = "analysis_data",
        time = gtx_null_code(input$surv_time),
        event = gtx_null_code(input$surv_event),
        event_of_interest = gtx_null_code(input$competing_event_of_interest),
        censor_code = gtx_null_code(input$competing_censor_code),
        exposures = gtx_vec_code(input$surv_exposures %||% character(0)),
        adjust_for = if (isTRUE(input$surv_multivariable)) NULL else {
          gtx_nonempty_vec_code(input$surv_adjust %||% character(0))
        },
        multivariable = gtx_bool_code(input$surv_multivariable),
        show_ref = gtx_bool_code(input$surv_show_ref),
        model_stats = gtx_bool_code(input$reg_model_stats %||% TRUE),
        format = shQuote("flextable")
      )),
      trial = rv$trial_code,
      mediation = paste(
        gtx_code_assign(base, "mediation_analysis", list(
          data = "analysis_data",
          exposure = gtx_null_code(input$med_exposure),
          mediator = gtx_null_code(input$med_mediator),
          outcome = gtx_null_code(input$med_outcome),
          covariates = gtx_nonempty_vec_code(input$med_covariates %||% character(0)),
          outcome_approach = shQuote(input$med_approach),
          sims = as.character(input$med_sims),
          seed = as.character(input$med_seed),
          format = shQuote("flextable")
        )),
        paste0("mediation_plot <- plot_mediation(", base, ")"),
        sep = "\n"
      ),
      ""
    )
  }

  # Name used for an entry in generated code and in the lists of tables.
  history_entry_name <- function(entry) {
    current <- isTRUE(identical(isolate(history_current[[entry$key]]), entry$id))
    if (current) history_bases[[entry$key]] else paste0(history_bases[[entry$key]], "_", entry$run)
  }
  history_entry_label <- function(entry, current) {
    if (current) return(history_short[[entry$key]])
    paste0(history_short[[entry$key]], " (", entry$time, if (entry$run > 1) paste0(", run ", entry$run) else "", ")")
  }

  history_enforce_limits <- function(entries) {
    current_ids <- unlist(lapply(names(history_bases), function(k) isolate(history_current[[k]])))
    removed <- character(0)
    while (length(entries) > history_max) {
      older <- which(!vapply(entries, function(e) e$id %in% current_ids, logical(1)))
      if (!length(older)) break
      removed <- c(removed, history_entry_label(entries[[older[1]]], FALSE))
      entries[[older[1]]] <- NULL
    }
    compacted <- 0L
    model_bytes <- function() sum(vapply(entries, function(e) if (isTRUE(e$compact)) 0 else e$bytes, numeric(1)))
    for (i in seq_along(entries)) {
      if (model_bytes() <= history_budget) break
      e <- entries[[i]]
      if (e$id %in% current_ids || isTRUE(e$compact) || !e$key %in% history_mergeable) next
      e$result <- gtx_compact_result(e$result)
      e$compact <- TRUE
      entries[[i]] <- e
      compacted <- compacted + 1L
    }
    if (length(removed)) {
      showNotification(
        paste0("To keep memory in check, the oldest result", if (length(removed) > 1) "s were" else " was",
               " removed from this session's list: ", paste(removed, collapse = "; "), "."),
        type = "message", duration = 8
      )
    }
    if (compacted) {
      showNotification(
        paste0(compacted, " older result", if (compacted > 1) "s now keep" else " now keeps",
               " the table only (merge, modify, plots and export still work; model checks need a re-run)."),
        type = "message", duration = 8
      )
    }
    entries
  }

  lapply(names(history_bases), function(key) {
    observeEvent(rv[[key]], {
      result <- rv[[key]]
      if (is.null(result)) {
        history_current[[key]] <- NULL
        return(invisible(NULL))
      }
      history_counter$id <- history_counter$id + 1L
      run <- (history_counter$runs[[key]] %||% 0L) + 1L
      history_counter$runs[[key]] <- run
      entry <- list(
        id = history_counter$id, key = key, run = run,
        time = format(Sys.time(), "%H:%M"),
        code = isolate(tryCatch(history_call_code(key), error = function(e) "")),
        data = isolate(data_stamp()),
        result = result,
        bytes = gtx_result_bytes(result),
        compact = FALSE
      )
      history_current[[key]] <- entry$id
      entries <- c(isolate(history_store()), list(entry))
      history_store(history_enforce_limits(entries))
    }, ignoreNULL = FALSE, ignoreInit = TRUE)
  })

  # Tables for Merge and Modify: the latest of each kind first (with their
  # usual names), then earlier runs, newest first.
  history_tables <- function(include = history_mergeable) {
    entries <- Filter(function(e) e$key %in% include, history_store())
    current <- Filter(function(e) identical(history_current[[e$key]], e$id), entries)
    older <- rev(Filter(function(e) !identical(history_current[[e$key]], e$id), entries))
    ordered <- c(current[order(match(vapply(current, `[[`, "", "key"), names(history_bases)))], older)
    out <- list()
    for (e in ordered) {
      is_current <- identical(history_current[[e$key]], e$id)
      label <- history_entry_label(e, is_current)
      if (!identical(e$data, data_stamp())) label <- paste0(label, " [earlier data]")
      name <- history_entry_name(e)
      result <- e$result
      if (inherits(result, "multinomial_reg")) {
        for (category in names(result$per_outcome)) {
          tables_label <- paste0(label, ": ", category, " vs ", result$outcome_reference)
          out[[tables_label]] <- list(result = result$per_outcome[[category]],
                                      code = paste0(name, "$per_outcome[[", shQuote(category), "]]"),
                                      entry = e, current = is_current)
        }
      } else {
        out[[label]] <- list(result = result, code = name, entry = e, current = is_current)
      }
    }
    out
  }

  output$history_summary <- renderUI({
    entries <- history_store()
    if (!length(entries)) {
      return(div(class = "gtx-help", "No results yet. Every table you run is listed here and stays available to Merge, Modify and plots."))
    }
    model_mb <- sum(vapply(entries, function(e) if (isTRUE(e$compact)) 0 else e$bytes, numeric(1))) / 1e6
    tagList(
      div(
        class = "gtx-help",
        sprintf("%d of up to %d results kept · fitted models use about %s MB of %d MB.",
                length(entries), history_max, format(round(model_mb, 1), nsmall = 1), as.integer(history_budget / 1e6))
      ),
      tags$ul(
        class = "gtx-history-list",
        lapply(rev(entries), function(e) {
          is_current <- identical(history_current[[e$key]], e$id)
          tags$li(
            strong(history_entry_label(e, is_current)),
            span(class = "gtx-history-meta", paste0(
              " · ", e$time,
              if (is_current) " · latest" else "",
              if (isTRUE(e$compact)) " · table only" else "",
              " · ", history_entry_name(e)
            ))
          )
        })
      ),
      actionButton("history_clear_older", "Remove earlier results", class = "btn-default btn-sm")
    )
  })

  observeEvent(input$history_clear_older, {
    entries <- history_store()
    keep <- Filter(function(e) identical(history_current[[e$key]], e$id), entries)
    removed <- length(entries) - length(keep)
    history_store(keep)
    showNotification(
      if (removed) paste0("Removed ", removed, " earlier result", if (removed > 1) "s", ". The latest result of each kind is kept.") else "There are no earlier results to remove.",
      type = "message"
    )
  }, ignoreInit = TRUE)

  require_analysis_data <- function() {
    data <- analysis_data()
    if (is.null(data)) {
      showNotification(
        "Load a dataset first. Original data is active automatically; use Data Prep only when you want prepared data.",
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
        message <- gtx_friendly_error(e)
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

  environment_datasets <- reactiveVal(
    if (gtx_addin_mode && is.environment(gtx_data_env)) {
      gtx_discover_data_frames(gtx_data_env)
    } else {
      gtx_discover_data_frames(new.env(parent = emptyenv()))
    }
  )

  output$environment_dataset_input <- renderUI({
    metadata <- environment_datasets()
    if (!nrow(metadata)) {
      return(div(
        class = "gtx-note",
        "No data frames found in the R environment. Create or import a dataset in R, or use one of the other data sources."
      ))
    }
    labels <- paste0(
      metadata$name,
      "     ",
      format(metadata$rows, big.mark = ",", trim = TRUE),
      " x ",
      metadata$columns
    )
    # Keep the current choice when the list is refreshed (D-4); on first
    # render there is no current choice and the default (first) applies.
    current <- isolate(input$environment_dataset_name)
    selectInput(
      "environment_dataset_name",
      "Dataset",
      choices = stats::setNames(metadata$name, labels),
      selected = if (length(current) && current %in% metadata$name) current else NULL
    )
  })

  # D-4: re-scan the R environment for data frames. Only the list of available
  # names changes; the loaded dataset and every analysis input are untouched.
  observeEvent(input$refresh_environment, {
    if (!gtx_addin_mode || !is.environment(gtx_data_env)) return(NULL)
    environment_datasets(gtx_discover_data_frames(gtx_data_env))
    count <- nrow(environment_datasets())
    showNotification(
      paste0("R environment checked: ", count, if (count == 1) " data frame" else " data frames", " available."),
      type = "message",
      duration = 4
    )
  }, ignoreInit = TRUE)

  observeEvent(input$load_environment_data, {
    if (!gtx_addin_mode || !is.environment(gtx_data_env)) return(NULL)
    name <- input$environment_dataset_name %||% ""
    data <- tryCatch(
      gtx_get_data_frame(gtx_data_env, name),
      error = function(e) {
        showNotification(gtx_friendly_error(e), type = "error", duration = 8)
        NULL
      }
    )
    if (is.null(data)) return(NULL)

    rv$data <- data
    rv$data_name <- name
    rv$data_origin <- "R Environment"
    rv$data_reference <- gtx_object_reference(name)
    rv$data_code <- paste0(
      "# Data source: existing R object ", rv$data_reference, "\n",
      "# This object was created outside GT Regression."
    )
    rv$last_message <- "Original data is active. Open Data Prep only if you want to make changes."
    showNotification("R-environment dataset loaded. Original data is ready for analysis.", type = "message")
  }, ignoreInit = TRUE)

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
    select_candidates(c(input$reg_outcome, input$reg_stratifier %||% "", input$reg_adjust %||% character(0)))
  }))
  wire_select_buttons("reg_adjust", reactive({
    select_candidates(c(input$reg_outcome, input$reg_stratifier %||% "", input$reg_exposures %||% character(0)))
  }))
  wire_select_buttons("special_exposures", reactive({
    select_candidates(c(
      input$special_outcome,
      input$special_group,
      input$special_matched_set,
      input$special_adjust %||% character(0)
    ))
  }))
  wire_select_buttons("special_adjust", reactive({
    select_candidates(c(
      input$special_outcome,
      input$special_group,
      input$special_matched_set,
      input$special_exposures %||% character(0)
    ))
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
  wire_select_buttons("conf_exposures", reactive({
    select_candidates(c(
      input$adv_outcome,
      input$adv_time,
      input$adv_event,
      input$conf_candidate
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
        !is.null(rv$special_exposure) || !is.null(rv$special_multi) ||
        !is.null(rv$cox) || !is.null(rv$survreg) || !is.null(rv$competing) ||
        !is.null(rv$mediation) || !is.null(rv$trial),
      Visualise = !is.null(rv$km) || !is.null(rv$plotreg) ||
        !is.null(rv$cuminc) || !is.null(rv$forest) || !is.null(rv$fitplot),
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
    if (gtx_addin_mode) {
      # Addin status line: dataset and application context only. Outcome and
      # approach stay within each analysis panel.
      sep <- function() span(class = "gtw-status-sep", HTML("&middot;"))
      if (is.null(data)) {
        return(div(
          class = "gtx-analysis-strip gtw-status",
          span(class = "gtw-status-empty", "No dataset loaded")
        ))
      }
      return(div(
        class = "gtx-analysis-strip gtw-status",
        span(class = "gtw-status-name", rv$data_name %||% "data"),
        sep(),
        span(HTML(paste0(format(nrow(data), big.mark = ",", trim = TRUE), " &times; ", ncol(data)))),
        if (gtx_has_text(rv$data_origin)) tagList(sep(), span(rv$data_origin)),
        sep(),
        span(source)
      ))
    }
    current_tab <- input$main_nav %||% "Data"
    outcome <- switch(
      current_tab,
      "Special Models" = input$special_outcome %||% "",
      "Survival" = "",
      input$reg_outcome %||% ""
    )
    model <- switch(
      current_tab,
      "Special Models" = switch(
        input$special_mode %||% "ordinal",
        ordinal = "Ordinal regression",
        multinomial = "Multinomial regression",
        multilevel = "Multilevel regression",
        matched = "Conditional logistic regression"
      ),
      "Survival" = if (identical(input$surv_mode, "survreg")) {
        "Parametric survival regression"
      } else if (identical(input$surv_mode, "competing")) {
        "Fine-Gray competing-risk regression"
      } else {
        "Cox regression"
      },
      input$reg_approach %||% ""
    )
    div(
      class = "gtx-analysis-strip",
      div(
        class = "gtx-analysis-strip-inner",
        span(class = "gtx-analysis-item", strong("Dataset: "), rv$data_name %||% "None"),
        if (gtx_has_text(rv$data_origin)) span(class = "gtx-analysis-item", strong("Source: "), rv$data_origin),
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
    checks <- gtx_model_checks(
      data, outcome = input$reg_outcome, exposures = exposures, adjust_for = adjustment,
      approach = input$reg_approach, multivariable = length(unique(c(exposures, adjustment))) > 1
    )
    div(
      class = "gtx-preflight",
      gtx_checks_ui(checks),
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
    if (identical(input$surv_mode, "competing")) {
      ready <- ready && gtx_has_text(input$competing_event_of_interest) &&
        gtx_has_text(input$competing_censor_code)
    }
    if (!ready) {
      return(div(class = "gtx-preflight", strong("Before running: "), "choose time, event, and at least one exposure."))
    }
    adjustment <- if (isTRUE(input$surv_multivariable)) {
      character(0)
    } else {
      input$surv_adjust %||% character(0)
    }
    mode <- if (isTRUE(input$surv_multivariable)) "single multivariable model" else "separate exposure models"
    model_label <- if (identical(input$surv_mode, "survreg")) {
      paste0("parametric survival (", input$surv_dist %||% "weibull", ")")
    } else if (identical(input$surv_mode, "competing")) {
      paste0(
        "Fine-Gray competing-risk regression; event of interest = ",
        input$competing_event_of_interest %||% "not selected",
        "; censoring = ", input$competing_censor_code %||% "not selected"
      )
    } else {
      "Cox regression"
    }
    code_note <- if (identical(input$surv_mode, "competing")) {
      observed <- table(as.character(data[[input$surv_event]]), useNA = "no")
      paste0(
        "Observed event codes: ",
        paste(names(observed), as.integer(observed), sep = " = ", collapse = "; "),
        "."
      )
    } else NULL
    checks <- gtx_model_checks(
      data, time = input$surv_time, event = if (identical(input$surv_mode, "competing")) NULL else input$surv_event,
      exposures = exposures, adjust_for = adjustment,
      multivariable = length(unique(c(exposures, adjustment))) > 1
    )
    div(
      class = "gtx-preflight",
      gtx_checks_ui(checks),
      strong("Ready to run: "),
      paste0(
        model_label, "; time = ", input$surv_time, "; event = ", input$surv_event,
        "; ", length(exposures), " exposure", if (length(exposures) == 1) "" else "s",
        "; ", length(adjustment), " adjustment variable", if (length(adjustment) == 1) "" else "s",
        "; ", mode, "; N = ", nrow(data), "."
      ),
      if (!is.null(code_note)) tagList(tags$br(), span(code_note))
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
    rv$data_origin <- "Example data"
    rv$data_reference <- NULL
    rv$data_code <- gtx_template_code(template)
    rv$last_message <- "Original data is active. Open Data Prep only if you want to make changes."
    showNotification(paste(name, "template loaded. Original data is ready for analysis."), type = "message")
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

  # ---- Save and resume a session --------------------------------------------
  output$download_session_state <- downloadHandler(
    filename = function() paste0(gtx_object_name(rv$data_name %||% "analysis", "analysis"), ".gtrsession"),
    content = function(file) {
      data <- analysis_data()
      if (is.null(data)) stop("Load a dataset before saving a session.", call. = FALSE)
      state <- list(
        format = "gtregression-session",
        version = 1L,
        saved = format(Sys.time(), "%Y-%m-%d %H:%M"),
        package_version = as.character(utils::packageVersion("gtregression")),
        data = data,
        data_name = rv$data_name,
        prepared = isTRUE(data_prep$using_prepared()),
        inputs = gtx_session_inputs(isolate(reactiveValuesToList(input)))
      )
      saveRDS(state, file)
    }
  )

  restore_state <- new.env(parent = emptyenv())
  restore_active <- reactiveVal(FALSE)

  observeEvent(input$restore_session_file, {
    req(gtx_session_restore_allowed())
    upload <- input$restore_session_file
    req(upload)
    state <- tryCatch(readRDS(upload$datapath), error = function(e) NULL)
    valid <- is.list(state) && identical(state$format, "gtregression-session") &&
      is.data.frame(state$data) && is.list(state$inputs)
    if (!valid) {
      showNotification("This file is not a saved GT Regression session. Choose a .gtrsession file saved from the app.", type = "error", duration = 10)
      return(NULL)
    }
    rv$data <- state$data
    rv$data_name <- state$data_name %||% "restored session"
    rv$data_origin <- "Saved session"
    rv$data_reference <- NULL
    rv$data_code <- paste0(
      "# Data restored from a saved GT Regression session.\n",
      "session_file <- readRDS(\"", gsub("\"", "", upload$name, fixed = TRUE), "\")\n",
      "df <- session_file$data"
    )
    # Build every selection panel now, so saved values can be applied
    # without visiting each workspace first.
    for (out_id in c("desc_inputs", "reg_inputs", "reg_reference_inputs", "reg_strata_inputs",
                     "special_inputs", "special_reference_inputs", "surv_inputs", "km_group_input",
                     "competing_event_codes", "advanced_config", "med_inputs", "trial_inputs",
                     "trial_event_input", "trial_reference_input")) {
      try(outputOptions(output, out_id, suspendWhenHidden = FALSE), silent = TRUE)
    }
    restore_state$queue <- list(inputs = state$inputs, tries = 0L, progress = 0L)
    restore_active(TRUE)
    showNotification(
      paste0("Session opened", if (gtx_has_text(state$saved)) paste0(" (saved ", state$saved, ")") else "", ". Restoring your selections..."),
      type = "message", duration = 5
    )
  }, ignoreInit = TRUE)

  # Inputs appear as their panels are built for the restored data, so the
  # saved values are applied over a few passes.
  observe({
    req(restore_active())
    invalidateLater(500)
    queue <- restore_state$queue
    req(queue)
    isolate({
      pending <- queue$inputs
      present <- names(input)
      done <- character(0)
      for (id in names(pending)) {
        target <- pending[[id]]
        if (!id %in% present) next
        if (identical(as.character(input[[id]] %||% character(0)), as.character(target %||% character(0)))) {
          done <- c(done, id)
          next
        }
        session$sendInputMessage(id, list(value = target, selected = target))
      }
      pending <- pending[setdiff(names(pending), done)]
      tries <- queue$tries + 1L
      # Stop when everything is restored, or when nothing has changed for a
      # while (inputs that do not exist for this data or mode are skipped).
      stalled <- if (length(done)) 0L else queue$progress + 1L
      if (!length(pending) || stalled >= 20L || tries >= 240L) {
        restore_state$queue <- NULL
        restore_active(FALSE)
        rv$last_message <- "Selections restored. Run each analysis again to recreate its results."
        showNotification("Selections restored. Run each analysis again to recreate its results.", type = "message", duration = 8)
      } else {
        restore_state$queue <- list(inputs = pending, tries = tries, progress = stalled)
      }
    })
  })

  observeEvent(input$load_dataset, {
    req(gtx_has_text(input$dataset_name))
    rv$data <- gtx_dataset(input$dataset_name)
    rv$data_name <- input$dataset_name
    rv$data_origin <- "Example data"
    rv$data_reference <- NULL
    rv$data_code <- paste0('data("', input$dataset_name, '", package = "gtregression")\ndf <- ', input$dataset_name)
    rv$last_message <- "Original data is active. Open Data Prep only if you want to make changes."
    showNotification("Dataset loaded. Original data is ready for analysis.", type = "message")
  })

  observeEvent(input$load_upload, {
    req(input$data_file)
    path <- input$data_file$datapath
    ext <- tolower(tools::file_ext(input$data_file$name))
    sheet <- input$xlsx_sheet
    if (gtx_has_text(sheet) && grepl("^[0-9]+$", sheet)) sheet <- as.integer(sheet)
    if (!gtx_has_text(sheet)) sheet <- NULL
    separator <- if (identical(input$csv_sep, "auto")) NULL else input$csv_sep
    rv$data <- run_safely(
      "Data import",
      gtregression::import_data(
        path,
        format = ext,
        sheet = sheet,
        header = input$csv_header,
        sep = separator,
        name_repair = "unique"
      )
    )
    if (is.null(rv$data)) return(NULL)
    original_names <- tryCatch(
      names(suppressWarnings(suppressMessages(gtregression::import_data(
        path, format = ext, sheet = sheet, header = input$csv_header, sep = separator,
        name_repair = "minimal"
      )))),
      error = function(e) NULL
    )
    for (note in gtx_import_notes(rv$data, original_names, ext = ext, path = path, sheet = sheet)) {
      showNotification(note, type = "warning", duration = 12)
    }
    rv$data_origin <- "Imported file"
    rv$data_reference <- NULL
    code_args <- list(
      file = shQuote(paste0("path/to/file.", ext)),
      sheet = if (ext %in% c("xls", "xlsx") && !is.null(sheet)) {
        if (is.numeric(sheet)) as.character(sheet) else shQuote(sheet)
      } else NULL,
      header = if (ext %in% c("csv", "tsv", "txt")) {
        gtx_bool_code(input$csv_header)
      } else NULL,
      sep = if (ext %in% c("csv", "tsv", "txt") && !is.null(separator)) {
        shQuote(separator)
      } else NULL,
      name_repair = shQuote("unique")
    )
    rv$data_code <- gtx_code_assign("df", "import_data", code_args)
    rv$data_name <- input$data_file$name
    rv$last_message <- "Original data is active. Open Data Prep only if you want to make changes."
    showNotification("Uploaded data loaded. Original data is ready for analysis.", type = "message")
  })

  output$data_preview <- gtx_render_data(function() {
    req(rv$data)
    utils::head(rv$data, 10)
  }, page_length = 10)

  output$data_summary <- renderPrint({
    req(rv$data)
    cat("Dataset:", rv$data_name %||% "data", "\n")
    if (gtx_has_text(rv$data_origin)) cat("Source:", rv$data_origin, "\n")
    cat("Rows:", nrow(rv$data), "\n")
    cat("Columns:", ncol(rv$data), "\n\n")
    utils::str(rv$data)
  })

  output$data_code <- renderText(rv$data_code)

  # ---- D3 Data > Structure: describes the current analysis dataset --------
  # Computed once per analysis dataset (cached by the reactive) and only when
  # the Structure outputs are visible; switching panels does not recompute.
  structure_info <- reactive({
    req(analysis_data())
    gt_data_structure(analysis_data())
  })

  structure_plural <- function(n, one, many = paste0(one, "s")) {
    paste(format(n, big.mark = ",", trim = TRUE), if (n == 1) one else many)
  }

  output$structure_overview <- renderUI({
    info <- structure_info()
    ds <- info$dataset
    notes <- info$notes
    changes <- data_prep$changes()
    state <- if (changes > 0) {
      HTML(paste0("prepared &middot; ", structure_plural(changes, "change")))
    } else {
      "original"
    }
    dot <- span(class = "gta-structure-dot", `aria-hidden` = "true", HTML("&middot;"))
    join <- function(parts) {
      parts <- Filter(Negate(is.null), parts)
      out <- list()
      for (i in seq_along(parts)) out <- c(out, if (i > 1) list(dot), list(parts[[i]]))
      out
    }
    note_button <- function(text, variables, explain) {
      tags$button(
        type = "button", class = "gta-structure-note",
        `data-gta-structure-select` = variables[[1]],
        title = paste0(explain, ": ", paste(variables, collapse = ", ")),
        text
      )
    }
    worth <- join(list(
      if (notes$duplicate_rows > 0) span(class = "gta-structure-note-text", structure_plural(notes$duplicate_rows, "duplicate row")),
      if (length(notes$possible_id)) note_button(structure_plural(length(notes$possible_id), "possible identifier"), notes$possible_id, "Every non-missing value is different"),
      if (length(notes$constant)) note_button(structure_plural(length(notes$constant), "constant variable"), notes$constant, "One distinct value or none"),
      if (length(notes$high_missing)) note_button(paste(structure_plural(length(notes$high_missing), "variable"), "over 20% missing"), notes$high_missing, "More than 20% of values missing"),
      if (length(notes$zero_one)) note_button(structure_plural(length(notes$zero_one), "numeric 0/1 variable"), notes$zero_one, "Numeric with only the values 0 and 1")
    ))
    tagList(
      div(
        class = "gta-structure-source",
        span("Analysis dataset"), dot, span(rv$data_name %||% "data"), dot, span(state)
      ),
      div(
        class = "gta-structure-summary",
        join(list(
          span(structure_plural(ds$rows, "row")),
          span(structure_plural(ds$variables, "variable")),
          span(paste(format(ds$missing_cells, big.mark = ",", trim = TRUE), "missing")),
          span(structure_plural(ds$duplicate_rows, "duplicate row"))
        ))
      ),
      div(
        class = "gta-structure-types",
        join(lapply(names(ds$types), function(type) span(paste(type, ds$types[[type]]))))
      ),
      if (length(worth)) div(
        class = "gta-structure-worth",
        span(class = "gta-structure-worth-label", "Worth a look"),
        dot,
        worth,
        span(class = "gta-structure-worth-help")
      )
    )
  })

  output$structure_table <- renderUI({
    info <- structure_info()
    vars <- info$variables
    n <- info$dataset$rows
    if (!nrow(vars)) return(div(class = "gta-panel-empty", "This dataset has no variables."))
    tag_for <- function(i) {
      tags_i <- c(
        if (vars$possible_id[i]) "possible identifier",
        if (vars$constant[i]) "constant",
        if (vars$high_missing[i]) "over 20% missing",
        if (vars$zero_one[i]) "0/1"
      )
      if (!length(tags_i)) return(NULL)
      span(class = "gta-structure-tags", HTML(paste(tags_i, collapse = " &middot; ")))
    }
    rows <- lapply(seq_len(nrow(vars)), function(i) {
      missing <- vars$missing[i]
      tags$tr(
        role = "row", tabindex = if (i == 1) "0" else "-1", `aria-selected` = "false",
        `data-variable` = vars$variable[i],
        tags$td(
          class = "gta-structure-name",
          span(class = "gta-structure-varname", title = vars$variable[i], vars$variable[i]),
          if (nzchar(vars$label[i])) span(class = "gta-structure-label", title = vars$label[i], vars$label[i]),
          tag_for(i)
        ),
        tags$td(vars$type[i]),
        tags$td(
          class = "gta-num",
          if (missing) tagList(format(missing, big.mark = ",", trim = TRUE), span(class = "gta-structure-pct", paste0(" ", gt_ds_pct(missing, n)))) else span(class = "gta-structure-zero", "0")
        ),
        tags$td(class = "gta-num", format(vars$distinct[i], big.mark = ",", trim = TRUE)),
        tags$td(class = "gta-structure-sum", title = gt_ds_text(vars$summary[i]), HTML(gt_ds_html(vars$summary[i])))
      )
    })
    tags$table(
      class = "gta-structure-grid", role = "grid", `aria-label` = "Variables",
      tags$thead(tags$tr(
        tags$th(scope = "col", "Variable"), tags$th(scope = "col", "Type"),
        tags$th(scope = "col", class = "gta-num", "Missing"), tags$th(scope = "col", class = "gta-num", "Distinct"),
        tags$th(scope = "col", "Summary")
      )),
      tags$tbody(rows)
    )
  })


  # ---- D3 Data > Prepare: state, variables, preview, changes, code --------
  output$prep_state <- renderUI({
    req(analysis_data())
    n <- data_prep$changes()
    text <- if (n > 0) {
      HTML(paste0(structure_plural(n, "change"), " &middot; Analyses use prepared data"))
    } else {
      HTML("No changes &middot; Analyses use original data")
    }
    span(
      class = "gta-prep-state", `data-changes` = n,
      `data-can-undo` = if (n > 0) "true" else "false",
      `data-can-redo` = if (isTRUE(data_prep$can_redo())) "true" else "false",
      text
    )
  })

  output$prep_variables <- renderUI({
    info <- structure_info()
    data <- analysis_data()
    vars <- info$variables
    if (!nrow(vars)) return(div(class = "gta-panel-empty", "No variables."))
    items <- lapply(seq_len(nrow(vars)), function(i) {
      name <- vars$variable[i]
      x <- data[[name]]
      type <- vars$type[i]
      detail <- if (type %in% c("Factor", "Ordered")) {
        HTML(paste0(type, " &middot; ", nlevels(x)))
      } else if (isTRUE(vars$zero_one[i])) {
        HTML("Numeric &middot; 0/1")
      } else {
        type
      }
      codes <- is.numeric(x) && any(x %in% c(99, 999, 9999), na.rm = TRUE)
      div(
        class = "gta-prep-var", role = "option", tabindex = if (i == 1) "0" else "-1", `aria-selected` = "false",
        `data-variable` = name, `data-type` = type,
        `data-distinct` = vars$distinct[i], `data-missing` = vars$missing[i],
        `data-zero-one` = if (isTRUE(vars$zero_one[i])) "true" else "false",
        `data-codes` = if (codes) "true" else "false",
        `data-label` = vars$label[i],
        title = if (nzchar(vars$label[i])) paste0(name, ": ", vars$label[i]) else name,
        span(class = "gta-prep-var-name", name),
        span(class = "gta-prep-var-type", detail)
      )
    })
    div(class = "gta-prep-vars", role = "listbox", `aria-label` = "Variables", items)
  })

  # The working-data preview reuses Data View's table implementation and
  # always shows the current analysis dataset.
  output$prep_preview <- gtx_render_data(function() {
    req(analysis_data())
    analysis_data()
  })

  output$prep_changes <- renderUI({
    labels <- data_prep$history_labels()
    if (!length(labels)) return(div(class = "gta-prep-changes-empty", "No changes yet. Analyses use the original data."))
    tags$ol(class = "gta-prep-changes", lapply(labels, tags$li))
  })

  output$prep_code <- renderText({
    req(analysis_data())
    if (!isTRUE(data_prep$changed())) return(paste(analysis_code_prefix(), "# No preparation changes applied.", sep = "\n"))
    analysis_code_prefix()
  })

  output$structure_detail <- renderUI({
    data <- analysis_data()
    req(data)
    variable <- input$structure_variable %||% ""
    if (!nzchar(variable) || !variable %in% names(data)) {
      return(div(class = "gta-structure-detail-empty", "Select a variable to see its details."))
    }
    d <- gt_variable_detail(data[[variable]])
    fmt <- function(x) format(x, big.mark = ",", trim = TRUE)
    stat_rows <- c(
      Valid = fmt(d$counts[["valid"]]),
      Missing = paste0(fmt(d$counts[["missing"]]), if (d$counts[["missing"]]) paste0(" (", d$missing_pct, ")") else ""),
      Distinct = fmt(d$counts[["distinct"]]),
      d$stats
    )
    if (!is.null(d$levels) && d$type %in% c("Factor", "Ordered")) stat_rows <- c(stat_rows, Levels = fmt(nrow(d$levels) + d$levels_more))
    histogram <- NULL
    if (!is.null(d$histogram)) {
      counts <- d$histogram$counts
      breaks <- d$histogram$breaks
      k <- length(counts)
      width <- 280
      height <- 72
      bar <- width / k
      top <- max(counts)
      rects <- lapply(seq_len(k), function(i) {
        h <- if (top > 0) round(height * counts[i] / top, 1) else 0
        tags$rect(
          x = round((i - 1) * bar + 0.5, 1), y = round(height - h, 1), width = round(max(bar - 1, 0.5), 1), height = h,
          tags$title(paste0(gt_ds_num(breaks[i]), " to ", gt_ds_num(breaks[i + 1]), ": ", fmt(counts[i])))
        )
      })
      histogram <- div(
        class = "gta-structure-hist",
        tags$svg(
          viewBox = paste(0, 0, width, height), preserveAspectRatio = "none", role = "img",
          `aria-label` = paste("Distribution of", variable),
          rects
        ),
        div(class = "gta-structure-hist-axis", span(gt_ds_num(breaks[1])), span(gt_ds_num(breaks[k + 1])))
      )
    }
    levels_table <- NULL
    if (!is.null(d$levels) && nrow(d$levels)) {
      levels_table <- tags$table(
        class = "gta-structure-levels",
        tags$thead(tags$tr(tags$th(scope = "col", if (d$type == "Numeric") "Value" else "Level"), tags$th(scope = "col", class = "gta-num", "n"), tags$th(scope = "col", class = "gta-num", "%"))),
        tags$tbody(lapply(seq_len(nrow(d$levels)), function(i) tags$tr(
          tags$td(title = d$levels$level[i], d$levels$level[i]),
          tags$td(class = "gta-num", fmt(d$levels$n[i])),
          tags$td(class = "gta-num", d$levels$pct[i])
        ))),
        if (d$levels_more > 0) tags$tfoot(tags$tr(tags$td(colspan = "3", paste("+", fmt(d$levels_more), "more"))))
      )
    }
    tagList(
      div(class = "gta-structure-detail-title", title = variable, variable),
      div(
        class = "gta-structure-detail-type",
        if (identical(d$type, "Factor") || identical(d$type, "Ordered")) HTML(paste0(d$type, " &middot; ", fmt(nrow(d$levels) + d$levels_more), " levels")) else d$type,
        if (nzchar(d$label)) span(class = "gta-structure-detail-label", HTML(" &middot; "), d$label)
      ),
      tags$dl(
        class = "gta-structure-stats",
        lapply(names(stat_rows), function(name) tagList(tags$dt(HTML(gt_ds_html(name))), tags$dd(HTML(gt_ds_html(stat_rows[[name]])))))
      ),
      histogram,
      levels_table,
      div(
        class = "gta-structure-actions",
        tags$button(
          type = "button", class = "btn btn-default btn-sm gta-structure-prepare",
          `data-gta-structure-prepare` = variable,
          icon("wand-magic-sparkles", `aria-hidden` = "true"), " Open in Prepare"
        )
      )
    )
  })

  output$desc_inputs <- renderUI({
    req(analysis_data())
    vars <- names(analysis_data())
    ctx <- selection_context("desc_inputs")
    tagList(
      selectInput("desc_by", "Group by", choices = c("None" = "", vars), selected = keep_sel(ctx, "desc_by", vars)),
      checkboxGroupInput("desc_exposures", if (gta_shell_enabled) "Variables" else "Variables to summarise", choices = vars, selected = keep_sel(ctx, "desc_exposures", vars, multiple = TRUE)),
      gtx_select_buttons("desc_exposures")
    )
  })

  output$reg_inputs <- renderUI({
    req(analysis_data())
    vars <- names(analysis_data())
    ctx <- selection_context("reg_inputs")
    interaction <- if (isTRUE(ctx$keep)) gtx_keep_interaction(isolate(input$reg_interaction), vars, ctx$events) else ""
    tagList(
      selectInput("reg_outcome", "Outcome", choices = c("", vars), selected = keep_sel(ctx, "reg_outcome", vars)),
      checkboxGroupInput("reg_exposures", "Exposures", choices = vars, selected = keep_sel(ctx, "reg_exposures", vars, multiple = TRUE)),
      gtx_select_buttons("reg_exposures"),
      checkboxGroupInput("reg_adjust", "Adjust for", choices = vars, selected = keep_sel(ctx, "reg_adjust", vars, multiple = TRUE)),
      gtx_select_buttons("reg_adjust"),
      if (gta_shell_enabled) {
        textInput("reg_interaction", "Interaction", value = interaction, placeholder = "e.g. smoke*race")
      } else {
        textInput("reg_interaction", "Interaction term", value = interaction, placeholder = "Optional: smoke*race")
      }
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
    selected <- keep_references(selection_context("reg_reference_inputs"), "reg_reference_", candidates)
    if (!length(candidates)) return(NULL)
    controls <- Map(function(variable, levels, index) {
      selectInput(
        paste0("reg_reference_", index),
        paste0(variable, " reference category"),
        choices = levels,
        selected = selected[[index]]
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

  output$special_intro <- renderUI({
    switch(
      input$special_mode %||% "ordinal",
      ordinal = tagList(
        h3("Ordered Outcome"),
        div(class = "gtx-help", "Use when the outcome has three or more naturally ordered categories. Set it as an ordered factor in Data Prep first.")
      ),
      multinomial = tagList(
        h3("Unordered Outcome"),
        div(class = "gtx-help", "Use when the outcome has three or more categories without a natural order. The first factor level is the outcome reference.")
      ),
      multilevel = tagList(
        h3("Clustered Data"),
        div(class = "gtx-help", "Use a random-intercept model for observations grouped within clinics, schools, centres, or similar clusters.")
      ),
      matched = tagList(
        h3("Matched Case-Control"),
        div(class = "gtx-help", "Use conditional logistic regression when every case and its matched control or controls share a matched-set identifier.")
      )
    )
  })

  output$special_inputs <- renderUI({
    req(analysis_data())
    vars <- names(analysis_data())
    mode <- input$special_mode %||% "ordinal"
    # Changing the study design still starts afresh (as before); preparation
    # changes within the same design keep valid selections.
    ctx <- selection_context("special_inputs", extra = mode)
    tagList(
      selectInput("special_outcome", "Outcome", choices = c("", vars), selected = keep_sel(ctx, "special_outcome", vars)),
      checkboxGroupInput("special_exposures", "Reported exposures", choices = vars, selected = keep_sel(ctx, "special_exposures", vars, multiple = TRUE)),
      gtx_select_buttons("special_exposures"),
      checkboxGroupInput("special_adjust", "Adjust for", choices = vars, selected = keep_sel(ctx, "special_adjust", vars, multiple = TRUE)),
      gtx_select_buttons("special_adjust"),
      if (identical(mode, "multilevel")) {
        tagList(
          selectInput("special_group", "Grouping variable", choices = c("", vars), selected = keep_sel(ctx, "special_group", vars)),
          selectInput(
            "special_cluster_approach",
            "Outcome model",
            choices = c("Continuous (linear)" = "linear", "Binary (logistic)" = "logit"),
            selected = keep_value(ctx, isolate(input$special_cluster_approach), c("linear", "logit")) %||% "linear"
          )
        )
      },
      if (identical(mode, "matched")) {
        selectInput("special_matched_set", "Matched-set identifier", choices = c("", vars), selected = keep_sel(ctx, "special_matched_set", vars))
      },
      if (!identical(mode, "multilevel")) {
        checkboxInput("special_multivariable", "Single multivariable model", keep_value(ctx, isolate(input$special_multivariable)) %||% FALSE)
      },
      if (identical(mode, "ordinal")) {
        checkboxInput("special_assumption_test", "Check proportional-odds assumption", keep_value(ctx, isolate(input$special_assumption_test)) %||% TRUE)
      }
    )
  })

  special_reference_candidates <- reactive({
    req(analysis_data())
    predictors <- unique(c(
      input$special_exposures %||% character(0),
      input$special_adjust %||% character(0)
    ))
    gtx_reference_candidates(
      analysis_data(), predictors, outcome = input$special_outcome
    )
  })

  output$special_reference_inputs <- renderUI({
    candidates <- special_reference_candidates()
    selected <- keep_references(selection_context("special_reference_inputs", extra = isolate(input$special_mode) %||% "ordinal"), "special_reference_", candidates)
    if (!length(candidates)) return(NULL)
    controls <- Map(function(variable, levels, index) {
      selectInput(
        paste0("special_reference_", index),
        paste0(variable, " reference category"),
        choices = levels,
        selected = selected[[index]]
      )
    }, names(candidates), candidates, seq_along(candidates))
    tagList(
      tags$hr(),
      h4("Predictor reference categories"),
      div(class = "gtx-help", "These baselines are reproduced in the generated R code."),
      controls
    )
  })

  special_references <- reactive({
    candidates <- special_reference_candidates()
    if (!length(candidates)) return(list())
    values <- lapply(seq_along(candidates), function(index) {
      input[[paste0("special_reference_", index)]] %||% candidates[[index]][[1]]
    })
    stats::setNames(values, names(candidates))
  })

  special_data <- reactive({
    req(analysis_data())
    gtx_relevel_predictors(analysis_data(), special_references())
  })

  special_code_prefix <- reactive({
    lines <- c(
      analysis_code_prefix(),
      gtx_reference_code(
        special_references(), special_reference_candidates(),
        data_name = "analysis_data"
      )
    )
    paste(lines, collapse = "\n")
  })

  output$special_preflight <- renderUI({
    data <- analysis_data()
    mode <- input$special_mode %||% "ordinal"
    outcome <- input$special_outcome %||% ""
    exposures <- input$special_exposures %||% character(0)
    extra_ready <- if (mode == "multilevel") {
      gtx_has_text(input$special_group)
    } else if (mode == "matched") {
      gtx_has_text(input$special_matched_set)
    } else TRUE
    if (is.null(data) || !gtx_has_text(outcome) || !length(exposures) || !extra_ready) {
      return(div(
        class = "gtx-preflight",
        strong("Before running: "),
        "choose the outcome, reported exposures, and the required design variable."
      ))
    }
    outcome_note <- if (mode == "ordinal") {
      if (is.ordered(data[[outcome]])) {
        paste0("Ordered levels: ", paste(levels(data[[outcome]]), collapse = " < "), ".")
      } else {
        "Outcome is not an ordered factor. Set its level order in Data Prep before running."
      }
    } else if (mode == "multinomial") {
      if (is.factor(data[[outcome]]) && !is.ordered(data[[outcome]])) {
        paste0("Outcome reference: ", levels(data[[outcome]])[1], ".")
      } else {
        "Outcome must be an unordered factor with at least three levels."
      }
    } else ""
    checks <- gtx_model_checks(
      data, outcome = outcome,
      exposures = exposures, adjust_for = input$special_adjust %||% character(0),
      time = NULL, event = NULL
    )
    checks <- checks[!grepl("^In '", checks)]
    div(
      class = "gtx-preflight",
      gtx_checks_ui(checks),
      strong("Ready to run: "),
      paste0(length(exposures), " reported exposure",
             if (length(exposures) == 1L) "" else "s", "; N = ", nrow(data), "."),
      if (nzchar(outcome_note)) tagList(tags$br(), span(outcome_note))
    )
  })

  analysis_code_prefix <- reactive({
    source_label <- if (isTRUE(data_prep$using_prepared())) "prepared" else "original"
    data_assignment <- if (gtx_has_text(rv$data_reference)) {
      paste0("analysis_data <- ", rv$data_reference)
    } else {
      "analysis_data <- df"
    }
    lines <- c(
      paste0("# The app was set to use the ", source_label, " dataset."),
      data_assignment
    )
    if (isTRUE(data_prep$changed())) {
      # Rendered for analysis_data from the stored steps (never by text rewriting).
      lines <- c(lines, "# Reproduce the preparation choices made in the app.", data_prep$code_for("analysis_data"))
    }
    paste(lines, collapse = "\n")
  })

  strata_role_lists <- function() {
    lists <- list(list(id = "reg_exposures", label = "Exposures", value = input$reg_exposures %||% character(0)))
    if (isTRUE(input$reg_strata_multi)) {
      lists[[2]] <- list(id = "reg_adjust", label = "Adjust for", value = input$reg_adjust %||% character(0))
    }
    lists
  }

  strata_conflicts <- reactive({
    strat <- input$reg_stratifier
    if (!gtx_has_text(strat)) return(NULL)
    conflicts <- gtx_role_conflicts(list(Stratifier = strat), strata_role_lists())
    conflicts <- conflicts[conflicts$type == "role", , drop = FALSE]
    clash <- gtx_role_clashes(list(Outcome = input$reg_outcome, Stratifier = strat))
    if (!nrow(conflicts) && !length(clash)) return(NULL)
    list(conflicts = conflicts, clash = clash)
  })

  output$reg_strata_conflict <- renderUI({
    info <- strata_conflicts()
    if (is.null(info)) return(NULL)
    div(
      class = "alert alert-warning gtx-role-alert",
      role = "alert",
      tags$p(paste(c(info$clash, gtx_role_conflict_text(info$conflicts)), collapse = " ")),
      tags$p(class = "small", "A variable used to split the data cannot also be a model variable."),
      if (nrow(info$conflicts)) actionButton("reg_strata_fix", paste0("Remove '", info$conflicts$variable[1], "' from ", paste(unique(info$conflicts$label), collapse = " and ")), class = "btn-default btn-sm")
    )
  })

  observeEvent(input$reg_strata_fix, {
    info <- strata_conflicts()
    req(info, nrow(info$conflicts))
    for (id in unique(info$conflicts$id)) {
      drop <- info$conflicts$variable[info$conflicts$id == id]
      updateCheckboxGroupInput(session, id, selected = setdiff(input[[id]] %||% character(0), drop))
    }
    showNotification("Removed the stratifier from the model variables.", type = "message", duration = 5)
  }, ignoreInit = TRUE)

  output$reg_strata_inputs <- renderUI({
    req(analysis_data())
    vars <- names(analysis_data())
    ctx <- selection_context("reg_strata_inputs")
    selectInput("reg_stratifier", "Stratifier", choices = c("None" = "", vars), selected = keep_sel(ctx, "reg_stratifier", vars))
  })

  output$surv_inputs <- renderUI({
    req(analysis_data())
    vars <- names(analysis_data())
    ctx <- selection_context("surv_inputs")
    distributions <- c("weibull", "exponential", "lognormal", "loglogistic")
    tagList(
      selectInput("surv_time", "Time", choices = c("", vars), selected = keep_sel(ctx, "surv_time", vars)),
      selectInput("surv_event", "Event", choices = c("", vars), selected = keep_sel(ctx, "surv_event", vars)),
      checkboxGroupInput("surv_exposures", "Exposures", choices = vars, selected = keep_sel(ctx, "surv_exposures", vars, multiple = TRUE)),
      gtx_select_buttons("surv_exposures"),
      checkboxGroupInput("surv_adjust", "Adjust for", choices = vars, selected = keep_sel(ctx, "surv_adjust", vars, multiple = TRUE)),
      gtx_select_buttons("surv_adjust"),
      conditionalPanel(
        "input.surv_mode != 'competing'",
        selectInput("surv_stratifier", "Stratifier", choices = c("None" = "", vars), selected = keep_sel(ctx, "surv_stratifier", vars))
      ),
      uiOutput("competing_event_codes"),
      conditionalPanel(
        "input.surv_mode == 'survreg'",
        selectInput("surv_dist", "Parametric distribution", choices = distributions, selected = keep_value(ctx, isolate(input$surv_dist), distributions) %||% "weibull")
      )
    )
  })

  output$km_group_input <- renderUI({
    req(analysis_data())
    vars <- names(analysis_data())
    ctx <- selection_context("km_group_input")
    selectInput("km_by", if (gta_shell_enabled) "Group by" else "Group curves by", choices = c("None" = "", vars), selected = keep_sel(ctx, "km_by", vars))
  })

  output$competing_event_codes <- renderUI({
    if (!identical(input$surv_mode, "competing")) return(NULL)
    data <- analysis_data()
    event <- input$surv_event %||% ""
    if (is.null(data) || !gtx_has_text(event) || !event %in% names(data)) {
      return(div(class = "gtx-help", "Choose the event-status variable to see its observed codes."))
    }
    codes <- unique(as.character(stats::na.omit(data[[event]])))
    if (!length(codes)) return(NULL)
    default_censor <- if ("0" %in% codes) "0" else codes[[1]]
    event_choices <- setdiff(codes, default_censor)
    default_event <- if (length(event_choices)) event_choices[[1]] else codes[[1]]
    # Same event variable (renames followed) after a preparation change: keep
    # the chosen codes when they are still observed.
    ctx <- selection_context("competing_event_codes", extra = "competing")
    same_event <- isTRUE(ctx$keep) && identical(gtx_map_names(selection_memory$competing_event %||% "", ctx$events), event)
    selection_memory$competing_event <- event
    if (same_event) {
      default_censor <- keep_value(ctx, isolate(input$competing_censor_code), codes) %||% default_censor
      default_event <- keep_value(ctx, isolate(input$competing_event_of_interest), codes) %||% default_event
    }
    tagList(
      selectInput(
        "competing_censor_code", "Censoring code",
        choices = codes, selected = default_censor
      ),
      selectInput(
        "competing_event_of_interest", "Event of interest",
        choices = codes, selected = default_event
      ),
      div(class = "gtx-help", "At least one other observed code must represent a competing event.")
    )
  })

  observe({
    data <- analysis_data()
    if (is.null(data)) return(NULL)
    excluded <- c(input$surv_time, input$surv_event)
    choices <- c("None" = "", setdiff(names(data), excluded[nzchar(excluded)]))
    ctx <- selection_context("cuminc_by")
    updateSelectInput(session, "cuminc_by", choices = choices, selected = keep_sel(ctx, "cuminc_by", choices) %||% "")
  })

  output$surv_mode_intro <- renderUI({
    if (identical(input$surv_mode, "survreg")) {
      tagList(
        h3("Parametric Survival"),
        div(class = "gtx-help", "Estimate time ratios with a selected survival distribution. Choose the distribution below before running the model.")
      )
    } else if (identical(input$surv_mode, "competing")) {
      tagList(
        h3("Competing Risks"),
        div(class = "gtx-help", "Estimate subdistribution hazard ratios for one event and plot its cumulative incidence while retaining competing events.")
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

    model_roles <- function(include_covariates = FALSE, include_exposures = FALSE,
                            covariate_label = "Adjustment variables") {
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
          checkboxGroupInput("adv_covariates", covariate_label, choices = vars, selected = input$adv_covariates %||% character(0)),
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
        checkboxGroupInput("conf_exposures", "Exposures of interest", choices = vars, selected = input$conf_exposures %||% character(0)),
        gtx_select_buttons("conf_exposures"),
        selectInput("conf_candidate", "Potential confounder", choices = c("Choose a variable" = "", vars)),
        gtx_inline_help("The app screens every selected exposure against this candidate. A change-in-estimate result cannot establish causality; confirm decisions with a DAG and subject-matter knowledge."),
        actionButton("run_confounder", icon("magnifying-glass-chart"), " Assess confounding", class = "btn-primary btn-block")
      ),
      interaction = tagList(
        model_roles(
          include_covariates = TRUE,
          covariate_label = "Covariates included in both interaction models"
        ),
        selectInput("conf_exposure", "Exposure of interest", choices = c("Choose a variable" = "", vars)),
        selectInput("conf_candidate", "Possible effect modifier", choices = c("Choose a variable" = "", vars)),
        gtx_inline_help("Covariates are included in both the model with and without the interaction. A small interaction p-value suggests that the exposure effect may differ between groups; interpret stratum-specific estimates as well."),
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
    ctx <- selection_context("med_inputs")
    tagList(
      selectInput("med_exposure", "Exposure", choices = c("", vars), selected = keep_sel(ctx, "med_exposure", vars)),
      selectInput("med_mediator", "Mediator", choices = c("", vars), selected = keep_sel(ctx, "med_mediator", vars)),
      selectInput("med_outcome", "Outcome", choices = c("", vars), selected = keep_sel(ctx, "med_outcome", vars)),
      checkboxGroupInput("med_covariates", "Covariates", choices = vars, selected = keep_sel(ctx, "med_covariates", vars, multiple = TRUE)),
      gtx_select_buttons("med_covariates")
    )
  })

  observeEvent(input$run_desc, {
    data <- require_analysis_data()
    req(data, input$desc_exposures)
    if (!check_roles(
      roles = list(`Group by` = if (gtx_has_text(input$desc_by)) input$desc_by else NULL),
      lists = list(list(id = "desc_exposures", label = "Variables to summarise", value = input$desc_exposures)),
      run_button = "run_desc",
      context = "the descriptive table"
    )) return(NULL)
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
        showNotification(gtx_friendly_error(e), type = "error")
        NULL
      }
    )
    if (!is.null(rv$desc)) {
      rv$last_message <- "Run univariable regression, survival analysis, or export the descriptive table."
      showNotification("Descriptive table ready. Run univariable models when you are ready.", type = "message")
    }
  })

  output$desc_table <- gtx_render_table(function() rv$desc, stale = is_stale("desc"))
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
    lists <- list(list(id = "reg_exposures", label = "Exposures", value = input$reg_exposures))
    if (kind == "multi") {
      lists[[2]] <- list(id = "reg_adjust", label = "Adjust for", value = input$reg_adjust %||% character(0))
    }
    if (!check_roles(
      roles = list(Outcome = input$reg_outcome),
      lists = lists,
      run_button = if (kind == "uni") "run_uni" else "run_multi",
      context = if (kind == "uni") "univariable regression" else "multivariable regression"
    )) return(NULL)
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
    rv$uni <- tryCatch(run_regression("uni"), error = function(e) { showNotification(gtx_friendly_error(e), type = "error"); NULL })
    if (!is.null(rv$uni)) {
      rv$last_message <- "Choose final variables, then run the multivariable model or visualise crude estimates."
      updateTabsetPanel(session, "reg_output_tab", selected = "Univariate")
      showNotification("Univariable table ready. Choose final variables for multivariable analysis.", type = "message")
    }
  })

  observeEvent(input$run_multi, {
    rv$multi <- tryCatch(run_regression("multi"), error = function(e) { showNotification(gtx_friendly_error(e), type = "error"); NULL })
    if (!is.null(rv$multi)) {
      rv$last_message <- "Merge tables, visualise estimates, check diagnostics, or export outputs."
      updateTabsetPanel(session, "reg_output_tab", selected = "Multivariable")
      showNotification("Multivariable table ready. Merge tables or visualise results.", type = "message")
    }
  })

  observeEvent(input$run_strat, {
    data <- require_analysis_data()
    req(data, input$reg_outcome, input$reg_exposures)
    if (!gtx_has_text(input$reg_stratifier)) {
      showNotification("Choose a stratifier first.", type = "warning")
      return(NULL)
    }
    if (!check_roles(
      roles = list(Outcome = input$reg_outcome, Stratifier = input$reg_stratifier),
      lists = strata_role_lists(),
      run_button = "run_strat",
      context = "stratified regression"
    )) {
      rv$strat <- NULL
      return(NULL)
    }
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
      error = function(e) { showNotification(gtx_friendly_error(e), type = "error"); NULL }
    )
    if (!is.null(rv$strat)) {
      rv$last_message <- "Review strata-specific estimates, then export the table or use forest_reg()."
      updateTabsetPanel(session, "reg_output_tab", selected = "Stratified")
      showNotification("Stratified table ready. Review strata-specific estimates.", type = "message")
    }
  })

  output$uni_table <- gtx_render_table(function() rv$uni, stale = is_stale("uni"))
  output$multi_table <- gtx_render_table(function() rv$multi, stale = is_stale("multi"))
  output$strat_table <- gtx_render_table(function() rv$strat, stale = is_stale("strat"))
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

  special_function <- reactive({
    switch(
      input$special_mode %||% "ordinal",
      ordinal = "ordinal_reg",
      multinomial = "multinomial_reg",
      multilevel = "multilevel_reg",
      matched = "conditional_logit_reg"
    )
  })

  special_arguments <- reactive({
    mode <- input$special_mode %||% "ordinal"
    list(
      data = special_data(),
      outcome = input$special_outcome,
      exposures = input$special_exposures,
      adjust_for = if (length(input$special_adjust %||% character(0))) {
        input$special_adjust
      } else NULL,
      group = if (mode == "multilevel") input$special_group else NULL,
      matched_set = if (mode == "matched") input$special_matched_set else NULL,
      approach = if (mode == "multilevel") {
        input$special_cluster_approach %||% "linear"
      } else NULL,
      multivariable = if (mode %in% c("ordinal", "multinomial", "matched")) {
        isTRUE(input$special_multivariable)
      } else NULL,
      assumption_test = if (mode == "ordinal") {
        isTRUE(input$special_assumption_test)
      } else NULL,
      show_ref = isTRUE(input$special_show_ref),
      model_stats = isTRUE(input$special_model_stats),
      format = "flextable"
    )
  })

  special_result_is_multi <- reactive({
    identical(input$special_mode %||% "ordinal", "multilevel") ||
      isTRUE(input$special_multivariable)
  })

  special_call_text <- function(result_name = "special_model") {
    req(input$special_outcome, input$special_exposures)
    mode <- input$special_mode %||% "ordinal"
    args <- list(
      data = "analysis_data",
      outcome = gtx_null_code(input$special_outcome),
      exposures = gtx_vec_code(input$special_exposures),
      adjust_for = gtx_nonempty_vec_code(input$special_adjust %||% character(0)),
      group = if (mode == "multilevel") gtx_null_code(input$special_group) else NULL,
      matched_set = if (mode == "matched") {
        gtx_null_code(input$special_matched_set)
      } else NULL,
      approach = if (mode == "multilevel") {
        shQuote(input$special_cluster_approach %||% "linear")
      } else NULL,
      multivariable = if (mode %in% c("ordinal", "multinomial", "matched")) {
        gtx_bool_code(input$special_multivariable)
      } else NULL,
      assumption_test = if (mode == "ordinal") {
        gtx_bool_code(input$special_assumption_test)
      } else NULL,
      show_ref = gtx_bool_code(input$special_show_ref),
      model_stats = gtx_bool_code(input$special_model_stats),
      format = shQuote("flextable")
    )
    gtx_code_assign(result_name, special_function(), args)
  }

  special_code_snapshot <- function(result_name) {
    paste(special_code_prefix(), special_call_text(result_name), sep = "\n\n")
  }

  observeEvent(input$run_special, {
    data <- require_analysis_data()
    req(data, input$special_outcome, input$special_exposures)
    mode <- input$special_mode %||% "ordinal"
    if (mode == "multilevel") req(input$special_group)
    if (mode == "matched") req(input$special_matched_set)
    if (!check_roles(
      roles = list(
        Outcome = input$special_outcome,
        `Grouping variable` = if (mode == "multilevel") input$special_group else NULL,
        `Matched-set identifier` = if (mode == "matched") input$special_matched_set else NULL
      ),
      lists = list(
        list(id = "special_exposures", label = "Exposures", value = input$special_exposures),
        list(id = "special_adjust", label = "Adjust for", value = input$special_adjust %||% character(0))
      ),
      run_button = "run_special",
      context = "this model"
    )) return(NULL)
    is_multi <- special_result_is_multi()
    result_name <- if (is_multi) "special_multi_result" else "special_exposure_result"
    code_snapshot <- special_code_snapshot(result_name)
    result <- run_safely(
      "Special model",
      do.call(gtx_call, c(list(special_function()), special_arguments())),
      on_error = function() {
        if (is_multi) rv$special_multi <- NULL else rv$special_exposure <- NULL
      }
    )
    if (!is.null(result)) {
      rv$special <- result
      if (is_multi) {
        rv$special_multi <- result
        rv$special_multi_code <- code_snapshot
      } else {
        rv$special_exposure <- result
        rv$special_exposure_code <- code_snapshot
      }
      rv$last_message <- "Review the model table, diagnostics, and generated code before exporting."
      selected_tab <- if (is_multi) "Multivariable Model" else "Exposure Models"
      updateTabsetPanel(session, "special_output_tab", selected = selected_tab)
      showNotification(paste0(selected_tab, " table ready."), type = "message")
    }
  })

  output$special_exposure_table <- gtx_render_table(function() rv$special_exposure, stale = is_stale("special_exposure"))
  output$special_multi_table <- gtx_render_table(function() rv$special_multi, stale = is_stale("special_multi"))
  gtx_table_downloads(output, "special_exposure", reactive(rv$special_exposure))
  gtx_table_downloads(output, "special_multi", reactive(rv$special_multi))

  special_diagnostic_choices <- reactive({
    choices <- character(0)
    if (!is.null(rv$special_exposure)) {
      choices["Exposure Models"] <- "exposure"
    }
    if (!is.null(rv$special_multi)) {
      choices["Multivariable Model"] <- "multi"
    }
    choices
  })

  special_diagnostic_result <- function(input_id) {
    choices <- special_diagnostic_choices()
    req(length(choices))
    selected <- input[[input_id]] %||% unname(choices[[1L]])
    if (!selected %in% unname(choices)) selected <- unname(choices[[1L]])
    if (identical(selected, "multi")) rv$special_multi else rv$special_exposure
  }

  special_diagnostic_selector <- function(input_id) {
    choices <- special_diagnostic_choices()
    if (length(choices) < 2L) return(NULL)
    radioButtons(
      input_id,
      "Result to inspect",
      choices = choices,
      selected = unname(choices[[1L]]),
      inline = TRUE
    )
  }

  output$special_stats_panel <- renderUI({
    result <- special_diagnostic_result("special_stats_result")
    content <- if (!is.null(result$model_stats)) {
      gtx_data_output("special_stats")
    } else {
      div(
        class = "gtx-note",
        "Model statistics were not stored for this result. Rerun it with Store model statistics selected."
      )
    }
    div(
      class = "gtx-card",
      special_diagnostic_selector("special_stats_result"),
      content
    )
  })

  output$special_stats <- gtx_render_data(function() {
    result <- special_diagnostic_result("special_stats_result")
    req(result$model_stats)
    result$model_stats
  })

  output$special_assumptions_panel <- renderUI({
    result <- special_diagnostic_result("special_assumptions_result")
    content <- if (!is.null(result$assumption_tests)) {
      gtx_data_output("special_assumptions")
    } else {
      div(
        class = "gtx-note",
        "No proportional-odds assumption result is available. This check applies only to ordinal models fitted with the check selected."
      )
    }
    div(
      class = "gtx-card",
      div(class = "gtx-help", "Available for proportional-odds ordinal models."),
      special_diagnostic_selector("special_assumptions_result"),
      content
    )
  })

  output$special_assumptions <- gtx_render_data(function() {
    result <- special_diagnostic_result("special_assumptions_result")
    req(result$assumption_tests)
    result$assumption_tests
  })

  special_code_text <- reactive({
    result_name <- if (special_result_is_multi()) {
      "special_multi_result"
    } else {
      "special_exposure_result"
    }
    special_code_snapshot(result_name)
  })
  output$special_code <- renderText(special_code_text())

  parse_limits <- function(x) {
    if (!gtx_has_text(x)) return(NULL)
    vals <- suppressWarnings(as.numeric(strsplit(x, ",")[[1]]))
    if (length(vals) == 2 && all(!is.na(vals))) vals else NULL
  }

  surv_role_lists <- function(include_adjust = TRUE) {
    lists <- list(list(id = "surv_exposures", label = "Exposures", value = input$surv_exposures %||% character(0)))
    if (include_adjust) {
      lists[[2]] <- list(id = "surv_adjust", label = "Adjust for", value = input$surv_adjust %||% character(0))
    }
    lists
  }

  observeEvent(input$run_cox, {
    data <- require_analysis_data()
    req(data, input$surv_time, input$surv_event, input$surv_exposures)
    if (!check_roles(
      roles = list(Time = input$surv_time, Event = input$surv_event,
                   Stratifier = if (gtx_has_text(input$surv_stratifier)) input$surv_stratifier else NULL),
      lists = surv_role_lists(),
      run_button = "run_cox",
      context = "Cox regression"
    )) return(NULL)
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
        model_stats = input$reg_model_stats,
        format = "flextable"
      ),
      error = function(e) { showNotification(gtx_friendly_error(e), type = "error"); NULL }
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
    if (!check_roles(
      roles = list(Time = input$surv_time, Event = input$surv_event,
                   Stratifier = if (gtx_has_text(input$surv_stratifier)) input$surv_stratifier else NULL),
      lists = surv_role_lists(),
      run_button = "run_survreg",
      context = "parametric survival regression"
    )) return(NULL)
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
        model_stats = input$reg_model_stats,
        format = "flextable"
      ),
      error = function(e) { showNotification(gtx_friendly_error(e), type = "error"); NULL }
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

  observeEvent(input$run_competing, {
    data <- require_analysis_data()
    req(
      data, input$surv_time, input$surv_event, input$surv_exposures,
      input$competing_event_of_interest, input$competing_censor_code
    )
    if (!check_roles(
      roles = list(Time = input$surv_time, Event = input$surv_event),
      lists = surv_role_lists(include_adjust = !isTRUE(input$surv_multivariable)),
      run_button = "run_competing",
      context = "competing-risk regression"
    )) return(NULL)
    result <- run_safely(
      "Fine-Gray competing-risk regression",
      gtx_call(
        "competing_risk_reg",
        data = data,
        time = input$surv_time,
        event = input$surv_event,
        event_of_interest = input$competing_event_of_interest,
        censor_code = input$competing_censor_code,
        exposures = input$surv_exposures,
        adjust_for = if (isTRUE(input$surv_multivariable)) NULL else input$surv_adjust,
        multivariable = input$surv_multivariable,
        show_ref = input$surv_show_ref,
        model_stats = input$reg_model_stats,
        format = "flextable"
      ),
      on_error = function() rv$competing <- NULL
    )
    if (!is.null(result)) {
      rv$competing <- result
      rv$last_message <- "Draw cumulative incidence curves or export the Fine-Gray table."
      updateTabsetPanel(session, "surv_output_tab", selected = "Model Result")
      showNotification("Fine-Gray regression table ready.", type = "message")
    }
  })

  observeEvent(input$run_cuminc, {
    data <- require_analysis_data()
    req(
      data, input$surv_time, input$surv_event,
      input$competing_event_of_interest, input$competing_censor_code
    )
    if (!check_roles(
      roles = list(Time = input$surv_time, Event = input$surv_event,
                   `Group by` = if (gtx_has_text(input$cuminc_by)) input$cuminc_by else NULL),
      context = "the cumulative-incidence plot"
    )) return(NULL)
    rv$cuminc <- run_safely(
      "Cumulative-incidence plot",
      gtx_call(
        "cuminc_plot",
        data = data,
        time = input$surv_time,
        event = input$surv_event,
        event_of_interest = input$competing_event_of_interest,
        censor_code = input$competing_censor_code,
        by = if (gtx_has_text(input$cuminc_by)) input$cuminc_by else NULL,
        conf.int = input$cuminc_conf,
        p_value = input$cuminc_p,
        break_time_by = gtx_km_break_value(input$cuminc_break),
        xlim = parse_limits(input$cuminc_xlim),
        ylim = parse_limits(input$cuminc_ylim),
        y_percent = input$cuminc_y_percent,
        theme = input$cuminc_theme,
        grid = input$cuminc_grid
      ),
      on_error = function() rv$cuminc <- NULL
    )
    if (!is.null(rv$cuminc)) {
      rv$last_message <- "Review absolute event probability and Gray's test, then export the plot."
      updateTabsetPanel(session, "surv_output_tab", selected = "Cumulative Incidence")
      showNotification("Cumulative-incidence plot ready.", type = "message")
    }
  })

  observeEvent(input$run_km, {
    data <- require_analysis_data()
    req(data, input$surv_time, input$surv_event)
    if (!check_roles(
      roles = list(Time = input$surv_time, Event = input$surv_event,
                   `Group by` = if (gtx_has_text(input$km_by)) input$km_by else NULL),
      context = "the Kaplan-Meier plot"
    )) return(NULL)
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
      error = function(e) { showNotification(gtx_friendly_error(e), type = "error"); NULL }
    )
    if (!is.null(rv$km)) {
      rv$last_message <- "Tune x/y limits for publication or export the plot."
      updateTabsetPanel(session, "surv_output_tab", selected = "KM Plot")
      showNotification("KM plot ready. Tune x/y limits or export.", type = "message")
    }
  })

  output$cox_exposure_table <- gtx_render_table(function() rv$cox_exposure, stale = is_stale("cox_exposure"))
  output$cox_multi_table <- gtx_render_table(function() rv$cox_multi, stale = is_stale("cox_multi"))
  output$survreg_exposure_table <- gtx_render_table(function() rv$survreg_exposure, stale = is_stale("survreg_exposure"))
  output$survreg_multi_table <- gtx_render_table(function() rv$survreg_multi, stale = is_stale("survreg_multi"))
  output$competing_table <- gtx_render_table(function() rv$competing, stale = is_stale("competing"))
  output$km_plot <- renderPlot({ req(rv$km); print(rv$km) })
  output$cuminc_plot <- renderPlot({ req(rv$cuminc); print(rv$cuminc) })
  gtx_table_downloads(output, "cox_exposure", reactive(rv$cox_exposure))
  gtx_table_downloads(output, "cox_multi", reactive(rv$cox_multi))
  gtx_table_downloads(output, "survreg_exposure", reactive(rv$survreg_exposure))
  gtx_table_downloads(output, "survreg_multi", reactive(rv$survreg_multi))
  gtx_table_downloads(output, "competing", reactive(rv$competing))
  gtx_plot_downloads(output, "km", reactive(rv$km))
  gtx_plot_downloads(output, "cuminc", reactive(rv$cuminc))

  output$logrank_table <- gtx_render_table(function() {
    req(analysis_data(), input$surv_time, input$surv_event, gtx_has_text(input$km_by))
    gtregression::logrank_test(analysis_data(), time = input$surv_time, event = input$surv_event, by = input$km_by, format = "flextable")
  })
  output$surv_summary_table <- gtx_render_table(function() {
    req(analysis_data(), input$surv_time, input$surv_event)
    gtregression::survival_summary(analysis_data(), time = input$surv_time, event = input$surv_event, by = if (gtx_has_text(input$km_by)) input$km_by else NULL, format = "flextable")
  })
  # ---- Trials: risk difference ---------------------------------------------
  output$trial_inputs <- renderUI({
    req(analysis_data())
    vars <- names(analysis_data())
    ctx <- selection_context("trial_inputs")
    tagList(
      selectInput("trial_outcome", "Outcome", choices = c("", vars), selected = keep_sel(ctx, "trial_outcome", vars)),
      selectInput("trial_exposure", "Treatment group", choices = c("", vars), selected = keep_sel(ctx, "trial_exposure", vars)),
      checkboxGroupInput("trial_adjust", "Adjust for", choices = vars, selected = keep_sel(ctx, "trial_adjust", vars, multiple = TRUE))
    )
  })

  trial_values <- function(variable, role) {
    data <- analysis_data()
    if (is.null(data) || !gtx_has_text(variable) || !variable %in% names(data)) return(NULL)
    gtx_trial_values(data[[variable]], variable, role)
  }

  # Event and Reference group are choices among the two observed values of
  # the chosen variable. A choice is kept while it is still for the same
  # variable (or that variable was renamed); a different variable starts from
  # the risk_difference() default (second value = event, first = reference).
  trial_level_memory <- new.env(parent = emptyenv())
  trial_level_input <- function(id, label, variable, role, default_index) {
    info <- trial_values(variable, role)
    if (is.null(info)) return(NULL)
    if (!identical(info$n, 2L)) {
      thing <- if (identical(role, "outcome")) "values" else "groups"
      problem <- if (identical(info$kind, "unsupported")) {
        paste0(variable, " cannot be used here.")
      } else if (info$n < 2L) {
        paste0(variable, " has only one observed ", sub("s$", "", thing), ".")
      } else if (identical(role, "outcome")) {
        paste0(variable, " has ", info$n, " values; the outcome must be binary.")
      } else {
        paste0(variable, " has ", info$n, " groups; exactly two are compared.")
      }
      return(div(
        class = "gta-trial-problem", `data-gta-problem` = problem,
        span(class = "gta-slot-label", label),
        span(class = "gta-trial-problem-text", if (info$n > 2L) paste0("Needs two ", thing) else "Not usable")
      ))
    }
    previous <- trial_level_memory[[id]]
    trial_level_memory[[id]] <- variable
    current <- isolate(input[[id]])
    same <- !is.null(previous) && (identical(previous, variable) || !previous %in% names(isolate(analysis_data())))
    selected <- if (same && gtx_has_text(current) && current %in% info$values) current else info$values[[default_index]]
    selectInput(id, label, choices = info$values, selected = selected)
  }

  output$trial_event_input <- renderUI({
    trial_level_input("trial_event", "Event", input$trial_outcome, "outcome", 2L)
  })

  output$trial_reference_input <- renderUI({
    trial_level_input("trial_reference", "Reference group", input$trial_exposure, "exposure", 1L)
  })

  # The Event and Reference group choices that belong to the variables chosen
  # now (never a value left over from another variable).
  trial_choice <- function(id, variable, role) {
    info <- trial_values(variable, role)
    value <- input[[id]]
    if (is.null(info) || !identical(info$n, 2L) || !gtx_has_text(value) || !value %in% info$values) return(NULL)
    list(value = value, kind = info$kind, other = setdiff(info$values, value))
  }

  output$trial_preflight <- renderUI({
    data <- analysis_data()
    event <- trial_choice("trial_event", input$trial_outcome, "outcome")
    reference <- trial_choice("trial_reference", input$trial_exposure, "exposure")
    if (is.null(data) || is.null(event) || is.null(reference)) {
      return(div(class = "gtx-preflight", strong("Before running: "), "choose a binary outcome and a variable with two groups."))
    }
    adjustment <- intersect(input$trial_adjust %||% character(0), names(data))
    used <- unique(c(input$trial_outcome, input$trial_exposure, adjustment))
    complete <- sum(stats::complete.cases(data[, used, drop = FALSE]))
    method <- if (length(adjustment)) {
      paste0(
        "adjusted for ", length(adjustment), " variable", if (length(adjustment) == 1L) "" else "s",
        " by logistic standardisation (Wald interval, influence-function standard error)"
      )
    } else {
      "unadjusted (Miettinen-Nurminen score interval and test)"
    }
    div(
      class = "gtx-preflight",
      strong("Ready to run: "),
      paste0(
        "risk of ", input$trial_outcome, " = ", event$value, "; ",
        reference$other, " minus ", reference$value, " (", input$trial_exposure, "); ",
        method, "."
      ),
      tags$br(),
      span(paste0(
        format(complete, big.mark = ","), " of ", format(nrow(data), big.mark = ","),
        " rows have no missing values in these variables and are analysed."
      )),
      if (gtx_has_text(input$trial_event_type)) tagList(
        tags$br(),
        span(paste0("Number needed to treat is reported, with the event treated as ", input$trial_event_type, "."))
      )
    )
  })

  trial_call_code <- reactive({
    event <- trial_choice("trial_event", input$trial_outcome, "outcome")
    reference <- trial_choice("trial_reference", input$trial_exposure, "exposure")
    level <- input$trial_conf_level
    gtx_code_assign(
      "rd_result",
      "risk_difference",
      list(
        data = "analysis_data",
        outcome = gtx_null_code(input$trial_outcome),
        exposure = gtx_null_code(input$trial_exposure),
        adjust_for = gtx_nonempty_vec_code(input$trial_adjust %||% character(0)),
        reference = if (is.null(reference)) NULL else gtx_trial_literal(reference$value, reference$kind),
        event = if (is.null(event)) NULL else gtx_trial_literal(event$value, event$kind),
        event_type = if (gtx_has_text(input$trial_event_type)) gtx_null_code(input$trial_event_type) else NULL,
        conf_level = if (length(level) == 1L && !is.na(level) && !isTRUE(all.equal(level, 0.95))) format(level, digits = 15) else NULL,
        format = shQuote("flextable")
      )
    )
  })

  observeEvent(input$run_trial, {
    data <- require_analysis_data()
    req(data, input$trial_outcome, input$trial_exposure)
    if (!check_roles(
      roles = list(Outcome = input$trial_outcome, `Treatment group` = input$trial_exposure),
      lists = list(list(id = "trial_adjust", label = "Adjust for", value = input$trial_adjust %||% character(0))),
      run_button = "run_trial",
      context = "the risk difference"
    )) {
      rv$trial <- NULL
      return(NULL)
    }
    event <- trial_choice("trial_event", input$trial_outcome, "outcome")
    reference <- trial_choice("trial_reference", input$trial_exposure, "exposure")
    rv$trial <- tryCatch(
      gtx_call(
        "risk_difference",
        data = data,
        outcome = input$trial_outcome,
        exposure = input$trial_exposure,
        adjust_for = if (length(input$trial_adjust)) input$trial_adjust else NULL,
        reference = reference$value,
        event = event$value,
        event_type = if (gtx_has_text(input$trial_event_type)) input$trial_event_type else NULL,
        conf_level = input$trial_conf_level %||% 0.95,
        format = "flextable"
      ),
      error = function(e) { showNotification(gtx_friendly_error(e), type = "error", duration = 10); NULL }
    )
    if (!is.null(rv$trial)) {
      rv$trial_code <- trial_call_code()
      rv$last_message <- "Export the risk difference table, or copy its R code."
      showNotification("Risk difference table ready.", type = "message")
    }
  })

  output$trial_table <- gtx_render_table(function() rv$trial, stale = is_stale("trial"))
  gtx_table_downloads(output, "risk_difference", reactive(rv$trial))
  output$trial_code <- renderText({
    paste(analysis_code_prefix(), trial_call_code(), sep = "\n\n")
  })

  output$surv_code <- renderText({
    competing_mode <- identical(input$surv_mode, "competing")
    ylim_code <- gtx_limits_code(parse_limits(
      if (competing_mode) input$cuminc_ylim else input$km_ylim
    ))
    xlim_code <- gtx_limits_code(parse_limits(
      if (competing_mode) input$cuminc_xlim else input$km_xlim
    ))
    result_name <- if (competing_mode) {
      "competing_result"
    } else if (identical(input$surv_mode, "survreg")) {
      if (isTRUE(input$surv_multivariable)) "survreg_multi_result" else "survreg_exposure_result"
    } else {
      if (isTRUE(input$surv_multivariable)) "cox_multi_result" else "cox_exposure_result"
    }
    model_code <- if (competing_mode) {
      gtx_code_assign(
        result_name,
        "competing_risk_reg",
        list(
          data = "analysis_data",
          time = gtx_null_code(input$surv_time),
          event = gtx_null_code(input$surv_event),
          event_of_interest = gtx_null_code(input$competing_event_of_interest),
          censor_code = gtx_null_code(input$competing_censor_code),
          exposures = gtx_vec_code(input$surv_exposures %||% character(0)),
          adjust_for = if (isTRUE(input$surv_multivariable)) NULL else {
            gtx_nonempty_vec_code(input$surv_adjust %||% character(0))
          },
          multivariable = gtx_bool_code(input$surv_multivariable),
          show_ref = gtx_bool_code(input$surv_show_ref),
          model_stats = gtx_bool_code(input$reg_model_stats),
          format = shQuote("flextable")
        )
      )
    } else if (identical(input$surv_mode, "survreg")) {
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
          model_stats = gtx_bool_code(input$reg_model_stats),
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
          model_stats = gtx_bool_code(input$reg_model_stats),
          format = shQuote("flextable")
        )
      )
    }
    plot_code <- if (competing_mode) {
      paste0(
        gtx_code_assign(
          "cuminc_result",
          "cuminc_plot",
          list(
            data = "analysis_data",
            time = gtx_null_code(input$surv_time),
            event = gtx_null_code(input$surv_event),
            event_of_interest = gtx_null_code(input$competing_event_of_interest),
            censor_code = gtx_null_code(input$competing_censor_code),
            by = gtx_null_code(input$cuminc_by),
            conf.int = gtx_bool_code(input$cuminc_conf),
            p_value = gtx_bool_code(input$cuminc_p),
            break_time_by = if (is.null(gtx_km_break_value(input$cuminc_break))) NULL else as.character(gtx_km_break_value(input$cuminc_break)),
            xlim = if (identical(xlim_code, "NULL")) NULL else xlim_code,
            ylim = if (identical(ylim_code, "NULL")) NULL else ylim_code,
            y_percent = gtx_bool_code(input$cuminc_y_percent),
            theme = shQuote(input$cuminc_theme),
            grid = gtx_bool_code(input$cuminc_grid)
          )
        ),
        "\n",
        "save_plot(cuminc_result, filename = \"cumulative_incidence.png\", width = 8, height = 6)"
      )
    } else {
      paste0(
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
    }
    paste0(
      analysis_code_prefix(),
      "\n\n",
      model_code,
      "\n\n",
      plot_code
    )
  })

  result_for <- function(source) {
    if (gtx_has_text(source) && grepl("^special_(exposure|multi)::", source)) {
      slot <- sub("::.*$", "", source)
      category <- sub("^[^:]+::", "", source)
      parent <- if (identical(slot, "special_multi")) rv$special_multi else rv$special_exposure
      return(parent$per_outcome[[category]])
    }
    if (!gtx_has_text(source)) return(NULL)
    if (grepl("^h[0-9]+$", source)) {
      entry <- history_entry_by_value(source)
      return(if (is.null(entry)) NULL else entry$result)
    }
    switch(source,
           uni = rv$uni, multi = rv$multi,
           special_exposure = rv$special_exposure,
           special_multi = rv$special_multi,
           cox_exposure = rv$cox_exposure, cox_multi = rv$cox_multi,
           survreg_exposure = rv$survreg_exposure, survreg_multi = rv$survreg_multi,
           competing = rv$competing,
           cox = rv$cox, survreg = rv$survreg, strat = rv$strat)
  }

  result_code_name <- function(source, fallback = "model_result") {
    if (!gtx_has_text(source)) return(fallback)
    if (grepl("^h[0-9]+$", source)) {
      entry <- history_entry_by_value(source)
      return(if (is.null(entry)) fallback else history_entry_name(entry))
    }
    if (grepl("^special_(exposure|multi)::", source)) {
      slot <- sub("::.*$", "", source)
      category <- sub("^[^:]+::", "", source)
      parent <- if (identical(slot, "special_multi")) {
        "special_multi_result"
      } else {
        "special_exposure_result"
      }
      return(paste0(parent, "$per_outcome[[", shQuote(category), "]]"))
    }
    names <- c(
      uni = "uni_result", multi = "multi_result",
      special_exposure = "special_exposure_result",
      special_multi = "special_multi_result",
      cox_exposure = "cox_exposure_result", cox_multi = "cox_multi_result",
      survreg_exposure = "survreg_exposure_result",
      survreg_multi = "survreg_multi_result", cox = "cox_result",
      survreg = "survreg_result", competing = "competing_result",
      strat = "stratified_result"
    )
    value <- unname(names[source])
    if (!length(value) || is.na(value)) fallback else value
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

  # Tables for Merge come from every result kept in this session (see
  # history_tables()); Modify also accepts risk-difference tables.
  available_merge_tables <- reactive({
    lapply(history_tables(), `[[`, "result")
  })

  available_merge_code_names <- reactive({
    vapply(history_tables(), `[[`, character(1), "code")
  })

  modify_tables <- reactive({
    history_tables(c(history_mergeable, "trial"))
  })

  modify_code_names <- reactive({
    vapply(modify_tables(), `[[`, character(1), "code")
  })

  output$merge_table_choices <- renderUI({
    tbls <- available_merge_tables()
    if (!length(tbls)) {
      return(div(class = "gtx-note", "No completed tables yet. Run descriptive or regression analyses first."))
    }
    latest <- names(Filter(function(x) isTRUE(x$current), history_tables()))
    checkboxGroupInput(
      "merge_selection",
      "Available tables",
      choices = stats::setNames(names(tbls), names(tbls)),
      selected = intersect(names(tbls), latest)
    )
  })

  output$modify_source_controls <- renderUI({
    tables <- lapply(modify_tables(), `[[`, "result")
    if (!is.null(rv$merged)) tables[["Merged table"]] <- rv$merged
    if (!length(tables)) {
      return(div(class = "gtx-note", "No completed table yet. Run a descriptive or regression analysis first."))
    }
    selectInput("modify_source", "Completed table", choices = stats::setNames(names(tables), names(tables)))
  })

  modify_source_object <- reactive({
    tables <- lapply(modify_tables(), `[[`, "result")
    if (!is.null(rv$merged)) tables[["Merged table"]] <- rv$merged
    tables[[input$modify_source %||% ""]]
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
    if (!is.null(rv$competing)) {
      choices["Fine-Gray competing-risk model"] <- "competing"
    }
    if (!is.null(rv$strat)) choices["Regression result (stratified)"] <- "strat"
    special_results <- list(
      "Special exposure models" = rv$special_exposure,
      "Special multivariable model" = rv$special_multi
    )
    for (special_label in names(special_results)) {
      result <- special_results[[special_label]]
      if (is.null(result)) next
      source <- if (identical(special_label, "Special multivariable model")) {
        "special_multi"
      } else {
        "special_exposure"
      }
      if (inherits(result, "multinomial_reg")) {
        for (category in names(result$per_outcome)) {
          label <- paste0(
            special_label, ": ", category, " vs ", result$outcome_reference
          )
          choices[label] <- paste0(source, "::", category)
        }
      } else {
        choices[special_label] <- source
      }
    }
    # Earlier runs kept in this session, newest first.
    plot_keys <- c("uni", "multi", "strat", "cox_exposure", "cox_multi", "survreg_exposure",
                   "survreg_multi", "competing", "special_exposure", "special_multi")
    for (e in rev(history_store())) {
      if (!e$key %in% plot_keys || identical(history_current[[e$key]], e$id)) next
      if (inherits(e$result, "multinomial_reg")) next
      choices[paste0("Earlier: ", history_entry_label(e, FALSE))] <- paste0("h", e$id)
    }
    choices
  })

  fit_plot_choices <- reactive({
    choices <- single_plot_choices()
    keep <- !grepl("^(special_(exposure|multi)(?:::|$)|competing$)", unname(choices))
    # Earlier results kept as tables only have no models to check.
    earlier <- grepl("^h[0-9]+$", unname(choices))
    keep[earlier] <- vapply(unname(choices)[earlier], function(v) {
      e <- history_entry_by_value(v)
      !is.null(e) && !isTRUE(e$compact) && e$key %in% c("uni", "multi", "strat", "cox_exposure", "cox_multi", "survreg_exposure", "survreg_multi")
    }, logical(1))
    choices[keep]
  })

  history_entry_by_value <- function(value) {
    if (!gtx_has_text(value) || !grepl("^h[0-9]+$", value)) return(NULL)
    id <- as.integer(sub("^h", "", value))
    Find(function(e) identical(e$id, id), history_store())
  }

  special_combined_choices <- reactive({
    exposure <- rv$special_exposure
    multi <- rv$special_multi
    if (!pair_can_combine(exposure, multi)) return(character(0))

    if (inherits(exposure, "multinomial_reg") &&
        inherits(multi, "multinomial_reg") &&
        identical(exposure$outcome, multi$outcome) &&
        identical(exposure$outcome_reference, multi$outcome_reference)) {
      categories <- intersect(
        names(exposure$per_outcome),
        names(multi$per_outcome)
      )
      labels <- paste0(
        "Multinomial exposure + multivariable: ", categories,
        " vs ", exposure$outcome_reference
      )
      return(stats::setNames(paste0("special_combined::", categories), labels))
    }

    compatible_class <- any(vapply(
      c("ordinal_reg", "conditional_logit_reg"),
      function(class_name) inherits(exposure, class_name) && inherits(multi, class_name),
      logical(1)
    ))
    if (!compatible_class || !identical(exposure$outcome, multi$outcome)) {
      return(character(0))
    }
    c("Special exposure + multivariable" = "special_combined")
  })

  special_combined_pair <- function(value) {
    if (!gtx_has_text(value) || !startsWith(value, "special_combined")) {
      return(NULL)
    }
    if (startsWith(value, "special_combined::")) {
      category <- sub("^special_combined::", "", value)
      return(list(
        rv$special_exposure$per_outcome[[category]],
        rv$special_multi$per_outcome[[category]]
      ))
    }
    list(rv$special_exposure, rv$special_multi)
  }

  special_combined_code_names <- function(value) {
    if (startsWith(value %||% "", "special_combined::")) {
      category <- sub("^special_combined::", "", value)
      suffix <- paste0("$per_outcome[[", shQuote(category), "]]")
      return(paste0(
        c("special_exposure_result", "special_multi_result"),
        suffix
      ))
    }
    c("special_exposure_result", "special_multi_result")
  }

  output$plot_content_controls <- renderUI({
    choices <- single_plot_choices()
    combined_ready <- pair_can_combine(rv$uni, rv$multi)
    cox_combined_ready <- pair_can_combine(rv$cox_exposure, rv$cox_multi)
    survreg_combined_ready <- pair_can_combine(rv$survreg_exposure, rv$survreg_multi)
    special_choices <- special_combined_choices()
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
          if (survreg_combined_ready) c("Parametric exposure + multivariable" = "survreg_combined"),
          special_choices
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
    special_choices <- special_combined_choices()
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
    forest_choices <- c(forest_choices, special_choices)
    if (desc_ready && length(special_choices)) {
      special_all <- sub("^special_combined", "special_all", special_choices)
      names(special_all) <- paste0("Descriptive + ", names(special_choices))
      forest_choices <- c(forest_choices, special_all)
    }
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
    choices <- fit_plot_choices()
    if (!length(choices)) {
      return(div(
        class = "gtx-note",
        "No compatible fitted model yet. Model-fit plots are available for standard regression and survival results."
      ))
    }
    selectInput("fit_source", "Fitted result", choices = choices, selected = unname(choices[1]))
  })

  observeEvent(input$visual_tool, {
    tab <- switch(
      input$visual_tool,
      merge = "Merged Table",
      modify = "Modified Table",
      plot = "plot_reg",
      forest = "forest_reg",
      fit = "Model fit",
      "Merged Table"
    )
    updateTabsetPanel(session, "visual_output_tab", selected = tab)
  }, ignoreInit = TRUE)

  observeEvent(input$run_plot_reg, {
    combined_layout <- input$plot_layout %in%
      c("combined", "cox_combined", "survreg_combined") ||
      startsWith(input$plot_layout %||% "", "special_combined")
    if (combined_layout) {
      pair <- if (startsWith(input$plot_layout, "special_combined")) {
        special_combined_pair(input$plot_layout)
      } else {
        switch(
          input$plot_layout,
          combined = list(rv$uni, rv$multi),
          cox_combined = list(rv$cox_exposure, rv$cox_multi),
          survreg_combined = list(rv$survreg_exposure, rv$survreg_multi)
        )
      }
      if (any(vapply(pair, is.null, logical(1)))) {
        showNotification("Run both the exposure models and multivariable model before creating the combined plot.", type = "warning")
        return(NULL)
      }
      rv$plotreg <- tryCatch(
        gtregression::plot_reg_combine(pair[[1]], pair[[2]], log_x = input$plot_log_x),
        error = function(e) { showNotification(gtx_friendly_error(e), type = "error"); NULL }
      )
    } else {
      if (is.null(selected_result())) {
        showNotification("Run a regression or survival model first, then create the plot.", type = "warning")
        return(NULL)
      }
      rv$plotreg <- tryCatch(
        gtregression::plot_reg(selected_result(), log_x = input$plot_log_x),
        error = function(e) { showNotification(gtx_friendly_error(e), type = "error"); NULL }
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
      error = function(e) { showNotification(gtx_friendly_error(e), type = "error"); NULL }
    )
    if (!is.null(rv$merged)) {
      rv$last_message <- "Download DOCX/HTML/PDF or apply modify_table() in RStudio."
      showNotification("Merged table ready. Tip: use show_ref = TRUE in regression tables when you want reference rows displayed.", type = "message")
    }
  })

  parse_named_mapping <- function(text, argument) {
    lines <- trimws(unlist(strsplit(text %||% "", "\n", fixed = TRUE)))
    lines <- lines[nzchar(lines)]
    if (!length(lines)) return(NULL)
    pieces <- strsplit(lines, "=", fixed = TRUE)
    valid <- vapply(pieces, function(x) {
      length(x) == 2L && nzchar(trimws(x[[1L]])) && nzchar(trimws(x[[2L]]))
    }, logical(1))
    if (!all(valid)) stop(paste0(argument, " must use one `name = label` mapping per line."), call. = FALSE)
    keys <- trimws(vapply(pieces, `[[`, character(1), 1L))
    values <- trimws(vapply(pieces, `[[`, character(1), 2L))
    if (anyDuplicated(keys)) stop(paste0(argument, " cannot repeat a name."), call. = FALSE)
    stats::setNames(values, keys)
  }

  parse_level_mapping <- function(text) {
    lines <- trimws(unlist(strsplit(text %||% "", "\n", fixed = TRUE)))
    lines <- lines[nzchar(lines)]
    if (!length(lines)) return(NULL)
    parsed <- lapply(lines, function(line) {
      sides <- strsplit(line, "=", fixed = TRUE)[[1L]]
      if (length(sides) != 2L) return(NULL)
      left <- strsplit(trimws(sides[[1L]]), ":", fixed = TRUE)[[1L]]
      if (length(left) != 2L || !all(nzchar(trimws(left))) || !nzchar(trimws(sides[[2L]]))) return(NULL)
      list(variable = trimws(left[[1L]]), level = trimws(left[[2L]]), label = trimws(sides[[2L]]))
    })
    if (any(vapply(parsed, is.null, logical(1)))) {
      stop("Level labels must use `variable: current level = new level` on each line.", call. = FALSE)
    }
    variables <- vapply(parsed, `[[`, character(1), "variable")
    levels <- vapply(parsed, `[[`, character(1), "level")
    if (anyDuplicated(paste(variables, levels, sep = "\r"))) {
      stop("Level labels cannot repeat the same variable and level.", call. = FALSE)
    }
    result <- lapply(unique(variables), function(variable) {
      matches <- which(variables == variable)
      stats::setNames(vapply(parsed[matches], `[[`, character(1), "label"), levels[matches])
    })
    stats::setNames(result, unique(variables))
  }

  observeEvent(input$run_modify_table, {
    source <- modify_source_object()
    if (is.null(source)) {
      showNotification("Choose a completed table to modify.", type = "warning")
      return(NULL)
    }
    rv$modified <- tryCatch(
      gtregression::modify_table(
        source,
        variable_labels = parse_named_mapping(input$modify_variable_labels, "Variable labels"),
        level_labels = parse_level_mapping(input$modify_level_labels),
        header_labels = parse_named_mapping(input$modify_header_labels, "Header labels"),
        caption = if (gtx_has_text(input$modify_caption)) input$modify_caption else NULL,
        bold_labels = isTRUE(input$modify_bold_labels),
        bold_levels = isTRUE(input$modify_bold_levels),
        italic_labels = isTRUE(input$modify_italic_labels),
        italic_levels = isTRUE(input$modify_italic_levels),
        remove_N = isTRUE(input$modify_remove_n),
        remove_N_obs = isTRUE(input$modify_remove_n_obs),
        remove_abbreviations = isTRUE(input$modify_remove_abbreviations),
        remove_adjustment_note = isTRUE(input$modify_remove_adjustment_note),
        caveat = if (gtx_has_text(input$modify_caveat)) input$modify_caveat else NULL
      ),
      error = function(e) {
        showNotification(gtx_friendly_error(e), type = "error")
        NULL
      }
    )
    if (!is.null(rv$modified)) {
      rv$last_message <- "Modified table ready. Download it or keep refining its wording."
      showNotification("Modified table ready.", type = "message")
    }
  })

  observeEvent(input$run_forest, {
    rv$forest <- tryCatch({
      forest_xlim <- gtx_parse_numeric_vector(input$forest_xlim, expected = 2, name = "forest x limits")
      forest_ticks <- gtx_parse_numeric_vector(input$forest_ticks, name = "forest tick marks")
      ci_width <- input$forest_ci_width %||% 20
      combined_forest <- input$forest_content %in%
        c("combined", "all", "cox_combined", "cox_all", "survreg_combined", "survreg_all") ||
        grepl("^special_(combined|all)", input$forest_content %||% "")
      if (combined_forest) {
        pair <- if (grepl("^special_(combined|all)", input$forest_content)) {
          special_value <- sub("^special_all", "special_combined", input$forest_content)
          special_combined_pair(special_value)
        } else {
          switch(
            input$forest_content,
            combined =, all = list(rv$uni, rv$multi),
            cox_combined =, cox_all = list(rv$cox_exposure, rv$cox_multi),
            survreg_combined =, survreg_all = list(rv$survreg_exposure, rv$survreg_multi)
          )
        }
        if (any(vapply(pair, is.null, logical(1)))) {
          stop("Run both the exposure models and multivariable model before creating the combined forest plot.", call. = FALSE)
        }
        include_desc <- input$forest_content %in% c("all", "cox_all", "survreg_all") ||
          startsWith(input$forest_content, "special_all")
        desc <- if (include_desc) rv$desc else NULL
        fd <- gtregression::forest_df(pair[[1]], pair[[2]], desc = desc)
      } else {
        source <- result_for(input$forest_source)
        if (is.null(source)) {
          stop("Choose a completed result for the forest plot. Run a regression or survival model first.", call. = FALSE)
        }
        fd <- gtregression::forest_df(source)
      }
      gtregression::forest_reg(
        fd,
        side = input$forest_side,
        ci_col_width = ci_width,
        xlim = forest_xlim,
        ticks_at = forest_ticks
      )
    }, error = function(e) { showNotification(gtx_friendly_error(e), type = "error"); NULL })
    if (!is.null(rv$forest)) {
      rv$last_message <- "Export with Wide or Many rows preset if the browser preview is cramped."
      showNotification("Forest preview ready. For publication, export with a wider preset or use save_forest() code.", type = "message")
    }
  })

  observeEvent(input$run_fit_plot, {
    fit_result <- result_for(input$fit_source)
    if (is.null(fit_result)) {
      showNotification("Run a regression or survival model first, then check its fit.", type = "warning")
      return(NULL)
    }
    rv$fitplot <- tryCatch(
      gtregression::plot_model_fit(
        fit_result,
        model_name = if (gtx_has_text(input$fit_model_name)) input$fit_model_name else NULL,
        type = input$fit_type,
        bins = input$fit_bins %||% 10,
        base_size = input$fit_base_size
      ),
      error = function(e) { showNotification(gtx_friendly_error(e), type = "error"); NULL }
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
  output$modified_table <- gtx_render_table(function() rv$modified)
  gtx_table_downloads(output, "merged", reactive(rv$merged))
  gtx_table_downloads(output, "modified", reactive(rv$modified))
  gtx_plot_downloads(output, "plotreg", reactive(rv$plotreg), size_get = reactive(input$export_preset %||% "standard"))
  gtx_plot_downloads(output, "forest", reactive(rv$forest), is_forest = TRUE, size_get = reactive(input$export_preset %||% "standard"))
  gtx_plot_downloads(output, "fitplot", reactive(rv$fitplot), size_get = reactive(input$export_preset %||% "standard"))
  output$visual_code <- renderText({
    if (identical(input$visual_tool, "modify")) {
      source_name <- unname(modify_code_names()[input$modify_source %||% ""])
      if (identical(input$modify_source, "Merged table")) source_name <- "merged_table"
      if (!length(source_name) || is.na(source_name)) source_name <- "table_result"
      object_code <- function(x) paste(utils::capture.output(dput(x)), collapse = "")
      variable_labels <- tryCatch(parse_named_mapping(input$modify_variable_labels, "Variable labels"), error = function(e) NULL)
      level_labels <- tryCatch(parse_level_mapping(input$modify_level_labels), error = function(e) NULL)
      header_labels <- tryCatch(parse_named_mapping(input$modify_header_labels, "Header labels"), error = function(e) NULL)
      return(paste0(
        "modified_table <- modify_table(\n",
        "  ", source_name,
        if (!is.null(variable_labels)) paste0(",\n  variable_labels = ", object_code(variable_labels)) else "",
        if (!is.null(level_labels)) paste0(",\n  level_labels = ", object_code(level_labels)) else "",
        if (!is.null(header_labels)) paste0(",\n  header_labels = ", object_code(header_labels)) else "",
        if (gtx_has_text(input$modify_caption)) paste0(",\n  caption = ", object_code(input$modify_caption)) else "",
        ",\n  bold_labels = ", gtx_bool_code(input$modify_bold_labels),
        ",\n  bold_levels = ", gtx_bool_code(input$modify_bold_levels),
        ",\n  italic_labels = ", gtx_bool_code(input$modify_italic_labels),
        ",\n  italic_levels = ", gtx_bool_code(input$modify_italic_levels),
        ",\n  remove_N = ", gtx_bool_code(input$modify_remove_n),
        ",\n  remove_N_obs = ", gtx_bool_code(input$modify_remove_n_obs),
        ",\n  remove_abbreviations = ", gtx_bool_code(input$modify_remove_abbreviations),
        ",\n  remove_adjustment_note = ", gtx_bool_code(input$modify_remove_adjustment_note),
        if (gtx_has_text(input$modify_caveat)) paste0(",\n  caveat = ", object_code(input$modify_caveat)) else "",
        "\n)\n",
        "save_table(modified_table, filename = \"modified_table.docx\")"
      ))
    }
    if (identical(input$visual_tool, "fit")) {
      fit_result_name <- result_code_name(input$fit_source %||% "uni", "model_result")
      return(paste0(
        "fit_plot <- plot_model_fit(\n  ", fit_result_name,
        if (gtx_has_text(input$fit_model_name)) paste0(",\n  model_name = ", shQuote(input$fit_model_name)) else "",
        ",\n  type = ", shQuote(input$fit_type),
        ",\n  bins = ", input$fit_bins %||% 10,
        ",\n  base_size = ", input$fit_base_size,
        "\n)\n",
        "save_plot(fit_plot, filename = \"model_fit.png\", width = 10, height = 7)"
      ))
    }
    plot_result_name <- result_code_name(input$plot_source %||% "uni")
    forest_result_name <- result_code_name(input$forest_source %||% "uni")
    forest_xlim_code <- gtx_numeric_vector_code(input$forest_xlim, expected = 2)
    forest_ticks_code <- gtx_numeric_vector_code(input$forest_ticks)
    ci_width <- input$forest_ci_width %||% 20
    plot_layout <- input$plot_layout %||% "single"
    plot_pair <- if (startsWith(plot_layout, "special_combined")) {
      special_combined_code_names(plot_layout)
    } else {
      switch(
        plot_layout,
        combined = c("uni_result", "multi_result"),
        cox_combined = c("cox_exposure_result", "cox_multi_result"),
        survreg_combined = c("survreg_exposure_result", "survreg_multi_result"),
        NULL
      )
    }
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
    forest_content <- input$forest_content %||% "single"
    forest_data_code <- if (grepl("^special_(combined|all)", forest_content)) {
      special_value <- sub("^special_all", "special_combined", forest_content)
      special_pair <- special_combined_code_names(special_value)
      desc_code <- if (startsWith(forest_content, "special_all")) {
        ", desc = desc_result"
      } else {
        ""
      }
      paste0(
        "forest_data <- forest_df(", special_pair[1], ", ",
        special_pair[2], desc_code, ")"
      )
    } else {
      switch(
        forest_content,
        all = "forest_data <- forest_df(uni_result, multi_result, desc = desc_result)",
        combined = "forest_data <- forest_df(uni_result, multi_result)",
        cox_all = "forest_data <- forest_df(cox_exposure_result, cox_multi_result, desc = desc_result)",
        cox_combined = "forest_data <- forest_df(cox_exposure_result, cox_multi_result)",
        survreg_all = "forest_data <- forest_df(survreg_exposure_result, survreg_multi_result, desc = desc_result)",
        survreg_combined = "forest_data <- forest_df(survreg_exposure_result, survreg_multi_result)",
        paste0("forest_data <- forest_df(", forest_result_name, ")")
      )
    }
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

    # Every result kept in this session, in the order it was run, written
    # from the selections used at the time (each carries its own reference
    # categories). Results made with an earlier version of the data cannot be
    # recreated from the current data and are listed as a note instead.
    entries <- history_store()
    current_data <- data_stamp()
    skipped <- 0L
    key_counts <- table(vapply(entries, `[[`, character(1), "key"))
    for (e in entries) {
      if (!gtx_has_text(e$code)) next
      if (!identical(e$data, current_data)) {
        skipped <- skipped + 1L
        next
      }
      name <- history_entry_name(e)
      code <- gsub(paste0("\\b", history_bases[[e$key]], "\\b"), name, e$code, perl = TRUE)
      header <- history_headers[[e$key]]
      if (key_counts[[e$key]] > 1) header <- paste0(header, " (run ", e$run, ", ", e$time, ")")
      sections <- c(sections, paste(header, code, sep = "\n"))
    }
    if (skipped) {
      sections <- c(sections, paste0(
        "# ", skipped, " earlier result", if (skipped > 1) "s were" else " was",
        " made with a different version of the data and ", if (skipped > 1) "are" else "is",
        " not recreated here."
      ))
    }

    if (!is.null(rv$km) && gtx_has_text(input$surv_time) && gtx_has_text(input$surv_event)) {
      sections <- c(sections, paste(
        "# 6. Kaplan-Meier curves",
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

    if (!is.null(rv$cuminc) && gtx_has_text(input$surv_time) && gtx_has_text(input$surv_event)) {
      sections <- c(sections, paste(
        "# 6a. Cumulative incidence",
        gtx_code_assign(
          "cuminc_result",
          "cuminc_plot",
          list(
            data = "analysis_data",
            time = gtx_null_code(input$surv_time),
            event = gtx_null_code(input$surv_event),
            event_of_interest = gtx_null_code(input$competing_event_of_interest),
            censor_code = gtx_null_code(input$competing_censor_code),
            by = gtx_null_code(input$cuminc_by),
            conf.int = gtx_bool_code(input$cuminc_conf),
            p_value = gtx_bool_code(input$cuminc_p),
            y_percent = gtx_bool_code(input$cuminc_y_percent),
            theme = shQuote(input$cuminc_theme),
            grid = gtx_bool_code(input$cuminc_grid)
          )
        ),
        sep = "\n"
      ))
    }

    if (!is.null(rv$merged) || !is.null(rv$plotreg) || !is.null(rv$forest) || !is.null(rv$fitplot)) {
      visual_lines <- character(0)
      if (!is.null(rv$merged)) {
        merge_labels <- input$merge_selection %||% character(0)
        merge_objects <- available_merge_code_names()[merge_labels]
        if (length(merge_objects) >= 2L) {
          visual_lines <- c(visual_lines, paste0(
            "merged_table <- merge_tables(\n  ", paste(unname(merge_objects), collapse = ",\n  "),
            ",\n  spanners = ", gtx_vec_code(merge_labels), "\n)\n",
            "save_table(merged_table, filename = \"merged_table.docx\")"
          ))
        }
      }
      if (!is.null(rv$plotreg)) {
        layout <- input$plot_layout %||% "single"
        pair <- switch(layout, combined = c("uni_result", "multi_result"), cox_combined = c("cox_exposure_result", "cox_multi_result"), survreg_combined = c("survreg_exposure_result", "survreg_multi_result"), NULL)
        if (is.null(pair)) {
          source <- result_code_name(input$plot_source %||% "uni", "uni_result")
          visual_lines <- c(visual_lines, paste0("reg_plot <- plot_reg(", source, ", log_x = ", gtx_bool_code(input$plot_log_x), ")", "\nsave_plot(reg_plot, filename = \"plot_reg.png\", width = 9, height = 6)"))
        } else {
          visual_lines <- c(visual_lines, paste0("reg_plot <- plot_reg_combine(", pair[[1]], ", ", pair[[2]], ", log_x = ", gtx_bool_code(input$plot_log_x), ")", "\nsave_plot(reg_plot, filename = \"plot_reg_combined.png\", width = 10, height = 7)"))
        }
      }
      if (!is.null(rv$forest)) {
        content <- input$forest_content %||% "single"
        forest_data <- switch(content,
          all = "forest_df(uni_result, multi_result, desc = desc_result)",
          combined = "forest_df(uni_result, multi_result)",
          cox_all = "forest_df(cox_exposure_result, cox_multi_result, desc = desc_result)",
          cox_combined = "forest_df(cox_exposure_result, cox_multi_result)",
          survreg_all = "forest_df(survreg_exposure_result, survreg_multi_result, desc = desc_result)",
          survreg_combined = "forest_df(survreg_exposure_result, survreg_multi_result)",
          paste0(
            "forest_df(",
            result_code_name(input$forest_source %||% "uni", "uni_result"),
            ")"
          )
        )
        visual_lines <- c(visual_lines, paste0(
          "forest_data <- ", forest_data, "\n",
          "forest_plot <- forest_reg(forest_data, side = ", shQuote(input$forest_side),
          ", ci_col_width = ", input$forest_ci_width %||% 20,
          if (!is.null(gtx_numeric_vector_code(input$forest_xlim, 2))) paste0(", xlim = ", gtx_numeric_vector_code(input$forest_xlim, 2)) else "",
          if (!is.null(gtx_numeric_vector_code(input$forest_ticks))) paste0(", ticks_at = ", gtx_numeric_vector_code(input$forest_ticks)) else "", ")\n",
          "save_forest(forest_plot, filename = \"forest_reg.pdf\", width = 13, height = 9)"
        ))
      }
      if (length(visual_lines)) sections <- c(sections, paste("# 7. Visualise and export", paste(visual_lines, collapse = "\n\n"), sep = "\n"))
    }

    if (!is.null(rv$fitplot)) {
      fit_result_name <- result_code_name(input$fit_source %||% "uni", "model_result")
      sections <- c(sections, paste(
        "# 7a. Model-fit diagnostic",
        gtx_code_assign(
          "fit_plot",
          "plot_model_fit",
          list(
            model = fit_result_name,
            model_name = if (gtx_has_text(input$fit_model_name)) shQuote(input$fit_model_name) else NULL,
            type = shQuote(input$fit_type),
            bins = as.character(input$fit_bins %||% 10),
            base_size = as.character(input$fit_base_size)
          )
        ),
        "save_plot(fit_plot, filename = \"model_fit.png\", width = 10, height = 7)",
        sep = "\n"
      ))
    }

    if (!is.null(rv$modified)) {
      modified_source <- unname(modify_code_names()[input$modify_source %||% ""])
      if (identical(input$modify_source, "Merged table")) modified_source <- "merged_table"
      if (!length(modified_source) || is.na(modified_source)) modified_source <- "table_result"
      object_code <- function(x) paste(utils::capture.output(dput(x)), collapse = "")
      variable_labels <- tryCatch(parse_named_mapping(input$modify_variable_labels, "Variable labels"), error = function(e) NULL)
      level_labels <- tryCatch(parse_level_mapping(input$modify_level_labels), error = function(e) NULL)
      header_labels <- tryCatch(parse_named_mapping(input$modify_header_labels, "Header labels"), error = function(e) NULL)
      sections <- c(sections, paste0(
        "# 7a. Modify table\n",
        "modified_table <- modify_table(\n",
        "  ", modified_source,
        if (!is.null(variable_labels)) paste0(",\n  variable_labels = ", object_code(variable_labels)) else "",
        if (!is.null(level_labels)) paste0(",\n  level_labels = ", object_code(level_labels)) else "",
        if (!is.null(header_labels)) paste0(",\n  header_labels = ", object_code(header_labels)) else "",
        if (gtx_has_text(input$modify_caption)) paste0(",\n  caption = ", object_code(input$modify_caption)) else "",
        ",\n  bold_labels = ", gtx_bool_code(input$modify_bold_labels),
        ",\n  bold_levels = ", gtx_bool_code(input$modify_bold_levels),
        ",\n  italic_labels = ", gtx_bool_code(input$modify_italic_labels),
        ",\n  italic_levels = ", gtx_bool_code(input$modify_italic_levels),
        ",\n  remove_N = ", gtx_bool_code(input$modify_remove_n),
        ",\n  remove_N_obs = ", gtx_bool_code(input$modify_remove_n_obs),
        ",\n  remove_abbreviations = ", gtx_bool_code(input$modify_remove_abbreviations),
        ",\n  remove_adjustment_note = ", gtx_bool_code(input$modify_remove_adjustment_note),
        if (gtx_has_text(input$modify_caveat)) paste0(",\n  caveat = ", object_code(input$modify_caveat)) else "",
        "\n)\n",
        "save_table(modified_table, filename = \"modified_table.docx\")"
      ))
    }

    if (!is.null(rv$advanced) && gtx_has_text(rv$advanced_code)) {
      sections <- c(sections, paste("# 8. Advanced analysis", rv$advanced_code, sep = "\n"))
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
    if (!check_roles(
      roles = if (survival_advanced) list(Time = input$adv_time, Event = input$adv_event) else list(Outcome = input$adv_outcome),
      lists = list(list(id = "adv_exposures", label = "Candidate predictors", value = input$adv_exposures)),
      run_button = "run_select_models",
      context = "model selection"
    )) return(NULL)
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
    res <- tryCatch(do.call(gtregression::compare_models, c(models, compare_options)), error = function(e) { showNotification(gtx_friendly_error(e), type = "error"); NULL })
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
    exposures <- input$conf_exposures %||% character(0)
    req(data, length(exposures), input$conf_candidate)
    survival_advanced <- input$adv_approach %in% c("cox", "survreg")
    if (survival_advanced) {
      req(input$adv_time, input$adv_event)
    } else {
      req(input$adv_outcome)
    }
    if (!check_roles(
      roles = c(
        if (survival_advanced) list(Time = input$adv_time, Event = input$adv_event) else list(Outcome = input$adv_outcome),
        list(`Potential confounder` = input$conf_candidate)
      ),
      lists = list(list(id = "conf_exposures", label = "Exposures of interest", value = exposures)),
      run_button = "run_confounder",
      context = "the confounder assessment"
    )) return(NULL)
    res <- run_safely("Confounder assessment", gtx_capture(gtx_call(
      "identify_confounder",
      data = data,
      outcome = if (survival_advanced) NULL else input$adv_outcome,
      time = if (survival_advanced) input$adv_time else NULL,
      event = if (survival_advanced) input$adv_event else NULL,
      exposure = exposures,
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
        exposure = gtx_vec_code(exposures),
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
    if (!check_roles(
      roles = c(
        if (survival_advanced) list(Time = input$adv_time, Event = input$adv_event) else list(Outcome = input$adv_outcome),
        list(Exposure = input$conf_exposure, `Effect modifier` = input$conf_candidate)
      ),
      lists = list(list(id = "adv_covariates", label = "Covariates", value = input$adv_covariates %||% character(0))),
      run_button = "run_interaction",
      context = "the interaction assessment"
    )) return(NULL)
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
    if (!gtx_has_text(input$adv_collinearity_model) || is.null(available[[input$adv_collinearity_model]])) {
      showNotification("Run a regression or survival model first, then check collinearity.", type = "warning")
      return(NULL)
    }
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
    if (!check_roles(
      roles = list(Exposure = input$med_exposure, Mediator = input$med_mediator, Outcome = input$med_outcome),
      lists = list(list(id = "med_covariates", label = "Covariates", value = input$med_covariates %||% character(0))),
      run_button = "run_mediation",
      context = "mediation analysis"
    )) return(NULL)
    rv$mediation <- tryCatch(
      gtx_call(
        "mediation_analysis",
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
      error = function(e) { showNotification(gtx_friendly_error(e), type = "error"); NULL }
    )
    if (!is.null(rv$mediation)) {
      showNotification("Mediation results ready. Read the causal assumptions before interpreting them.", type = "message")
    }
  })

  output$med_table <- gtx_render_table(function() rv$mediation, stale = is_stale("mediation"))
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

  # Pre-run checks feed the ready line even while their details are closed.
  for (id in c("reg_preflight", "surv_preflight", "special_preflight")) {
    outputOptions(output, id, suspendWhenHidden = FALSE)
  }
}

shinyApp(ui, server)
