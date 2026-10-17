# Tests for the reusable workbench presentation components (Stage D2).
# The components are presentation-only: they must not change input IDs,
# input defaults, or any text that carries methodological meaning.

gtx_components_app_env <- function(mode = "addin", data_env = new.env(parent = emptyenv())) {
  app_file <- system.file("shiny", "app.R", package = "gtregression")
  app_env <- new.env(parent = globalenv())
  if (!is.null(mode)) app_env$gtx_launch_context <- list(mode = mode, data_env = data_env)
  sys.source(app_file, envir = app_env)
  app_env
}

gtx_components_html <- function(app_env) {
  rendered <- htmltools::renderTags(app_env$ui)
  paste(as.character(rendered$head), rendered$html)
}

gtx_input_markup <- function(html, id) {
  select <- regmatches(html, regexpr(paste0("(?s)<select id=\"", id, "\"[^>]*>.*?</select>"), html, perl = TRUE))
  if (length(select)) return(select)
  input <- regmatches(html, regexpr(paste0("<input id=\"", id, "\"[^>]*/?>"), html, perl = TRUE))
  if (length(input)) return(input)
  NA_character_
}

test_that("component markup is emitted only when components are enabled (Addin)", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  skip_if_not_installed("jsonlite")
  addin_env <- gtx_components_app_env("addin")
  web_env <- gtx_components_app_env(NULL)
  expect_true(addin_env$gtw_components_enabled)
  expect_false(web_env$gtw_components_enabled)

  addin_html <- gtx_components_html(addin_env)
  web_html <- gtx_components_html(web_env)
  markers <- c("id=\"gtw-config\"", "gtw-options", "gtw-toolbar", "gtw-caution-box", "gtwComponentsBoot")
  for (marker in markers) {
    expect_match(addin_html, marker, fixed = TRUE)
    expect_false(grepl(marker, web_html, fixed = TRUE), info = marker)
  }
})

test_that("help registry and targets are consistent", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  skip_if_not_installed("jsonlite")
  app_env <- gtx_components_app_env("addin")
  registry <- app_env$gtw_help_registry
  for (entry in registry) {
    expect_true(is.character(entry$title) && nzchar(entry$title))
    expect_true(is.character(entry$short) && nzchar(entry$short))
  }
  for (target in app_env$gtw_help_targets) {
    expect_true(target$help %in% names(registry), info = target$help)
    expect_true(xor(is.null(target$input), is.null(target$selector)), info = target$help)
  }
  config_html <- as.character(app_env$gtw_component_config())
  json <- sub("^<script[^>]*>", "", sub("</script>$", "", config_html))
  config <- jsonlite::fromJSON(json, simplifyVector = FALSE)
  expect_setequal(names(config$help), names(registry))
  expect_length(config$pickers, length(app_env$gtw_picker_targets))
})

test_that("options, pickers and help attach to existing inputs without changing their defaults", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  addin_html <- gtx_components_html(gtx_components_app_env("addin"))
  web_html <- gtx_components_html(gtx_components_app_env(NULL))
  static_ids <- c(
    "desc_percent", "desc_overall", "desc_missing", "desc_dich", "desc_statistic",
    "reg_approach", "reg_show_ref", "reg_model_stats", "reg_strata_multi",
    "csv_header", "csv_sep", "xlsx_sheet"
  )
  for (id in static_ids) {
    addin_markup <- gtx_input_markup(addin_html, id)
    expect_false(is.na(addin_markup), info = id)
    expect_identical(addin_markup, gtx_input_markup(web_html, id), info = id)
  }

  data_env <- new.env(parent = emptyenv())
  data_env$study <- gtregression::data_birthwt
  app_env <- gtx_components_app_env("addin", data_env)
  shiny::testServer(app_env$server, {
    session$flushReact()
    suppressWarnings(session$setInputs(environment_dataset_name = "study", load_environment_data = 1))
    session$flushReact()
    rendered <- paste(
      unlist(output$reg_inputs), unlist(output$desc_inputs), unlist(output$reg_strata_inputs),
      collapse = ""
    )
    for (target in app_env$gtw_picker_targets) {
      expect_match(
        rendered,
        paste0("id=\"", target$input, "\" class=\"form-group shiny-input-checkboxgroup"),
        fixed = TRUE
      )
      for (other in names(target$conflicts)) {
        expect_true(grepl(paste0("id=\"", other, "\""), rendered, fixed = TRUE) ||
          grepl(paste0("id=\"", other, "\""), gtx_components_html(app_env), fixed = TRUE), info = other)
      }
    }
    all_html <- paste(rendered, gtx_components_html(app_env))
    for (target in app_env$gtw_help_targets) {
      if (!is.null(target$input)) {
        expect_match(all_html, paste0("id=\"", target$input, "\""), fixed = TRUE)
      }
    }
  })
})

test_that("methodological caution keeps its full text in both presentations", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  full <- "The result is a screening aid and depends on no unmeasured exposure-mediator, mediator-outcome, or exposure-outcome confounding assumptions."
  addin_html <- gtx_components_html(gtx_components_app_env("addin"))
  web_html <- gtx_components_html(gtx_components_app_env(NULL))
  expect_match(addin_html, full, fixed = TRUE)
  expect_match(web_html, full, fixed = TRUE)
  expect_match(addin_html, "Screening aid only:", fixed = TRUE)
})
