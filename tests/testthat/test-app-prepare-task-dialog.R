# Prepare task dialogs (UX only). Operations that build, map, order or combine
# open the existing action sheet as a centred dialog; short edits keep the
# compact side sheet. The module, its inputs and its outputs are unchanged.

gtx_prep_app_source <- function() {
  paste(readLines(system.file("shiny", "app.R", package = "gtregression"), warn = FALSE), collapse = "\n")
}

gtx_prep_css_rule <- function(source, selector) {
  hit <- regmatches(source, regexpr(paste0(selector, "\\s*\\{[^}]*\\}"), source, perl = TRUE))
  if (length(hit)) hit else ""
}

test_that("the five construction operations, and only those, open as a task dialog", {
  src <- gtx_prep_app_source()
  toggle <- regmatches(src, regexpr("classList\\.toggle\\('gta-prep-dialog', \\[[^]]*\\]", src))
  expect_length(toggle, 1L)
  actions <- regmatches(toggle, gregexpr("'[a-z]+'", toggle))[[1]]
  actions <- setdiff(gsub("'", "", actions), "gta")
  expect_setequal(actions, c("recode", "type", "calculation", "groups", "filter"))
  expect_false(any(c("rename", "label", "missing", "keep") %in% actions))
})

test_that("the sheet keeps its ids, its module outputs and a persistent Cancel/Apply footer", {
  src <- gtx_prep_app_source()
  expect_match(src, "id = \"gta-prep-sheet\", class = \"gta-prep-sheet\", role = \"dialog\"", fixed = TRUE)
  expect_match(src, "id = \"gta-prep-sheet-title\"", fixed = TRUE)
  body <- substr(src, regexpr("class = \"gta-prep-sheet-body\"", src, fixed = TRUE), regexpr("class = \"gta-prep-sheet-foot\"", src, fixed = TRUE))
  expect_match(body, "uiOutput(ns(\"controls\"))", fixed = TRUE)
  expect_match(body, "uiOutput(ns(\"value_guide\"))", fixed = TRUE)
  expect_match(body, "uiOutput(ns(\"impact_preview\"))", fixed = TRUE)
  foot <- regmatches(src, regexpr("class = \"gta-prep-sheet-foot\",[^\n]*\n[^\n]*\n[^\n]*", src))
  expect_match(foot, "data-gta-prep-cancel", fixed = TRUE)
  expect_match(foot, "actionButton(ns(\"apply\"), \"Apply\"", fixed = TRUE)
  # Head and footer never scroll away, in the sheet or in the dialog.
  expect_match(gtx_prep_css_rule(src, "html\\.gta-shell \\.gta-prep-sheet-head"), "flex: 0 0 auto", fixed = TRUE)
  expect_match(gtx_prep_css_rule(src, "html\\.gta-shell \\.gta-prep-sheet-foot"), "flex: 0 0 auto", fixed = TRUE)
})

test_that("a task dialog never scrolls as a whole; only variable-length regions scroll", {
  src <- gtx_prep_app_source()
  dialog <- gtx_prep_css_rule(src, "html\\.gta-shell \\.gta-prep-sheet\\.gta-prep-dialog")
  expect_match(dialog, "position: fixed", fixed = TRUE)
  expect_match(dialog, "max-height: calc(100vh - 56px)", fixed = TRUE)
  body <- gtx_prep_css_rule(src, "html\\.gta-shell \\.gta-prep-dialog \\.gta-prep-sheet-body")
  expect_match(body, "overflow: hidden", fixed = TRUE)
  expect_match(body, "grid-template-columns", fixed = TRUE)
  for (region in c("\\.gtx-recode-map", "#data_prep-impact_preview")) {
    rule <- gtx_prep_css_rule(src, paste0("html\\.gta-shell \\.gta-prep-dialog ", region))
    expect_match(rule, "max-height: calc\\(100vh - [0-9]+px\\)")
    expect_match(rule, "overflow: auto", fixed = TRUE)
  }
  # Group rules scroll in place only on short windows.
  expect_match(src, "@media \\(max-height: 780px\\) \\{\\s*html\\.gta-shell \\.gta-prep-dialog #data_prep-group_rules \\{ max-height: calc\\(100vh - [0-9]+px\\); min-height: 96px; overflow: auto;", perl = TRUE)
  # The preview keeps its own column beside the controls.
  expect_match(gtx_prep_css_rule(src, "html\\.gta-shell \\.gta-prep-dialog #data_prep-impact_preview"), "grid-column: 2", fixed = TRUE)
})

test_that("the new variable's name is outside the scrolling group-rule region", {
  # Coverage tools rewrite function bodies, so the deparsed order is not
  # meaningful while coverage is being measured.
  skip_if(identical(Sys.getenv("R_COVR"), "true"), "function bodies are instrumented by covr")
  ui <- paste(deparse(body(get("mod_data_prep_server", envir = asNamespace("gtregression")))), collapse = "\n")
  rules <- regexpr("uiOutput\\(ns\\(\"group_rules\"\\)\\)", ui)
  name <- regexpr("textInput\\(ns\\(\"transform_new\"\\), \"Name for the new grouped variable\"", ui)
  expect_gt(rules, 0)
  expect_gt(name, rules)
})

test_that("compact operations keep the side sheet's own scrolling body", {
  src <- gtx_prep_app_source()
  sheet <- gtx_prep_css_rule(src, "html\\.gta-shell \\.gta-prep-sheet")
  expect_match(sheet, "position: absolute", fixed = TRUE)
  expect_match(sheet, "width: min(440px, 62%)", fixed = TRUE)
  expect_match(gtx_prep_css_rule(src, "html\\.gta-shell \\.gta-prep-sheet-body"), "overflow: auto", fixed = TRUE)
})
