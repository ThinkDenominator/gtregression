# Prepare quick starts fill the normal change form (never apply on their own),
# and a variable chosen in the variable-first workspace preselects the
# variable fields of an operation.

qs_data <- function() {
  data.frame(
    age = c(34, 70, 66, 29, 81, 45, 52, 67, 38, 90, 61, 25),
    flag = c(1, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0, 0),
    other01 = c(0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1),
    score = c(3, 99, 4, 999, 2, 5, 1, 99, 3, 4, 2, 1),
    weight = c(60, 72, 80, 55, 90, 68, 75, 82, 59, 63, 70, 77),
    stringsAsFactors = FALSE
  )
}

qs_run <- function(body) {
  code <- substitute(body)
  source_data <- shiny::reactiveVal(qs_data())
  suppressWarnings(shiny::testServer(mod_data_prep_server, args = list(source_data = source_data), {
    html <- function(id) paste(as.character(output[[id]]$html), collapse = "")
    selected <- function(h, id) {
      select <- regmatches(h, regexpr(paste0("(?s)<select id=\"", session$ns(id), "\"[^>]*>.*?</select>"), h, perl = TRUE))
      if (!length(select)) return(NA_character_)
      sub(".*<option value=\"([^\"]*)\" selected>.*", "\\1", select)
    }
    text_value <- function(h, id) {
      m <- regmatches(h, regexpr(paste0("id=\"", session$ns(id), "\" type=\"(text|number)\"[^>]*value=\"[^\"]*\""), h))
      if (!length(m)) return(NA_character_)
      sub(".*value=\"([^\"]*)\"$", "\\1", m)
    }
    session$flushReact()
    eval(code, envir = environment())
  }))
}

test_that("0/1 to No/Yes maps labels by value and prefers the chosen variable", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  qs_run({
    session$setInputs(focus_variable = "flag")
    session$setInputs(template_binary = 1)
    session$flushReact()
    expect_identical(operation(), "recode")
    h <- html("controls")
    expect_identical(selected(h, "variable"), "flag")
    # The browser takes the selected variable; the mapping then renders.
    session$setInputs(variable = "flag")
    m <- html("recode_mapping")
    first <- sub("<code>([^<]*)</code>", "\\1", regmatches(m, regexpr("<code>[^<]*</code>", m)))
    # Whatever the order of the listed values, 0 -> No and 1 -> Yes.
    v1 <- text_value(m, "recode_to_1")
    v2 <- text_value(m, "recode_to_2")
    if (identical(first, "0")) {
      expect_identical(c(v1, v2), c("No", "Yes"))
    } else {
      expect_identical(c(v1, v2), c("Yes", "No"))
    }
    expect_length(state$history, 0L)
  })
})

test_that("99/999 to missing and age groups fill the form without applying", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  qs_run({
    session$setInputs(focus_variable = "score")
    session$setInputs(template_missing = 1)
    session$flushReact()
    expect_identical(operation(), "missing")
    h <- html("controls")
    expect_identical(selected(h, "variable"), "score")
    session$setInputs(variable = "score", missing_action = "to_na")
    expect_match(html("missing_controls"), "99, 999", fixed = TRUE)
    expect_length(state$history, 0L)

    session$setInputs(focus_variable = "age")
    session$setInputs(template_age = 1)
    session$flushReact()
    expect_identical(operation(), "transform")
    h <- html("controls")
    expect_match(h, "age_group", fixed = TRUE)
    expect_length(state$history, 0L)
    expect_identical(session$returned$working_data(), qs_data())
  })
})

test_that("a menu choice after a quick start starts from a clean form", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  qs_run({
    session$setInputs(focus_variable = "score")
    session$setInputs(template_missing = 1)
    session$flushReact()
    session$setInputs(menu_missing = 1)
    session$flushReact()
    session$setInputs(variable = "score", missing_action = "to_na")
    expect_false(grepl("99, 999", html("missing_controls"), fixed = TRUE))
  })
})

test_that("the focus variable preselects an operation's variable", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  qs_run({
    session$setInputs(focus_variable = "weight")
    session$setInputs(menu_rename = 1)
    session$flushReact()
    expect_identical(selected(html("controls"), "variable"), "weight")
    # A focus variable that the operation cannot use is ignored.
    session$setInputs(focus_variable = "not_a_variable")
    session$setInputs(menu_label = 1)
    session$flushReact()
    expect_identical(selected(html("controls"), "variable"), "age")
  })
})
