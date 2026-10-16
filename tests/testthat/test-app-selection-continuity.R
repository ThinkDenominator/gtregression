# Selection continuity: analysis selections in Describe, Regression, Survival
# and More models survive data-preparation changes while they remain valid.
# Renamed variables are followed, removed variables are cleared, Undo and
# Reset select a removed variable again when it is back, and a new import
# starts afresh. Statistical code paths are not involved.

sc_start_data <- function() {
  data.frame(
    age = c(34, 51, 62, 45, 70, 29, 58, 66, 41, 73, 38, 55),
    smoke = c(0, 1, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0),
    sex = factor(rep(c("F", "M"), 6), levels = c("F", "M")),
    bmi = c(22.1, 27.4, 31.0, 24.8, 29.3, 21.7, 26.5, 33.2, 23.9, 28.8, 25.1, 30.4),
    grade = factor(rep(c("low", "mid", "high"), 4), levels = c("low", "mid", "high")),
    time = c(3, 6, 9, 12, 15, 18, 21, 24, 27, 30, 33, 36),
    status = c(0, 1, 2, 1, 0, 2, 1, 0, 1, 2, 1, 0),
    clinic = rep(c("a", "b", "c"), 4),
    stringsAsFactors = FALSE
  )
}

# Selected values of a select or checkbox group named `name` in rendered HTML.
sc_selected <- function(html, name) {
  select <- regmatches(html, regexpr(paste0("(?s)<select id=\"", name, "\"[^>]*>.*?</select>"), html, perl = TRUE))
  if (length(select)) {
    options <- regmatches(select, gregexpr("<option value=\"[^\"]*\" selected>", select))[[1]]
    return(sub("<option value=\"([^\"]*)\" selected>", "\\1", options))
  }
  boxes <- regmatches(html, gregexpr(paste0("<input type=\"checkbox\" name=\"", name, "\" value=\"[^\"]*\" checked=\"checked\"/>"), html))[[1]]
  sub(".* value=\"([^\"]*)\".*", "\\1", boxes)
}

sc_checked <- function(html, name) {
  grepl(paste0("<input id=\"", name, "\" type=\"checkbox\"[^>]*checked=\"checked\""), html)
}

# Reference-category selects: variable -> selected level.
sc_references <- function(html, prefix) {
  labels <- regmatches(html, gregexpr(paste0("<label[^>]*for=\"", prefix, "[0-9]+\">[^<]*</label>"), html))[[1]]
  if (!length(labels)) return(character(0))
  ids <- sub(".*for=\"([^\"]+)\".*", "\\1", labels)
  vars <- sub(".*>(.*) reference category<.*", "\\1", labels)
  stats::setNames(vapply(ids, function(id) sc_selected(html, id)[1], character(1)), vars)
}

# Run `body` inside the full app's server with `study` imported. The helpers
# drive Data Prep through its module inputs and mimic the browser by copying
# rendered selections back into the inputs.
sc_with_app <- function(body) {
  code <- substitute(body)
  data_env <- new.env(parent = emptyenv())
  data_env$study <- sc_start_data()
  data_env$study2 <- transform(sc_start_data(), extra = 1)
  app_env <- new.env(parent = globalenv())
  app_env$gtx_launch_context <- list(mode = "app", data_env = data_env)
  sys.source(system.file("shiny", "app.R", package = "gtregression"), envir = app_env)
  suppressWarnings(shiny::testServer(app_env$server, {
    html <- function(id) paste(as.character(output[[id]]$html), collapse = "")
    set <- function(...) {
      session$setInputs(...)
      session$flushReact()
    }
    count <- 0L
    prep <- function(menu, ...) {
      count <<- count + 1L
      args <- list(...)
      names(args) <- paste0("data_prep-", names(args))
      do.call(set, stats::setNames(list(count), paste0("data_prep-menu_", menu)))
      if (!is.null(args[["data_prep-variable"]])) do.call(set, args["data_prep-variable"])
      do.call(set, args)
      do.call(set, stats::setNames(list(count), "data_prep-apply"))
    }
    undo_n <- 0L
    redo_n <- 0L
    undo <- function() { undo_n <<- undo_n + 1L; set(`data_prep-undo` = undo_n) }
    redo <- function() { redo_n <<- redo_n + 1L; set(`data_prep-redo` = redo_n) }
    import_n <- 0L
    import <- function(name) {
      import_n <<- import_n + 1L
      set(environment_dataset_name = name, load_environment_data = import_n)
    }
    # Browser behaviour: an input takes the value its re-rendered control shows.
    sync <- function(output_id, names) {
      h <- html(output_id)
      values <- lapply(names, function(n) {
        v <- sc_selected(h, n)
        if (length(v)) v else NULL
      })
      do.call(set, stats::setNames(values, names))
    }
    session$flushReact()
    import("study")
    eval(code, envir = environment())
  }))
}

# The standard sequence of preparation changes, checking `check()` after each.
# `check(stage)` receives one of: "label", "recode", "type",
# "rename", "filter", "keep", "undo_keep", "redo_keep", "undo_all".
sc_sequence <- function(check, sync_all) {
  # Selections made by the user are shown once a change re-renders the controls.
  prep("label", variable = "age", variable_label = "Age (years)"); sync_all(); check("label")
  prep("recode", variable = "smoke", recode_to_1 = "No", recode_to_2 = "Yes"); sync_all(); check("recode")
  prep("type", variable = "grade", variable_type = "ordered", type_rank_1 = "3", type_rank_2 = "2", type_rank_3 = "1"); sync_all(); check("type")
  prep("rename", variable = "age", new_name = "age_years"); sync_all(); check("rename")
  prep("filter", variable1 = "time", operator1 = ">=", value1 = "6", filter_second = FALSE); sync_all(); check("filter")
  keep <- setdiff(names(analysis_data()), "bmi")
  prep("keep", keep_vars = keep); sync_all(); check("keep")
  undo(); sync_all(); check("undo_keep")
  redo(); sync_all(); check("redo_keep")
  for (i in 1:6) { undo(); sync_all() }
  check("undo_all")
}

test_that("Describe keeps, follows, clears and restores its selections", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  sc_with_app({
    set(desc_by = "sex", desc_exposures = c("age", "bmi", "smoke"))
    environment(sc_sequence) <- environment()
    sc_sequence(
      sync_all = function() sync("desc_inputs", c("desc_by", "desc_exposures")),
      check = function(stage) {
        h <- html("desc_inputs")
        expect_identical(sc_selected(h, "desc_by"), "sex", info = stage)
        age <- if (stage %in% c("rename", "filter", "keep", "undo_keep", "redo_keep")) "age_years" else "age"
        expected <- if (stage %in% c("keep", "redo_keep")) c(age, "smoke") else c(age, "smoke", "bmi")
        expect_setequal(sc_selected(h, "desc_exposures"), expected)
      }
    )
    expect_identical(names(analysis_data()), names(sc_start_data()))
    # A new import starts afresh.
    import("study2")
    h <- html("desc_inputs")
    expect_identical(sc_selected(h, "desc_by"), "")
    expect_length(sc_selected(h, "desc_exposures"), 0)
  })
})

test_that("Regression keeps outcome, exposures, adjustment, interaction and references", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  sc_with_app({
    set(reg_outcome = "smoke", reg_exposures = c("age", "grade"), reg_adjust = c("sex", "bmi"), reg_interaction = "age*sex")
    refs <- sc_references(html("reg_reference_inputs"), "reg_reference_")
    expect_true(all(c("grade", "sex") %in% names(refs)))
    ids <- paste0("reg_reference_", seq_along(refs))
    ref_values <- refs
    ref_values[["grade"]] <- "mid"
    ref_values[["sex"]] <- "M"
    do.call(set, stats::setNames(as.list(unname(ref_values)), ids))
    environment(sc_sequence) <- environment()
    sc_sequence(
      sync_all = function() {
        sync("reg_inputs", c("reg_outcome", "reg_exposures", "reg_adjust"))
        i <- regmatches(html("reg_inputs"), regexpr("id=\"reg_interaction\"[^>]*value=\"[^\"]*\"", html("reg_inputs")))
        set(reg_interaction = sub(".*value=\"([^\"]*)\"", "\\1", i))
        h <- html("reg_reference_inputs")
        n <- length(sc_references(h, "reg_reference_"))
        if (n) do.call(set, stats::setNames(lapply(seq_len(n), function(k) sc_selected(h, paste0("reg_reference_", k))), paste0("reg_reference_", seq_len(n))))
      },
      check = function(stage) {
        h <- html("reg_inputs")
        renamed <- stage %in% c("rename", "filter", "keep", "undo_keep", "redo_keep")
        age <- if (renamed) "age_years" else "age"
        expect_identical(sc_selected(h, "reg_outcome"), "smoke", info = stage)
        expect_setequal(sc_selected(h, "reg_exposures"), c(age, "grade"))
        expected_adjust <- if (stage %in% c("keep", "redo_keep")) "sex" else c("sex", "bmi")
        expect_setequal(sc_selected(h, "reg_adjust"), expected_adjust)
        expect_match(h, paste0("value=\"", age, "*sex\""), fixed = TRUE, info = stage)
        refs <- sc_references(html("reg_reference_inputs"), "reg_reference_")
        # The ordered grade keeps "mid" (still a level after the order change).
        expect_identical(refs[["grade"]], "mid", info = stage)
        expect_identical(refs[["sex"]], "M", info = stage)
      }
    )
    # Removing an interaction variable clears the interaction.
    prep("keep", keep_vars = setdiff(names(analysis_data()), "sex"))
    expect_match(html("reg_inputs"), "id=\"reg_interaction\" type=\"text\"[^>]*value=\"\"")
    import("study2")
    h <- html("reg_inputs")
    expect_identical(sc_selected(h, "reg_outcome"), "")
    expect_length(sc_selected(h, "reg_exposures"), 0)
    expect_length(sc_selected(h, "reg_adjust"), 0)
  })
})

test_that("Survival keeps time, event, exposures, groups, distribution and competing codes", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  sc_with_app({
    set(surv_mode = "survreg")
    set(surv_time = "time", surv_event = "status", surv_exposures = c("age", "grade"), surv_adjust = c("bmi"),
        surv_stratifier = "clinic", surv_dist = "lognormal", km_by = "sex")
    environment(sc_sequence) <- environment()
    sc_sequence(
      sync_all = function() {
        sync("surv_inputs", c("surv_time", "surv_event", "surv_exposures", "surv_adjust", "surv_stratifier", "surv_dist"))
        sync("km_group_input", "km_by")
      },
      check = function(stage) {
        h <- html("surv_inputs")
        age <- if (stage %in% c("rename", "filter", "keep", "undo_keep", "redo_keep")) "age_years" else "age"
        expect_identical(sc_selected(h, "surv_time"), "time", info = stage)
        expect_identical(sc_selected(h, "surv_event"), "status", info = stage)
        expect_setequal(sc_selected(h, "surv_exposures"), c(age, "grade"))
        expect_identical(sc_selected(h, "surv_adjust"), if (stage %in% c("keep", "redo_keep")) character(0) else "bmi", info = stage)
        expect_identical(sc_selected(h, "surv_stratifier"), "clinic", info = stage)
        expect_identical(sc_selected(h, "surv_dist"), "lognormal", info = stage)
        expect_identical(sc_selected(html("km_group_input"), "km_by"), "sex", info = stage)
      }
    )
    # Competing risks: chosen codes are kept for the same event variable,
    # including after it is renamed; a different event variable starts afresh.
    set(surv_mode = "competing")
    expect_identical(sc_selected(html("competing_event_codes"), "competing_event_of_interest"), "1")
    set(competing_censor_code = "0", competing_event_of_interest = "2")
    prep("filter", variable1 = "time", operator1 = "<=", value1 = "33", filter_second = FALSE)
    h <- html("competing_event_codes")
    expect_identical(sc_selected(h, "competing_censor_code"), "0")
    expect_identical(sc_selected(h, "competing_event_of_interest"), "2")
    prep("rename", variable = "status", new_name = "state")
    sync("surv_inputs", c("surv_time", "surv_event"))
    expect_identical(sc_selected(html("surv_inputs"), "surv_event"), "state")
    expect_identical(sc_selected(html("competing_event_codes"), "competing_event_of_interest"), "2")
    set(surv_event = "smoke")
    set(surv_event = "state")
    expect_identical(sc_selected(html("competing_event_codes"), "competing_event_of_interest"), "1")
    import("study2")
    h <- html("surv_inputs")
    expect_identical(sc_selected(h, "surv_time"), "")
    expect_identical(sc_selected(h, "surv_dist"), "weibull")
    expect_identical(sc_selected(html("km_group_input"), "km_by"), "")
  })
})

test_that("More models keeps selections within a design and resets on a design change", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  sc_with_app({
    set(special_mode = "ordinal")
    set(special_outcome = "grade", special_exposures = c("age", "sex"), special_adjust = "bmi",
        special_multivariable = TRUE, special_assumption_test = FALSE)
    refs <- sc_references(html("special_reference_inputs"), "special_reference_")
    expect_true("sex" %in% names(refs))
    k <- match("sex", names(refs))
    do.call(set, stats::setNames(list("M"), paste0("special_reference_", k)))
    set(med_exposure = "smoke", med_mediator = "bmi", med_outcome = "age", med_covariates = "sex")
    environment(sc_sequence) <- environment()
    sc_sequence(
      sync_all = function() {
        sync("special_inputs", c("special_outcome", "special_exposures", "special_adjust"))
        sync("med_inputs", c("med_exposure", "med_mediator", "med_outcome", "med_covariates"))
      },
      check = function(stage) {
        h <- html("special_inputs")
        age <- if (stage %in% c("rename", "filter", "keep", "undo_keep", "redo_keep")) "age_years" else "age"
        dropped <- stage %in% c("keep", "redo_keep")
        expect_identical(sc_selected(h, "special_outcome"), "grade", info = stage)
        expect_setequal(sc_selected(h, "special_exposures"), c(age, "sex"))
        expect_identical(sc_selected(h, "special_adjust"), if (dropped) character(0) else "bmi", info = stage)
        expect_true(sc_checked(h, "special_multivariable"), info = stage)
        expect_false(sc_checked(h, "special_assumption_test"), info = stage)
        expect_identical(sc_references(html("special_reference_inputs"), "special_reference_")[["sex"]], "M", info = stage)
        m <- html("med_inputs")
        expect_identical(sc_selected(m, "med_exposure"), "smoke", info = stage)
        expect_identical(sc_selected(m, "med_mediator"), if (dropped) "" else "bmi", info = stage)
        expect_identical(sc_selected(m, "med_outcome"), age, info = stage)
        expect_identical(sc_selected(m, "med_covariates"), "sex", info = stage)
      }
    )
    # A design switch still starts afresh (unchanged behaviour).
    set(special_mode = "multilevel")
    h <- html("special_inputs")
    expect_identical(sc_selected(h, "special_outcome"), "")
    expect_length(sc_selected(h, "special_exposures"), 0)
    # Within the multilevel design the grouping variable follows a rename.
    set(special_outcome = "bmi", special_group = "clinic", special_cluster_approach = "logit")
    prep("rename", variable = "clinic", new_name = "centre")
    h <- html("special_inputs")
    expect_identical(sc_selected(h, "special_group"), "centre")
    expect_identical(sc_selected(h, "special_cluster_approach"), "logit")
    expect_identical(sc_selected(h, "special_outcome"), "bmi")
    import("study2")
    expect_identical(sc_selected(html("special_inputs"), "special_group"), "")
    expect_identical(sc_selected(html("med_inputs"), "med_exposure"), "")
  })
})

test_that("Reset restores removed selections and follows renames back", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  sc_with_app({
    set(desc_by = "sex", desc_exposures = c("age", "bmi"))
    prep("rename", variable = "age", new_name = "age_years")
    sync("desc_inputs", c("desc_by", "desc_exposures"))
    prep("keep", keep_vars = setdiff(names(analysis_data()), c("bmi", "sex")))
    sync("desc_inputs", c("desc_by", "desc_exposures"))
    h <- html("desc_inputs")
    expect_identical(sc_selected(h, "desc_by"), "")
    expect_identical(sc_selected(h, "desc_exposures"), "age_years")
    set(`data_prep-reset` = 1)
    set(`data_prep-confirm_reset` = 1)
    h <- html("desc_inputs")
    expect_identical(sc_selected(h, "desc_by"), "sex")
    expect_setequal(sc_selected(h, "desc_exposures"), c("age", "bmi"))
  })
})

test_that("selection helpers map renames and interaction terms", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  app_env <- new.env(parent = globalenv())
  app_env$gtx_launch_context <- list(mode = "app", data_env = new.env(parent = emptyenv()))
  sys.source(system.file("shiny", "app.R", package = "gtregression"), envir = app_env)
  ev <- list(c(from = "a", to = "b"), c(from = "b", to = "c"))
  expect_identical(app_env$gtx_map_names(c("a", "x"), ev), c("c", "x"))
  expect_identical(app_env$gtx_keep_selection(c("a", "gone"), c("c", "x"), ev), "c")
  expect_null(app_env$gtx_keep_selection("gone", c("c", "x"), ev))
  expect_identical(app_env$gtx_keep_interaction("a*x", c("c", "x"), ev), "c*x")
  expect_identical(app_env$gtx_keep_interaction("a:x", c("c", "x"), ev), "c:x")
  expect_identical(app_env$gtx_keep_interaction("a*gone", c("c", "x"), ev), "")
})
