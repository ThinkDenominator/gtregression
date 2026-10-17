# Regression tests for plot_reg() / plot_reg_combine() audit findings
# (C5, N3 and the ggtext fallback). Every check maps a drawn point to the
# axis line it sits on and compares it with table_body.

fixp_bw <- function() {
  data_birthwt |>
    dplyr::mutate(
      race = factor(race, 1:3, c("White", "Black", "Other")),
      smoke = factor(smoke, 0:1, c("No", "Yes")),
      ht = factor(ht, 0:1, c("No", "Yes")),
      low = factor(low, 0:1, c("Normal BW", "Low BW"))
    )
}
fixp_quiet <- function(expr) suppressWarnings(suppressMessages(expr))
fixp_ex <- c("age", "lwt", "smoke", "race", "ht")
fixp_clean <- function(x) trimws(gsub("<[^>]+>|\\.\\.", "", x))

# Axis lines of a built panel (top to bottom) with the x of the point drawn
# on each line, read from the ggplot build rather than from plot data.
fixp_axis <- function(plot) {
  built <- ggplot2::ggplot_build(plot)
  y_scale <- built$layout$panel_params[[1]]$y
  breaks <- as.character(y_scale$breaks)
  labels <- y_scale$get_labels()
  points <- NULL
  for (layer in built$data) {
    if (all(c("x", "y", "fill") %in% names(layer)) && !"xmin" %in% names(layer)) {
      points <- layer
    }
  }
  position <- seq_along(breaks)
  x <- rep(NA_real_, length(breaks))
  if (!is.null(points) && nrow(points)) {
    x[match(as.numeric(points$y), position)] <- points$x
  }
  out <- data.frame(row_id = as.integer(breaks), axis = fixp_clean(labels),
                    raw = labels, x = x, stringsAsFactors = FALSE)
  out <- out[order(out$row_id), ]
  out$exposure <- plot$data$exposure[match(out$row_id, as.integer(as.character(plot$data$row_id)))]
  out$level <- plot$data$level[match(out$row_id, as.integer(as.character(plot$data$row_id)))]
  rownames(out) <- NULL
  out
}
fixp_estimate <- function(tbl, exposure, level) {
  body <- tbl$table_body
  hit <- body$exposure == exposure & body$level == level & !(body$ref %in% TRUE)
  if (!any(hit)) NA_real_ else as.numeric(body$estimate[hit][1])
}
# every non-reference coefficient of `tbl` is drawn on its own (exposure, level) line
expect_points_on_rows <- function(axis, tbl) {
  body <- tbl$table_body[!(tbl$table_body$ref %in% TRUE), , drop = FALSE]
  for (i in seq_len(nrow(body))) {
    line <- which(axis$exposure == body$exposure[i] & axis$level == body$level[i])
    expect_length(line, 1L)
    expect_equal(axis$x[line], as.numeric(body$estimate[i]), tolerance = 1e-8,
                 info = paste(body$exposure[i], body$level[i]))
  }
  expected <- mapply(function(e, l) fixp_estimate(tbl, e, l), axis$exposure, axis$level)
  expect_identical(is.na(axis$x), is.na(unname(expected)))
}
fixp_panels <- function(combined) {
  first <- combined$patches$plots[[1]]
  second <- combined
  class(second) <- setdiff(class(second), "patchwork")
  list(crude = first, adjusted = second)
}

test_that("C5: plot_reg() keeps levels relabelled with modify_table()", {
  skip_if_not_installed("ggplot2")
  bw <- fixp_bw()
  for (tbl in list(fixp_quiet(uni_reg(bw, "low", fixp_ex)),
                   fixp_quiet(multi_reg(bw, "low", fixp_ex)))) {
    relabelled <- modify_table(
      tbl,
      variable_labels = c(race = "Ethnicity"),
      level_labels = list(race = c(Black = "Blk"), smoke = c(Yes = "Smoker"))
    )
    original <- plot_reg(tbl)
    plot <- plot_reg(relabelled)
    expect_identical(nrow(plot$data), nrow(original$data))

    axis <- fixp_axis(plot)
    expect_identical(
      axis$axis,
      c("age", "lwt", "smoke", "No (Ref.)", "Smoker", "Ethnicity", "White (Ref.)",
        "Blk", "Other", "ht", "No (Ref.)", "Yes")
    )
    expect_points_on_rows(axis, tbl)
    expect_equal(axis$x[axis$axis == "Blk"], fixp_estimate(tbl, "race", "Black"))
    expect_equal(axis$x[axis$axis == "Smoker"], fixp_estimate(tbl, "smoke", "Yes"))
    expect_equal(fixp_axis(original)$x, axis$x)
  }
})

test_that("plot_reg() rows are unchanged for unmodified and compact tables", {
  skip_if_not_installed("ggplot2")
  bw <- fixp_bw()
  uni <- fixp_quiet(uni_reg(bw, "low", fixp_ex))
  axis <- fixp_axis(plot_reg(uni))
  expect_identical(
    axis$axis,
    c("age", "lwt", "smoke", "No (Ref.)", "Yes", "race", "White (Ref.)", "Black",
      "Other", "ht", "No (Ref.)", "Yes")
  )
  expect_points_on_rows(axis, uni)

  compact <- fixp_axis(plot_reg(uni, show_ref = FALSE))
  expect_identical(compact$axis, c("age", "lwt", "smoke", "race", "Black", "Other", "ht"))
  expect_points_on_rows(compact, uni)

  interaction <- fixp_quiet(multi_reg(bw, "low", c("age", "smoke", "race"),
                                      interaction = "age*smoke"))
  expect_points_on_rows(fixp_axis(plot_reg(interaction)), interaction)
})

test_that("C5: plot_reg_combine() keeps relabelled levels on their own line", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("patchwork")
  bw <- fixp_bw()
  uni <- fixp_quiet(uni_reg(bw, "low", fixp_ex))
  multi <- fixp_quiet(multi_reg(bw, "low", fixp_ex))
  relabel <- function(x) modify_table(x, level_labels = list(race = c(Black = "Blk")))

  for (case in list(list(relabel(uni), multi), list(relabel(uni), relabel(multi)),
                    list(modify_table(uni, variable_labels = c(smoke = "Smoking")), multi))) {
    panels <- fixp_panels(plot_reg_combine(case[[1]], case[[2]]))
    crude <- fixp_axis(panels$crude)
    adjusted <- fixp_axis(panels$adjusted)
    expect_identical(nrow(crude), 12L)
    expect_identical(crude$level, adjusted$level)
    expect_points_on_rows(crude, uni)
    expect_points_on_rows(adjusted, multi)
  }
  panels <- fixp_panels(plot_reg_combine(relabel(uni), multi))
  crude <- fixp_axis(panels$crude)
  expect_identical(crude$axis[8], "Blk")
  expect_identical(crude$exposure[8], "race")
})

test_that("N3: rows only in the adjusted table sit inside their variable block", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("patchwork")
  bw <- fixp_bw()
  multi <- fixp_quiet(multi_reg(bw, "low", fixp_ex))

  # (a) a level missing from the crude table
  crude_tbl <- fixp_quiet(uni_reg(droplevels(bw[bw$race != "Other", ]), "low", fixp_ex))
  panels <- fixp_panels(plot_reg_combine(crude_tbl, multi))
  crude <- fixp_axis(panels$crude)
  adjusted <- fixp_axis(panels$adjusted)
  expect_identical(
    crude$axis,
    c("age", "lwt", "smoke", "No (Ref.)", "Yes", "race", "White (Ref.)", "Black",
      "Other", "ht", "No (Ref.)", "Yes")
  )
  other <- which(crude$exposure == "race" & crude$level == "Other")
  expect_identical(other, 9L)
  expect_true(is.na(crude$x[other]))
  expect_equal(adjusted$x[other], fixp_estimate(multi, "race", "Other"))
  expect_points_on_rows(crude, crude_tbl)
  expect_points_on_rows(adjusted, multi)

  # (b) interaction rows only in the adjusted model
  uni3 <- fixp_quiet(uni_reg(bw, "low", c("age", "smoke", "race")))
  inter <- fixp_quiet(multi_reg(bw, "low", c("age", "smoke", "race"), interaction = "age*smoke"))
  panels <- fixp_panels(plot_reg_combine(uni3, inter))
  crude <- fixp_axis(panels$crude)
  adjusted <- fixp_axis(panels$adjusted)
  expect_identical(anyDuplicated(paste(crude$exposure, crude$level)), 0L)
  extra <- setdiff(paste(inter$table_body$exposure, inter$table_body$level),
                   paste(uni3$table_body$exposure, uni3$table_body$level))
  expect_true(length(extra) > 0L)
  blocks <- rle(crude$exposure)
  expect_identical(blocks$values, unique(crude$exposure))
  last_race <- max(which(crude$exposure == "race"))
  expect_identical(last_race, nrow(crude))
  expect_identical(crude$level[last_race], "Other")
  expect_points_on_rows(crude, uni3)
  expect_points_on_rows(adjusted, inter)

  # (c) variables only in the adjusted table: own block with header and Ref.
  small <- fixp_quiet(uni_reg(bw, "low", c("smoke", "age")))
  panels <- fixp_panels(plot_reg_combine(small, multi))
  crude <- fixp_axis(panels$crude)
  adjusted <- fixp_axis(panels$adjusted)
  expect_identical(
    crude$axis,
    c("smoke", "No (Ref.)", "Yes", "age", "lwt", "race", "White (Ref.)", "Black",
      "Other", "ht", "No (Ref.)", "Yes")
  )
  race_header <- which(crude$exposure == "race" & crude$level == "__header__")
  expect_match(crude$raw[race_header], "^<b>race</b>$")
  expect_points_on_rows(crude, small)
  expect_points_on_rows(adjusted, multi)
})

test_that("plot_reg_combine() alignment holds for reordered and subset tables", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("patchwork")
  bw <- fixp_bw()
  uni <- fixp_quiet(uni_reg(bw, "low", fixp_ex))
  cases <- list(
    fixp_quiet(multi_reg(bw, "low", fixp_ex)),
    fixp_quiet(multi_reg(bw, "low", c("smoke", "race"), adjust_for = c("age", "lwt"))),
    fixp_quiet(multi_reg(bw, "low", c("ht", "race", "age", "smoke", "lwt")))
  )
  for (multi in cases) {
    panels <- fixp_panels(plot_reg_combine(uni, multi))
    crude <- fixp_axis(panels$crude)
    expect_identical(crude$axis, fixp_axis(plot_reg(uni))$axis)
    expect_points_on_rows(crude, uni)
    expect_points_on_rows(fixp_axis(panels$adjusted), multi)
  }
})

test_that("ggtext fallback prints plain axis labels, not raw HTML", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("patchwork")
  bw <- fixp_bw()
  uni <- fixp_quiet(uni_reg(bw, "low", fixp_ex))
  multi <- fixp_quiet(multi_reg(bw, "low", fixp_ex))

  marked <- c("<b>race</b>",
              "<span style='color:white;'>..</span>White <span style='color:gray;'>(Ref.)</span>",
              "<span style='color:white;'>..</span>Black")
  expect_identical(.plot_axis_labels(marked, markdown = TRUE), marked)
  expect_identical(.plot_axis_labels(marked, markdown = FALSE),
                   c("race", "    White (Ref.)", "    Black"))

  testthat::local_mocked_bindings(.plot_has_ggtext = function() FALSE)
  single <- plot_reg(uni)
  panels <- fixp_panels(plot_reg_combine(uni, multi))
  for (plot in list(single, panels$crude)) {
    labels <- ggplot2::ggplot_build(plot)$layout$panel_params[[1]]$y$get_labels()
    expect_false(any(grepl("[<>]", labels)))
    expect_true("race" %in% labels)
    expect_true("    White (Ref.)" %in% labels)
    expect_false(inherits(plot$theme$axis.text.y, "element_markdown"))
    expect_s3_class(plot$theme$axis.text.y, "element_text")
    expect_no_error(ggplot2::ggplotGrob(plot))
  }
  expect_points_on_rows(fixp_axis(single), uni)
})
