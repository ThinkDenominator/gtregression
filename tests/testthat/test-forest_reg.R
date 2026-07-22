birthwt_forest_data <- function() {
  data_birthwt |>
    dplyr::mutate(
      race = factor(race, levels = c(1, 2, 3),
                    labels = c("White", "Black", "Other")),
      smoke = factor(smoke, levels = c(0, 1), labels = c("No", "Yes")),
      ht = factor(ht, levels = c(0, 1), labels = c("No", "Yes")),
      ui = factor(ui, levels = c(0, 1), labels = c("No", "Yes")),
      low = factor(low, levels = c(0, 1), labels = c("Normal BW", "Low BW"))
    )
}

test_that("forest_df builds univariate and adjusted forest data", {
  df <- birthwt_forest_data()

  uni <- uni_reg(
    df,
    outcome = "low",
    exposures = c("age", "lwt", "smoke", "ht"),
    approach = logit
  )
  multi <- multi_reg(
    df,
    outcome = "low",
    exposures = c("smoke", "ht"),
    adjust_for = c("age", "lwt"),
    approach = logit
  )

  out <- forest_df(uni = uni, multi = multi)
  meta <- attr(out, "forest_meta")

  expect_s3_class(out, "data.frame")
  expect_true(all(c("Characteristic", "OR (95% CI)", "Adjusted OR (95% CI)",
                    " ", "  ", "se_uni", "se_adj") %in% names(out)))
  expect_true(all(c("est", "lo", "hi", "est2", "lo2", "hi2") %in% names(attributes(out))))
  expect_equal(meta$x_trans, "log")
  expect_equal(meta$ref_line, 1)
})

test_that("forest_df handles linear metadata and descriptive table merge", {
  df <- birthwt_forest_data()

  desc <- descriptive_table(
    data = df,
    exposures = c("age", "lwt", "smoke"),
    by = "low",
    percent = column
  )
  uni <- uni_reg(
    df,
    outcome = "bwt",
    exposures = c("age", "lwt", "smoke"),
    approach = linear
  )

  out <- forest_df(uni = uni, desc = desc)
  meta <- attr(out, "forest_meta")

  expect_s3_class(out, "data.frame")
  expect_true("Beta (95% CI)" %in% names(out))
  expect_equal(meta$x_trans, "none")
  expect_equal(meta$ref_line, 0)
  expect_true(any(grepl("Normal BW", names(out), fixed = TRUE)))
  expect_false(is.na(out[out$Characteristic == "age", "Normal BW"]))
  expect_false(is.na(out[out$Characteristic == "lwt", "Normal BW"]))
})

test_that("forest_df pulls continuous descriptive values even when labels differ", {
  df <- birthwt_forest_data()

  desc <- descriptive_table(
    data = df,
    exposures = c("age", "lwt", "smoke"),
    by = "low",
    percent = column
  )
  desc$table_display$Characteristic[desc$table_display$Characteristic == "age"] <- "Maternal age"
  uni <- uni_reg(
    df,
    outcome = "low",
    exposures = c("age", "lwt", "smoke"),
    approach = logit
  )

  out <- forest_df(uni = uni, desc = desc)

  expect_false(is.na(out[out$Characteristic == "age", "Normal BW"]))
  expect_false(is.na(out[out$Characteristic == "age", "Low BW"]))
  expect_match(out[out$Characteristic == "age", "Normal BW"], "\\(")
})

test_that("forest_df preserves continuous descriptive summaries for multivariable tables", {
  df <- birthwt_forest_data()

  desc <- descriptive_table(
    data = df,
    exposures = c("age", "lwt", "smoke", "ht"),
    by = "low",
    percent = column
  )
  multi <- multi_reg(
    df,
    outcome = "low",
    exposures = c("age", "lwt", "smoke", "ht"),
    approach = logit
  )

  out <- forest_df(multi, desc = desc)

  expect_true("Adjusted OR (95% CI)" %in% names(out))
  expect_false(is.na(out[out$Characteristic == "age", "Normal BW"]))
  expect_false(is.na(out[out$Characteristic == "lwt", "Low BW"]))
  expect_true(any(grepl("\\(", out[out$Characteristic == "age", "Normal BW"])))
})

test_that("forest_df handles user-facing positional descriptive workflows", {
  df <- birthwt_forest_data() |>
    dplyr::mutate(
      ptl_cat = factor(ifelse(ptl > 0, "Yes", "No"), levels = c("No", "Yes")),
      ftv_cat = factor(
        dplyr::case_when(
          ftv == 0 ~ "None",
          ftv == 1 ~ "One",
          ftv >= 2 ~ "Two or more"
        ),
        levels = c("None", "One", "Two or more")
      )
    )
  exposures <- c("age", "lwt", "race", "smoke", "ht", "ui", "ptl_cat", "ftv_cat")

  desc <- descriptive_table(
    data = df,
    exposures = exposures,
    by = "low",
    percent = column,
    show_overall = last
  )
  uni <- uni_reg(
    df,
    outcome = "low",
    exposures = exposures,
    approach = logit
  )
  multi <- multi_reg(
    df,
    outcome = "low",
    exposures = c("race", "smoke", "ht", "ui", "ptl_cat", "ftv_cat"),
    adjust_for = c("age", "lwt"),
    approach = logit
  )
  multi_full <- multi_reg(
    df,
    outcome = "low",
    exposures = exposures,
    approach = logit
  )

  uni_desc <- forest_df(uni, desc)
  multi_desc <- forest_df(multi_full, desc = desc)
  both_desc <- forest_df(uni, multi, desc = desc)

  expect_false(is.na(uni_desc[uni_desc$Characteristic == "age", "Normal BW"]))
  expect_false(is.na(uni_desc[uni_desc$Characteristic == "lwt", "Low BW"]))
  expect_true("Adjusted OR (95% CI)" %in% names(multi_desc))
  expect_false("OR (95% CI)" %in% names(multi_desc))
  expect_false(is.na(multi_desc[multi_desc$Characteristic == "age", "Normal BW"]))
  expect_false(is.na(multi_desc[multi_desc$Characteristic == "lwt", "Low BW"]))
  expect_true(all(c("OR (95% CI)", "Adjusted OR (95% CI)") %in% names(both_desc)))
  expect_false(is.na(both_desc[both_desc$Characteristic == "age", "Overall"]))
  expect_equal(both_desc[both_desc$Characteristic == "age", "Adjusted OR (95% CI)"], "")
  expect_equal(both_desc[both_desc$Characteristic == "lwt", "Adjusted OR (95% CI)"], "")
})

test_that("forest_reg builds forest objects from data frames and regression objects", {
  skip_if_not_installed("forestploter")

  df <- birthwt_forest_data()

  uni <- uni_reg(
    df,
    outcome = "low",
    exposures = c("age", "lwt", "smoke", "ht"),
    approach = logit
  )
  multi <- multi_reg(
    df,
    outcome = "low",
    exposures = c("smoke", "ht"),
    adjust_for = c("age", "lwt"),
    approach = logit
  )

  forest_data <- forest_df(uni = uni, multi = multi)

  res_df <- forest_reg(df = forest_data, side = left, quiet = TRUE)
  res_obj <- forest_reg(uni = uni, multi = multi, side = right, quiet = TRUE)

  expect_s3_class(res_df, "gtregression_forest")
  expect_s3_class(res_obj, "gtregression_forest")
  expect_equal(res_df$meta$ref_line, 1)
  expect_equal(res_df$meta$x_trans, "log")
  expect_s3_class(res_df$data, "data.frame")
  expect_invisible(suppressWarnings(print(res_df)))
})

test_that("forest_reg preserves forest_df effect headers in plotted data", {
  skip_if_not_installed("forestploter")

  df <- birthwt_forest_data()

  uni <- uni_reg(
    df,
    outcome = "low",
    exposures = c("age", "lwt", "race", "smoke"),
    approach = logit
  )
  multi <- multi_reg(
    df,
    outcome = "low",
    exposures = c("race", "smoke"),
    adjust_for = c("age", "lwt"),
    approach = logit
  )

  res <- forest_reg(uni = uni, multi = multi, quiet = TRUE)

  expect_true(all(c("OR (95% CI)", "Adjusted OR (95% CI)") %in%
                    names(res$data)))
  expect_true(all(c("se_uni", "se_adj") %in% names(res$input_data)))
  expect_false(any(grepl("Odds Ratio", names(res$data), fixed = TRUE)))
})

test_that("forest_reg preserves adjusted header for multi-only plotted data", {
  skip_if_not_installed("forestploter")

  df <- birthwt_forest_data()
  multi <- multi_reg(
    df,
    outcome = "low",
    exposures = c("race", "smoke"),
    adjust_for = c("age", "lwt"),
    approach = logit
  )

  res <- forest_reg(multi = multi, quiet = TRUE)

  expect_true("Adjusted OR (95% CI)" %in% names(res$data))
  expect_false("OR (95% CI)" %in% names(res$data))
  expect_true("se_uni" %in% names(res$input_data))
  expect_false(any(grepl("Odds Ratio", names(res$data), fixed = TRUE)))
})

test_that("forest_reg leaves default axis ticks to forestploter and respects overrides", {
  skip_if_not_installed("forestploter")

  df <- birthwt_forest_data()
  uni <- uni_reg(
    df,
    outcome = "low",
    exposures = c("age", "lwt", "race", "smoke", "ht", "ui"),
    approach = logit
  )
  multi <- multi_reg(
    df,
    outcome = "low",
    exposures = c("race", "smoke", "ht", "ui"),
    adjust_for = c("age", "lwt"),
    approach = logit
  )

  auto <- forest_reg(uni = uni, multi = multi, quiet = TRUE)
  custom <- forest_reg(
    uni = uni,
    multi = multi,
    ticks_at = list(c(1, 2, 4), c(1, 3, 9)),
    ticks_digits = 1,
    quiet = TRUE
  )

  expect_null(auto$meta$ticks_at)
  expect_equal(custom$meta$ticks_at, list(c(1, 2, 4), c(1, 3, 9)))
  expect_equal(custom$meta$ticks_digits, 1L)
})

test_that("forest_reg validates missing inputs", {
  expect_error(
    forest_reg(df = NULL, uni = NULL, quiet = TRUE),
    "Provide `df = forest_df"
  )
  expect_error(forest_reg(df = mtcars), "`Characteristic` column")
  expect_error(forest_reg(df = "not data"), "`df` must be a data frame")
  expect_error(forest_reg(df = data.frame(Characteristic = "x"), quiet = NA), "`quiet` must be")
  expect_error(
    forest_reg(df = data.frame(Characteristic = "x"), ci_col_width = c(0.1, 0.2, 0.3)),
    "`ci_col_width` must be"
  )
  expect_error(
    forest_reg(df = data.frame(Characteristic = "x"), ci_col_width = 0),
    "`ci_col_width` must be"
  )
  expect_error(
    forest_reg(df = data.frame(Characteristic = "x"), effects = 1),
    "`effects` must be"
  )
})

test_that("forest_df accepts multi-only and desc-only inputs", {
  df <- birthwt_forest_data()

  multi <- multi_reg(
    df,
    outcome = "low",
    exposures = c("smoke", "ht"),
    adjust_for = c("age", "lwt"),
    approach = logit
  )
  desc <- descriptive_table(
    data = df,
    exposures = c("age", "smoke"),
    by = "low"
  )

  multi_only <- forest_df(uni = multi)
  desc_only <- forest_df(uni = desc)

  expect_s3_class(multi_only, "data.frame")
  expect_true("Adjusted OR (95% CI)" %in% names(multi_only))
  expect_false("OR (95% CI)" %in% names(multi_only))
  expect_equal(
    attr(multi_only, "est")[is.finite(attr(multi_only, "est")) &
                              attr(multi_only, "est") != 1][1],
    multi$table_body$estimate[is.finite(multi$table_body$estimate) &
                                !multi$table_body$ref][1]
  )
  expect_s3_class(desc_only, "data.frame")
  expect_true("exposure" %in% names(desc_only))
})

test_that("forest_reg handles multi-only and descriptive-only workflows clearly", {
  skip_if_not_installed("forestploter")

  df <- birthwt_forest_data()
  multi <- multi_reg(
    df,
    outcome = "low",
    exposures = c("smoke", "ht"),
    adjust_for = c("age", "lwt"),
    approach = logit
  )
  desc <- descriptive_table(
    data = df,
    exposures = c("age", "smoke"),
    by = "low"
  )

  df_multi <- forest_df(multi)
  df_desc <- forest_df(desc)

  expect_s3_class(forest_reg(df_multi), "gtregression_forest")
  expect_s3_class(forest_reg(multi = multi), "gtregression_forest")
  expect_error(forest_reg(df_desc), "descriptive-only")
  expect_error(forest_reg(df_desc, df_multi), "do not pass another data frame")
})

test_that("forest_df rejects duplicate and unknown inputs", {
  df <- birthwt_forest_data()
  uni <- uni_reg(df, outcome = "low", exposures = "smoke", approach = logit)
  multi <- multi_reg(df, outcome = "low", exposures = "smoke", approach = logit)

  expect_error(forest_df(uni = uni, digits = -1), "`digits` must be")
  expect_error(forest_df(uni = uni, digits = NA_real_), "`digits` must be")
  expect_error(forest_df(uni = uni, multi = uni), "Two univariate")
  expect_error(forest_df(uni = multi, multi = multi), "Two multivariable")
  expect_error(forest_df(uni = mtcars), "Unknown input type")
  expect_error(.df_from_desc(list()), "`desc` must be")
})

test_that("forest_reg applies default metadata when forest_meta is absent", {
  skip_if_not_installed("forestploter")

  df <- birthwt_forest_data()
  uni <- uni_reg(
    df,
    outcome = "low",
    exposures = c("age", "lwt", "smoke"),
    approach = logit
  )
  forest_data <- forest_df(uni)
  attr(forest_data, "forest_meta") <- NULL

  res <- forest_reg(df = forest_data, side = right, quiet = TRUE)

  expect_s3_class(res, "gtregression_forest")
  expect_equal(res$meta$x_trans, "none")
  expect_equal(res$meta$ref_line, 0)
})
