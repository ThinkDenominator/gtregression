## Figures for the pkgdown home page ------------------------------------------
##
## Run from the package root after devtools::load_all() (or with the installed
## package) to refresh the images in man/figures/ used by README.Rmd.
## The images are illustrations of real package output on the bundled data.

library(gtregression)
library(dplyr)

out_dir <- file.path("man", "figures")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

data("data_birthwt", package = "gtregression")
data("data_lungcancer", package = "gtregression")

bw <- data_birthwt |>
  mutate(
    race = factor(race, levels = c(1, 2, 3), labels = c("White", "Black", "Other")),
    smoke = factor(smoke, levels = c(0, 1), labels = c("No", "Yes")),
    ht = factor(ht, levels = c(0, 1), labels = c("No", "Yes")),
    ui = factor(ui, levels = c(0, 1), labels = c("No", "Yes")),
    low = factor(low, levels = c(0, 1), labels = c("Normal BW", "Low BW"))
  )
attr(bw$age, "label") <- "Maternal age (years)"
attr(bw$lwt, "label") <- "Maternal weight (lb)"
attr(bw$race, "label") <- "Race"
attr(bw$smoke, "label") <- "Smoking during pregnancy"
attr(bw$ht, "label") <- "History of hypertension"
attr(bw$ui, "label") <- "Uterine irritability"

exposures <- c("age", "lwt", "race", "smoke", "ht", "ui")

desc <- descriptive_table(bw, exposures = exposures, by = "low",
                          show_dichotomous = "all_levels")
crude <- uni_reg(bw, outcome = "low", exposures = exposures,
                 approach = "logit", show_ref = TRUE)
adjusted <- multi_reg(bw, outcome = "low", exposures = exposures,
                      approach = "logit", show_ref = TRUE)

## 1. Merged manuscript table
table_2 <- merge_tables(desc, crude, adjusted,
                        spanners = c("Low birth weight", "Crude", "Adjusted"))
ft_table <- flextable::line_spacing(table_2$table, space = 1.3, part = "footer")
flextable::save_as_image(ft_table, path = file.path(out_dir, "home-table.png"),
                         res = 150)

## 2. Forest plot of crude and adjusted estimates
forest <- forest_reg(
  forest_df(crude, adjusted),
  xlim = list(c(0.5, 16), c(0.5, 32)),
  ticks_at = list(c(0.5, 1, 2, 4, 8, 16), c(0.5, 1, 4, 16)),
  ci_col_width = 30
)
save_forest(forest, filename = file.path(out_dir, "home-forest.png"),
            format = "png", width = 9.5, height = 5, dpi = 170)

## 3. Kaplan-Meier curves
lung <- data_lungcancer |>
  mutate(trt = factor(trt, levels = c(1, 2), labels = c("Standard", "Test")))
km <- km_plot(lung, time = "time", event = "status", by = "trt",
              legend_title = "Treatment", xlab = "Days since randomisation",
              p_value_position = c(600, 0.6), break_time_by = 200)
save_plot(km, filename = file.path(out_dir, "home-km.png"),
          format = "png", width = 8, height = 6, dpi = 170)

## 4. Risk difference with number needed to treat
bw_rd <- mutate(bw, smoke = factor(smoke, labels = c("Non-smoker", "Smoker")))
attr(bw_rd$smoke, "label") <- "Smoking during pregnancy"
attr(bw_rd$low, "label") <- "Low birth weight"
rd <- risk_difference(
  bw_rd,
  outcome = "low", exposure = "smoke", event = "Low BW",
  reference = "Non-smoker", adjust_for = c("age", "lwt", "race"),
  event_type = "harmful", format = "flextable"
)
flextable::save_as_image(flextable::line_spacing(rd$table, space = 1.3, part = "footer"),
                         path = file.path(out_dir, "home-rd.png"),
                         res = 150)
