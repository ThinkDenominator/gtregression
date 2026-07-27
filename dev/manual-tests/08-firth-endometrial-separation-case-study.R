## Manual test: Firth logistic regression for separation ----------------------
##
## Story:
## The endometrial cancer dataset is a classic example where ordinary logistic
## regression can struggle because neovascularization is absent among low-grade
## cases. Firth penalized logistic regression is useful here because it gives
## finite, stable odds-ratio estimates while keeping the same gtregression
## workflow.
##
## Run this script from the package root after:
## devtools::document()
## devtools::load_all(".")

library(dplyr)
library(gtregression)


## 1. Load and understand the dataset ----------------------------------------

data("data_endometrial", package = "gtregression")

endometrial_data <- data_endometrial |>
  mutate(
    HG = factor(HG, levels = c(0, 1),
                labels = c("Low grade", "High grade")),
    NV = factor(NV, levels = c(0, 1),
                labels = c("Absent", "Present"))
  )

attr(endometrial_data$HG, "label") <- "High histology grade"
attr(endometrial_data$NV, "label") <- "Neovascularization"
attr(endometrial_data$PI, "label") <- "Pulsatility index"
attr(endometrial_data$EH, "label") <- "Endometrium height"

head(endometrial_data)
str(endometrial_data)

## The key sparse-cell pattern: no low-grade cases have neovascularization.
table(endometrial_data$HG, endometrial_data$NV)


## 2. Descriptive table -------------------------------------------------------

endometrial_summary <- descriptive_table(
  data = endometrial_data,
  exposures = c(NV, PI, EH),
  by = HG,
  statistic = c(PI = mean, EH = mean),
  percent = column,
  show_overall = last
)

endometrial_summary


## 3. Compare standard logistic and Firth logistic regression -----------------

## Standard logistic regression may show very large estimates, very wide
## intervals, or convergence/separation warnings in sparse data.
uni_logit <- uni_reg(
  data = endometrial_data,
  outcome = HG,
  exposures = c(NV, PI, EH),
  approach = logit
)

uni_logit

## Firth logistic regression uses the same interface. The only change is
## approach = firth.
uni_firth <- uni_reg(
  data = endometrial_data,
  outcome = HG,
  exposures = c(NV, PI, EH),
  approach = firth
)

uni_firth
uni_firth$table_body


## 4. Multivariable Firth model ----------------------------------------------

multi_firth <- multi_reg(
  data = endometrial_data,
  outcome = HG,
  exposures = c(NV, PI, EH),
  approach = firth,
  model_stats = TRUE
)

multi_firth
multi_firth$model_stats


## 5. Publication polish ------------------------------------------------------

multi_firth_paper <- modify_table(
  multi_firth,
  caption = "Firth logistic regression for high-grade endometrial cancer",
  caveat = paste(
    "Firth penalized logistic regression is useful when standard logistic",
    "regression has sparse cells or separation concerns."
  )
)

multi_firth_paper


## 6. Visualise Firth regression results --------------------------------------

plot_reg(
  uni_firth,
  title = "Univariable Firth logistic regression"
)

plot_reg(
  multi_firth,
  title = "Multivariable Firth logistic regression"
)

plot_reg_combine(
  uni_firth,
  multi_firth,
  title_uni = "Crude Firth odds ratios",
  title_multi = "Adjusted Firth odds ratios"
)


## 7. Forest table workflow ---------------------------------------------------

firth_forest_data <- forest_df(uni_firth, multi_firth, desc = endometrial_summary)
firth_forest_data

forest_reg(firth_forest_data)

## If x-axis labels overlap, control the plotting range explicitly:
forest_reg(
  firth_forest_data,
  xlim = c(0.1, 100),
  ticks_at = c(0.1, 0.5, 1, 2, 10, 100)
)


## 8. Merge descriptive and regression tables --------------------------------

final_firth_table <- merge_tables(
  endometrial_summary,
  uni_firth,
  multi_firth,
  spanners = c("Clinical profile", "Crude Firth OR", "Adjusted Firth OR")
)

final_firth_table


## 9. Final checklist ---------------------------------------------------------

## Things to confirm manually:
## - data_endometrial loads without extra packages.
## - descriptive_table() shows the separation pattern clearly.
## - approach = firth works with quoted and unquoted arguments.
## - uni_reg() and multi_reg() return OR tables with Ref. rows.
## - plot_reg(), plot_reg_combine(), forest_df(), and forest_reg() work.
## - merge_tables() combines descriptive, crude Firth, and adjusted Firth tables.

## End of manual Firth case study --------------------------------------------
