## Manual real-time test: gtregression_app()
## Package: gtregression 1.1
##
## Story:
## A beginner-friendly app should let users move from data to descriptive
## tables, regression, survival analysis, plots, mediation, and export without
## memorising every argument. This script is a guided release rehearsal for the
## Shiny app. Run it section by section from the package root.
##
## This is a manual test script, not a CRAN test. It intentionally launches an
## interactive Shiny app.


## 0. Setup -------------------------------------------------------------------

## During package development, run from the package root:
## devtools::load_all(".")
##
## After installing from GitHub or CRAN, use:
## library(gtregression)

library(gtregression)
library(dplyr)


## 1. Check app dependencies --------------------------------------------------

## shiny is required to launch the app.
## DT and readxl are optional: DT gives interactive data previews, and readxl
## allows Excel upload. The app should still launch without them.

required_for_launch <- "shiny"
optional_for_polish <- c("DT", "readxl")

required_status <- vapply(
  required_for_launch,
  requireNamespace,
  quietly = TRUE,
  FUN.VALUE = logical(1)
)

optional_status <- vapply(
  optional_for_polish,
  requireNamespace,
  quietly = TRUE,
  FUN.VALUE = logical(1)
)

required_status
optional_status

if (!all(required_status)) {
  stop(
    "Install shiny before testing the app: install.packages('shiny')",
    call. = FALSE
  )
}


## 2. Check that the app is bundled -------------------------------------------

app_file <- system.file("shiny", "app.R", package = "gtregression")
app_file
file.exists(app_file)

stopifnot(nzchar(app_file), file.exists(app_file))


## 3. Prepare built-in datasets for app testing -------------------------------

## The app includes built-in datasets, but it is useful to keep ready-made
## objects in your R session as well. These are the same teaching datasets used
## across the package documentation.

data("data_birthwt", package = "gtregression")
data("data_lungcancer", package = "gtregression")
data("data_diabetes_mediation", package = "gtregression")

birthwt_data <- data_birthwt |>
  mutate(
    race = factor(
      race,
      levels = c(1, 2, 3),
      labels = c("White", "Black", "Other")
    ),
    smoke = factor(smoke, levels = c(0, 1), labels = c("No", "Yes")),
    ht = factor(ht, levels = c(0, 1), labels = c("No", "Yes")),
    ui = factor(ui, levels = c(0, 1), labels = c("No", "Yes")),
    low = factor(low, levels = c(0, 1), labels = c("Normal BW", "Low BW")),
    ptl_cat = ifelse(ptl > 0, "Yes", "No"),
    ftv_cat = case_when(
      ftv == 0 ~ "None",
      ftv == 1 ~ "One",
      ftv >= 2 ~ "Two or more"
    ),
    ptl_cat = factor(ptl_cat, levels = c("No", "Yes")),
    ftv_cat = factor(ftv_cat, levels = c("None", "One", "Two or more"))
  )

lung_data <- data_lungcancer |>
  mutate(
    trt = factor(
      trt,
      levels = c(1, 2),
      labels = c("Standard treatment", "Test treatment")
    ),
    prior = factor(prior, levels = c(0, 10), labels = c("No", "Yes")),
    celltype = factor(
      celltype,
      levels = c("squamous", "smallcell", "adeno", "large"),
      labels = c("Squamous", "Small cell", "Adenocarcinoma", "Large cell")
    )
  )

## Useful variable sets to copy into the app if needed.

birthwt_exposures <- c(
  "age", "lwt", "race", "smoke", "ht", "ui", "ptl_cat", "ftv_cat"
)

lung_exposures <- c("age", "karno", "trt", "celltype", "prior")

mediation_covariates <- c(
  "age", "blood_pressure", "pregnancies", "diabetes_pedigree"
)


## 4. Launch the app ----------------------------------------------------------

## This opens the app in the RStudio Viewer when available, otherwise in the
## browser. Keep this R session running while you test the tabs.
##
## Use the Close app button in the bottom-right corner to stop cleanly. This is
## less alarming for beginners than pressing the console stop button.

gtregression_app()


## 5. Manual testing checklist ------------------------------------------------

## Data tab:
##   - Confirm the app starts in Simple mode.
##   - Switch to Advanced mode and confirm extra controls appear on later tabs.
##   - Switch back to Simple mode and confirm the core workflow remains clean.
##   - Click each quick-start template:
##       Birthweight regression
##       Lung survival
##       Diabetes mediation
##       Firth logistic
##   - Confirm templates load labelled data and preselect sensible variables.
##   - Confirm the workflow guide near the top shows one logical
##     "Suggested action" and does not repeatedly say "Next".
##   - Select "Birth weight".
##   - Confirm rows and columns appear.
##   - Upload a small CSV if available and confirm preview updates.
##   - Import an RDS containing a data.frame/tibble and confirm the preview and
##     generated readRDS() code update.
##   - With haven installed, import one DTA and one SAV or SAS file. Confirm the
##     generated code uses the corresponding haven::read_*() function.
##   - Try an RDS containing a non-data-frame object and confirm a clear error.
##   - If DT is not installed, confirm a simple static preview still appears.
##   - Click the Copy button in the Reusable Code panel and paste into the
##     console to confirm the copied code is complete.
##   - After completing several analyses, open Visualise & Export and click
##     Download complete R script. Confirm `gtregression-session.R` contains
##     the data source, Data Prep steps, reference levels, completed analyses,
##     visualisation code, and export commands shown in Full Workflow Code.
##   - Source the downloaded script in a fresh R session and confirm it
##     recreates the completed workflow after any uploaded-data path is updated.

## Data Prep tab:
##   - Immediately after loading data, confirm model tabs ask for an explicit
##     original/prepared data choice rather than silently choosing one.
##   - Click Use original data and confirm the workflow guide enables analysis.
##   - Reload the dataset, apply a rename or grouped-variable change, and click
##     Use prepared data. Confirm the new variable appears in model selectors.
##   - Test Quick starts, Undo, Redo, and Reset.
##   - Create three groups from `age`: first `< 35` as `Young`, second `< 65`
##     as `Older`, and everyone else as `Elder`. Confirm that the preview shows
##     all three groups and that 35 belongs to Older while 65 belongs to Elder.
##   - Repeat with `between` 35 and 64 (inclusive), then test `>=`, `<=`, and
##     `outside the range`. Confirm a second bound appears only for range rules.
##   - Confirm Reset opens a confirmation dialog and clears changes only after
##     confirmation.
##   - Download prepared data as CSV and RDS; open both files.
##   - If rio is installed, also test XLSX and DTA downloads.
##   - Download/copy the reusable preparation code and run it against `df`.
##   - After another preparation change, confirm analysis is paused until the
##     user explicitly chooses original or prepared data again.

## Descriptive tab:
##   - Dataset: Birth weight.
##   - Exposures: age, lwt, race, smoke, ht, ui, ptl_cat, ftv_cat.
##   - By: low.
##   - Click Select all beside Variables to summarise and confirm the By
##     variable is not accidentally selected when a grouping variable is chosen.
##   - Click Clear all and confirm the exposure list is emptied.
##   - Percent: column, then row.
##   - Overall: first, then last.
##   - Confirm the table renders as a publication-style flextable preview.
##   - Download DOCX, RTF, and HTML. The DOCX/RTF output should keep the same
##     flextable-style formatting seen in the app preview.
##   - In Advanced mode, type:
##       age = mean, lwt = median
##     and confirm the Code panel uses statistic = c(age = "mean", lwt = "median").
##   - Type a deliberately malformed statistic override such as:
##       age mean
##     and confirm the descriptive table gives a clear message rather than
##     breaking the whole app.

## Regression tab:
##   - Dataset: Birth weight.
##   - Outcome: low.
##   - Exposures: birthwt_exposures.
##   - Click Select all beside Exposures and confirm the outcome is not selected.
##   - Click Clear all and confirm the exposure list is emptied.
##   - Click Select all beside Adjust for after choosing exposures and confirm
##     the app avoids selecting the outcome and already selected exposures.
##   - Approach: logit.
##   - Run univariable first.
##   - Review the univariable table, then reselect exposures/adjustment variables.
##   - Run multivariable after the variable set is clear.
##   - Toggle show_ref and confirm binary reference rows behave as expected.
##   - Select categorical predictors such as race and smoke. Confirm a Reference
##     categories section appears with only observed levels.
##   - Confirm no reference control appears for continuous age/lwt or outcome low.
##   - Change smoke baseline from No to Yes, run the model, and confirm the
##     coefficient direction and reference row change appropriately.
##   - Confirm the plain-language note says estimates are relative to the
##     selected baseline.
##   - Copy the generated code and confirm it contains factor(),
##     stats::relevel(), the selected baseline, and data = analysis_data.
##   - Download/copy Full Workflow Code and rerun it in a fresh session to
##     confirm the same reference category is used.
##   - Add adjust_for if available and confirm footnotes mention adjustment.
##   - Open each Code panel and confirm the generated code can be copied into
##     RStudio as uni_result <- ... and multi_result <- ....
##   - Use the Copy button rather than manually selecting code.
##   - Try a role conflict deliberately:
##       Select outcome low as an exposure.
##     Confirm the app shows a clear message explaining that the outcome cannot
##     also be selected as an exposure.
##   - In Advanced mode, enable Store model statistics and confirm the Model
##     Stats tab populates after running models.

## Survival tab:
##   - Dataset: Lung cancer.
##   - Confirm the top menu offers Cox regression and Parametric survival.
##   - Select Cox regression and confirm the distribution control is hidden,
##     the Cox run button is shown, and Model Result displays the Cox table.
##   - Select Parametric survival and confirm the distribution control and
##     parametric run button appear, then choose Weibull.
##   - Time: time.
##   - Event: status.
##   - Exposures: lung_exposures.
##   - Leave Single multivariable model clear and run Cox regression. Confirm
##     the result is retained under Exposure models.
##   - Check Single multivariable model, choose the joint predictor set, and
##     run Cox regression again. Confirm Multivariable model is populated and
##     the earlier Exposure models result is still present.
##   - Repeat those two runs in Parametric survival mode and confirm its two
##     result subtabs are retained independently from the Cox results.
##   - Open Visualise & Export > Merge tables. Confirm all completed Cox and
##     parametric exposure/multivariable tables are offered as separate choices.
##   - Confirm the same named results are available to selected-result plots,
##     forest plots, model-fit diagnostics, and compatible Advanced tools.
##   - Under Regression plot, choose Cox exposure + multivariable. Confirm the
##     crude and adjusted HRs appear together and the Code tab uses
##     cox_exposure_result and cox_multi_result.
##   - Under Forest plot, test Cox exposure + multivariable and, after creating
##     a descriptive table, Descriptive + Cox exposure + multivariable.
##   - Repeat both combined plot checks for Parametric exposure + multivariable.
##     Confirm the Code tab uses survreg_exposure_result and
##     survreg_multi_result rather than ordinary regression objects.
##   - Click Select all beside Exposures and confirm time, event, stratifier,
##     and existing adjustment variables are not selected as exposures.
##   - Click Select all beside Adjust for after choosing exposures and confirm
##     the app avoids time, event, stratifier, and exposure overlap.
##   - Run each model from its own menu and confirm generated code contains
##     only the active model call, not both model calls.
##   - Run Kaplan-Meier plot by trt.
##   - Test y_percent, y limits, x limits, risk table, and log-rank p value.
##   - Confirm grid = FALSE gives a clean ggsurvplot-style display by default.
##   - In Advanced mode, set Time break interval to 200 and confirm the KM code
##     includes break_time_by = 200.
##   - Try selecting the survival event variable as an adjustment variable and
##     confirm the app shows a clear role-conflict message.

## Visualise and Export tab:
##   - Confirm the top menu contains Merge tables, Regression plot, Forest plot,
##     and Model fit in one row.
##   - Select each tool and confirm only its controls appear on the left and its
##     corresponding output tab opens on the right.
##   - Treat the browser plots as previews, not final publication exports.
##   - Choose "Selected result only" and preview plot_reg() from each available
##     regression result.
##   - Choose "Crude + adjusted" and preview
##     plot_reg_combine(uni_result, multi_result).
##   - Clear either model, retry combined mode, and confirm the app tells the
##     user to run both univariable and multivariable regression first.
##   - Choose Merge tables after running descriptive, univariable, and
##     multivariable tables. Select any two tables and confirm only those two
##     are merged; then select all three and merge again.
##   - Confirm the Merged Table tab displays merge_tables() output with
##     descriptive, crude, and adjusted sections when those objects are present.
##   - Download the merged table as DOCX, RTF, and HTML.
##   - Confirm table PDF is not offered in the app; use DOCX/RTF/HTML for
##     formatted tables and PNG/PDF only for plots.
##   - Choose "Selected result" and preview forest_df()/forest_reg().
##   - Choose "Crude + adjusted" and confirm it contains both effect columns
##     without descriptive columns.
##   - Choose "Descriptive + crude + adjusted" and confirm descriptive columns
##     are included only in this mode.
##   - Test side = left/right if exposed.
##   - Confirm side, CI column width, x limits, and tick marks remain visible in
##     Simple mode as well as Advanced mode.
##   - For a crowded forest plot, set:
##       Forest x limits: 0.25, 12
##       Forest tick marks: 0.5, 1, 2, 4, 8
##       CI column width: 24
##   - Download plot and forest output from the app.
##   - Confirm completed tables still provide DOCX, RTF, and HTML downloads;
##     HTML should open as a formatted browser table.
##   - Test every export size preset:
##       Standard
##       Wide
##       Many rows
##       Compact
##     Confirm the size hint updates and downloaded files open.
##   - Open the forest PNG. It should be a real image file, not a corrupt HTML
##     or PDF download with a .png extension.
##   - Open the forest PDF. It should render the same forest plot.
##   - Open the Code panel, copy it into RStudio, and confirm:
##       reg_plot <- plot_reg(...)
##       combined_plot <- plot_reg_combine(...)
##       save_plot(reg_plot, ...)
##       forest_data <- forest_df(...)
##       forest_plot <- forest_reg(...)
##       forest_both <- forest_df(uni_result, multi_result, ...)
##       forest_both_plot <- forest_reg(forest_both, ...)
##       save_forest(forest_plot, ...)
##   - For crowded forest plots, test xlim and CI column width guidance.
##   - Use the Copy button and confirm it copies all generated plot/export code.
##   - Open Full Workflow Code after running data, descriptive, regression, and
##     visualise steps. Confirm it gives one readable script-style block.
##   - Copy Full Workflow Code into a fresh R script and check it is easy to
##     adapt manually.

## Advanced tab:
##   - Confirm the top menu contains Select models, Compare models, Confounder,
##     Interaction, Convergence, and Collinearity.
##   - Move through all six tools and confirm only relevant inputs are shown.
##     Cox/survreg should show time and event; ordinary approaches should show
##     outcome. Distribution should appear only for survreg.
##   - Run Convergence on a binomial/log-binomial example.
##   - Run Collinearity and confirm the app asks which fitted multivariable
##     model to inspect rather than silently using a plot selection.
##   - Test Select all and Clear all for Advanced exposures and covariates.
##   - Run Select models, then confirm the output opens automatically and the
##     guidance explains that automated selection is a screening aid.
##   - Open Compare models without fitting models elsewhere in the app.
##   - Choose logit and outcome low. Build these candidates:
##       Clinical core: reported exposure smoke; adjust for age and lwt.
##       Expanded model: reported exposure smoke; adjust for age, lwt, race,
##       and ht.
##   - Track smoke as the primary exposure, run the comparison, and confirm the
##     table uses the names Clinical core and Expanded model.
##   - Confirm generated code first creates two multi_reg() objects and then
##     passes those objects to compare_models().
##   - Add a third candidate, give it a unique name, include an interaction
##     between two predictors already in that model, and run again.
##   - Remove the last candidate and confirm at least two models always remain.
##   - Repeat with Cox and survreg. Confirm the shared controls change to time,
##     event, and (for survreg) distribution, and that generated code uses
##     cox_reg() or surv_reg() rather than survival:: model calls.
##   - Run Confounder and Interaction for logit.
##   - Confirm identify_confounder() is available as a visible button, not only
##     hidden in generated code.
##   - Repeat Confounder and Interaction for Cox/survreg if time and event variables
##     are selected.
##   - Confirm every table can be downloaded as DOCX, RTF, and HTML.
##   - Confirm the Code panel gives complete copy-paste commands for
##     identify_confounder(), interaction_models(), select_models(), and
##     compare_models().
##   - Confirm warning/status messages are clear and not hidden.
##   - Confirm role-conflict messages are short and actionable.
##   - Confirm compare_models() output uses fitted gtregression objects rather
##     than asking the user to fit survival::coxph() manually.

## Mediation tab:
##   - Dataset: Diabetes mediation.
##   - Exposure: obesity.
##   - Mediator: glucose.
##   - Outcome: diabetes.
##   - Covariates: mediation_covariates.
##   - Click Select all beside Covariates and confirm exposure, mediator, and
##     outcome are not selected as covariates.
##   - Outcome approach: logit.
##   - Run with sims = 300 first, then 500.
##   - Confirm table and path diagram render.
##   - Confirm the Diabetes mediation quick-start template preselects obesity,
##     glucose, diabetes, and common covariates.
##   - Confirm the Mediation Code panel copies complete mediation_analysis()
##     and plot_mediation() code.

## Help tab:
##   - Confirm beginner guidance is readable.
##   - Confirm function coverage is clear.
##   - Confirm users are directed toward the package articles for deeper
##     workflows.
##   - Confirm the Close app guidance is visible.
##   - Confirm the guide explains that copied code should be saved in RStudio
##     for final publication exports.


## 6. Quick smoke test without launching --------------------------------------

## Use this when you only want to confirm the app file parses. It should return
## a shiny.appobj without opening the browser.

app_env <- new.env(parent = globalenv())
app_obj <- source(app_file, local = app_env)$value
inherits(app_obj, "shiny.appobj")

stopifnot(inherits(app_obj, "shiny.appobj"))
