## Manual real-time test: gtregression_app()
## Package: gtregression 1.2
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

## Trials tab (risk difference). Same coding as
## dev/manual-tests/18-risk-difference-api.R, so the app must reproduce the
## numbers listed there. `bw_trial` is chosen from R Environment in the app.

bw_trial <- data_birthwt |>
  mutate(
    smoke = factor(smoke, levels = c(0, 1), labels = c("Non-smoker", "Smoker")),
    low = factor(low, levels = c(0, 1), labels = c("Normal BW", "Low BW")),
    race = factor(race, levels = c(1, 2, 3), labels = c("White", "Black", "Other")),
    visit = factor(ftv > 0, levels = c(FALSE, TRUE), labels = c("No visit", "Visit"))
  )


## 4. Launch the app ----------------------------------------------------------

## This opens GT Regression in your web browser (Addins > GT Regression app
## opens the same app in an RStudio dialog window). Keep this R session
## running while you test the tabs.
##
## Use the Close button (power icon) at the top right to stop cleanly. This is
## less alarming for beginners than pressing the console stop button.

gtregression_app()


## 5. Manual testing checklist ------------------------------------------------

## Data tab:
##   - Confirm there is no global Simple/Advanced mode switch.
##   - Confirm less-common controls, such as the KM time break interval, appear
##     in a local Additional options section.
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
##   - After completing several analyses, open Session Code and click
##     Download complete R script. Confirm `gtregression-session.R` contains
##     the data source, Data Prep steps, reference levels, completed analyses,
##     visualisation code, and export commands shown in Session Code.
##   - Source the downloaded script in a fresh R session and confirm it
##     recreates the completed workflow after any uploaded-data path is updated.

## Data Prep tab:
##   - Immediately after loading data, confirm model tabs can use the original
##     data without any extra selection. Choose prepared data only after
##     applying a change.
##   - Click Use original data and confirm the workflow guide enables analysis.
##   - Reload the dataset, apply a rename or grouped-variable change, and click
##     Use prepared data. Confirm the new variable appears in model selectors.
##   - Use Set display label to label `age` as Maternal age. Select age in a
##     descriptive or regression analysis and confirm the output shows Maternal
##     age while the selector and generated analysis code still use `age`.
##   - Select a categorical variable and change a category display label, for
##     example `0` to `No` and `1` to `Yes`. Confirm output uses the new labels
##     and the generated preparation code contains the reproducible recode.
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
##   - After another preparation change, confirm the existing source remains
##     clear in the status line; click Use prepared data to use the latest copy.

## Descriptive tab:
##   - Dataset: Birth weight.
##   - Select Group by `low` before choosing the variables to summarise; confirm
##     the selected grouping variable is excluded from Select all.
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
##   - In Numeric statistic override, type:
##       age = mean, lwt = median
##     and confirm the Code panel uses statistic = c(age = "mean", lwt = "median").
##   - Type a deliberately malformed statistic override such as:
##       age mean
##     and confirm the descriptive table gives a clear message rather than
##     breaking the whole app.

## Visualise & Export tab:
##   - Under Model Fit, confirm Auto, All diagnostics, residual, Q-Q,
##     scale-location, Cook's distance, observed-vs-predicted, and calibration
##     are available. Confirm Calibration bins and Base font size appear in the
##     generated code and Session Code.
##   - Run a descriptive or regression table, select Modify table, and choose
##     the completed table as the source.
##   - Enter `age = Maternal age` under Variable labels and confirm only the
##     display label changes.
##   - Enter `smoke: Yes = Smoker` under Level labels and confirm the factor
##     level changes without altering the reference category.
##   - Test a header alias such as `p.value = P value`, a caption, bold levels,
##     removal of the adjustment note, and an additional note.
##   - Confirm Modified Table downloads as DOCX, RTF, and HTML, while the
##     original source table remains unchanged.

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
##   - Download/copy Session Code and rerun it in a fresh session to
##     confirm the same reference category is used.
##   - Add adjust_for if available and confirm footnotes mention adjustment.
##   - Open each Code panel and confirm the generated code can be copied into
##     RStudio as uni_result <- ... and multi_result <- ....
##   - Use the Copy button rather than manually selecting code.
##   - Try a role conflict deliberately:
##       Select outcome low as an exposure.
##     Confirm the app shows a clear message explaining that the outcome cannot
##     also be selected as an exposure.
##   - Confirm Model Stats populates after univariable or multivariable models.
##     Clear Store model statistics, fit a new model, and confirm statistics are
##     not retained for that newly fitted result.

## Special Models tab:
##   - Confirm the top menu offers Ordered outcome, Unordered outcome,
##     Clustered data, and Matched case-control without crowding Regression.
##   - For each mode, confirm only its design-specific controls are visible.
##   - Confirm Select all and Clear all work for exposures and adjustment
##     variables and do not select the outcome.
##   - Select categorical predictors and confirm their observed reference
##     categories appear; continuous predictors and the outcome must not appear.
##   - Ordered outcome: use an ordered-factor outcome with at least three levels.
##     Confirm the preflight note shows the level order, the table renders, and
##     the assumption check appears when requested.
##   - Change that outcome to an unordered factor and confirm the preflight note
##     asks the user to set the order in Data Prep rather than fitting silently.
##   - Unordered outcome: use an unordered outcome with at least three levels.
##     Confirm the preflight note identifies the first outcome level as the
##     reference and the table presents comparisons side by side.
##   - Clustered data: select a grouping variable, then test continuous and
##     binary outcomes. Confirm the generated call uses multilevel_reg().
##   - Matched case-control: select a binary outcome and matched-set identifier.
##     Confirm the generated call uses conditional_logit_reg().
##   - For every mode, inspect Model Stats and copy the generated code. Confirm
##     predictor releveling and all design variables are preserved.
##   - Leave Single multivariable model off and run a model. Confirm its table
##     remains under Exposure Models. Turn the option on, run the joint model,
##     and confirm it appears under Multivariable Model without replacing the
##     exposure-model table.
##   - Confirm both retained results appear independently in Modify table and
##     Merge tables, and that Session Code contains the exact code captured
##     when each result was run.
##   - Open Model Stats and Assumption Check after retaining both results.
##     Switch Result to inspect and confirm each panel follows the selected
##     result. Confirm a clear note replaces a blank table when statistics were
##     not stored or the proportional-odds check does not apply.
##   - Open Visualise & Export. Confirm multinomial comparisons are offered
##     separately in Regression plot and Forest plot.
##   - Retain compatible exposure and multivariable ordinal models. Confirm
##     Special exposure + multivariable appears for Regression plot and Forest
##     plot, and that both previews and generated code use the retained pair.
##   - Repeat with multinomial models. Confirm one combined choice appears per
##     shared outcome comparison and no choice combines different outcomes.
##   - Create a descriptive table and confirm Descriptive + Special Models is
##     available for the forest output without changing the selected pair.
##   - Confirm Special Models are not listed under Model fit; its general
##     diagnostic plots do not apply consistently across these model families.
##   - Confirm retained Special Models are available under Modify table and
##     that Session Code includes both completed special-model calls.

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
##   - Open Additional KM options, set Time break interval to 200, and confirm the KM code
##     includes break_time_by = 200.
##   - Try selecting the survival event variable as an adjustment variable and
##     confirm the app shows a clear role-conflict message.
##   - Select Competing risks and confirm observed censoring/event codes appear
##     only after an event-status variable has been selected.
##   - Confirm Kaplan-Meier controls are hidden and cumulative-incidence
##     controls are shown in competing-risk mode.
##   - Use a dataset with censoring, the event of interest, and at least one
##     competing event. Confirm the preflight note lists observed code counts.
##   - Run Fine-Gray regression and confirm the table reports SHR (95% CI), the
##     selected event, and model statistics when requested.
##   - Draw cumulative incidence without and with a grouping variable. Confirm
##     Gray's test appears for grouped curves when requested.
##   - Download the Fine-Gray table as DOCX, RTF, and HTML, and the cumulative-
##     incidence plot as PNG and PDF.
##   - Confirm Code and Session Code preserve censoring code, event of interest,
##     grouping variable, plot options, and model specification.
##   - Confirm the Fine-Gray result is offered for modify/merge and selected
##     plot/forest workflows, but not under the generic Model fit tool.

## Trials tab (risk difference):
##   - Load `bw_trial` from R Environment and open Trials (between Survival
##     and More models). With no outcome chosen, the ready line says
##     "Choose outcome." and the result area shows the same prompt.
##   - Outcome: race. Confirm the Event field says "Needs two values" and the
##     ready line explains that race has 3 values. Then choose Outcome: low.
##   - Confirm Event defaults to Low BW and offers only Normal BW and Low BW.
##   - Treatment group: race. Confirm "exactly two are compared" appears.
##     Then choose Treatment group: smoke.
##   - Confirm Reference group defaults to Non-smoker and the ready line reads
##     "Ready · Risk difference · low = Low BW · Smoker − Non-smoker ·
##     unadjusted · N=189".
##   - Click Details and confirm it names the Miettinen-Nurminen method and
##     "189 of 189 rows".
##   - Click Risk difference. Expected table: Non-smoker 29 / 115, 25.2%, Ref.;
##     Smoker 30 / 74, 40.5%, 15.3 (1.7 to 29.0), p = 0.027.
##   - Set Number needed to treat to Event is harmful and run again. Expected:
##     NNT column NNTH 7 (4 to 59) and the NNT caution in the footnote.
##   - Add Adjust for: age, lwt, race (use the + picker). Run. Expected:
##     Adjusted risk 23.4% and 44.3%; 21.0 (6.9 to 35.1); p = 0.004;
##     NNTH 5 (3 to 15); footnote "Wald, influence-function standard error".
##   - Change Reference group to Smoker and run. Confirm the sign reverses
##     (Non-smoker -21.0) and the ready line reads Non-smoker − Smoker.
##   - Clear Adjust for, Treatment group: visit, Reference group: No visit,
##     Event is harmful, run. Expected: -10.2 (-23.0 to 3.1) and
##     NNTB 10 (NNTB 5 to ∞ to NNTH 32).
##   - Open Options, set Confidence level to 0.9. Confirm the Options badge
##     shows 1 and the ready line says "options changed". Run again and
##     confirm the header reads "(90% CI)".
##   - Open Code. Confirm it starts from analysis_data and contains
##     rd_result <- risk_difference(...) with outcome, exposure, reference,
##     event, event_type and conf_level = 0.9 exactly as chosen. Copy it, run
##     it in the console with `analysis_data <- bw_trial`, and confirm the same
##     table.
##   - Export the table as DOCX, RTF and HTML and open each file.
##   - Open Session Code. Confirm "# 6b. Risk difference (Trials)" holds the
##     call as it was last run (change a setting without running and confirm
##     Session Code does not change).
##   - Choose Adjust for: low (the outcome) and run. Confirm a clear error
##     notification and that the previous table is cleared.
##   - In Data > Prepare, rename smoke to smoking. Return to Trials and confirm
##     Treatment group follows the rename and Reference group is kept.
##   - Confirm Visualise & Export does not offer the risk-difference table for
##     merging, plotting or forest plots.

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
##   - Confirm side, CI column width, x limits, and tick marks remain visible.
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
##   - Open Session Code after running data, descriptive, regression, and
##     visualise steps. Confirm it gives one readable script-style block.
##   - Copy Session Code into a fresh R script and check it is easy to
##     adapt manually.

## Advanced tab:
##   - Under Confounder, select two or more exposures and one candidate. Confirm
##     the output has one screening row per exposure-candidate combination.
##   - Under Interaction, select covariates and confirm the generated code uses
##     covariates = c(...) so both compared models are adjusted consistently.
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
