## Run all focused gtregression 1.2.0 manual API checks ----------------------

if (!"package:gtregression" %in% search()) {
  message(
    "Loading the installed gtregression package. Use devtools::load_all() ",
    "first when checking uninstalled source changes."
  )
  library(gtregression)
}

manual_scripts <- c(
  "08-competing-risk-case-study.R",
  "12-conditional-logit-matched-case-control.R",
  "13-multilevel-clinic-case-study.R",
  "14-ordinal-recovery-case-study.R",
  "15-multinomial-destination-case-study.R",
  "16-import-data-formats.R",
  "17-cox-advanced-survival-api.R",
  "18-risk-difference-api.R"
)

manual_dir <- file.path("dev", "manual-tests")
missing_scripts <- manual_scripts[
  !file.exists(file.path(manual_dir, manual_scripts))
]

if (length(missing_scripts) > 0L) {
  stop(
    "Run this script from the gtregression package root. Missing: ",
    paste(missing_scripts, collapse = ", "),
    call. = FALSE
  )
}

completed <- character()
for (script in manual_scripts) {
  message("\n", strrep("=", 72L))
  message("Running ", script)
  message(strrep("=", 72L))
  sys.source(file.path(manual_dir, script), envir = new.env(parent = globalenv()))
  completed <- c(completed, script)
}

message(
  "\nAll gtregression 1.2.0 API verification scripts completed:\n- ",
  paste(completed, collapse = "\n- ")
)
