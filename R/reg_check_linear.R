.reg_check_linear <- function(model, exposure) {
  lmtest <- getNamespace("lmtest")
  bp <- lmtest::bptest(model)
  sw <- shapiro.test(residuals(model))
  reset <- lmtest::resettest(model, power = 2:3, type = "fitted")
  cooks <- cooks.distance(model)
  n <- nobs(model)
  high_infl <- sum(cooks > (4 / n), na.rm = TRUE)


  interp <- c(
      if (bp$p.value < 0.05) {
        "Heteroskedasticity detected: residual variance may not be constant."
      } else {
        "No evidence of heteroskedasticity."
      },
      if (sw$p.value < 0.05) {
        "Residuals may not be normally distributed.
        Use caution with small samples."
      } else {
        "Residuals appear normally distributed."
      },
      if (reset$p.value < 0.05) {
        "Model may be mis-specified.
        Consider adding non-linear terms or interactions."
      } else {
        "Functional form appears adequate."
      },
      if (high_infl > 0) {
        "Influential points detected.
        Review for outliers or high-leverage observations."
      } else {
        "No strong influential observations detected."
      }
    )

  data.frame(
    Exposure = rep(exposure, 4),
    Test = c("Breusch-Pagan", "Shapiro-Wilk", "RESET", "Cook's Distance"),
    Statistic = c(
      paste0("p = ", signif(bp$p.value, 4)),
      paste0("p = ", signif(sw$p.value, 4)),
      paste0("p = ", signif(reset$p.value, 4)),
      paste0(high_infl, " obs > 4/n (", round(4 / n, 4), ")")
    ),
    Interpretation = format(interp, justify = "left"),
    stringsAsFactors = FALSE
  )
}
