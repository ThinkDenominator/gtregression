#' Check Collinearity Using VIF for Fitted Models
#'
#' Computes Variance Inflation Factors (VIF) for fitted models returned by
#' uni_reg(), multi_reg(), uni_reg_nbin(), or multi_reg_nbin().
#' Returns one VIF table per model. For multivariate models only
#'
#' @param model A fitted model object with class "uni_reg", "multi_reg",
#'   "uni_reg_nbin", or "multi_reg_nbin".
#' @importFrom stats terms
#' @return A tibble containing VIF values and interpretation.
#' For multivariable models, returns one tibble.
#' For univariate models, an error is raised indicating VIF is not applicable.
#' @examples
#' if (requireNamespace("gtregression", quietly = TRUE) &&
#'   requireNamespace("mlbench", quietly = TRUE) &&
#'   getRversion() >= "4.1.0") {
#'   data(PimaIndiansDiabetes2, package = "mlbench")
#'   pima <- PimaIndiansDiabetes2 |> dplyr::filter(!is.na(diabetes))
#'   pima$diabetes <- ifelse(pima$diabetes == "pos", 1, 0)
#'   fit <- multi_reg(pima,
#'     outcome = "diabetes",
#'     exposures = c("age", "mass", "glucose"),
#'     approach = "logit"
#'   )
#'   check_collinearity(fit)
#' }
#' @export
check_collinearity <- function(model) {
  # Ensure required package is available
  if (!requireNamespace("car", quietly = TRUE))
    stop("Package 'car' is required.")

  # Define appropriate sources for VIF checking
  valid_sources <- c("uni_reg", "multi_reg", "uni_reg_nbin", "multi_reg_nbin")

  # Extract model type and model list from object attributes
  model_source <- attr(model, "source")
  model_list <- attr(model, "models")

  # Validate source and model content
  if (is.null(model_source) || !(model_source %in% valid_sources)) {
    stop("Input must be a fitted model from uni_reg, multi_reg, uni_reg_nbin,
         or multi_reg_nbin.")
  }
  if (is.null(model_list)) {
    stop("Model list not found in object. Cannot compute VIF.")
  }

  # Throw error for uni_reg (univariate)
  if (model_source %in% c("uni_reg", "uni_reg_nbin")) {
    stop("VIF is not applicable for univariate models.
         Use multi_reg() to check collinearity among predictors.")
  }

  # For multi_reg and multi_reg_nbin
  # Extract the fitted model from the model list
  fit_model <- model_list[[1]]
  if (!inherits(fit_model, c("glm", "lm"))) {
    stop("Unsupported model type for VIF calculation.")
  }

  # Ensure at least two predictors
  if (length(attr(terms(fit_model), "term.labels")) < 2) {
    stop("Model must have at least two predictors to compute VIF.")
  }

  # Compute VIF using the 'car' package
  vif_vals <- tryCatch(car::vif(fit_model), error = function(e) NULL)

  if (is.matrix(vif_vals)) {
    vif_vals <- vif_vals[, 1] # extract GVIF column- first col
  }

  # Return a tibble with interpretation
  tibble::tibble(
    Variable = names(vif_vals),
    VIF = round(as.numeric(vif_vals), 2),
    Interpretation = dplyr::case_when(
      VIF < 2 ~ "No collinearity",
      VIF >= 2 & VIF < 5 ~ "Moderate",
      VIF >= 5 ~ "High"
    )
  )
}
