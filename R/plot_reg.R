#' Visualize a Regression Model as a Forest Plot
#'
#' Creates a forest plot from a `gtsummary`-style object produced by
#' `gtregression` functions (e.g., `uni_reg()`, `multi_reg()`).
#' The function supports both univariate and multivariable models,
#' renders hierarchical labels (variable headers vs. levels), and
#' computes significance highlighting using either *p*-values (linear models)
#' or CI-vs-reference rules (non-linear models).
#'
#' @param tbl A `gtsummary`-like object returned by `gtregression`
#'   (must contain `table_body` and attributes `source` and `approach`).
#' @param title Optional plot title (character).
#' @param order_y Optional character vector to customize the y-axis header ordering.
#' @param log_x Logical. If `TRUE`, log x-axis (ignored for linear models).
#' @param xlim Optional numeric vector of length 2 for x-axis limits.
#' @param breaks Optional numeric vector for x-axis tick breaks (ignored if `log_x = TRUE`).
#' @param point_color Fill color for points (default `"#1F77B4"`).
#' @param errorbar_color Color for all error bars (default `"#4C4C4C"`).
#' @param base_size Base font size for `theme_minimal()` (default `14`).
#' @param show_ref Logical. If `TRUE`, includes the reference level on the plot and labels it `(Ref.)`.
#' @param sig_color Optional fill color for **significant** points; if `NULL`,
#'   significant points reuse `point_color`.
#' @param sig_errorbar_color Optional color for **significant** error bars; if `NULL`,
#'   significant bars reuse `errorbar_color`.
#' @param alpha Significance level for linear models when `p.value` is available (default `0.05`).
#'
#' @details
#' **Reference line**: The vertical reference is fixed at `0` for linear models and `1` for all
#' other approaches, inferred from `attr(tbl, "approach")`.
#'
#' **Header / data detection**: Variable headers are recognized via `row_type == "label"`
#' together with `header_row` or missing CI; categorical levels use `row_type == "level"`;
#' continuous predictors appear as `row_type == "label"` **with** CIs and are treated as data rows.
#'
#' **Significance highlighting**:
#' - For `approach == "linear"` with available `p.value`, rows are significant when `p.value < alpha`.
#' - Otherwise, rows are significant when the CI does not cross the reference (`0` or `1` as above).
#'   Use `sig_color` / `sig_errorbar_color` to customize the appearance.
#'
#' @return A `ggplot2` object representing the forest plot.
#'
#'
#' @seealso \code{\link{uni_reg}}, \code{\link{multi_reg}}, \code{\link{plot_reg_combine}}
#'
#' @importFrom rlang .data
#' @importFrom stats setNames
#' @examples
#' \donttest{
#' if (requireNamespace("mlbench", quietly = TRUE) &&
#'     requireNamespace("gtregression", quietly = TRUE)) {
#'   data("PimaIndiansDiabetes2", package = "mlbench")
#'   pima <- PimaIndiansDiabetes2
#'   pima$diabetes <- ifelse(pima$diabetes == "pos", 1, 0)
#'   pima$bmi_cat <- cut(
#'     pima$mass,
#'     breaks = c(-Inf, 18.5, 24.9, 29.9, Inf),
#'     labels = c("Underweight", "Normal", "Overweight", "Obese")
#'   )
#'
#'   # Univariate logistic regression table via gtregression
#'   tbl_uni <- gtregression::uni_reg(
#'     data = pima,
#'     outcome = "diabetes",
#'     exposures = c("age", "bmi_cat"),
#'     approach = "logit"
#'   )
#'
#'   p <- plot_reg(tbl_uni, title = "Univariate (logit)", sig_color = "#D55E00")
#'   print(p)
#' }
#' }
#' @export
plot_reg <- function(tbl,
                     title = NULL,
                     order_y = NULL,
                     log_x = FALSE,
                     xlim = NULL,
                     breaks = NULL,
                     point_color = "#1F77B4",
                     errorbar_color = "#4C4C4C",
                     base_size = 14,
                     show_ref = TRUE,
                     sig_color = NULL,
                     sig_errorbar_color = NULL,
                     alpha = 0.05) {
  df <- tbl$table_body

  # meta
  source_type <- attr(tbl, "source")
  approach    <- attr(tbl, "approach")

  # reference line: 0 for linear, 1 otherwise
  ref_line <- if (identical(approach, "linear")) 0 else 1

  # axis label
  is_multi <- source_type %in% c("multi_reg", "multi_reg_nbin")
  base_label <- dplyr::case_when(
    approach == "logit" ~ "Odds Ratio",
    approach == "log-binomial" ~ "Risk Ratio",
    approach %in% c("poisson", "negbin") ~ "Incidence Rate Ratio",
    approach == "robpoisson" ~ "Risk Ratio",
    approach == "linear" ~ "Beta Coefficient",
    TRUE ~ "Effect Size"
  )
  x_axis_label <- if (is_multi) paste("Adjusted", base_label) else base_label
  if (identical(approach, "linear")) log_x <- FALSE
  if (log_x) x_axis_label <- paste0(x_axis_label, " (log scale)")

  # --- LABELS: robust header/data/ref detection using row_type + header_row ----
  df <- df |>
    dplyr::mutate(
      # vectorized ref flag (TRUE only for the reference level; NA -> FALSE)
      ref_flag = (.data$reference_row %in% TRUE),

      # header rows: variable headers (categoricals) are row_type=="label" with either
      # header_row==TRUE OR no CI present; continuous "label" rows have CIs -> not headers
      is_header = (.data$row_type == "label") &
        ( (.data$header_row %in% TRUE) |
            (is.na(.data$conf.low) & is.na(.data$conf.high)))|
        (.data$var_type == "continuous"),

      # data rows: all level rows + label rows that are NOT headers (e.g., continuous vars)
      is_data = (.data$row_type == "level") | ((.data$row_type == "label") & !.data$is_header),

      # y-axis labels
      label_clean = dplyr::case_when(
        is_header ~ .data$label,                                   # header: no indent
        ref_flag  & show_ref ~ paste0("  ", .data$label, " (Ref.)"),# ref level
        is_data   ~ paste0("  ", .data$label),                      # data rows (levels + continuous)
        TRUE ~ NA_character_
      )
    )

  # --- KEEP ROWS (headers + data; show ref row even if no estimate) --------------
  df <- dplyr::filter(
    df,
    .data$is_header | .data$is_data | (.data$ref_flag & show_ref)
  )

  # --- y factor + stable label mapping ------------------------------------------
  df$row_id <- factor(seq_len(nrow(df)), levels = rev(seq_len(nrow(df))))
  label_map <- stats::setNames(df$label_clean, as.character(df$row_id))
  label_fun <- function(x) unname(label_map[as.character(x)])

  # --- SIGNIFICANCE --------------------------------------------------------------
  # Linear: prefer p.value if available; otherwise fall back to CI rule.
  # Non-linear: keep your CI-vs-ref rule.
  cutoff <- ref_line
  has_p  <- "p.value" %in% names(df)

  # Ensure numeric for safe comparisons (no behavioral change if already numeric)
  num_cols <- intersect(c("estimate", "conf.low", "conf.high"), names(df))
  df[num_cols] <- lapply(df[num_cols], function(x) suppressWarnings(as.numeric(x)))

  if (identical(approach, "linear") && has_p) {
    df <- dplyr::mutate(
      df,
      is_sig = dplyr::case_when(
        !is_data | ref_flag ~ FALSE,                         # only color true data rows
        !is.na(.data$p.value) & (.data$p.value < alpha) ~ TRUE,
        TRUE ~ FALSE
      )
    )
  } else {
    df <- dplyr::mutate(
      df,
      is_sig = dplyr::case_when(
        !is_data | ref_flag ~ FALSE,                         # only color true data rows
        is.finite(conf.low) & is.finite(conf.high) & (conf.high >= conf.low) &
          ((conf.low > cutoff) | (conf.high < cutoff)) ~ TRUE,
        TRUE ~ FALSE
      )
    )
  }



  # optional ordering by header blocks
  if (!is.null(order_y)) {
    df <- dplyr::mutate(
      df,
      header_order = dplyr::case_when(
        is_header ~ match(.data$label, order_y),
        TRUE ~ NA_real_
      )
    )
    df <- tidyr::fill(df, header_order, .direction = "down")
    df <- dplyr::arrange(df, header_order, dplyr::row_number())
  }


  # colors (keep existing logic)
  fill_vals <- c("FALSE" = point_color,
                 "TRUE"  = if (!is.null(sig_color)) sig_color else point_color)
  line_vals <- c("FALSE" = errorbar_color,
                 "TRUE"  = if (!is.null(sig_errorbar_color)) sig_errorbar_color else errorbar_color)

  # auto xlim
  if (is.null(xlim)) {
    vals <- c(df$conf.low, df$conf.high, df$estimate, ref_line)
    vals <- vals[is.finite(vals)]
    if (length(vals) >= 2L) {
      rng <- range(vals, na.rm = TRUE)
      if (log_x) {
        lower <- max(min(rng[1], ref_line, na.rm = TRUE), .Machine$double.eps * 10)
        upper <- max(rng[2], ref_line, na.rm = TRUE)
      } else {
        span <- diff(rng)
        pad  <- if (is.finite(span) && span > 0) 0.05 * span else 0.1
        lower <- min(rng[1], ref_line, na.rm = TRUE) - pad
        upper <- max(rng[2], ref_line, na.rm = TRUE) + pad
      }
      xlim <- c(lower, upper)
    }
  }

  # plot
  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$estimate, y = .data$row_id)) +
    .add_h_ci(df) +
    ggplot2::geom_point(
      ggplot2::aes(fill = .data$is_sig),
      shape = 21, size = 3, stroke = 0.6, show.legend = FALSE, na.rm = TRUE
    ) +
    ggplot2::geom_vline(xintercept = ref_line, linetype = "dashed", colour = "gray60") +
    ggplot2::scale_fill_manual(values = fill_vals, guide = "none") +
    ggplot2::scale_color_manual(values = line_vals, guide = "none") +
    ggplot2::scale_y_discrete(labels = label_fun) +
    ggplot2::labs(title = title, x = x_axis_label, y = NULL) +
    ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(hjust = 0),
      plot.title = ggplot2::element_text(face = "bold"),
      plot.margin = ggplot2::margin(10, 40, 10, 10)
    )

  if (!is.null(breaks) && !log_x) p <- p + ggplot2::scale_x_continuous(breaks = breaks)
  if (log_x) p <- p + ggplot2::scale_x_log10()
  if (!is.null(xlim)) p <- p + ggplot2::coord_cartesian(xlim = xlim)

  p
}

#' Horizontal CI helper (internal)
#'
#' Chooses the appropriate horizontal CI geometry depending on ggplot2 version.
#' Not exported; used internally by \code{plot_reg()}.
#'
#' @param data A data frame with columns \code{conf.low}, \code{conf.high}, and \code{is_sig}.
#' @return A `ggplot2` layer.
#' @keywords internal
#' @noRd


.add_h_ci <- function(data) {
  v <- utils::packageVersion("ggplot2")
  if (v >= "4.0.0") {
    ggplot2::geom_errorbar(
      data = data,
      ggplot2::aes(
        xmin = .data$conf.low,
        xmax = .data$conf.high,
        colour = .data$is_sig
      ),
      orientation = "y",
      width = 0.2,
      na.rm = TRUE
    )
  } else if (v >= "3.5.0") {
    ggplot2::geom_errorbarh(
      data = data,
      ggplot2::aes(
        xmin = .data$conf.low,
        xmax = .data$conf.high,
        colour = .data$is_sig
      ),
      width = 0.2,
      na.rm = TRUE
    )
  } else {
    ggplot2::geom_errorbarh(
      data = data,
      ggplot2::aes(
        xmin = .data$conf.low,
        xmax = .data$conf.high,
        colour = .data$is_sig
      ),
      height = 0.2,
      na.rm = TRUE
    )
  }
}
