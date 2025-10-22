analyze_skewed_durations <- function(
    DT,
    duration_col        = "duration_sec",
    chart_dir           = NULL,
    minimum_cutoff_sec  = 2L,             # drop durations below this many seconds
    upper_cutoff_sec    = 8L * 86400L,    # truncate extreme tail at this many seconds
    upper_cutoff_days   = NULL,           # legacy arg; if provided, converted to seconds
    print_summary       = TRUE,
    create_plots        = TRUE,
    filename_base       = "skewed_duration_analysis"
) {
  # --- Basic checks -----------------------------------------------------------
  if (!data.table::is.data.table(DT)) stop("DT must be a data.table.")
  if (!duration_col %in% names(DT)) stop(sprintf("DT must have a '%s' column.", duration_col))
  if (create_plots && is.null(chart_dir)) stop("chart_dir required when create_plots = TRUE")
  
  # Optional deps for plotting/labels
  has_scales <- requireNamespace("scales", quietly = TRUE)
  comma_lab  <- if (has_scales) scales::comma else function(x) format(x, big.mark = ",", trim = TRUE)
  
  # --- Legacy arg support -----------------------------------------------------
  if (!is.null(upper_cutoff_days)) {
    warning("`upper_cutoff_days` is deprecated; use `upper_cutoff_sec`. Converting days -> seconds.")
    upper_cutoff_sec <- as.numeric(upper_cutoff_days) * 86400
  }
  
  # --- Extract column and ensure numeric/positive -----------------------------
  vals <- DT[[duration_col]]
  if (!is.numeric(vals)) {
    # try coerce; NA-introducing coercion is fine (we drop NA next)
    vals <- suppressWarnings(as.numeric(vals))
  }
  
  # Positive durations
  # Extract column and ensure numeric
  vals <- DT[[duration_col]]
  if (!is.numeric(vals)) vals <- suppressWarnings(as.numeric(vals))
  
  # Positive durations (vectorized)
  pos_idx  <- !is.na(vals) & vals > 0     # ✅ correct
  raw_data <- data.table::data.table(val = vals[pos_idx])
  
  
  if (!nrow(raw_data)) {
    message("No positive durations available in the specified column.")
    return(invisible(NULL))
  }
  
  # --- Minimum cutoff (lower bound) -------------------------------------------
  raw_data <- raw_data[val >= minimum_cutoff_sec]
  
  if (!nrow(raw_data)) {
    message(sprintf("No rows >= minimum_cutoff_sec = %s seconds.", comma_lab(minimum_cutoff_sec)))
    return(invisible(NULL))
  }
  
  # --- Upper truncation (right tail) ------------------------------------------
  truncated_data <- raw_data[val <= upper_cutoff_sec]
  
  if (!nrow(truncated_data)) {
    message(sprintf("No rows between %s and %s seconds.",
                    comma_lab(minimum_cutoff_sec), comma_lab(upper_cutoff_sec)))
    return(invisible(NULL))
  }
  
  # --- Summaries --------------------------------------------------------------
  if (print_summary) {
    original_pos_n <- sum(!is.na(vals) & vals > 0)
    cat("\n=== SKEWED DURATION OUTLIER ANALYSIS (SECONDS) ===\n")
    cat(sprintf("Original positive durations (before cutoffs): %s\n", comma_lab(original_pos_n)))
    cat(sprintf("After applying minimum cutoff (%s sec): %s\n",
                comma_lab(minimum_cutoff_sec), comma_lab(nrow(raw_data))))
    cat(sprintf("After applying maximum cutoff %s sec: %s (%.2f%% retained)\n",
                comma_lab(upper_cutoff_sec),
                comma_lab(nrow(truncated_data)),
                100 * nrow(truncated_data) / nrow(raw_data)))
    
    # Compare raw vs truncated statistics
    cat("\nDistribution comparison (seconds):\n")
    cat("                   Raw Data    Truncated\n")
    cat(sprintf("Mean:              %9.0f   %9.0f\n",
                mean(raw_data$val), mean(truncated_data$val)))
    cat(sprintf("Median:            %9.0f   %9.0f\n",
                stats::median(raw_data$val), stats::median(truncated_data$val)))
    cat(sprintf("Std Dev:           %9.0f   %9.0f\n",
                stats::sd(raw_data$val), stats::sd(truncated_data$val)))
    
    raw_skew   <- mean(raw_data$val) / stats::median(raw_data$val)
    trunc_skew <- mean(truncated_data$val) / stats::median(truncated_data$val)
    cat(sprintf("Mean/Median ratio: %9.2f   %9.2f\n", raw_skew, trunc_skew))
    cat("\n(A ratio = 1.0 indicates normal distribution. A ratio >1.0 indicates right-skewed)\n")
  }
  
  # --- Threshold methods on truncated data ------------------------------------
  trunc_mean    <- mean(truncated_data$val)
  trunc_sd      <- stats::sd(truncated_data$val)
  trunc_median  <- stats::median(truncated_data$val)
  trunc_mad     <- stats::mad(truncated_data$val, constant = 1.4826)  # default consistent with normal
  
  # Mean ± k*SD (lower)
  trunc_2sd_lower <- trunc_mean - 2 * trunc_sd
  trunc_3sd_lower <- trunc_mean - 3 * trunc_sd
  
  # Median ± k*MAD (lower)
  mad_2_lower <- trunc_median - 2 * trunc_mad
  mad_3_lower <- trunc_median - 3 * trunc_mad
  
  # Log-normal (lower)
  log_vals      <- log(truncated_data$val)    # safe: >0 ensured above
  log_mean      <- mean(log_vals)
  log_sd        <- stats::sd(log_vals)
  log_2sd_lower <- exp(log_mean - 2 * log_sd)
  log_3sd_lower <- exp(log_mean - 3 * log_sd)
  
  # Percentiles (lower tail)
  pct_1   <- as.numeric(stats::quantile(truncated_data$val, 0.01, names = FALSE, type = 7))
  pct_5   <- as.numeric(stats::quantile(truncated_data$val, 0.05, names = FALSE, type = 7))
  pct_10  <- as.numeric(stats::quantile(truncated_data$val, 0.10, names = FALSE, type = 7))
  
  # IQR-based (lower)
  trunc_q1  <- as.numeric(stats::quantile(truncated_data$val, 0.25, names = FALSE, type = 7))
  trunc_q3  <- as.numeric(stats::quantile(truncated_data$val, 0.75, names = FALSE, type = 7))
  trunc_iqr <- trunc_q3 - trunc_q1
  iqr_15_lower <- trunc_q1 - 1.5 * trunc_iqr
  iqr_3_lower  <- trunc_q1 - 3   * trunc_iqr
  
  # Methods table
  methods_dt <- data.table::data.table(
    method = c("Truncated_2SD", "Truncated_3SD", "MAD_2x", "MAD_3x",
               "LogNormal_2SD", "LogNormal_3SD", "Percentile_1%", "Percentile_5%",
               "Percentile_10%", "IQR_1.5", "IQR_3.0"),
    threshold_seconds = c(trunc_2sd_lower, trunc_3sd_lower, mad_2_lower, mad_3_lower,
                          log_2sd_lower, log_3sd_lower, pct_1, pct_5, pct_10,
                          iqr_15_lower, iqr_3_lower)
  )
  
  # Outlier counts computed on RAW data (below threshold)
  methods_dt[, `:=`(
    outlier_count = vapply(threshold_seconds, function(x) {
      sum(raw_data$val < x, na.rm = TRUE)
    }, numeric(1)),
    outlier_pct = vapply(threshold_seconds, function(x) {
      round(100 * sum(raw_data$val < x, na.rm = TRUE) / nrow(raw_data), 3)
    }, numeric(1))
  )]
  
  # Thresholds should not be negative seconds
  methods_dt[threshold_seconds < 0, threshold_seconds := 0]
  
  if (print_summary) {
    cat("\n=== OUTLIER DETECTION METHODS (Applied to Truncated Data) ===\n")
    print(methods_dt, row.names = FALSE)
    
    cat("\nMethod interpretations:\n")
    cat("• Truncated SD: mean±SD computed on truncated data\n")
    cat("• MAD: robust median ± MAD\n")
    cat("• LogNormal: assumes log-normal; back-transformed to seconds\n")
    cat("• Percentile: bottom X% of truncated data\n")
    cat("• IQR: quartile-based outlier detection\n")
    
    reasonable_methods <- methods_dt[threshold_seconds > 0 & threshold_seconds < 3600]
    if (nrow(reasonable_methods) > 0) {
      cat("\nReasonable cutoff suggestions (seconds):\n")
      print(reasonable_methods[order(threshold_seconds)], row.names = FALSE)
    }
  }
  
  # --- Plots ------------------------------------------------------------------
  p1 <- p2 <- NULL
  if (create_plots) {
    if (!requireNamespace("ggplot2", quietly = TRUE)) {
      stop("Package 'ggplot2' is required for plotting.")
    }
    if (!dir.exists(chart_dir)) dir.create(chart_dir, recursive = TRUE, showWarnings = FALSE)
    
    # Plot 1: Before/After truncation comparison
    before_after_data <- data.table::data.table(
      dataset   = rep(c("Raw Data", "Truncated"), each = 2),
      statistic = rep(c("Mean", "Median"), 2),
      value     = c(mean(raw_data$val), stats::median(raw_data$val),
                    mean(truncated_data$val), stats::median(truncated_data$val))
    )
    
    p1 <- ggplot2::ggplot(before_after_data, ggplot2::aes(x = dataset, y = value, fill = statistic)) +
      ggplot2::geom_col(position = "dodge", alpha = 0.8) +
      ggplot2::scale_fill_manual(values = c("Mean" = "#E69F00", "Median" = "#009E73")) +
      ggplot2::labs(
        title    = "Impact of Truncation on Central Tendency",
        subtitle = sprintf("Analysis range: %s–%s seconds",
                           comma_lab(minimum_cutoff_sec), comma_lab(upper_cutoff_sec)),
        x = "Dataset",
        y = "Duration (seconds)",
        fill = "Statistic"
      ) +
      ggplot2::scale_y_continuous(labels = comma_lab) +
      ggplot2::theme_minimal()
    
    # Plot 2: Thresholds by method (show “reasonable” cutoffs < 2 hours)
    reasonable_for_plot <- methods_dt[threshold_seconds > 0 & threshold_seconds < 7200]
    if (nrow(reasonable_for_plot) > 0) {
      p2 <- ggplot2::ggplot(reasonable_for_plot,
                            ggplot2::aes(x = stats::reorder(method, threshold_seconds),
                                         y = threshold_seconds)) +
        ggplot2::geom_col(fill = "#0072B2", alpha = 0.7) +
        ggplot2::coord_flip() +
        ggplot2::labs(
          title    = "Outlier Detection Thresholds by Method",
          subtitle = "Lower values = more aggressive outlier detection",
          x = "Method",
          y = "Threshold (seconds)"
        ) +
        ggplot2::scale_y_continuous(labels = comma_lab) +
        ggplot2::theme_minimal()
    }
    
    # Save plots
    outfile1 <- file.path(chart_dir, paste0(filename_base, "_comparison.pdf"))
    ggplot2::ggsave(outfile1, plot = p1, width = 10, height = 6, dpi = 300)
#    cat(sprintf("\nSaved comparison plot to: %s\n", outfile1))
    
    if (!is.null(p2)) {
      outfile2 <- file.path(chart_dir, paste0(filename_base, "_methods.pdf"))
      ggplot2::ggsave(outfile2, plot = p2, width = 10, height = 8, dpi = 300)
#      cat(sprintf("Saved methods plot to: %s\n", outfile2))
    }
  }
  
  # --- Return object ----------------------------------------------------------
  result <- list(
    raw_stats = list(
      n      = nrow(raw_data),
      mean   = mean(raw_data$val),
      median = stats::median(raw_data$val),
      sd     = stats::sd(raw_data$val)
    ),
    truncated_stats = list(
      n      = nrow(truncated_data),
      mean   = trunc_mean,
      median = trunc_median,
      sd     = trunc_sd,
      mad    = trunc_mad
    ),
    thresholds = list(
      trunc_2sd_lower = max(0, trunc_2sd_lower),
      trunc_3sd_lower = max(0, trunc_3sd_lower),
      mad_2_lower     = max(0, mad_2_lower),
      mad_3_lower     = max(0, mad_3_lower),
      log_2sd_lower   = max(0, log_2sd_lower),
      log_3sd_lower   = max(0, log_3sd_lower),
      pct_1           = max(0, pct_1),
      pct_5           = max(0, pct_5),
      pct_10          = max(0, pct_10),
      iqr_15_lower    = max(0, iqr_15_lower),
      iqr_3_lower     = max(0, iqr_3_lower)
    ),
    methods = methods_dt,
    plots   = list(
      comparison = if (create_plots) p1 else NULL,
      methods    = if (create_plots) p2 else NULL
    )
  )
  
  
  return(invisible(result))
  
}
