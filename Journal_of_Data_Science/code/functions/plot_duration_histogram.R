plot_duration_histogram <- function(
    DT,                           # data.table with duration_sec and/or duration_days
    duration_col      = "duration_sec",   # "duration_sec" or "duration_days"
    chart_dir,
    filename          = "duration_histogram.pdf",
    title             = NULL,
    subtitle          = NULL,
    max_value         = NULL,             # limits in same units as duration_col
    min_value         = 1,                # limits in same units as duration_col
    include_zero      = FALSE,            # include rows with duration == 0
    include_one       = FALSE,            # include rows with duration == 1
    exclude_negative  = TRUE,             # exclude rows with duration < 0
    bin_width         = 1,                # bin width in same units as duration_col
    bin_type          = c("custom","individual","minutes","hours","days","auto"),
    create_cumulative = TRUE,
    width_in          = 13,
    height_in         = 8.5,
    bar_color         = "#009E73",
    text_size         = 12,
    threshold_numeric = NULL,   # Optional: where to draw vertical red line
    left_bar_color    = "#D55E00",  # Color for bars to the left of threshold
    x_axis_angle      = 0,                # angle for x-axis labels (0, 45, 90)
    x_label_skip      = 1                 # show every nth x-axis label (1 = all, 2 = every other, etc.)
) {
  stopifnot(data.table::is.data.table(DT))
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Package 'ggplot2' is required.")
  if (!dir.exists(chart_dir)) dir.create(chart_dir, recursive = TRUE, showWarnings = FALSE)
  
  # --- 1) Validate duration column and determine units ------------------------
  if (!duration_col %in% names(DT)) stop(sprintf("DT must have a '%s' column.", duration_col))
  
  if (duration_col == "duration_sec") {
    data_units <- "seconds"
    sec_conversion <- 1
  } else if (duration_col == "duration_days") {
    data_units <- "days"
    sec_conversion <- 86400
  } else {
    stop("duration_col must be either 'duration_sec' or 'duration_days'.")
  }
  
  # --- 2) Get data and apply filters ------------------------------------------
  x <- DT[[duration_col]]
  df <- data.table::data.table(duration = as.numeric(x))
  df <- df[!is.na(duration)]
  
  if (exclude_negative) df <- df[duration >= 0]
  if (!include_zero)    df <- df[duration != 0]
  if (!include_one)     df <- df[duration != 1]
  
  # Set default max_value if not provided
  if (is.null(max_value)) {
    if (data_units == "seconds") {
      max_value <- 300
    } else {
      max_value <- 300 / 86400  # ~0.003 days (300 seconds)
    }
  }
  
  # Apply range filter
  df <- df[duration >= min_value & duration <= max_value]
  if (!nrow(df)) {
    message(sprintf("No data in range [%g, %g] %s after filters.", min_value, max_value, data_units))
    return(invisible(NULL))
  }
  
  # --- 3) Decide bin width (in data units) ------------------------------------
  bin_type <- match.arg(bin_type)
  if (bin_type != "custom") {
    rng <- max_value - min_value
    if (data_units == "seconds") {
      bin_width <- switch(
        bin_type,
        individual = 1,
        minutes    = 60,
        hours      = 3600,
        days       = 86400,
        auto       = {
          if (rng <= 120) 1 else if (rng <= 3600) 60 else if (rng <= 86400) 3600 else 86400
        }
      )
    } else { # days
      bin_width <- switch(
        bin_type,
        individual = 1/86400,      # 1 second in days
        minutes    = 60/86400,     # 1 minute in days
        hours      = 3600/86400,   # 1 hour in days
        days       = 1,            # 1 day
        auto       = {
          if (rng <= 120/86400) 1/86400 else if (rng <= 1) 60/86400 else 1
        }
      )
    }
  }
  if (bin_width <= 0) stop("bin_width must be > 0.")
  
  # Create bin label
  if (data_units == "seconds") {
    bin_label <- if (bin_width == 1) {
      "Individual seconds"
    } else if (bin_width == 60) {
      "1-minute bins"
    } else if (bin_width == 3600) {
      "1-hour bins"
    } else if (bin_width == 86400) {
      "1-day bins"
    } else {
      sprintf("Binned by %g sec", bin_width)
    }
  } else {
    bin_label <- if (bin_width == 1/86400) {
      "Individual seconds"
    } else if (bin_width == 60/86400) {
      "1-minute bins"
    } else if (bin_width == 1) {
      "1-day bins"
    } else {
      sprintf("Binned by %g days", bin_width)
    }
  }
  
  # --- 4) Titles --------------------------------------------------------------
  if (is.null(title)) {
    title <- sprintf("Distribution of Durations (%s to %s %s)",
                     scales::comma(min_value), scales::comma(max_value), data_units)
  }
  if (is.null(subtitle)) subtitle <- sprintf("n = %s | %s",
                                             scales::comma(nrow(df)), bin_label)
  
  # --- 5) Build histogram table ----------------------------------------------
  df[, duration_binned := floor(duration / bin_width) * bin_width]
  hist_data <- df[, .(count = .N), by = duration_binned][order(duration_binned)]
  
  if (!is.null(threshold_numeric)) {
    hist_data[, is_suspicious := duration_binned < threshold_numeric]
  } else {
    hist_data[, is_suspicious := FALSE]
  }
  
  # Fill gaps in bins
  bin_starts <- seq(
    from = floor(min_value / bin_width) * bin_width,
    to   = floor(max_value / bin_width) * bin_width,
    by   = bin_width
  )
  all_bins <- data.table::data.table(duration_binned = bin_starts)
  hist_data <- hist_data[all_bins, on = "duration_binned"]
  hist_data[is.na(count), count := 0L]
  
  # --- 6) Create x-axis breaks with skip functionality -----------------------
  range_size <- max_value - min_value
  if (data_units == "seconds") {
    if (bin_width >= 86400) {
      x_breaks_by <- max(86400, round(range_size / 20))
    } else if (bin_width >= 3600) {
      x_breaks_by <- max(3600, round(range_size / 24))
    } else if (bin_width >= 60) {
      x_breaks_by <- max(60, round(range_size / 30))
    } else {
      x_breaks_by <- max(1, round(range_size / 25))
    }
  } else {
    x_breaks_by <- max(bin_width, round(range_size / 20))
  }
  
  # Generate all breaks
  all_breaks <- seq(min_value, max_value, by = x_breaks_by)
  
  # Apply skip factor
  if (x_label_skip > 1) {
    breaks_to_show <- all_breaks[seq(1, length(all_breaks), by = x_label_skip)]
  } else {
    breaks_to_show <- all_breaks
  }
  
  # --- 7) Plot histogram ------------------------------------------------------
  x_axis_title <- paste("Duration", paste0("(", data_units, ")"))
  
  p <- ggplot2::ggplot(hist_data, ggplot2::aes(x = duration_binned, y = count)) +
    ggplot2::geom_col(
      ggplot2::aes(fill = is_suspicious),
      width = bin_width * 0.8,
      color = "gray40",
      linewidth = 0.2,
      alpha = 1.0
    ) +
    ggplot2::scale_fill_manual(
      values = c(`TRUE` = left_bar_color, `FALSE` = bar_color),
      guide = "none"
    ) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = x_axis_title,
      y = ""
    ) +
    ggplot2::scale_x_continuous(
      breaks = breaks_to_show,
      labels = scales::comma,
      expand = c(0.01, 0)
    ) +
    ggplot2::scale_y_continuous(labels = scales::comma, expand = c(0, 0)) +
    david_theme(text_size = 12, x_axis_angle = 45)
  
  if (!is.null(threshold_numeric)) {
    p <- p + ggplot2::geom_vline(
      xintercept = threshold_numeric,
      color = "#D55E00",
      linewidth = 1,
      linetype = "dashed",
      alpha = 0.85
    )
  }
  
  if (!is.null(threshold_numeric)) {
    p <- p + ggplot2::annotate(
      "text",
      x = threshold_numeric,
      y = max(hist_data$count, na.rm = TRUE) * 0.9,
      label = "Suspicious Threshold",
      hjust = -0.1,
      size = 4,
      color = "#D55E00"
    )
  }
  
  print(p)
  Sys.sleep(3)
  
  outfile <- file.path(chart_dir, filename)
  ggplot2::ggsave(outfile, plot = p, width = width_in, height = height_in, dpi = 300)
  #  cat(sprintf("\nSaved histogram to: %s\n", outfile))
  
  # --- 8) Cumulative chart ---------------------------------------------------
  p2 <- NULL
  if (create_cumulative) {
    hist_data_cum <- data.table::copy(hist_data[count > 0][order(duration_binned)])
    hist_data_cum[, cum_count := cumsum(count)]
    hist_data_cum[, cum_pct := 100 * cum_count / sum(count)]
    cum_title <- if (is.null(title)) paste("Cumulative Distribution of Durations", paste0("(", data_units, ")"))
    else paste("Cumulative", title)
    
    p2 <- ggplot2::ggplot(hist_data_cum, ggplot2::aes(x = duration_binned, y = cum_pct)) +
      ggplot2::geom_line(color = "#0072B2", linewidth = 1.2) +
      ggplot2::geom_point(color = "#0072B2", size = 1, alpha = 0.7) +
      ggplot2::geom_hline(yintercept = c(90), color = c("#CC79A7"),
                          linetype = "dashed", alpha = 0.7) +
      ggplot2::labs(
        title = cum_title,
        subtitle = sprintf("n = %s | Line at 90%%",
                           scales::comma(sum(hist_data_cum$count))),
        x = x_axis_title,
        y = "Cumulative Percentage"
      ) +
      ggplot2::scale_x_continuous(
        breaks = breaks_to_show,
        labels = scales::comma,
        expand = c(0.01, 0)
      ) +
      ggplot2::scale_y_continuous(breaks = seq(0, 100, by = 10),
                                  limits = c(0, 100), expand = c(0, 0)) +
      david_theme(text_size = 12, x_axis_angle = 45)
    
    # Add threshold line and label to cumulative plot
    if (!is.null(threshold_numeric)) {
      p2 <- p2 + 
        ggplot2::geom_vline(
          xintercept = threshold_numeric,
          color = "#D55E00",
          linewidth = 1,
          linetype = "dashed",
          alpha = 0.85
        ) +
        ggplot2::annotate(
          "text",
          x = threshold_numeric,
          y = 90,  # Position at 90% on cumulative chart
          label = "Suspicious Threshold",
          hjust = -0.1,
          size = 4,
          color = "#D55E00"
        )
    }
    
    print(p2)
    Sys.sleep(3)
    
    outfile2 <- file.path(chart_dir, sub("\\.pdf$", "_cumulative.pdf", filename))
    ggplot2::ggsave(outfile2, plot = p2, width = width_in, height = height_in, dpi = 300)
    #    cat(sprintf("Saved cumulative chart to: %s\n", outfile2))
    
    # Percentile readouts (always show both seconds and days)
    cat("\nKey percentile cutoff values:\n")
    for (pct in c(90, 95, 99, 99.9)) {
      row <- hist_data_cum[cum_pct >= pct][1]
      if (nrow(row)) {
        val <- row$duration_binned
        val_sec <- val * sec_conversion  # convert to seconds
        cat(sprintf("  %g%% of data is <= %s seconds (%.4f days)\n",
                    pct, scales::comma(val_sec), val_sec / 86400))
      }
    }
  }
  
  # --- 9) Summary stats (always show both seconds and days) ------------------
  vals <- df$duration
  vals_sec <- vals * sec_conversion  # convert to seconds
  m_sec  <- mean(vals_sec); md_sec <- stats::median(vals_sec); s_sec <- stats::sd(vals_sec)
  
  cat(sprintf("\nDuration histogram summary (%s to %s %s):\n",
              scales::comma(min_value), scales::comma(max_value), data_units))
  cat(sprintf("Total observations: %s\n", scales::comma(sum(hist_data$count))))
  cat(sprintf("Mean:   %s seconds (%.6f days)\n", scales::comma(round(m_sec, 2)), m_sec / 86400))
  cat(sprintf("Median: %s seconds (%.6f days)\n", scales::comma(round(md_sec, 2)), md_sec / 86400))
  cat(sprintf("Std dev: %s seconds (%.6f days)\n", scales::comma(round(s_sec, 2)), s_sec / 86400))
  bin_width_sec <- bin_width * sec_conversion
  cat(sprintf("Bin width: %g %s (%s seconds, %.6f days)\n", 
              bin_width, data_units, scales::comma(bin_width_sec), bin_width_sec / 86400))
  
  invisible(list(
    histogram_plot   = p,
    cumulative_plot  = p2,
    data             = hist_data,
    files            = list(
      histogram = outfile,
      cumulative = if (create_cumulative) sub("\\.pdf$", "_cumulative.pdf", outfile) else NULL
    )
  ))
}