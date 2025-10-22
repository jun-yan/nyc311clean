plot_annual_counts_with_projection <- function(
    DT,
    created_col = "created_date",
    estimate_flag = FALSE,
    estimate_date = NULL,          # e.g., "2025-08-24"
    estimate_value = NULL,         # e.g., 1500000 as of estimate_date
    chart_dir = "charts",
    filename = "annual_trend_plot.pdf",
    title = "Annual 311 Service Requests",
    subtitle = NULL,
    include_projection_in_growth = FALSE,  # exclude projection from growth %
    include_projection_in_stats  = FALSE   # exclude projection from multi-year stats
) {
  stopifnot(is.data.table(DT), created_col %in% names(DT))
  
  # Ensure POSIXct and derive year
  DT[, (created_col) := as.POSIXct(get(created_col), tz = "America/New_York")]
  DT[, year := lubridate::year(get(created_col))]
  
  # Annual counts
  annual <- DT[, .N, by = year][order(year)]
  data.table::setnames(annual, "N", "count")
  annual[, is_projection := FALSE]
  annual[, proj_label := fifelse(is_projection, "Projected", "Actual")]
  
  # Optional projection row
  if (isTRUE(estimate_flag)) {
    stopifnot(!is.null(estimate_date), !is.null(estimate_value))
    estimate_date <- as.Date(estimate_date)
    year_est     <- lubridate::year(estimate_date)
    days_elapsed <- lubridate::yday(estimate_date)
    total_days   <- ifelse(lubridate::leap_year(year_est), 366, 365)
    projected_value <- round(estimate_value * total_days / days_elapsed)
    
    annual <- rbind(
      annual,
      data.table::data.table(
        year = year_est,
        count = projected_value,
        is_projection = TRUE
      ),
      fill = TRUE
    )
    setorder(annual, year)
    annual[, proj_label := fifelse(is_projection, "Projected", "Actual")]
  }
  
  # Force factor levels for proj_label so ggpattern doesn’t choke
  annual[, proj_label := factor(proj_label, levels = c("Actual", "Projected"))]
  
  # Total n across non-projected rows → subtitle "(n=xxx)"
  n_total <- annual[is_projection == FALSE, sum(count, na.rm = TRUE)]
  subtitle_final <- if (is.null(subtitle) || !nzchar(subtitle)) {
    sprintf("(n=%s)", scales::comma(n_total))
  } else {
    paste0(subtitle, " ", sprintf("(n=%s)", scales::comma(n_total)))
  }
  
  # Base bars
  p <- ggplot2::ggplot(
    annual,
    ggplot2::aes(
      x = factor(year), y = count,
      fill = proj_label,
      pattern = proj_label,
      pattern_fill = proj_label,
      pattern_colour = proj_label
    )
  ) +
    ggpattern::geom_col_pattern(
      color = "gray40", linewidth = 0.2,
      pattern_density = 0.5,
      pattern_spacing = 0.05,
      pattern_angle = 45,
      pattern_size = 0.2
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = scales::comma(count)),
      vjust = 1.5,   # positions text inside, near the top
      size = 4,
      color = "black",
      fontface = "bold"
    ) +
    ggplot2::scale_fill_manual(
      values = c("Actual" = "#009E73", "Projected" = "#F0E442"),
      drop = FALSE
    ) +
    ggpattern::scale_pattern_manual(
      values = c("Actual" = "none", "Projected" = "stripe"),
      drop = FALSE
    ) +
    ggpattern::scale_pattern_fill_manual(
      values = c("Actual" = NA, "Projected" = "#F0E442"),
      drop = FALSE
    ) +
    ggpattern::scale_pattern_colour_manual(
      values = c("Actual" = NA, "Projected" = "#D55E00"),
      drop = FALSE
    ) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle_final,
      x = "",
      y = "",
      fill = NULL,
      pattern = NULL
    ) +
    david_theme()
  
  # Trendline + growth stats only if ≥ 2 years
  model <- NULL
  if (nrow(if (include_projection_in_growth) annual else annual[is_projection == FALSE]) >= 2) {
    model_data <- if (include_projection_in_growth) annual else annual[is_projection == FALSE]
    
    model    <- stats::lm(count ~ year, data = model_data)
    trend_df <- copy(model_data)[, trend := predict(model)]
    
    # Add trend line
    p <- p +
      ggplot2::geom_line(
        data = trend_df,
        mapping = aes(x = factor(year), y = trend, group = 1),
        inherit.aes = FALSE,
        color = "#D55E00",
        linetype = "longdash",
        linewidth = 1.5
      )
    
    # Growth %
    setorder(model_data, year)
    first_year  <- model_data[1, year]
    first_count <- model_data[1, count]
    last_year   <- model_data[.N, year]
    last_count  <- model_data[.N, count]
    growth_pct  <- (last_count / first_count) - 1
    
    label_text <- sprintf(
      "Growth %d-->%d: %s",
      first_year, last_year, scales::percent(growth_pct, accuracy = 0.1)
    )
    label_df <- trend_df[year == last_year, .(
      year  = factor(year),
      y     = trend,
      label = label_text
    )]
    
    p <- p +
      ggplot2::geom_label(
        data = label_df,
        mapping = aes(x = year, y = y, label = label),
        inherit.aes = FALSE,
        hjust = 2,
        vjust = 0.4,
        fill = "white",
        color = "gray20",
        size = 4,
        label.size = 0.4
      )
  }
  
  # "Projected" bar label (if present)
  if (any(annual$is_projection)) {
    p <- p +
      ggplot2::geom_label(
        data = annual[is_projection == TRUE],
        mapping = aes(x = factor(year), y = count/2, label = "Projected"),
        inherit.aes = FALSE,
        vjust = 0.5,
        fill = "#F0E442",
        color = "gray40",
        size = 5
      )
  }
  
  # Always print the plot
  print(p)
  Sys.sleep(2)
  
  ## ---------- Multi-year statistics (console) ----------
  stats_data <- if (include_projection_in_stats) annual else annual[is_projection == FALSE]
  multi_year_stats <- data.table()
  
  if (nrow(stats_data) >= 1) {
    mean_val   <- mean(stats_data$count, na.rm = TRUE)
    median_val <- median(stats_data$count, na.rm = TRUE)
    sd_val     <- sd(stats_data$count, na.rm = TRUE)
    max_idx    <- which.max(stats_data$count)
    min_idx    <- which.min(stats_data$count)
    max_year   <- stats_data$year[max_idx]
    max_count  <- stats_data$count[max_idx]
    min_year   <- stats_data$year[min_idx]
    min_count  <- stats_data$count[min_idx]
    total_records <- sum(stats_data$count, na.rm = TRUE)
    
    # Calculate daily statistics from original data
    daily_data <- DT[, .(daily_count = .N), by = 
                       .(date = as.Date(get(created_col), 
                        tz = "America/New_York"))]    
                        setorder(daily_data, date)
    
    # Find busiest and least busy days
    max_day_idx <- which.max(daily_data$daily_count)
    min_day_idx <- which.min(daily_data$daily_count)
    
    busiest_date <- daily_data$date[max_day_idx]
    busiest_count <- daily_data$daily_count[max_day_idx]
    least_busy_date <- daily_data$date[min_day_idx]
    least_busy_count <- daily_data$daily_count[min_day_idx]
    
    cat("\nMulti-year statistics ",
        if (include_projection_in_stats) "(Actuals + Projected):\n" else "(Actuals only):\n",
        sep = "")
    cat("  Years covered: ", paste(range(stats_data$year), collapse = "–"), "\n", sep = "")
    cat("\n")
    cat("  Total Records: ", scales::comma(total_records), "\n", sep = "")
    cat("  Yearly Mean:   ", scales::comma(round(mean_val, 1)), "\n", sep = "")
    cat("  Yearly Median: ", scales::comma(round(median_val, 1)), "\n", sep = "")
    cat("  StdDev:        ", scales::comma(round(sd_val, 1)), "\n", sep = "")
    cat("  Max Year: ", max_year, " (", scales::comma(max_count), ")\n", sep = "")
    cat("  Min Year: ", min_year, " (", scales::comma(min_count), ")\n", sep = "")
    if (exists("growth_pct")) {
      cat("  Growth %: ", scales::percent(growth_pct, accuracy = 0.1), "\n", sep = "")
    }
    cat("  Busiest Day: ", format(busiest_date, "%Y-%m-%d"), " (", scales::comma(busiest_count), ")\n", sep = "")
    cat("  Least Busy Day: ", format(least_busy_date, "%Y-%m-%d"), " (", scales::comma(least_busy_count), ")\n", sep = "")
    
    multi_year_stats <- data.table(
      years_start = min(stats_data$year),
      years_end   = max(stats_data$year),
      mean        = mean_val,
      median      = median_val,
      sd          = sd_val,
      max_year    = max_year,
      max_count   = max_count,
      min_year    = min_year,
      min_count   = min_count,
      busiest_date = busiest_date,
      busiest_count = busiest_count,
      least_busy_date = least_busy_date,
      least_busy_count = least_busy_count,
      included_projection = include_projection_in_stats
    )
  }
  
  # Add this section right after the multi-year statistics and before the ggsave line
  
  ## ---------- Year-by-year counts ----------
  cat("\nYear-by-year counts:\n")
  for (i in 1:nrow(annual)) {
    year_val <- annual$year[i]
    count_val <- annual$count[i]
    proj_status <- if (annual$is_projection[i]) " (Projected)" else ""
    cat("  ", year_val, ": ", scales::comma(count_val), proj_status, "\n", sep = "")
  }
  cat("\n")
  
  ## ----------------------------------------
  
  if (!dir.exists(chart_dir)) dir.create(chart_dir, recursive = TRUE)
  ggsave(file.path(chart_dir, filename), plot = p, width = 10, height = 8.5)
  
  invisible(list(data = annual, model = model, plot = p, stats = multi_year_stats))
}
