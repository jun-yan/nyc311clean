analyze_datetime_patterns <- function(
    DT,
    datetime_col,           # unquoted column (e.g., closed_date)
    chart_dir,
    label = "Closed",       # label for titles ("Closed" or "Created")
    agency_col = agency             # unquoted column with agency name
) {
  stopifnot(data.table::is.data.table(DT))
  
  
  # No need for deparse/substitute - already strings
  datetime_name <- datetime_col
  agency <- agency_col
  
  class(agency)
  length(agency)
  head(agency)
  
  # Ensure POSIXct
  if (!inherits(DT[[datetime_name]], "POSIXct")) {
    stop(sprintf("%s must be POSIXct", datetime_name))
  }
  
  # --- Time zone diagnostics & TZ-safe year -------------------------------
  LOCAL_TZ <- "America/New_York"
  tz_att <- attr(DT[[datetime_name]], "tzone")
  
  # Check if we need to convert from UTC to local
  if (is.null(tz_att) || tz_att == "" || tz_att == "UTC") {
    # Convert from UTC to local and extract components
    DT[, `:=`(
      year_local = lubridate::year(lubridate::with_tz(get(datetime_name), LOCAL_TZ)),
      hour = lubridate::hour(lubridate::with_tz(get(datetime_name), LOCAL_TZ)),
      minute = lubridate::minute(lubridate::with_tz(get(datetime_name), LOCAL_TZ)),
      second = lubridate::second(lubridate::with_tz(get(datetime_name), LOCAL_TZ))
    )]
  } else {
    # Already America/New_York → just extract components
    DT[, `:=`(
      year_local = lubridate::year(get(datetime_name)),
      hour = lubridate::hour(get(datetime_name)),
      minute = lubridate::minute(get(datetime_name)),
      second = lubridate::second(get(datetime_name))
    )]
  }
  
  # ===============================
  # Hour-of-day distribution (ALL 24 hours, ordered by working hours)
  # ===============================
  all_hours <- data.table(hour = 0:23)
  hour_summary <- DT[!is.na(hour), .N, by = hour]
  hour_summary <- merge(all_hours, hour_summary, by = "hour", all.x = TRUE)
  hour_summary[is.na(N), N := 0]
  
  # Create working hours order: 08-23, then 00-07
  working_hours_order <- c(8:23, 0:7)
  hour_summary[, display_order := match(hour, working_hours_order)]
  setorder(hour_summary, display_order)
  
  # Percent and cumulative percent
  hour_summary[, pct := round(100 * N / sum(N), 2)]
  hour_summary[, cum_pct := cumsum(pct)]
  
  # For the chart, use the same working hours order as the table
  hour_summary_chart <- copy(hour_summary)
  hour_summary_chart[, hour_factor := factor(sprintf("%02d", hour), 
                                             levels = sprintf("%02d", 
                                                              working_hours_order))]
  
  # BARCHART CHECK: Only skip if all rows are zero, but since we merge all 24 hours 
  # and fill NA with 0, this check isn't strictly needed for hour_summary_chart.
  # However, if total N is zero, plotting is meaningless.
  if (sum(hour_summary_chart$N) > 0) {
    plot_barchart(
      DT        = hour_summary_chart,
      x_col     = "hour_factor",
      y_col     = "N",
      title     = sprintf("%s Requests by Hour of Day (Working Hours Order)", label),
      subtitle  = sprintf("n = %s | Working hours: 08:00–23:59", 
                          format(sum(hour_summary$N), big.mark = ",", scientific = FALSE)),
      bar_width = 0.8,
      rows_to_print = 24,
      add_mean  = TRUE,
      add_3sd = TRUE,
      show_labels = FALSE,
      x_axis_angle  = 0,
      y_axis_labels = scales::comma,
      chart_width = 13,
      chart_height = 8.5,
      chart_dir = chart_dir,
      filename  = sprintf("%s_hour_distribution_working_order.pdf", 
                          tolower(label))
    )
  }
  
  # ===============================
  # Minute-of-hour distribution (ALL 60 minutes in midnight hour)
  # ===============================
  midnight_data <- DT[hour == 0]
  
  if (nrow(midnight_data) > 0) {
    
    all_minutes <- data.table(minute = 0:59)
    midnight_summary <- midnight_data[, .N, by = minute]
    midnight_summary <- merge(all_minutes, midnight_summary, by = "minute", all.x = TRUE)
    midnight_summary[is.na(N), N := 0]
    setorder(midnight_summary, minute)
    
    midnight_summary[, pct := round(100 * N / sum(N), 2)]
    midnight_summary[, cum_pct := cumsum(pct)]
    
    # BARCHART CHECK: Check for total N > 0 (since it contains 60 rows, 
    # this check is sufficient to prevent mean/sd errors from empty data)
    if (sum(midnight_summary$N) > 0) {
      plot_barchart(
        DT        = midnight_summary,
        x_col     = "minute",
        y_col     = "N",
        title     = sprintf("%s Requests by Minute (Midnight Hour: 00:00–00:59)", 
                            label),
        subtitle  = sprintf("n = %s | Expect spike at 00:00", 
                            format(sum(midnight_summary$N), big.mark = ",")),
        bar_width = 0.8,
        add_mean  = FALSE,
        add_3sd = TRUE,
        show_labels = FALSE,
        x_axis_angle = 0,
        y_axis_labels = scales::comma,
        chart_width = 13,
        chart_height = 8.5,
        chart_dir = chart_dir,
        filename  = sprintf("%s_midnight_minute_distribution.pdf", tolower(label))
      )
    }
    
    # Right before the n_unique_agencies_midnight line:
    # cat("\nDEBUG agency variable:\n")
    # cat("  class:", class(agency), "\n")
    # cat("  length:", length(agency), "\n")
    # cat("  value:", paste(agency, collapse = " | "), "\n")
    

    # Right before the n_unique_agencies_midnight line:
    if (!agency %in% names(midnight_data)) {
      stop(sprintf("Column '%s' not found in midnight_data. Available columns: %s", 
                   agency, paste(names(midnight_data), collapse=", ")))
    }
    
    
    
    # PARETO CHECK: Requires > 2 unique agencies
    n_unique_agencies_midnight <- uniqueN(midnight_data[[agency]])
    
    if (n_unique_agencies_midnight > 2) {
      plot_pareto_combo(
        DT        = midnight_data,
        x_col     = "agency",
        chart_dir = chart_dir,
        filename  = sprintf("pareto_%s_midnight_agency.pdf", tolower(label)),
        title     = sprintf("Agencies with Midnight Hour %s by the Minute (00:00–00:59)", 
                            label),
        subtitle  = sprintf("n = %s | %s unique agencies", 
                            format(nrow(midnight_data), big.mark = ","), n_unique_agencies_midnight),
        top_n     = 30,
        include_na = FALSE
      )
    }
  }
  
  # ===============================
  # Analysis for EXACTLY 00:00:00 (Midnight Moment)
  # (No further checks needed, as this was fixed in previous steps)
  # ===============================
  
  exact_midnight_moment <- DT[hour == 0 & minute == 0 & second == 0]
  
  if (nrow(exact_midnight_moment) > 0) {
    
    total_n_exact <- nrow(exact_midnight_moment)
    
    # ----------------------------------------------------
    # BARCHART Section (Requires > 1 unique year for mean/sd)
    # ----------------------------------------------------
    cy_summary <- exact_midnight_moment[, .N, by = year_local]
    
    if (nrow(cy_summary) > 1) { 
      
      # Assign the factorized year_local to a new column called 'CY' for plotting
      cy_summary[, CY := factor(year_local)]  
      
      # Determine the total count for the plot subtitle
      total_n_cy <- sum(cy_summary$N)
      
      # 2. Call the plot_barchart function
      plot_barchart(
        DT        = cy_summary,
        x_col     = "CY", # QUOTED column name
        y_col     = "N",  
        title     = sprintf("%s at Exactly Midnight (00:00:00) by Year ",
                            label),
        subtitle  = sprintf("n = %s | Total records exactly at midnight", 
                            format(total_n_cy, big.mark = ",")),
        bar_width    = 0.8,
        add_mean     = TRUE,
        add_3sd      = TRUE,
        show_labels  = TRUE,
        x_axis_angle = 0,
        y_axis_labels = scales::comma,
        chart_width  = 10,
        chart_height = 6,
        chart_dir = chart_dir,
        filename  = sprintf("%s_exact_midnight_cy_distribution.pdf", tolower(label))
      )
    }
    
    # ----------------------------------------------------
    # PARETO Section (Requires > 2 unique agencies)
    # ----------------------------------------------------
    # Calculate unique agencies using the column name stored in the 'agency' variable (a string)
    n_unique_agencies <- uniqueN(midnight_data[[agency]])
    
    if (n_unique_agencies > 2) { 
      plot_pareto_combo(
        DT        = exact_midnight_moment,
        x_col     = agency, # 'agency' is the string name of the column
        chart_dir = chart_dir,
        filename  = sprintf("pareto_%s_exact_midnight_agency.pdf", tolower(label)),
        title     = sprintf("Agencies with %s at Exactly Midnight (00:00:00)", label),
        subtitle  = sprintf("n = %s | %s unique agencies", 
                            format(total_n_exact, big.mark = ","), n_unique_agencies),
        top_n     = 30,
        include_na = FALSE
      )
    } 
  }
  
  # ===============================
  # Analysis for EXACTLY NOON (12:00:00)
  # (No further checks needed, as this was fixed in previous steps)
  # ===============================
  
  # 1. Filter for the exact noon moment
  exact_noon_moment <- DT[hour == 12 & minute == 0 & second == 0]
  
  if (nrow(exact_noon_moment) > 0) {
    
    total_n_noon <- nrow(exact_noon_moment)
    
    # ----------------------------------------------------
    # A. BARCHART Section (Requires > 1 unique year for mean/sd)
    # ----------------------------------------------------
    cy_summary_noon <- exact_noon_moment[, .N, by = year_local]
    
    if (nrow(cy_summary_noon) > 1) {
      
      cy_summary_noon[, CY := factor(year_local)] # Ensure CY is treated as a factor
      
      plot_barchart(
        DT        = cy_summary_noon,
        x_col     = "CY",
        y_col     = "N",
        title     = sprintf(" %s at Exactly Noon (12:00:00) by Year ",
                            label),
        subtitle  = sprintf("n = %s | Total records exactly at noon", 
                            format(total_n_noon, big.mark = ",")),
        bar_width    = 0.8,
        add_3sd      = TRUE,
        add_mean    = TRUE,
        add_median = FALSE,
        show_labels  = TRUE,
        x_axis_angle = 0,
        y_axis_labels = scales::comma,
        chart_width  = 10,
        chart_height = 6,
        chart_dir = chart_dir,
        filename  = sprintf("%s_exact_noon_cy_distribution.pdf", tolower(label))
      )
    }
    
    # ----------------------------------------------------
    # B. PARETO Section (Requires > 2 unique agencies)
    # ----------------------------------------------------
    # Calculate unique agencies using the column name stored in the 'agency' variable (a string)
    n_unique_agencies <- uniqueN(exact_noon_moment[[agency]])
    
    if (n_unique_agencies > 2) { 
      plot_pareto_combo(
        DT        = exact_noon_moment,
        x_col     = agency,
        chart_dir = chart_dir,
        filename  = sprintf("pareto_%s_exact_noon_agency.pdf", tolower(label)),
        title     = sprintf("Agencies with %s at  Exactly Noon (12:00:00)", label),
        subtitle  = sprintf("n = %s | %s unique agencies", 
                            format(total_n_noon, big.mark = ","), n_unique_agencies),
        top_n     = 30,
        include_na = FALSE
      )
    }
  }
  
  # ===============================
  # Second-of-minute distribution (ALL 60 seconds in first minute)
  # ===============================
  first_minute_data <- DT[hour == 0 & minute == 0]
  
  if (nrow(first_minute_data) > 0) {
    
    all_seconds <- data.table(second = 0:59)
    first_minute_summary <- first_minute_data[, .N, by = second]
    first_minute_summary <- merge(all_seconds, first_minute_summary, 
                                  by = "second", all.x = TRUE)
    first_minute_summary[is.na(N), N := 0]
    setorder(first_minute_summary, second)
    
    first_minute_summary[, pct := round(100 * N / sum(N), 2)]
    first_minute_summary[, cum_pct := cumsum(pct)]
    
    midnight_total <- DT[hour == 0, .N]
    first_minute_total <- sum(first_minute_summary$N)
    first_minute_pct <- round(100 * first_minute_total / midnight_total, 2)
    
    cat(sprintf(
      "\n%s in first minute (00:00:00–00:00:59): %s (%.2f%% of midnight-hour %s)\n",
      label, format(first_minute_total, big.mark = ","), first_minute_pct, 
      tolower(label)
    ))
    
    # BARCHART CHECK: Check for total N > 0 (since it contains 60 rows, 
    # this check is sufficient to prevent mean/sd errors from empty data)
    if (sum(first_minute_summary$N) > 0) {
      plot_barchart(
        DT        = first_minute_summary,
        x_col     = "second",
        y_col     = "N",
        title     = sprintf("%s Requests by Second (00:00:00–00:00:59)", label),
        subtitle  = sprintf("n = %s | Expect spike at 00:01",
                            format(sum(first_minute_summary$N), big.mark = ",")),
        bar_width = 0.8,
        add_median = FALSE,
        add_3sd = TRUE,
        show_labels = FALSE,
        x_axis_angle = 20,
        y_axis_labels = scales::comma,
        chart_width = 13,
        chart_height = 8.5,
        chart_dir = chart_dir,
        filename  = sprintf("%s_first_minute_second_distribution.pdf", 
                            tolower(label))
      )
    }
    
    zero_second_cases <- DT[hour == 0 & minute == 0 & second == 0]
    zero_second_count <- nrow(zero_second_cases)
    zero_second_pct <- round(100 * zero_second_count / first_minute_total, 2)
    cat(sprintf(
      "%s at exactly 00:00:00: %s (%.2f%% of first-minute %s)\n",
      label, format(zero_second_count, big.mark = ","), zero_second_pct, 
      tolower(label)
    ))
    
    # PARETO CHECK: Requires > 2 unique agencies
    n_unique_agencies_first_minute <- uniqueN(first_minute_data[[agency]])
    
    if (n_unique_agencies_first_minute > 2) {
      plot_pareto_combo(
        DT        = first_minute_data,
        x_col     = agency,
        chart_dir = chart_dir,
        filename  = sprintf("pareto_%s_first_minute_agency.pdf", tolower(label)),
        title     = sprintf("Agencies with First Minute %s (00:00:00–00:00:59)", 
                            label),
        subtitle  = sprintf("n = %s | %s unique agencies", 
                            format(nrow(first_minute_data), big.mark = ","), n_unique_agencies_first_minute),
        top_n     = 30,
        show_labels     = TRUE,
        include_na = FALSE
      )
    }
  }
  
  # ===============================
  # On-the-minute analysis (second = 00)
  # ===============================
  total_records <- nrow(DT[!is.na(second)])
  on_minute_records <- nrow(DT[second == 0])
  
  # BARCHART CHECK: Only plot if total records with second data is > 0
  if (total_records > 0) {
    
    on_minute_pct <- round(100 * on_minute_records / total_records, 2)
    expected_pct <- round(100 / 60, 2)
    ratio_to_expected <- round(on_minute_pct / expected_pct, 1)
    
    cat(sprintf("\n%s Records Exactly On-the-Minute (second = 00):\n", label))
    cat(sprintf("  Total records with second data: %s\n", format(total_records, 
                                                                 big.mark = ",")))
    cat(sprintf("  On-the-minute records (00 sec): %s (%.2f%%)\n", 
                format(on_minute_records, big.mark = ","), on_minute_pct))
    
    all_seconds_full <- data.table(second = 0:59)
    seconds_summary <- DT[!is.na(second), .N, by = second]
    seconds_summary <- merge(all_seconds_full, seconds_summary, by = "second", 
                             all.x = TRUE)
    seconds_summary[is.na(N), N := 0]
    setorder(seconds_summary, second)
    seconds_summary[, pct := round(100 * N / sum(N), 2)]
    seconds_summary[, is_on_minute := second == 0]
    
    plot_barchart(
      DT        = seconds_summary,
      x_col     = "second",
      y_col     = "N",
      title     = sprintf("%s Requests by Second (All Minutes)", label),
      subtitle  = sprintf("n = %s | %.2f%% occur at exactly 00 seconds (%.1fx expected)",
                          format(sum(seconds_summary$N), big.mark = ","),
                          on_minute_pct, ratio_to_expected),
      bar_width = 0.8,
      add_mean  = FALSE,
      add_3sd   = TRUE,
      show_labels = FALSE,
      x_axis_angle  = 0,
      y_axis_labels = scales::comma,
      chart_width = 13,
      chart_height = 8.5,
      chart_dir = chart_dir,
      filename  = sprintf("%s_all_seconds_distribution.pdf", tolower(label))
    )
  }
  
  hourly_on_minute <- DT[!is.na(hour) & !is.na(second), .(
    total = .N,
    on_minute = sum(second == 0)
  ), by = hour][order(hour)]
  hourly_on_minute[, pct_on_minute := round(100 * on_minute / total, 1)]
  
  cat(sprintf("\nOn-the-minute percentages by hour:\n"))
  hourly_display <- hourly_on_minute[, .(
    hour = sprintf("%02d", hour),
    total = format(total, big.mark = ","),
    on_minute = format(on_minute, big.mark = ","),
    pct = sprintf("%.1f%%", pct_on_minute)
  )]
  print(hourly_display, row.names = FALSE, right = FALSE)
  
  # ===============================
  # Top-of-the-hour analysis (minute = 00 and second = 00)
  # ===============================
  total_records_top_hour <- nrow(DT[!is.na(minute) & !is.na(second)])
  top_hour_records <- nrow(DT[minute == 0 & second == 0])
  
  # BARCHART CHECK: Only plot if top_hour_records > 0 and hourly_top_hour has > 1 row
  if (top_hour_records > 0) {
    
    top_hour_pct <- round(100 * top_hour_records / total_records_top_hour, 3)
    expected_pct <- round(100 / (60 * 60), 3)
    ratio_to_expected <- round(top_hour_pct / expected_pct, 1)
    
    cat(sprintf("\n%s Records Exactly at Top-of-the-Hour (00:00):\n", label))
    cat(sprintf("  Total records with minute+second data: %s\n", 
                format(total_records_top_hour, big.mark = ",")))
    cat(sprintf("  Top-of-the-hour records (00:00): %s (%.3f%%)\n", 
                format(top_hour_records, big.mark = ","), top_hour_pct))
    
    hourly_top_hour <- DT[!is.na(hour) & !is.na(minute) & !is.na(second), .(
      total = .N,
      top_of_hour = sum(minute == 0 & second == 0)
    ), by = hour][order(hour)]
    hourly_top_hour[, pct_top_hour := round(100 * top_of_hour / total, 3)]
    
    cat(sprintf("\nTop-of-the-hour percentages by hour:\n"))
    hourly_display <- hourly_top_hour[, .(
      hour = sprintf("%02d", hour),
      total = format(total, big.mark = ","),
      top_of_hour = format(top_of_hour, big.mark = ","),
      pct = sprintf("%.3f%%", pct_top_hour)
    )]
    print(hourly_display, row.names = FALSE, right = FALSE)
    
    # BARCHART CHECK: Requires > 1 unique hour with top_of_hour records
    if (nrow(hourly_top_hour[top_of_hour > 0]) > 1) {
      plot_barchart(
        DT        = hourly_top_hour,
        x_col     = "hour",
        y_col     = "top_of_hour",
        title     = sprintf("%s Requests at Top-of-the-Hour (00:00) by Hour", 
                            label),
        subtitle  = sprintf("n = %s | %.2f%% occur exactly at top-of-the-hour (%.1fx expected)",
                            format(top_hour_records, big.mark = ","),
                            top_hour_pct, 
                            ratio_to_expected),
        bar_width = 0.8,
        add_median = FALSE,
        add_3sd   = TRUE,
        show_labels = FALSE,
        x_axis_angle  = 0,
        y_axis_labels = scales::comma,
        chart_width = 13,
        chart_height = 8.5,
        chart_dir = chart_dir,
        filename  = sprintf("%s_top_of_hour_distribution.pdf", tolower(label))
      )
    }
  }
  
  invisible(list(
    hour_summary          = hour_summary,
    midnight_summary      = if(exists("midnight_summary")) midnight_summary else NULL,
    first_minute_summary  = if(exists("first_minute_summary")) first_minute_summary else NULL
  ))
}