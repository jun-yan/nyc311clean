date_checks_character <- function(DT, date_cols) {
  
  cat("\n Entering character date check\n")
  
  # Ensure data.table
  if (!is.data.table(DT)) DT <- as.data.table(DT)
  
  summary_list <- lapply(date_cols, function(col) {
    if (!col %in% names(DT)) {
      cat("\n[Warning] Column not found:", col, "\n")
      return(NULL)
    }
    
    raw_vals <- DT[[col]]
    na_count <- sum(is.na(raw_vals) | trimws(raw_vals) == "")
    
    vals <- trimws(raw_vals)
    vals <- vals[!is.na(vals) & vals != ""]
    
    if (length(vals) == 0) {
      cat("\n[Info] Column has no non-empty values:", col, "\n")
      return(NULL)
    }
    
    # Character-length summary
    len_table <- data.table(nchar = nchar(vals))[
      , .N, by = nchar
    ][order(nchar)]
    
    # Extract HH, MM, AM/PM
    hour_vals   <- substr(vals, 12, 13)
    minute_vals <- substr(vals, 15, 16)
    ampm_vals   <- substr(vals, 21, 22)
    time_part   <- substr(vals, 12, 22)  # "HH:MM:SS AM/PM"
    
    # Hour table
    hour_table <- data.table(hour = hour_vals)[
      !is.na(hour) & hour != "",
      .N, by = hour
    ][order(as.integer(hour))]
    hour_table[, pct := round(100 * N / sum(N), 2)]
    
    # Minute table
    minute_table <- data.table(minute = minute_vals)[
      !is.na(minute) & minute != "",
      .N, by = minute
    ][order(as.integer(minute))]
    minute_table[, pct := round(100 * N / sum(N), 2)]
    
    # Count specific times (in 12-hour format)
    count_midnight <- sum(time_part == "12:00:00 AM", na.rm = TRUE)
    count_noon     <- sum(time_part == "12:00:00 PM", na.rm = TRUE)
    total_non_na   <- length(vals)
    total_rows     <- nrow(DT)
    
    # Combine main summary
    result <- data.table(
      column = col,
      total_rows    = total_rows,
      na_count      = na_count,
      total_non_na  = total_non_na,
      nchar_summary = paste(
        paste0(len_table$nchar, " (", len_table$N, ")"),
        collapse = ", "
      ),
      count_midnight = count_midnight,
      pct_midnight   = round(100 * count_midnight / total_non_na, 4),
      count_noon     = count_noon,
      pct_noon       = round(100 * count_noon / total_non_na, 4)
    )
    
    # ---- Print detailed results ----
    cat("\n============================================================\n")
    cat("Field:", col, "\n")
    cat("============================================================\n")
    cat(sprintf("Total rows: %d | Non-empty: %d | NA/empty: %d (%.2f%%)\n",
                total_rows, total_non_na, na_count, 100 * na_count / total_rows))
    
    cat("\n", col, " — Hour Distribution\n", sep = "")
    print(hour_table)
    
    cat("\n", col, " — Minute Distribution\n", sep = "")
    print(minute_table)
    
    cat("\n", col, " — Counts of Special Times\n", sep = "")
    cat(sprintf("Midnight (12:00:00 AM): %d (%.2f%%)\n",
                count_midnight, 100 * count_midnight / total_non_na))
    cat(sprintf("Noon (12:00:00 PM):     %d (%.2f%%)\n",
                count_noon, 100 * count_noon / total_non_na))
    
    return(result)
  })
  
  summary_dt <- rbindlist(summary_list, fill = TRUE)
  
  cat("\n--- Date Field Character-Length, 'Midnight', 'Noon', and NA Summary ---\n")
  print(summary_dt)
  
  invisible(summary_dt)
}
