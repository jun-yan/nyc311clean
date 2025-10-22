parse_date_column <- function(
                              temp_raw_data,
                              valid_date_columns,
                              unique_key_col = "unique_key",
                              agency_col = "agency",
                              fmt = "%m/%d/%Y %I:%M:%S %p",
                              local_tz = "America/New_York")
  {
  
  # Make an explicit copy to avoid shallow copy warning
  temp_raw_data <- data.table::copy(temp_raw_data)
  
  # --- Containers for diagnostics ---
  parse_failures <- list()
  time_summary   <- list()
  
  # --- 1. Parse each date field and collect failures ---
  for (col in valid_date_columns) {
    cat(sprintf("\n[%s] Converting to POSIXct...\n", col))
    
    vals <- trimws(temp_raw_data[[col]])
    vals[vals == ""] <- NA_character_
    
    parsed <- suppressWarnings(
      as.POSIXct(vals, format = fmt, tz = local_tz)
    )
    
    # Identify failures
    bad_idx <- which(!is.na(vals) & is.na(parsed))
    
    if (length(bad_idx)) {
      n_bad <- length(bad_idx)
      cat(sprintf("  ⚠️  %d rows failed to parse in %s\n", n_bad, col))
      
      parse_failures[[col]] <- data.table(
        column         = col,
        unique_key     = temp_raw_data[[unique_key_col]][bad_idx],
        agency         = if ("agency" %in% names(temp_raw_data))
          temp_raw_data[[agency_col]][bad_idx]
        else NA_character_,
        original_value = vals[bad_idx]
      )
      
      cat("  Sample failed values:\n")
      print(head(parse_failures[[col]], 10))
    } else {
      cat("  ✓ Parsed successfully (no failures)\n")
    }
    
    # Save parsed result for later update
    new_col <- paste0(gsub(" ", "_", tolower(col)), "_posix")
    temp_raw_data[, (new_col) := parsed]
  }
  
  # --- 2. Combine and review all failures ---
  if (length(parse_failures)) {
    all_failures <- rbindlist(parse_failures, fill = TRUE, idcol = "source_column")
    cat("\n--- Combined Summary of All Parse Failures ---\n")
    print(all_failures)
    
    total_fail <- nrow(all_failures)
    cat(sprintf("\nTotal rows with parsing failures across all fields: %d\n", total_fail))
    
    # --- 3. Run DST spring-forward fix across all failures ---
    cat("\n--- Running DST Spring-Forward Fix for All Failures ---\n")
    
    for (col in names(parse_failures)) {
      vals <- trimws(temp_raw_data[[col]])
      parsed <- temp_raw_data[[paste0(gsub(" ", "_", tolower(col)), "_posix")]]
      bad_idx <- which(!is.na(vals) & is.na(parsed))
      if (!length(bad_idx)) next
      
      dst_result <- fix_dst_spring_forward(
        out           = parsed,
        original_vals = vals,
        failures      = bad_idx,
        fmt           = fmt,
        local_tz      = local_tz,
        agency        = if ("agency" %in% names(temp_raw_data)) temp_raw_data[[agency_col]] else NULL,
        unique_key    = temp_raw_data[[unique_key_col]]
      )
      
      temp_raw_data[[paste0(gsub(" ", "_", tolower(col)), "_posix")]] <- dst_result$out
      
      cat(sprintf("\n  ✓ [%s] DST-fix applied: %d rows corrected\n", col, dst_result$fixed_count))
    }
  } else {
    cat("\nNo parsing failures detected.\n")
  }
  
  # --- 4. Summarize true midnight / noon times ---
  for (col in valid_date_columns) {
    new_col <- paste0(gsub(" ", "_", tolower(col)), "_posix")
    
    time_ok <- temp_raw_data[!is.na(get(new_col)),
                             .(
                               midnight = sum(hour(get(new_col)) == 0 &
                                                minute(get(new_col)) == 0 &
                                                second(get(new_col)) == 0),
                               noon     = sum(hour(get(new_col)) == 12 &
                                                minute(get(new_col)) == 0 &
                                                second(get(new_col)) == 0),
                               total    = .N
                             )
    ]
    
    time_ok[, midnight_pct := round(100 * midnight / total, 4)]
    time_ok[, noon_pct     := round(100 * noon / total, 4)]
    time_ok[, column := col]
    
    time_summary[[col]] <- time_ok
    
    cat(sprintf("\n[%s] Midnights: %d (%.4f%%), Noons: %d (%.4f%%)\n",
                col, time_ok$midnight, time_ok$midnight_pct,
                time_ok$noon, time_ok$noon_pct))
  }
  
  summary_dt <- rbindlist(time_summary, fill = TRUE)
  cat("\n--- Summary: True Midnight / Noon by Date Field ---\n")
  print(summary_dt)
  
  # --- 5. Replace originals with _posix versions and delete extras ---
  cat("\n--- Replacing original date columns with corrected POSIX versions ---\n")
  
  for (col in valid_date_columns) {
    posix_col <- paste0(gsub(" ", "_", tolower(col)), "_posix")
    
    if (posix_col %in% names(temp_raw_data)) {
      cat(sprintf("Replacing '%s' with '%s' ...\n", col, posix_col))
      temp_raw_data[[col]] <- temp_raw_data[[posix_col]]
      temp_raw_data[, (posix_col) := NULL]
    } else {
      cat(sprintf("  Skipping '%s' (no _posix column found)\n", col))
    }
  }
  
  cat("\nReplacement complete. Verifying column classes:\n")
  print(sapply(temp_raw_data[, ..valid_date_columns], class))
  
  invisible(list(
    parsed_data = temp_raw_data,
    summary     = summary_dt,
    failures    = if (exists("all_failures")) all_failures else NULL
  ))
}
