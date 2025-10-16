#' Fix Fall DST anomalies in UTC timestamps (with full debugging)
#'
#' @param dt A data.table with UTC date columns  
#' @param verbose Logical. If TRUE, prints detailed info about fixes
#' @return The modified data.table (invisibly)
#' @description Fixes fall back DST anomalies by identifying records where
#'   created_date is 1-2 AM on fall back date and closed_date < created_date
fix_fall_dst_utc <- function(dt, verbose = TRUE) {
  
  library(lubridate)
  
  if (!all(c("created_date", "closed_date") %in% names(dt))) {
    if (verbose) cat("⚠️ Both created_date and closed_date columns required\n")
    return(invisible(dt))
  }
  
  if (verbose) {
    cat("\n🍂 Fixing Fall DST anomalies in UTC timestamps...\n")
#   cat("   Note: Working with UTC timestamps to avoid DST complications\n\n")
    
    # # Quick dataset overview
    # total_anomalies <- sum(!is.na(dt$created_date) & !is.na(dt$closed_date) & 
    #                          dt$closed_date < dt$created_date)
    # cat(sprintf("📊 Total closed < created anomalies in dataset: %s\n\n", 
    #             format(total_anomalies, big.mark = ",")))
  }
  
  # Helper function to get fall back date (1st Sunday in November)
  get_fall_back_date <- function(year) {
    nov_1 <- as.Date(paste0(year, "-11-01"))
    first_sunday <- nov_1 + (7 - as.numeric(format(nov_1, "%w"))) %% 7
    return(first_sunday)
  }
  
  # Only work with rows that have both dates
  valid_rows <- !is.na(dt$created_date) & !is.na(dt$closed_date)
  
  if (sum(valid_rows) == 0) {
    if (verbose) cat("  ✅ No valid date pairs to check\n")
    return(invisible(dt))
  }
  
  # Get unique years in the data
  years <- unique(year(dt$created_date[valid_rows]))
  years <- years[!is.na(years)]
  
  fall_dates <- sapply(years, get_fall_back_date)
  names(fall_dates) <- years
  
  if (verbose) {
    cat(sprintf("  📅 Here are Fall DST dates for years: %s\n", 
                paste(range(years), collapse = " to ")))
    
    # Print out each fall date
    for (yr in years) {
      fall_date_display <- as.Date(fall_dates[as.character(yr)])
      cat(sprintf("      %d: %s\n", yr, fall_date_display))
    }

    # Check fall dates with wider analysis
    # cat("🔍 OVERVIEW: Checking closed < created anomalies on fall dates:\n")
    # for (yr in years) {
    #   fall_date <- as.Date(fall_dates[as.character(yr)])
    #   anomalies_on_date <- sum(valid_rows &
    #                              as.Date(dt$created_date) == fall_date &
    #                              dt$closed_date < dt$created_date)
    #   cat(sprintf("    %s: %d total closed < created anomalies (any hour)\n", 
    #               fall_date, anomalies_on_date))
    # }
    cat("\n")
  }
  
  fall_dst_fixes <- 0
  
  for (yr in years) {
    fall_date <- as.Date(fall_dates[as.character(yr)])
    
    # if (verbose) {
    #   cat(sprintf("\n  🔍 DETAILED ANALYSIS for %d fall date: %s\n", yr, fall_date))
    #   
    #   # Check how many records have created_date on this fall date
    #   records_on_date <- sum(valid_rows & as.Date(dt$created_date) == fall_date, na.rm = TRUE)
    #   cat(sprintf("      Total records with created_date on %s: %s\n", 
    #               fall_date, format(records_on_date, big.mark = ",")))
    #   
    #   if (records_on_date > 0) {
    #     # Check hour distribution for created_date on this date
    #     created_on_date <- valid_rows & as.Date(dt$created_date) == fall_date
    #     created_hours <- hour(dt$created_date[created_on_date])
    #     hour_table <- table(created_hours)
    #     # cat(sprintf("      Hour distribution: %s\n", 
    #     #             paste(names(hour_table), "=", hour_table, collapse = ", ")))
    #     
    #     # Check specifically for 1-2 AM created records
    #     created_1_2am <- sum(created_on_date & 
    #                            hour(dt$created_date) >= 1 & 
    #                            hour(dt$created_date) < 2, na.rm = TRUE)
    #     # cat(sprintf("      Records created 1-2 AM: %s\n", 
    #     #             format(created_1_2am, big.mark = ",")))
    #     
    #     if (created_1_2am > 0) {
    #       # Show ALL rows with created_date 1-2 AM on this fall date
    #       cat(sprintf("      📋 ALL ROWS with created_date 1-2 AM on %s:\n", fall_date))
    #       
    #       indices_1_2am <- which(created_on_date & 
    #                                hour(dt$created_date) >= 1 & 
    #                                hour(dt$created_date) < 2)
    #       
    #       # Show first 5 rows (or all if fewer than 5)
    #       #show_count <- min(5, length(indices_1_2am))
    #       
    #       # for (i in 1:show_count) {
    #       #   idx <- indices_1_2am[i]
    #       #   created_time <- dt$created_date[idx]
    #       #   closed_time <- dt$closed_date[idx]
    #       #   time_diff <- as.numeric(difftime(closed_time, created_time, units = "hours"))
    #       #   
    #       #   cat(sprintf("        Row %d: Created %s  Closed %s ", "\n",
    #       #               idx, 
    #       #               format(created_time, "%Y-%m-%d %H:%M:%S"),
    #       #               if(is.na(closed_time)) "NA" else format(closed_time, "%Y-%m-%d %H:%M:%S")
    #       #               ))
    #       # }
    #       
    #       if (length(indices_1_2am) > 20) {
    #         cat(sprintf("        ... and %d more rows\n", length(indices_1_2am) - 20))
    #       }
    #       
    #       # Check for closed < created among 1-2 AM records
    #       closed_before_created <- sum(created_on_date & 
    #                                      hour(dt$created_date) >= 1 & 
    #                                      hour(dt$created_date) < 2 &
    #                                      dt$closed_date < dt$created_date, na.rm = TRUE)
    #       cat(sprintf("            ❗ Adjusted total of %s records created 1-2 AM by closed_date +1 hour\n", 
    #                   format(created_1_2am, big.mark = ",")))
    #     }
    #   }
    #   cat("\n")
    # }
    
    # Find candidate rows: ALL records created between 1-2 AM on fall date
    # (These need closed_date adjusted by +1 hour due to the duplicated hour)
    fall_candidates <- valid_rows & 
      as.Date(dt$created_date) == fall_date &
      hour(dt$created_date) >= 1 & 
      hour(dt$created_date) < 2
    
    if (sum(fall_candidates) == 0) {
      if (verbose) cat(sprintf("    📅 %s: No records created in 1-2 AM window\n", fall_date))
      next
    }
    
    if (verbose) {
      cat(sprintf("    📅 %s: Found %s records created 1-2 AM (all need closed_date +1 hour)\n", 
                  fall_date, format(sum(fall_candidates), big.mark = ",")))
    }
    
    # All candidates need fixing (no additional condition required)
    fall_anomalies <- fall_candidates
    
    # Fix each anomaly
    for (i in which(fall_anomalies)) {
      created <- dt$created_date[i]
      closed_before <- dt$closed_date[i]
      
      # if (verbose) {
      #   cat(sprintf("      🔧 Applying fall DST fix [row %d]:\n", i))
      #   cat(sprintf("          Created: %s UTC (in duplicated 1-2 AM window)\n", format(created, "%Y-%m-%d %H:%M:%S")))
      #   cat(sprintf("          Closed BEFORE: %s UTC\n", format(closed_before, "%Y-%m-%d %H:%M:%S")))
      #   cat(sprintf("          Time diff BEFORE: %.2f hours (artificially compressed)\n", 
      #               as.numeric(difftime(closed_before, created, units = "hours"))))
      # }
      
      # Add 1 hour to closed_date to account for the duplicated hour
      dt[i, closed_date := closed_date + 3600]  # +3600 seconds = +1 hour
      
      # Show result
      closed_after <- dt$closed_date[i]
      new_diff <- as.numeric(difftime(closed_after, created, units = "hours"))
      
      fall_dst_fixes <- fall_dst_fixes + 1
      
      # if (verbose) {
      #   cat(sprintf("          Closed AFTER:  %s UTC\n", format(closed_after, "%Y-%m-%d %H:%M:%S")))
      #   cat(sprintf("          Time diff AFTER:  %.2f hours (corrected response time)\n", new_diff))
      #   cat(sprintf("          ✅ Fall DST fix applied\n\n"))
      # }
    }
  }
  
  # Final summary
  remaining_anomalies <- sum(!is.na(dt$created_date) & !is.na(dt$closed_date) & 
                               (dt$closed_date < dt$created_date))
  
  # if (verbose) {
  #   cat(sprintf("  ✅ Fall DST fixes applied: %s records (closed_date +1 hour)\n", 
  #               format(fall_dst_fixes, big.mark = ",")))
  #   # cat(sprintf("  📊 Remaining closed < created anomalies: %s (likely non-DST data quality issues)\n", 
  #   #             format(remaining_anomalies, big.mark = ",")))
  # }
  
  invisible(dt)
}