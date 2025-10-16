#' Fix Spring DST anomalies in UTC timestamps
#'
#' @param dt A data.table with UTC date columns
#' @param verbose Logical. If TRUE, prints detailed info about fixes
#' @return The modified data.table (invisibly)
#' @description Fixes spring forward DST anomalies by identifying records with 
#'   artificially inflated durations and subtracting 1 hour from closed_date
fix_spring_dst_utc <- function(dt, verbose = TRUE) {
  
  library(lubridate)
  
  if (!all(c("created_date", "closed_date") %in% names(dt))) {
    if (verbose) cat("⚠️ Both created_date and closed_date columns required\n")
    return(invisible(dt))
  }
  
  if (verbose) {
    cat("\n🌸 Fixing Spring DST anomalies in UTC timestamps...\n")
#    cat("   Note: Working with UTC timestamps to avoid DST complications\n\n")
  }
  
  # Helper function to get spring forward date (2nd Sunday in March)
  get_spring_forward_date <- function(year) {
    march_1 <- as.Date(paste0(year, "-03-01"))
    first_sunday <- march_1 + (7 - as.numeric(format(march_1, "%w"))) %% 7
    return(first_sunday + 7)
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
  
  spring_dates <- sapply(years, get_spring_forward_date)
  names(spring_dates) <- years
  
  if (verbose) {
    cat(sprintf("\n  📅 Checking Spring DST dates for years: %s\n", 
                paste(range(years), collapse = " to ")))
  }
  
  spring_dst_fixes <- 0
  
  for (yr in years) {
    spring_date <- as.Date(spring_dates[as.character(yr)])
    
    # Find records where created_date is on spring forward date and in 1-2 AM hour
    spring_candidates <- valid_rows & 
      as.Date(dt$created_date) == spring_date &
      hour(dt$created_date) >= 1 & 
      hour(dt$created_date) < 2
    
    if (sum(spring_candidates) == 0) next
    
    if (verbose) {
      cat(sprintf("    📅 %s: Found %s records with created_date in spring window (1-2 AM)\n", 
                  spring_date, format(sum(spring_candidates), big.mark = ",")))
    }
    
    # From candidates, find ones with closed_date suggesting artificial inflation
    # Look for closed times that would be >= 2 AM on the same date (artificially inflated)
    spring_anomalies <- spring_candidates & 
      as.Date(dt$closed_date) == spring_date &
      hour(dt$closed_date) >= 3
    
    if (sum(spring_anomalies) == 0) {
      if (verbose) cat(sprintf("      ✅ No artificially inflated durations found\n"))
      next
    }
    
    if (verbose) {
      cat(sprintf("\n      🔍 Found %s anomalies (close_date +1 hour due to DST)\n", 
                  format(sum(spring_anomalies), big.mark = ",")))
    }
    
    # Fix each anomaly
    for (i in which(spring_anomalies)) {
      created <- dt$created_date[i]
      closed_before <- dt$closed_date[i] 
      
#      if (verbose) {
        # cat(sprintf("\n      🔧 Attempting spring DST fix [row %d]:\n", i))
        # cat(sprintf("          Created: %s UTC\n", format(created, "%Y-%m-%d %H:%M:%S")))
        # cat(sprintf("          Closed BEFORE: %s UTC\n", format(closed_before, "%Y-%m-%d %H:%M:%S")))
#        cat(sprintf("          Time diff BEFORE: %.2f hours\n", 
#                    as.numeric(difftime(closed_before, created, units = "hours"))))
#      }
      
      # SUBTRACT 1 hour from closed_date (simple arithmetic in UTC)
      dt[i, closed_date := closed_date - 3600]
      
      # Test if correction worked
      closed_after <- dt$closed_date[i]
      new_diff <- as.numeric(difftime(closed_after, created, units = "hours"))
      
      if (closed_after >= created) {
        spring_dst_fixes <- spring_dst_fixes + 1
        
#        if (verbose) {
#          cat(sprintf("          Closed AFTER:  %s UTC\n", format(closed_after, "%Y-%m-%d %H:%M:%S")))
#         cat(sprintf("          Time diff AFTER:  %.2f hours\n", new_diff))
#          cat(sprintf("          ✅ Spring DST fix applied\n\n"))
#        }
        
      } else {
        # Revert if fix didn't work
        dt[i, closed_date := closed_before]
        
        if (verbose) {
          cat(sprintf("          Closed AFTER:  %s UTC\n", format(closed_after, "%Y-%m-%d %H:%M:%S")))
          cat(sprintf("          Time diff AFTER:  %.2f hours\n", new_diff))
          cat(sprintf("          ❌ FAILED - Fix didn't work\n"))
          cat(sprintf("          📊 DIAGNOSTIC INFO:\n"))
          cat(sprintf("              • Original created: %s UTC\n", as.character(created)))
          cat(sprintf("              • Original closed:  %s UTC\n", as.character(closed_before)))
          cat(sprintf("              • After -1hr closed: %s UTC\n", as.character(closed_after)))
          cat(sprintf("              • Time difference still: %.4f hours\n", new_diff))
          cat(sprintf("          🔄 Reverted change\n\n"))
        }
        
        stop("Spring DST fix failed for row ", i, ". Check diagnostic info above.")
      }
    }
  }
  
  # Final summary
  if (verbose) {
    cat(sprintf("  ✅ Spring DST fixes applied: %s\n", 
                format(spring_dst_fixes, big.mark = ",")))
  }
  
  invisible(dt)
}