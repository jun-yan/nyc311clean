save_and_verify <- function(dt,
                            path,                    # RDS path
                            write_csv = TRUE,
                            csv_path = NULL,
                            tz_for_csv = Sys.timezone()) {  # Default to system timezone
  
  # Validate inputs
  if (!is.data.table(dt)) stop("Input must be a data.table")
  if (nrow(dt) == 0) {
    cat("Warning: Empty data.table provided\n")
    return(invisible(list(rds = NULL, csv = NULL)))
  }
  
  # Ensure output dir exists
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  
  ##############################################################################
  # ---- RDS Save and Verify ----
  cat("\nSaving RDS formatted file...\n")
  saveRDS(dt, path)
  rb <- readRDS(path)
  
  # Get dimensions
  orig_rows <- nrow(dt)
  orig_cols <- ncol(dt)
  read_rows <- nrow(rb)
  read_cols <- ncol(rb)
  
  # Check integrity
  rows_match <- orig_rows == read_rows
  cols_match <- orig_cols == read_cols
  all_good <- rows_match && cols_match
  
  # Determine status
  if (all_good) {
    status <- "✓ OK"
    status_color <- ""  # or use crayon package for green if you have it
  } else if (rows_match && !cols_match) {
    status <- "⚠ COLUMN MISMATCH"
    status_color <- ""  # or use crayon package for yellow
  } else if (!rows_match && cols_match) {
    status <- "✗ ROW MISMATCH" 
    status_color <- ""  # or use crayon package for red
  } else {
    status <- "✗ FULL MISMATCH"
    status_color <- ""  # or use crayon package for red
  }
  
  # Main status line
  cat(sprintf("%-50s | %9s rows × %4s cols | %s\n",
              basename(path), 
              format(orig_rows, big.mark = ","),
              format(orig_cols, big.mark = ","),
              status))
  
  # Additional details only if there are issues
  if (!all_good) {
    if (!rows_match) {
      cat(sprintf("  ├─ Row mismatch: wrote %s, read %s\n", 
                  format(orig_rows, big.mark = ","), 
                  format(read_rows, big.mark = ",")))
    }
    if (!cols_match) {
      cat(sprintf("  ├─ Column mismatch: wrote %s, read %s\n", 
                  orig_cols, read_cols))
    }
  }
  
  # Additional checks for data integrity
  type_issues <- FALSE
  if (all_good && !identical(sapply(dt, class), sapply(rb, class))) {
    cat("  ├─ ⚠ Column types may have changed during save/load\n")
    type_issues <- TRUE
  }
  
  # File size check
  file_size_mb <- round(file.size(path) / 1024^2, 1)
  
  # Final status with file size
  if (all_good && !type_issues) {
    cat(sprintf("  └─ File size: %s MB | Full integrity verified\n\n", file_size_mb))
  } else if (all_good && type_issues) {
    cat(sprintf("  └─ File size: %s MB | Dimensions OK but check column types\n\n", file_size_mb))
  } else {
    cat(sprintf("  └─ File size: %s MB | ⚠ Please investigate data integrity issues\n\n", file_size_mb))
  }
  
  # Clean up temporary read-back object
  rm(rb)
  
  ##############################################################################
  # ---- CSV (optional) ----
  csv_result <- NULL
  if (isTRUE(write_csv)) {
    cat("\nSaving CSV formatted file...\n")
    
    # Determine CSV path
    if (is.null(csv_path)) {
      csv_path <- sub("\\.rds$", ".csv", path, ignore.case = TRUE)
      if (identical(csv_path, path)) csv_path <- paste0(path, ".csv")
    }
    dir.create(dirname(csv_path), recursive = TRUE, showWarnings = FALSE)
    
    # Prepare a CSV-safe copy (format POSIXct to text in specified timezone)
    dt_csv <- copy(dt)
    posix_cols <- names(dt_csv)[vapply(dt_csv, inherits, logical(1), "POSIXct")]
    if (length(posix_cols) > 0L) {
      cat(sprintf("  Converting %d POSIXct column(s) to text with timezone: %s\n", 
                  length(posix_cols), tz_for_csv))
      dt_csv[, (posix_cols) := lapply(.SD, function(x) {
        format(x, "%Y-%m-%d %H:%M:%S", tz = tz_for_csv)
      }), .SDcols = posix_cols]
    }
    
    # Warn about problematic column types
    list_cols <- names(dt_csv)[vapply(dt_csv, is.list, logical(1))]
    if (length(list_cols) > 0L) {
      cat(sprintf("  [WARNING] List columns will be coerced: %s\n",
                  paste(list_cols, collapse = ", ")))
    }
    
    # Check for very wide data
    if (ncol(dt_csv) > 1000) {
      cat(sprintf("  [NOTE] Large number of columns (%d) - CSV may be slow\n", ncol(dt_csv)))
    }
    
    # Write and verify CSV
    tryCatch({
      fwrite(dt_csv, csv_path, bom = TRUE, showProgress = FALSE)
      rb_csv <- fread(csv_path, showProgress = FALSE,
                      nThread = max(1L, parallel::detectCores() - 1L))
      
      # Get dimensions
      orig_rows <- nrow(dt_csv)
      orig_cols <- ncol(dt_csv)
      read_rows <- nrow(rb_csv)
      read_cols <- ncol(rb_csv)
      
      # Check integrity
      rows_match <- orig_rows == read_rows
      cols_match_count <- orig_cols == read_cols
      cols_match_names <- identical(names(dt_csv), names(rb_csv))
      
      # Determine status
      if (rows_match && cols_match_count && cols_match_names) {
        status <- "✓ OK"
      } else if (rows_match && cols_match_count && !cols_match_names) {
        status <- "⚠ COLUMN NAMES"
      } else if (rows_match && !cols_match_count) {
        status <- "✗ COLUMN COUNT"
      } else if (!rows_match && cols_match_count) {
        status <- "✗ ROW COUNT"
      } else {
        status <- "✗ FULL MISMATCH"
      }
      
      # Main status line
      cat(sprintf("%-50s | %9s rows × %4s cols | %s\n",
                  basename(csv_path), 
                  format(orig_rows, big.mark = ","),
                  format(orig_cols, big.mark = ","),
                  status))
      
      # Track issues for summary
      has_issues <- FALSE
      
      # Report specific issues
      if (!rows_match) {
        cat(sprintf("  ├─ Row mismatch: wrote %s, read %s\n", 
                    format(orig_rows, big.mark = ","), 
                    format(read_rows, big.mark = ",")))
        has_issues <- TRUE
      }
      
      if (!cols_match_count) {
        cat(sprintf("  ├─ Column count mismatch: wrote %s, read %s\n", 
                    orig_cols, read_cols))
        has_issues <- TRUE
      }
      
      if (cols_match_count && !cols_match_names) {
        cat("  ├─ Column names differ between written and read data\n")
        has_issues <- TRUE
      }
      
      # Check for data type changes (CSV-specific considerations)
      type_issues <- FALSE
      if (rows_match && cols_match_count && cols_match_names) {
        # Get original types (excluding POSIXct which we converted)
        orig_types <- sapply(dt_csv, class)
        read_types <- sapply(rb_csv, class)
        
        # Compare types, but be lenient about expected CSV conversions
        type_diffs <- which(!mapply(function(o, r) {
          # Allow character -> character (expected)
          # Allow numeric -> numeric or integer -> numeric (expected in CSV)
          if (identical(o, r)) return(TRUE)
          if (any(o == "character") && any(r == "character")) return(TRUE)
          if (any(o %in% c("numeric", "integer")) && any(r %in% c("numeric", "integer"))) return(TRUE)
          return(FALSE)
        }, orig_types, read_types))
        
        if (length(type_diffs) > 0) {
          cat(sprintf("  ├─ ⚠ Unexpected type changes in %d column(s): %s\n", 
                      length(type_diffs), 
                      paste(names(type_diffs)[1:min(3, length(type_diffs))], collapse = ", ")))
          if (length(type_diffs) > 3) {
            cat(sprintf("      (and %d more...)\n", length(type_diffs) - 3))
          }
          type_issues <- TRUE
        }
      }
      
      # File size check
      file_size_mb <- round(file.size(csv_path) / 1024^2, 1)
      
      # Final status with file size
      all_good <- rows_match && cols_match_count && cols_match_names && !type_issues
      
      if (all_good) {
        cat(sprintf("  └─ File size: %s MB | Full integrity verified\n\n", file_size_mb))
      } else if (!has_issues && type_issues) {
        cat(sprintf("  └─ File size: %s MB | Dimensions OK but check column types\n\n", file_size_mb))
      } else {
        cat(sprintf("  └─ File size: %s MB | ⚠ Please investigate data integrity issues\n\n", file_size_mb))
      }
      
      csv_result <- csv_path
      
      # Clean up temporary objects
      rm(rb_csv)
      
    }, error = function(e) {
      cat(sprintf("  ✗ CSV write/read failed: %s\n\n", e$message))
      csv_result <- NULL
    })
  }
  
  cat("Files saved in RDS and CSV format complete. Saved and verified.\n")
  invisible(list(rds = path, csv = csv_result))
}