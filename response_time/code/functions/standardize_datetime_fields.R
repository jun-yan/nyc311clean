###################################################################
standardize_datetime_fields <- function(dt, date_columns, 
                                        format = "%m/%d/%Y %I:%M:%S %p",
                                        timezone = "America/New_York",
                                        verbose = TRUE) {
  stopifnot(data.table::is.data.table(dt))
  
  valid_cols <- intersect(date_columns, names(dt))
  if (length(valid_cols) == 0) {
    if (verbose) message("⚠️ No matching date columns found in dataset.")
    return(invisible(dt))
  }
  
  # Convert to POSIXct
#  if (verbose) cat("🔄 Converting date columns to POSIXct...\n")
  cat("\n")
  
  for (col in valid_cols) {
    if (verbose) cat(sprintf("🔄  Converting column %s...\n", col))
    dt[, (col) := as.POSIXct(get(col), format = format, tz = timezone)]
    if (verbose) cat(sprintf("  ✅ Conversion of column %s complete\n\n", col))
  }
  
  if (verbose) cat("✅ All date columns converted to POSIXct\n")
  
  invisible(dt)
}
###################################################################