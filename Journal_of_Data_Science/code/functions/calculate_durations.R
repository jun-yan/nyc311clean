calculate_durations <- function(
    DT,
    created_col = "created_date",
    closed_col = "closed_date",
    tz = "America/New_York",
    in_place = TRUE,
    keep_parsed_timestamps = TRUE
) {
  stopifnot(data.table::is.data.table(DT))
  if (!all(c(created_col, closed_col) %in% names(DT))) {
    stop("created_col and/or closed_col not found in DT.")
  }
  
  X <- if (in_place) DT else data.table::copy(DT)
  
  # Helper: parse a column to POSIXct (fasttime if available), preserving tz
  to_posix <- function(v) {
    if (inherits(v, "POSIXct")) {
      v
    } else if (requireNamespace("fasttime", quietly = TRUE)) {
      fasttime::fastPOSIXct(as.character(v), tz = tz)
    } else {
      as.POSIXct(as.character(v), tz = tz)
    }
  }
  
  # Create parsed timestamp columns (always; keeps originals intact)
  X[, created_ts := to_posix(.SD[[1]]), .SDcols = created_col]
  X[, closed_ts := to_posix(.SD[[1]]), .SDcols = closed_col]
  
  # Calculate durations (only for rows with both timestamps present)
  denom <- !is.na(X$created_ts) & !is.na(X$closed_ts)
  X[denom, duration_sec := as.numeric(closed_ts) - as.numeric(created_ts)]
  X[denom, duration_days := duration_sec / 86400]
  
  # Optionally remove parsed timestamps
  if (!keep_parsed_timestamps) {
    X[, c("created_ts", "closed_ts") := NULL]
  }
  
  # Return the data.table (modified in place if in_place = TRUE)
  if (in_place) {
    invisible(X)
  } else {
    return(X)
  }
}
