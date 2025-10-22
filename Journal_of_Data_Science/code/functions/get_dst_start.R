get_dst_start <- function(years,
                          tz = "America/New_York",
                          verbose = TRUE) {
  if (verbose) cat("\n[get_dst_start] ENTER\n")
  
  # Normalize and validate input
  years <- suppressWarnings(as.integer(years))
  years <- unique(years[is.finite(years)])
  
  if (length(years) == 0L) {
    if (verbose) cat("  No valid years supplied. Returning empty table.\n[get_dst_start] EXIT (empty)\n")
    return(data.table::data.table(
      year       = integer(),
      dst_start  = as.POSIXct(character(), tz = tz),
      dst_end    = as.POSIXct(character(), tz = tz),
      dst_date   = as.Date(character())
    ))
  }
  
  res_list <- lapply(years, function(y) {
    # Find the 2nd Sunday in March
    base_start <- as.POSIXct(sprintf("%d-03-08 00:00:00", y), tz = tz)
    wk         <- seq(base_start, by = "1 day", length.out = 7)
    second_sunday <- wk[as.POSIXlt(wk)$wday == 0][1]
    date_str   <- format(second_sunday, "%Y-%m-%d")
    
    # Define start and end of the DST jump
    dst_start  <- as.POSIXct(paste0(date_str, " 01:59:59"), tz = tz)
    dst_end    <- as.POSIXct(paste0(date_str, " 03:00:00"), tz = tz)
    
    data.table::data.table(
      year      = y,
      dst_date  = as.Date(second_sunday),
      dst_start = dst_start,
      dst_end   = dst_end
    )
  })
  
  out <- data.table::rbindlist(res_list, use.names = TRUE)
  
  if (verbose && nrow(out)) print(out)
  if (verbose) cat("[get_dst_start] EXIT\n\n")
  
  out
}
