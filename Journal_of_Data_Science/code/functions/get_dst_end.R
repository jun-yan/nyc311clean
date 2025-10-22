# ==========================================================
# Compute DST fall-back datetime for a given year
# (US rules: first Sunday in November, at 02:00 local time)
# ==========================================================
# First Sunday in November (DATE), US DST end
get_dst_end <- function(year, tz = "America/New_York") {
  stopifnot(is.numeric(year))
  
  nov1 <- as.Date(sprintf("%d-11-01", year))
  wday <- as.POSIXlt(nov1)$wday   # 0 = Sunday, 6 = Saturday
  dst_date <- nov1 + ((7 - wday) %% 7)  # first Sunday in Nov
  
  start <- as.POSIXct(paste(dst_date, "01:00:00"), tz = tz)
  end   <- as.POSIXct(paste(dst_date, "02:00:00"), tz = tz)
  
  data.table(
    dst_end_date = dst_date,
    dst_start    = start,
    dst_end      = end
  )
}
