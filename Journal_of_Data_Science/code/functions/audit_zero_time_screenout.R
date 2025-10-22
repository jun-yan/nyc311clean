# -- Screen-out audit by year (created/closed exactly at 00:00:00) ----------
audit_zero_time_screenout <- function(DT) {
  stopifnot(data.table::is.data.table(DT))
  # flags (keep NA-safe)
  cz <- with(DT, !is.na(created_date) &
               lubridate::hour(created_date)  == 0 &
               lubridate::minute(created_date)== 0 &
               lubridate::second(created_date)== 0)
  dz <- with(DT, !is.na(closed_date)  &
               lubridate::hour(closed_date)   == 0 &
               lubridate::minute(closed_date) == 0 &
               lubridate::second(closed_date) == 0)
  
  tmp <- data.table::data.table(
    year  = lubridate::year(DT$created_date),
    cz    = cz,
    dz    = dz
  )
  
  out <- tmp[, .(
    total         = .N,
    created_zero  = sum(cz, na.rm = TRUE),
    closed_zero   = sum(dz, na.rm = TRUE),
    either_zero   = sum(cz | dz, na.rm = TRUE),
    both_zero     = sum(cz & dz, na.rm = TRUE)
  ), by = year][order(year)]
  
  out[, `:=`(
    created_zero_pct = round(100 * created_zero / pmax(total, 1), 3),
    closed_zero_pct  = round(100 * closed_zero  / pmax(total, 1), 3),
    either_zero_pct  = round(100 * either_zero  / pmax(total, 1), 3),
    both_zero_pct    = round(100 * both_zero    / pmax(total, 1), 3)
  )]
  
  # pretty print
  cat("\n[Zero-time screenout by created year]\n")
  print(out[, .(
    year,
    total        = format(total, big.mark=","),
    created_zero = sprintf("%s (%.3f%%)", format(created_zero, big.mark=","), created_zero_pct),
    closed_zero  = sprintf("%s (%.3f%%)", format(closed_zero,  big.mark=","), closed_zero_pct),
    either_zero  = sprintf("%s (%.3f%%)", format(either_zero,  big.mark=","), either_zero_pct),
    both_zero    = sprintf("%s (%.3f%%)", format(both_zero,    big.mark=","), both_zero_pct)
  )], row.names = FALSE)
  
  # Create plots for each category
  plot_barchart(
    DT = out,
    x_col = "year",
    y_col = "created_zero",
    title = "Created Date missing time values (HH:MM:SS) by Year",
    subtitle = sprintf("time values (HH:MM:SS) by Year: %s", 
                       format(sum(out$created_zero), big.mark = ",")),
    x_label = "Year",
    y_label = "Count",
    console_print_title = "Created Zero Time Distribution"
  )
  
  plot_barchart(
    DT = out,
    x_col = "year",
    y_col = "closed_zero",
    title = "Closed Date missing time values (HH:MM:SS) by Year",
    subtitle = sprintf("time values (HH:MM:SS) by Year: %s", 
                       format(sum(out$created_zero), big.mark = ",")),
    x_label = "Year",
    y_label = "Count",
    console_print_title = "Closed Zero Time Distribution"
  )
  
  plot_barchart(
    DT = out,
    x_col = "year",
    y_col = "either_zero",
    title = "Either Created/Closed Date missing time values (HH:MM:SS) by Year",
    subtitle = sprintf("time values (HH:MM:SS) by Year: %s", 
                       format(sum(out$created_zero), big.mark = ",")),
    x_label = "Year",
    y_label = "Count",
    console_print_title = "Either Zero Time Distribution"
  )
  
  plot_barchart(
    DT = out,
    x_col = "year",
    y_col = "both_zero",
    title = "Both Created & Closed Date missing time values (HH:MM:SS) by Year",
    subtitle = sprintf("time values (HH:MM:SS) by Year: %s", 
                       format(sum(out$created_zero), big.mark = ",")),
    x_label = "Year",
    y_label = "Count",
    console_print_title = "Both Zero Time Distribution"
  )
  
  invisible(out)
}