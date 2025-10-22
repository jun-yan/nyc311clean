analyze_dst_fallback <- function(
    DT,
    chart_dir,
    show_examples = 10
) {
  stopifnot(data.table::is.data.table(DT))
  
  # --- Lightweight copy & early cleansing ---
  DT_copy <- copy(DT)[
    !is.na(created_date) & !is.na(closed_date) &
      !(lubridate::hour(created_date) == 0 & lubridate::minute(created_date) == 0 
        & lubridate::second(created_date) == 0) &
      !(lubridate::hour(closed_date)  == 0 & 
          lubridate::minute(closed_date)  == 0 & 
          lubridate::second(closed_date)  == 0)
  ]
  if (!nrow(DT_copy)) {
    cat("\nNo rows with usable created/closed timestamps.\n")
    return(invisible(data.table()))
  }
  
  # --- Build fallback windows (DST end) for all years present ---
  years <- sort(unique(lubridate::year(DT_copy$created_date)))
  dst_info <- rbindlist(lapply(years, function(y) {
    # get_dst_end(y) should return: 
    #       dst_start (POSIXct), dst_end (POSIXct), dst_end_date (Date/POSIXct)
    di <- get_dst_end(y)
    di[, year := y]
    di
  }))
  data.table::setorder(dst_info, year)
  
  # Use distinct names for the window bounds
  W <- dst_info[, .(
    dst_year     = year,
    win_start    = dst_start,     # start of ambiguous hour
    win_end      = dst_end,       # end of second 01:59 (exclusive)
    dst_end_date = as.Date(dst_end_date)
  )]
  
  # --- Vectorized selection via foverlaps (no custom 'on=' parsing) ---
  # Represent closed_date as a degenerate interval [end, end]
  DTx <- DT_copy[, .SD]  # retain all columns
  DTx[, `:=`(
    start    = created_date,
    end      = closed_date,
    iv_start = closed_date,
    iv_end   = closed_date
  )]
  
  # Keys for foverlaps
  data.table::setkey(DTx, iv_start, iv_end)
  data.table::setkey(W,   win_start, win_end)
  
  # 1) Rows whose CLOSED time falls within the fallback window
  # joins by iv_start/iv_end vs win_start/win_end
  hits <- data.table::foverlaps(DTx, W, nomatch = 0L)  
  
  # 2) Require CREATED  within the same window + negative elapsed (end < start)
  affected <- hits[start >= win_start & start < win_end & end < start]
  
  if (!nrow(affected)) {
    cat("\nNo DST fall-back negative durations detected.\n")
    return(invisible(data.table(dst_end_date = as.Date(character()), 
                            n_rows = integer(), avg_negative_mins = numeric())))
  }
  
  # Negative minutes
  affected[, neg_mins := as.numeric(difftime(end, start, units = "mins"))]
  
  # --- Summary by fallback date (plus TOTAL) ---
  fallback_summary <- affected[, .(
    n_rows = .N,
    avg_negative_mins = round(mean(neg_mins), 2)
  ), by = .(dst_end_date)][order(dst_end_date)]
  
  if (nrow(fallback_summary)) {
    total <- affected[, .(
      dst_end_date = as.Date(NA),
      n_rows = .N,
      avg_negative_mins = round(mean(neg_mins), 2)
    )]
    fallback_summary <- rbind(fallback_summary, total, fill = TRUE)
  }
  
  # --- Print summary ---
  cat("\nDST Fall-back systemic negative durations:\n")
  print(
    fallback_summary[, .(
      dst_end_date      = ifelse(is.na(dst_end_date), 
                                 "TOTAL", as.character(dst_end_date)),
      n_rows            = format(n_rows, big.mark = ","),
      avg_negative_mins = formatC(avg_negative_mins, format = "f", digits = 2)
    )],
    row.names = FALSE, right = FALSE
  )
  
  # --- Chart by fallback date (skip TOTAL row) ---
  plot_data <- fallback_summary[!is.na(dst_end_date)]
  stopifnot(inherits(plot_data$dst_end_date, "Date"))
  
  if (nrow(plot_data) > 0) {
    totalN <- sum(plot_data$n_rows)
    plot_data[, count_labels := scales::comma(n_rows)]
    
    plot_barchart(
      DT            = plot_data,
      x_col         = "dst_end_date",
      y_col         = "n_rows",
      title         = "DST Fall-back Negative Duration Cases by Date",
      subtitle      = sprintf("n = %s", scales::comma(totalN)),
      bar_width     = 45,
      show_labels   = TRUE,
      label_col     = "count_labels",
      label_size    = 3,
      label_angle   = 0,
      x_axis_angle  = 30,
      y_axis_labels = scales::comma,
      text_size     = 12,
      chart_width   = 13,
      chart_height  = 8.5,
      chart_dir     = chart_dir,
      filename      = "dst_fallback_negative_duration_by_date.pdf"
    )
    
    Sys.sleep(3)
  }
  
  # --- Optional examples for sanity (limit by show_examples) ---
  if (isTRUE(show_examples) || (is.numeric(show_examples) && show_examples > 0)) {
    n_show <- if (isTRUE(show_examples)) 10L else as.integer(show_examples)
    n_show <- min(n_show, nrow(affected))
    if (n_show > 0) {
      cat("\nExamples (first ", n_show, "):\n", sep = "")
      print(
        affected[1:n_show, .(created_date = start, closed_date = end, 
                             dst_end_date, round(neg_mins, 2))],
        row.names = FALSE
      )
    }
  }
  
  invisible(fallback_summary)
}
