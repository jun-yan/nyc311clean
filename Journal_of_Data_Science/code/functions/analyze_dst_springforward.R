analyze_dst_springforward <- function(
    DT,
    chart_dir,
    show_examples = 10
) {
  stopifnot(data.table::is.data.table(DT))
  
  DT_copy <- copy(DT)
  
  #  Prep: drop rows with "missing time component" stamps & NAs we can't use
  DT_copy <- DT_copy[
    !is.na(created_date) & !is.na(closed_date) &
      !(hour(created_date) == 0 & minute(created_date) == 0 & 
          second(created_date) == 0) &
      !(hour(closed_date)  == 0 & minute(closed_date)  == 0 & 
          second(closed_date)  == 0)
  ]
  
  # ---- DST windows: include next year's start so we can cap closed_date ----
  years     <- sort(unique(lubridate::year(DT_copy$created_date)))
  years_ext <- sort(unique(c(years, years + 1L)))
  
  # --- Build DST info table for all relevant years ---
  dst_info <- rbindlist(lapply(years_ext, function(y) {
    di <- get_dst_start(y, tz = "America/New_York", verbose = FALSE)
    di[, year := y]
    di
  }))
  
  # Ensure chronological order
  setorder(dst_info, year)
  
  # Add the next year's DST start for window boundaries
  dst_info[, next_dst_start := shift(dst_start, type = "lead")]
  
  # Define analysis window:
  #   For each DST year:
  #   - window begins at this year's dst_end  (03:00:00 on DST start day)
  #   - window ends   at next year's dst_start (01:59:59 before next DST)
  W <- dst_info[!is.na(next_dst_start),
                .(
                  dst_year  = year,
                  dst_date  = dst_date,      # the actual DST-change date
                  dst_start = dst_start,     # 01:59:59
                  dst_end   = dst_end,       # 03:00:00
                  win_start = dst_end,       # start of valid post-DST window
                  win_end   = next_dst_start # start of next DST event
                )]
  
  cat("\n--- DST Window Summary ---\n")
  print(W)
  
  # ---- Vectorized hit-finding (no loop): closed_date inside window, and created_date <= dst_start ----
  # Prepare SR spans
  DTx <- DT_copy[, .(created_date, closed_date, .SD), 
                 .SDcols = setdiff(names(DT_copy), 
                                   c("created_date","closed_date"))]
  DTx[, `:=`(start = created_date, end = closed_date)]
  setkey(DTx, end)               # key on end for fast non-equi
  setkey(W,   win_start, win_end)
  
  # Non-equi join: end between [win_start, win_end]
  hits <- DTx[W, on = .(end >= win_start, end <= win_end), 
                nomatch = 0L, allow.cartesian = TRUE]
  
  # Enforce the "span this DST transition" predicate: created_date <= dst_start
  affected <- hits[start <= dst_start]
  
  # If nothing, exit early
  if (!nrow(affected)) {
    cat("No affected records found!\n")
    return(invisible(NULL))
  }
  
  # Tag metadata
  affected[, `:=`(dst_year = dst_year, dst_date = as.Date(dst_date))]
  
  cat("\nTotal combined rows:", nrow(affected), "\n")
  cat("Number of rows:", nrow(affected), "\n")
  
  # ---- Per-DST-date summary (plus TOTAL) ----
  spring_summary <- affected[, .(n_rows = .N), by = .(dst_date)][order(dst_date)]
  if (nrow(spring_summary)) {
    spring_summary <- rbind(
      spring_summary,
      data.table(dst_date = as.Date(NA), n_rows = nrow(affected)),
      fill = TRUE
    )
  }
  
  if (!nrow(spring_summary)) {
    cat("\nNo DST spring-forward overlaps detected.\n")
    return(invisible(NULL))
  }
  
  cat("\nDST Spring-forward systemic +1 hour durations:\n")
  print(
    spring_summary[, .(
      dst_date = ifelse(is.na(dst_date), "TOTAL", as.character(dst_date)),
      n_rows   = format(n_rows, big.mark = ",")
    )],
    row.names = FALSE, right = FALSE
  )
  
  # ---- Chart by spring-forward date (skip TOTAL row) ----
  plot_data <- spring_summary[!is.na(dst_date)]
  if (nrow(plot_data) > 0) {
    totalN <- sum(plot_data$n_rows)
    plot_data[, count_labels := scales::comma(n_rows)]
    
    plot_barchart(
      DT           = plot_data,
      x_col        = "dst_date",
      y_col        = "n_rows",
      title        = "DST Spring-forward Overlap Cases by Date",
      subtitle     = sprintf("n = %s", scales::comma(totalN)),
      bar_width    = 60,
      show_labels  = TRUE,
      label_col    = "count_labels",
      label_size   = 3,
      label_angle  = 0,
      x_axis_angle = 30,
      y_axis_labels = scales::comma,
      text_size    = 12,
      chart_width  = 13,
      chart_height = 8.5,
      chart_dir    = chart_dir,
      filename     = "dst_springforward_overlap_by_date.pdf"
    )
    
    Sys.sleep(3)
  }
  
  # ---- Optional examples for sanity (respects show_examples) ----
  if (isTRUE(show_examples) || (is.numeric(show_examples) && show_examples > 0)) {
    n_show <- if (isTRUE(show_examples)) 10L else as.integer(show_examples)
    n_show <- min(n_show, nrow(affected))
    if (n_show > 0) {
      cat("\nExamples (first ", n_show, "):\n", sep = "")
      print(
        affected[1:n_show,
                 .(created_date, closed_date, dst_date, dst_start, dst_end)],
        row.names = FALSE
      )
    }
  }
  
  # ---- Save affected rows to CSV with timezone verification ----
  if (nrow(affected) > 0) {
    cat("\n=== Saving DST Spring-forward Affected Records ===\n")
    # cat("Timezone verification:\n")
    # cat("  created_date timezone:", attr(affected$created_date, "tzone"), "\n")
    # cat("  closed_date  timezone:", attr(affected$closed_date,  "tzone"), "\n")
    
    # Format as local time strings to avoid UTC drift on write
    affected[, created_date := format(created_date, "%Y-%m-%d %H:%M:%S", tz = "America/New_York")]
    affected[, closed_date  := format(closed_date,  "%Y-%m-%d %H:%M:%S", tz = "America/New_York")]
    
    outfile <- file.path(chart_dir, "dst_start_affected_records.csv")
    fwrite(affected, outfile)
    # cat("\nSaved", scales::comma(nrow(affected)),
    #     "DST spring-forward affected records to:", outfile, "\n")
  }
  
  invisible(spring_summary)
}
