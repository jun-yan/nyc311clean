summarize_backlog <- function(DT, tz = NULL, start_year = 2020, 
                              data_dir = NULL, 
                              prior_year_file = "311_Service_Requests_for_2019.csv") {
  stopifnot(data.table::is.data.table(DT))
  if (is.null(tz)) tz <- (attr(DT$created_date, "tzone") %||% "America/New_York")
  
  yrs <- sort(unique(lubridate::year(DT$created_date)))
  yrs <- yrs[yrs >= start_year]
  
  res <- data.table::rbindlist(lapply(yrs, function(y) {
    start <- as.POSIXct(sprintf("%d-01-01 00:00:00", y), tz = tz)
    
    # backlog_in criteria:
    #  1. created before start of year
    #  2. closed_date not NA
    #  3. status != "CLOSED"
    backlog_in <- if (y == start_year && !is.null(data_dir)) {
      # Special handling for start_year: read prior year data
      prior_year_path <- file.path(data_dir, prior_year_file)
      
      if (!file.exists(prior_year_path)) {
        warning(sprintf("Prior year file not found: %s\nUsing 0 for backlog.", 
                        prior_year_path))
        0L
      } else {
        cat(sprintf("Reading prior year data from: %s\n", prior_year_path))
        
        # Read with raw column names (spaces and title case)
        DT_prior <- data.table::fread(
          prior_year_path,
          select = c("Created Date", "Closed Date", "Status"),
          showProgress = TRUE
        )
        
        # Rename to match processed format
        data.table::setnames(DT_prior, 
                             old = c("Created Date", "Closed Date", "Status"),
                             new = c("created_date", "closed_date", "status"))
        
        # Parse character dates to POSIXct
        # NYC 311 raw format is typically "MM/DD/YYYY HH:MM:SS AM/PM"
        fmt_ts <- "%m/%d/%Y %I:%M:%S %p"
        
        DT_prior[, created_date := as.POSIXct(created_date, format = fmt_ts, tz = tz)]
        DT_prior[, closed_date := as.POSIXct(closed_date, format = fmt_ts, tz = tz)]
        
        # Calculate backlog: items created before 2020 that weren't closed
        backlog_count <- DT_prior[
          created_date < start &
            !is.na(closed_date) &
            toupper(status) != "CLOSED",
          .N
        ]
        
        cat(sprintf("Backlog from %d: %s\n", y - 1, 
                    format(backlog_count, big.mark = ",")))
        
        backlog_count
      }
      
    } else if (y == start_year) {
      # No data_dir provided
      0L
    } else {
      # Use main dataset for subsequent years
      DT[
        created_date < start &
          !is.na(closed_date) &
          toupper(status) != "CLOSED",
        .N
      ]
    }
    
    data.table::data.table(
      year       = y,
      backlog_in = backlog_in
    )
  }))
  
  # cumulative backlog
  res[, backlog_cum := cumsum(backlog_in)]
  
  # pretty print
  cat("\n[Backlog by year]\n")
  print(res[, .(
    year,
    backlog_in  = format(backlog_in, big.mark = ","),
    backlog_cum = format(backlog_cum, big.mark = ",")
  )], row.names = FALSE)
  
  # Create plots
  plot_barchart(
    DT = res,
    x_col = "year",
    y_col = "backlog_in",
    title = "Annual Backlog Entering Each Year",
    subtitle = sprintf("Total backlog entering across all years: %s", 
                       format(sum(res$backlog_in), big.mark = ",")),
    x_label = "Year",
    y_label = "Backlog In",
    show_labels = TRUE,
    add_trendline = TRUE,
    console_print_title = "Annual Backlog Distribution"
  )
  
  # plot_barchart(
  #   DT = res,
  #   x_col = "year",
  #   y_col = "backlog_cum",
  #   title = "Cumulative Backlog by Year",
  #   subtitle = sprintf("Total cumulative backlog: %s", 
  #                      format(max(res$backlog_cum), big.mark = ",")),
  #   x_label = "Year",
  #   y_label = "Cumulative Backlog",
  #   show_labels = TRUE,
  #   console_print_title = "Cumulative Backlog",
  #   add_trendline = TRUE
  # )
  
  invisible(res)
}