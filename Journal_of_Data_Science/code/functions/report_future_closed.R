report_future_closed <- function(
    DT,
    max_closed_date = NULL,
    tz              = "UTC",
    sample_n        = 5L,
    make_plots      = TRUE,
    chart_dir       = "charts",
    boxplot_file    = "future_closed_boxplot.pdf",
    pareto_file     = "future_closed_pareto.pdf"
) {
  stopifnot(requireNamespace("data.table", quietly = TRUE))
  if (!data.table::is.data.table(DT)) data.table::setDT(DT)
  
  # --- Column checks ---
  needed <- c("unique_key", "created_date", "closed_date", "duration_sec", "agency")
  missing_cols <- setdiff(needed, names(DT))
  if (length(missing_cols)) {
    stop("DT is missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # --- Ensure POSIXct ---
  to_posix <- function(x) {
    if (inherits(x, "POSIXt")) return(x)
    if (inherits(x, "Date"))   return(as.POSIXct(x, tz = tz))
    if (is.character(x))       return(as.POSIXct(x, tz = tz))
    stop("Datetime columns must be POSIXct/Date/character. Got: ", class(x)[1])
  }
  DT[, created_date := to_posix(created_date)]
  DT[,  closed_date := to_posix(closed_date)]
  
  # --- Cutoff default: max(created_date) + 1 day ---
  if (is.null(max_closed_date)) {
    max_created <- DT[, suppressWarnings(max(created_date, na.rm = TRUE))]
    if (is.infinite(max_created)) stop("created_date is entirely NA; cannot set default max_closed_date")
    max_closed_date <- max_created + 86400
  }
  stopifnot(inherits(max_closed_date, "POSIXt"))
  
  # --- Core filter ---
  cols_keep <- c("unique_key", "created_date", "closed_date", "duration_sec", "agency")
  closed_in_future <- DT[
    !is.na(closed_date) & closed_date > max_closed_date,
    ..cols_keep
  ][
    , future_days := round(as.numeric(closed_date - max_closed_date, units = "days"), 4)
  ]
  
  n_future         <- closed_in_future[, .N]
  n_nonblank_close <- DT[!is.na(closed_date), .N]
  pct_of_nonblank  <- if (n_nonblank_close > 0) 100 * n_future / n_nonblank_close else NA_real_
  
  # --- Console report ---
  fmt_int <- function(x) format(x, big.mark = ",", trim = TRUE)
  fmt_pct <- function(x, digits = 4) if (is.na(x)) "NA" else paste0(round(x, digits), "%")
  
  cat("\n— Closed_date-in-the-Future Check —\n")
  cat("Cutoff (max_closed_date): ", format(max_closed_date, "%Y-%m-%d %H:%M:%S"), "\n", sep = "")
  cat("Non-blank closed_date rows: ", fmt_int(n_nonblank_close), "\n", sep = "")
  cat("Closed in future: ", fmt_int(n_future), " (", fmt_pct(pct_of_nonblank), " of non-blank)\n", sep = "")
  
  sample_rows <- data.table::setDT(NULL)
  agency_summary <- data.table::setDT(NULL)
  files <- list(boxplot = NA_character_, pareto = NA_character_)
  
  if (n_future > 0) {
    # Sample
    sample_rows <- closed_in_future[
      sample.int(.N, min(.N, as.integer(sample_n)))
    ][
      , .(unique_key, agency, created_date, closed_date, duration_sec, future_days)
    ]
    
    cat("\nSample of SRs with 'closed_date' in the future:\n")
    print(sample_rows, row.names = FALSE, right = FALSE)
    
    # By-agency counts (for Pareto & console)
    agency_summary <- closed_in_future[
      , .(count = .N), by = agency
    ][order(-count)][
      , `:=`(
        pct = round(count / sum(count), 2),
        cum_pct = round(cumsum(count) / sum(count),2),
        rank = .I
      )
    ]
    
    cat("\nBy-agency counts (top 10):\n")
    print(agency_summary[1:min(.N, 10)], row.names = FALSE, right = FALSE)
    
    # --- Plots (N > 0 only) ---
    if (make_plots) {
      if (!dir.exists(chart_dir)) dir.create(chart_dir, recursive = TRUE, showWarnings = FALSE)
      
      # 1) Boxplot of future_days by agency (new helper signature)
      if (exists("plot_boxplot", mode = "function")) {
        # Check if there's more than one row in the dataset
        if (nrow(closed_in_future) > 1) {
          # Opinionated defaults for this diagnostic:
          # - top_n = 20 to avoid unreadable axes
          # - flip = TRUE (horizontal) for long labels
          tryCatch({
            plot_boxplot(
              DT           = closed_in_future,
              value_col    = future_days,                 # NSE
              chart_dir    = chart_dir,
              filename     = boxplot_file,
              title        = "SRs Closed in the Future (days beyond cutoff)",
              by_col       = agency,                      # NSE
              include_na_group = FALSE,
              top_n        = 20L,
              order_by     = "count",
              flip         = TRUE,
              zero_line    = TRUE,
              x_scale_type = "linear",
              min_count    = 1L
            )
            files$boxplot <- file.path(chart_dir, boxplot_file)
            #      cat("\nBoxplot saved to: ", files$boxplot, "\n", sep = "")
          }, error = function(e) {
            cat("\nNOTE: plot_boxplot() errored: ", conditionMessage(e), "\n", sep = "")
          })
        } else {
          cat("\nNOTE: Only", nrow(closed_in_future), "row(s) found; skipping boxplot.\n")
        }
      } else {
        cat("\nNOTE: plot_boxplot() not found; skipping boxplot.\n")
      }
      
      # 2) Pareto combo (agency counts)
      if (exists("plot_pareto_combo", mode = "function")) {
        # Check if there's more than one row AND more than one agency
        unique_agencies <- length(unique(closed_in_future$agency))
        if (nrow(closed_in_future) > 1 && unique_agencies > 1) {
          tryCatch({
            plot_pareto_combo(
              DT       = closed_in_future,
              x_col    = agency,
              title    = "SRs Closed in the Future (by agency)",
              chart_dir = chart_dir,
              filename = pareto_file
            )
            files$pareto <- file.path(chart_dir, pareto_file)
          }, error = function(e) {
            cat("\nNOTE: plot_pareto_combo() errored: ", conditionMessage(e), "\n", sep = "")
          })
        } else {
          if (unique_agencies == 1) {
            cat("\nNOTE: Only 1 unique agency found; skipping Pareto chart.\n")
          } else {
            cat("\nNOTE: Only", nrow(closed_in_future), "row(s) found; skipping Pareto chart.\n")
          }
        }
      } else {
        cat("\nNOTE: plot_pareto_combo() not found; skipping Pareto chart.\n")
      }
    }
  }
  
  invisible(list(
    rows           = closed_in_future,
    agency_summary = agency_summary,
    sample         = sample_rows,
    stats          = list(
      cutoff              = max_closed_date,
      n_future            = n_future,
      n_nonblank_closed   = n_nonblank_close,
      pct_of_nonblank     = pct_of_nonblank
    ),
    files          = files
  ))
}