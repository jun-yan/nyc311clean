#' Report SRs where due_date is BEFORE created_date (negative days)
report_due_before_created <- function(
    DT,
    tz              = "UTC",
    sample_n        = 10L,
    make_plots      = TRUE,
    chart_dir       = "charts",
    boxplot_file    = "due_before_created_boxplot.pdf",
    pareto_file     = "due_before_created_pareto.pdf"
) {
  stopifnot(requireNamespace("data.table", quietly = TRUE))
  if (!data.table::is.data.table(DT)) data.table::setDT(DT)
  
  # --- Column checks ---
  needed <- c("unique_key", "created_date", "due_date", "agency")
  missing_cols <- setdiff(needed, names(DT))
  if (length(missing_cols)) {
    stop("DT is missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # --- Ensure POSIXct for the datetime columns (light coercion) ---
  to_posix <- function(x) {
    if (inherits(x, "POSIXt")) return(x)
    if (inherits(x, "Date"))   return(as.POSIXct(x, tz = tz))
    if (is.character(x))       return(as.POSIXct(x, tz = tz))
    stop("Datetime columns must be POSIXct/Date/character. Got: ", class(x)[1])
  }
  DT[, created_date := to_posix(created_date)]
  DT[,    due_date := to_posix(due_date)]
  
  # --- Denominator: rows with non-missing due_date ---
  n_due_nonblank <- DT[!is.na(due_date), .N]
  
  # --- CHANGED: Compute difference so "due BEFORE create" is NEGATIVE ---
  #     due_duration = (due_date - created_date) in days  -> negative means due earlier than create
  cols_keep <- c("unique_key", "created_date", "due_date", "agency")
  due_checked <- DT[
    !is.na(due_date),
    ..cols_keep
  ][
    , due_duration := round(as.numeric(difftime(due_date, created_date, units = "days")), 2)  # CHANGED
  ]
  
  # --- CHANGED: select rows where due_date is BEFORE created_date (negative values) ---
  bad_due <- due_checked[due_duration < 0]  # CHANGED
  n_bad   <- bad_due[, .N]
  pct_bad <- if (n_due_nonblank > 0) 100 * n_bad / n_due_nonblank else NA_real_
  
  # --- Console report ---
  fmt_int <- function(x) format(x, big.mark = ",", trim = TRUE)
  fmt_pct <- function(x, digits = 2) if (is.na(x)) "NA" else paste0(round(x, digits), "%")
  
  cat("\n— Due_date-before-Created_date Check (negative days) —\n")
  cat("Non-blank due_date rows: ", fmt_int(n_due_nonblank), "\n", sep = "")
  cat("Bad due dates (due < created): ", fmt_int(n_bad), " (", fmt_pct(pct_bad), " of non-blank)\n", sep = "")
  
  sample_rows <- data.table::setDT(NULL)
  agency_summary <- data.table::setDT(NULL)
  files <- list(boxplot = NA_character_, pareto = NA_character_)
  
  if (n_bad > 0) {
    stats <- bad_due[, .(
      min    = min(due_duration, na.rm = TRUE),
      max    = max(due_duration, na.rm = TRUE),
      mean   = mean(due_duration, na.rm = TRUE),
      median = median(due_duration, na.rm = TRUE),
      sd     = sd(due_duration, na.rm = TRUE)
    )]
    
    cat("\nSummary of 'due_duration' (days) for SRs with due_date before created_date:\n")
    print(stats, row.names = FALSE, right = FALSE)
    
    # Sample
    sample_rows <- bad_due[
      sample.int(.N, min(.N, as.integer(sample_n)))
    ][
      , .(unique_key, agency, created_date, due_date, due_duration)  # due_duration is negative
    ]
    
    cat("\nSample of SRs with 'due_date' before 'created_date' (negative days):\n")
    print(sample_rows, row.names = FALSE, right = FALSE)
    
    # By-agency counts (for Pareto & console)
    agency_summary <- bad_due[
      , .(count = .N), by = agency
    ][order(-count)][
      , `:=`(
        pct = count / sum(count),
        cum_pct = cumsum(count) / sum(count),
        rank = .I
      )
    ]
    
    cat("\nBy-agency counts (top 10):\n")
    print(agency_summary[1:min(.N, 10)], row.names = FALSE, right = FALSE)
    
    # --- Plots (N > 0 only) ---
    if (make_plots) {
      if (!dir.exists(chart_dir)) dir.create(chart_dir, recursive = TRUE, showWarnings = FALSE)
      
      # 1) Boxplot of negative days by agency (uses your plot_boxplot)
      
      if (exists("plot_boxplot", mode = "function")) {
        
        # Check if there's more than one row in the dataset
        if (nrow(bad_due) > 1) {
          tryCatch({
            plot_boxplot(
              DT               = bad_due,
              value_col        = due_duration,          # negative values
              chart_dir        = chart_dir,
              filename         = boxplot_file,
              title            = "Due Date Before Created Date (negative days)",
              by_col           = agency,
              include_na_group = FALSE,
              top_n            = 30L,
              order_by         = "count",
              flip             = TRUE,
              zero_line        = TRUE,
              x_scale_type     = "linear",
              min_count        = 5L
            )
            Sys.sleep(3)
            files$boxplot <- file.path(chart_dir, boxplot_file)
            #      cat("\nBoxplot saved to: ", files$boxplot, "\n", sep = "")
          }, error = function(e) {
            cat("\nNOTE: plot_boxplot() errored: ", conditionMessage(e), "\n", sep = "")
          })
        } else {
          cat("\nNOTE: Only", nrow(bad_due), "row(s) found; skipping boxplot.\n")
        }
      } else {
        cat("\nNOTE: plot_boxplot() not found; skipping boxplot.\n")
      }
      
      # 2) Pareto combo (agency counts)
      if (exists("plot_pareto_combo", mode = "function")) {
        
        # Check if there's more than one row AND more than one agency
        unique_agencies <- length(unique(bad_due$agency))
        
        if (nrow(bad_due) > 1 && unique_agencies > 1) {
          tryCatch({
            plot_pareto_combo(
              DT           = bad_due,
              x_col        = agency,
              title        = "Due Date Before Created Date (by agency)",
              chart_dir    = chart_dir,
              filename     = pareto_file
            )
            files$pareto <- file.path(chart_dir, pareto_file)
          }, error = function(e) {
            cat("\nNOTE: plot_pareto_combo() errored: ", conditionMessage(e), "\n", sep = "")
          })
        } else {
          if (unique_agencies == 1) {
            cat("\nNOTE: Only 1 unique agency found; skipping Pareto chart.\n")
          } else {
            cat("\nNOTE: Only", nrow(bad_due), "row(s) found; skipping Pareto chart.\n")
          }
        }
      } else {
        cat("\nNOTE: plot_pareto_combo() not found; skipping Pareto chart.\n")
      }
    }
  }
  
  invisible(list(
    rows           = bad_due,          # the violating rows (negative days)
    agency_summary = agency_summary,
    sample         = sample_rows,
    stats          = list(
      n_due_nonblank = n_due_nonblank,
      n_bad          = n_bad,
      pct_bad        = pct_bad
    ),
    files          = files
  ))
}