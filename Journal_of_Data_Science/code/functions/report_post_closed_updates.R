#' Report post-closed resolution updates that occur long after close
#' - Computes postClosedUpdateDuration (days) for positive updates (update > closed)
#' - Summarizes counts and basic stats
#' - Optionally renders a boxplot (duration by agency) and a Pareto chart (counts by agency)
#'
#' @param DT       data.table with: unique_key, agency, closed_date, resolution_action_updated_date
#' @param tz       Time zone used if coercing character/Date -> POSIXct (default "UTC")
#' @param resolution_action_threshold numeric days; if NULL uses get0("resolution_action_threshold", 30)
#' @param too_large_threshold         numeric days; if NULL uses get0("too_large_threshold", 730)
#' @param sample_n  number of random rows to print as an example
#' @param make_plots logical; if TRUE, produce charts when N > 0
#' @param chart_dir directory where charts are written (created if needed)
#' @param boxplot_file filename for the boxplot PDF
#' @param pareto_file  filename for the Pareto PDF
#'
#' @return (invisibly) list(
#'   raw_rows_positive, updated_late, updated_extremely_late, agency_summary, sample, stats, files
#' )
report_post_closed_updates <- function(
    DT,
    tz                           = "UTC",
    resolution_action_threshold  = 30L,
    too_large_threshold          = 365 * 6, # 6 yrs
    sample_n                     = 10L,
    make_plots                   = TRUE,
    chart_dir                    = "charts",
    boxplot_file                 = "post_closed_updates_boxplot.pdf",
    pareto_file                  = "post_closed_updates_pareto.pdf"
) {
  stopifnot(requireNamespace("data.table", quietly = TRUE))
  if (!data.table::is.data.table(DT)) data.table::setDT(DT)
  
  # --- Required columns ---
  req <- c("unique_key","agency","closed_date","resolution_action_updated_date")
  miss <- setdiff(req, names(DT))
  if (length(miss)) stop("Missing columns in DT: ", paste(miss, collapse = ", "))
  
  # # --- Light coercion to POSIXct if needed (safer than hard-stopping) ---
  # to_posix <- function(x) {
  #   if (inherits(x, "POSIXt")) return(x)
  #   if (inherits(x, "Date"))   return(as.POSIXct(x, tz = tz))
  #   if (is.character(x))       return(as.POSIXct(x, tz = tz))
  #   stop("Datetime columns must be POSIXct/Date/character. Got: ", class(x)[1])
  # }
  # DT[, closed_date := to_posix(closed_date)]
  # DT[, resolution_action_updated_date := to_posix(resolution_action_updated_date)]
  
  # --- Thresholds ---
  if (is.null(resolution_action_threshold)) {
    resolution_action_threshold <- get0("resolution_action_threshold", ifnotfound = 30)
  }
  if (is.null(too_large_threshold)) {
    too_large_threshold <- get0("too_large_threshold", ifnotfound = 730)
  }
  
  # --- Compute positive durations in days (else NA) ---
  DT[, postClosedUpdateDuration := data.table::fifelse(
    !is.na(resolution_action_updated_date) & !is.na(closed_date) &
      (resolution_action_updated_date > closed_date),
    as.numeric(difftime(resolution_action_updated_date, closed_date, units = "days")),
    NA_real_
  )]
  
  # --- Base subsets ---
  raw_rows_positive <- DT[postClosedUpdateDuration > 0,
                      .(unique_key, agency, closed_date,
                        resolution_action_updated_date, postClosedUpdateDuration)]
  
  cat("\n=== Raw Positive Post-Closed Update Duration Summary ===\n")
  cat(sprintf("    Total post-closed updates: %s (%.2f%%)", 
              prettyNum(nrow(raw_rows_positive), big.mark = ","),
              100 * nrow(raw_rows_positive) / nrow(DT)))
  cat("\n\n")
  print(summary(raw_rows_positive$postClosedUpdateDuration)) 
  
  # # Exclude extreme outliers > too_large_threshold
  # rows_reasonable <- raw_rows_positive[postClosedUpdateDuration
  #                                  <= too_large_threshold]
  # 
  # cat("\n\n=== Reasonable Positive Post-Closed Update Duration Summary ===\n")
  # cat(sprintf("    Total post-closed updates: %s (%.2f%%)", 
  #             prettyNum(nrow(rows_reasonable), big.mark = ","),
  #             100 * nrow(rows_reasonable) / nrow(DT)))
  # cat("\n\n")
  # print(summary(rows_reasonable$postClosedUpdateDuration)) 
  
  updated_late <- raw_rows_positive[
    postClosedUpdateDuration >  resolution_action_threshold &
      postClosedUpdateDuration <= 
      too_large_threshold ][order(-postClosedUpdateDuration)]
  
  cat("\n\n=== Updated-Late Post-Closed Update Duration Summary ===\n")
  cat(sprintf("    Total row updated-late: %s (%.2f%%)", 
              prettyNum(nrow(updated_late), big.mark = ","),
              100 * nrow(updated_late) / nrow(DT)))
  cat("\n\n")
  print(summary(updated_late$postClosedUpdateDuration)) 
  
  
  
  # Check the actual data range
  # More explicit console output
  cat("Summary of postClosedUpdateDuration:\n")
  print(summary(updated_late$postClosedUpdateDuration))
  
  cat("\nRange (min, max):\n")
  print(range(updated_late$postClosedUpdateDuration, na.rm = TRUE))
  
  cat("\nNumber of negative values:\n")
  print(sum(updated_late$postClosedUpdateDuration < 0, na.rm = TRUE))
  
  cat("\nNegative values (if any):\n")
  print(updated_late[postClosedUpdateDuration < 0, .(unique_key, postClosedUpdateDuration)])
  
  
  
  
  updated_extremely_late <- raw_rows_positive[
    postClosedUpdateDuration > too_large_threshold
  ][order(-postClosedUpdateDuration)]
  
  cat("\n\n=== Updated-Extremely-Late Post-Closed Update Duration Summary ===\n")
  cat(sprintf("    Total row updated-EXTRENMELY-late: %s (%.2f%%)", 
              prettyNum(nrow(updated_extremely_late), big.mark = ","),
              100 * nrow(updated_extremely_late) / nrow(DT)))
  cat("\n\n")
  print(summary(updated_extremely_late$postClosedUpdateDuration))
  cat("\n\n")
  
  # With custom parameters
  plot_histogram(
    DT = updated_late,
    value_col = "postClosedUpdateDuration",
    title = "Updated Late (>30 days) After Closed_Date",
    x_label = "Days After Closure",
    filename = "post_closed_updates_histogram",
    bins = 100,
    outlier_percentile = 0.99,  # trim top 1% of outliers
    chart_dir = chart_dir,
    add_stats = TRUE
  )
  
    # --- Counts & stats ---
  n_positive <- raw_rows_positive[, .N]
  n_late     <- updated_late[, .N]
  n_extreme  <- updated_extremely_late[, .N]
  
  fmt_int <- function(x) format(x, big.mark = ",", trim = TRUE)
  cat("\nPost-closed updates summary:\n")
  cat(sprintf("  %-25s %s\n", "Positive updates:", fmt_int(n_positive)))
  cat(sprintf("  %-25s %s\n", paste0("Late (>", resolution_action_threshold, 
                                     " & <=", too_large_threshold, "):"), fmt_int(n_late)))
  cat(sprintf("  %-25s %s\n", paste0("Extreme (>=", too_large_threshold, "):"), 
              fmt_int(n_extreme)))
  
  if (n_late > 0) {
    # --- existing stats (as you already have) ---
    late_vec  <- updated_late$postClosedUpdateDuration
    pos_vec   <- raw_rows_positive$postClosedUpdateDuration
    
    late_med  <- stats::median(late_vec, na.rm = TRUE)
    late_mean <- base::mean(late_vec,  na.rm = TRUE)
    late_sd   <- stats::sd(late_vec,   na.rm = TRUE)
    late_max  <- base::max(late_vec,   na.rm = TRUE)
    
    pos_med   <- stats::median(pos_vec, na.rm = TRUE)
    pos_mean  <- base::mean(pos_vec,    na.rm = TRUE)
    pos_max   <- base::max(pos_vec,     na.rm = TRUE)
    
    # Late updates table
    cat(sprintf("\nLate updates (> %d days) statistics:\n", resolution_action_threshold))
    cat(sprintf("  %-10s %.2f\n", "Median:",  late_med))
    cat(sprintf("  %-10s %.2f\n", "Mean:",    late_mean))
    cat(sprintf("  %-10s %.2f\n", "Std Dev:", late_sd))
    cat(sprintf("  %-10s %.2f\n", "Max:",     late_max))
    
    # All positive updates table  
    cat("\nAll positive post-closed updates statistics:\n")
    cat(sprintf("  %-15s %.2f days\n", "Median:", pos_med))
    cat(sprintf("  %-15s %.2f days\n", "Mean:",   pos_mean))
    cat(sprintf("  %-15s %.2f days (%.2f hours)\n", "Max:", pos_max, pos_max * 24))
    cat(sprintf("  %-15s %.2f hours\n", "Mean (hours):", pos_mean * 24))
    cat(sprintf("  %-15s %.2f hours\n", "Max (hours):",  pos_max * 24))
    
    # --- NEW: print full data for the max rows ---
    # Late bucket (threshold exceeders)
    late_rows_max <- updated_late[postClosedUpdateDuration == late_max]
    cat(sprintf("\nMax late update row(s) (>%d & <=%d days): %.2f days (%.2f years)\n",
                resolution_action_threshold, too_large_threshold, late_max, late_max / 365.25))
    print(late_rows_max[, c(.SD, .(postClosedUpdateDuration = round(postClosedUpdateDuration, 2))),
                        .SDcols = setdiff(names(late_rows_max), "postClosedUpdateDuration")], 
          nrows = Inf, trunc.cols = FALSE, row.names = FALSE)
    
    # All positive rows
    pos_rows_max <- raw_rows_positive[postClosedUpdateDuration == pos_max]
    cat(sprintf("\nExtreme max of all row(s) (>%d days): %.2f days (%.2f years)\n",
                too_large_threshold, pos_max, pos_max / 365.25))
    print(pos_rows_max[, c(.SD, .(postClosedUpdateDuration = round(postClosedUpdateDuration, 2))),
                       .SDcols = setdiff(names(pos_rows_max), "postClosedUpdateDuration")], 
          nrows = Inf, trunc.cols = FALSE, row.names = FALSE)
    
  } else {
    cat(sprintf(
      "No SRs exceed the %d-day threshold for post-closed resolution updates.\n",
      resolution_action_threshold
    ))
  }
  
  # --- Sample & agency summary (for Pareto) ---
  sample_rows <- data.table::setDT(NULL)
  agency_summary <- data.table::setDT(NULL)
  if (n_late > 0) {
    sample_rows <- updated_late[
      sample.int(.N, min(.N, as.integer(sample_n)))
    ]
    cat("\nSample of late post-closed updates:\n")
    print(sample_rows[, .(agency, closed_date, resolution_action_updated_date, 
                          postClosedUpdateDuration = round(postClosedUpdateDuration, 2))],
          row.names = FALSE, right = FALSE)
    
    agency_summary <- updated_late[, .(count = .N), by = agency
    ][order(-count)
    ][, `:=`(pct = round(count / sum(count), 2),
             cum_pct = round(cumsum(count) / sum(count), 0),
             rank = .I)]
    cat("\nBy-agency counts (top 10):\n")
    print(agency_summary[1:min(.N, 10)], row.names = FALSE, right = FALSE)
  }
  
  # --- Sample & agency summary for extremely late updates ---
  sample_rows_extreme <- data.table::setDT(NULL)
  agency_summary_extreme <- data.table::setDT(NULL)
  n_extremely_late <- nrow(updated_extremely_late)
  
  if (n_extremely_late > 0) {
    sample_rows_extreme <- updated_extremely_late[
      sample.int(.N, min(.N, as.integer(sample_n)))
    ]
    cat("\nSample of extremely late post-closed updates:\n")
    print(sample_rows_extreme[, .(agency, closed_date, resolution_action_updated_date, 
                                  postClosedUpdateDuration = round(postClosedUpdateDuration, 2))],
          row.names = FALSE, right = FALSE)
    
    agency_summary_extreme <- updated_extremely_late[, .(count = .N), by = agency
    ][order(-count)
    ][, `:=`(pct = round(count / sum(count), 2),
             cum_pct = round(cumsum(count) / sum(count), 2),
             rank = .I)]
    cat("\nBy-agency counts for extremely late updates (top 10):\n")
    print(agency_summary_extreme[1:min(.N, 10)], row.names = FALSE, right = FALSE)
  }
  
  # --- Optional charts ---
  files <- list(boxplot = NA_character_, pareto = NA_character_)
  if (make_plots && n_late > 0) {
    if (!dir.exists(chart_dir)) dir.create(chart_dir, recursive = TRUE, showWarnings = FALSE)
    
    # 1) Boxplot of durations by agency (new helper signature from you)
    if (exists("plot_boxplot", mode = "function")) {
#      tryCatch({
        plot_boxplot(
          DT               = updated_late,
          value_col        = postClosedUpdateDuration,     # NSE numeric
          chart_dir        = chart_dir,
          filename         = boxplot_file,
          title            = sprintf("Post-Closed Updates > %d days (by agency)", resolution_action_threshold),
          by_col           = agency,                        # NSE grouping
          include_na_group = FALSE,
          top_n            = 30L,
          order_by         = "count",
          plot_title_size    = 14,
          flip             = TRUE,
          zero_line        = TRUE,
          x_scale_type     = "linear",
          y_axis_side = "left", 
          y_axis_label_size = 12,
          count_label_hjust = 1,
          min_count        = 5L
        )
        files$boxplot <- file.path(chart_dir, boxplot_file)
        
        # Add violin + boxplot hybrid
        
        # Before calling plot_violin_boxplot, assign to a simple name
        violin_data <- updated_late
        
        # Compute min and max
        min_val <- min(violin_data$postClosedUpdateDuration, na.rm = TRUE)
        max_val <- max(violin_data$postClosedUpdateDuration, na.rm = TRUE)
        
        # Adjust by 10% based on sign
        lower_limit <- ifelse(min_val >= 0, min_val * 0.9, min_val * 1.1)
        upper_limit <- ifelse(max_val >= 0, max_val * 1.1, max_val * 0.9)
        
        plot_violin_boxplot(
          DT               = violin_data,
          value_col        = postClosedUpdateDuration,
          chart_dir        = chart_dir,
          filename         = gsub("boxplot", "violin_boxplot", boxplot_file),
          title            = sprintf("Post-Closed Updates > %d days (by agency) - Violin + Box", resolution_action_threshold),
          by_col           = agency,
          include_na_group = FALSE,
          top_n            = 30L,
          order_by         = "count",
          flip             = TRUE,
          plot_type        = "hybrid",
          violin_alpha     = 0.3,
          violin_trim      = FALSE,
          zero_line        = TRUE,
          x_scale_type     = "linear",
          y_axis_side      = "left",
          x_limits = c(lower_limit, upper_limit),
          y_axis_label_size = 12,
          y_axis_tick_size = 13,
          x_axis_tick_size = 15,
          plot_title_size  = 14,
          min_count        = 5L
        )

        
        #        cat("\nBoxplot saved to: ", files$boxplot, "\n", sep = "")
    #   }, error = function(e) {
    #     cat("\nNOTE: plot_boxplot() errored: ", conditionMessage(e), "\n", sep = "")
    #   })
    # } else {
    #   cat("\nNOTE: plot_boxplot() not found; skipping boxplot.\n")
 }
    
    # 2) Pareto of counts by agency (your tidy-eval version: x_col = agency)
    if (exists("plot_pareto_combo", mode = "function")) {
      tryCatch({
        plot_pareto_combo(
          DT        = updated_late,
          x_col     = agency,  # tidy-eval grouping column
          title     = sprintf(
            "Post-Closed Resolution Updates > %d days by Agency & cumulative %%", 
            resolution_action_threshold
          ),
          filename  = pareto_file,
          chart_dir = chart_dir
        )
        files$pareto <- file.path(chart_dir, pareto_file)
#        cat("Pareto saved to: ", files$pareto, "\n", sep = "")
      }, error = function(e) {
        #        cat("\nNOTE: plot_pareto_combo() errored: ", conditionMessage(e), "\n", sep = "")
      })
    } else {
      cat("NOTE: plot_pareto_combo() not found; skipping Pareto chart.\n")
    }
  }
  
  invisible(list(
    raw_rows_positive   = raw_rows_positive,
    updated_late    = updated_late,
    updated_extremely_late = updated_extremely_late,
    agency_summary  = agency_summary,
    sample          = sample_rows,
    stats = list(
      n_positive = n_positive,
      n_late     = n_late,
      n_extreme  = n_extreme,
      threshold_days = resolution_action_threshold,
      extreme_days   = too_large_threshold
    ),
    files = files
  ))
}