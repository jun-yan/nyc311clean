# ==========================================================
# Updated analyze_duration_QA (DST logic removed)
# ==========================================================
analyze_duration_QA <- function(
    DT,
    created_col      = "created_date",
    closed_col       = "closed_date", 
    key_col          = "unique_key",
    tz               = "UTC",
    lower_neg_days   = -2 * 365.25,
    extreme_neg_days = -5 * 365.25,
    upper_pos_days   = 2 * 365.25,
    extreme_pos_days = 5 * 365.25,
    max_outlier_days = 10 * 365.25,
    near_zero_secs   = threshold_numeric,
    show_examples    = 10,
    show_samples     = TRUE,        # NEW: control sample display
    sample_size      = 10,           # NEW: how many samples per category
    in_place         = TRUE,
    print_summary    = TRUE,
    extra_keep_cols  = "agency",
    genesis_date     = as.POSIXct("2003-03-09", tz = tz),
    chart_dir        = "charts",
    example_seed     = NULL
) {
  
  stopifnot(data.table::is.data.table(DT))
  
  # thresholds must be negative and ordered: extreme < lower < 0
  stopifnot(is.numeric(lower_neg_days), is.numeric(extreme_neg_days))
  stopifnot(extreme_neg_days < lower_neg_days, lower_neg_days < 0)
  
  rsample <- function(DT, n) DT[sample(.N, min(.N, n))]    #will use later
  
  if (!all(c(created_col, closed_col) %in% names(DT))) {
    stop("created_col and/or closed_col not found in DT.")
  }
  
  X <- if (in_place) DT else data.table::copy(DT)
  
  # Check if durations already exist, if not calculate them
  if (!all(c("duration_sec", "duration_days") %in% names(X))) {
    calculate_durations(
      X, created_col, closed_col, tz,
      in_place = TRUE, keep_parsed_timestamps = TRUE
    )
  } else {
    # Ensure parsed timestamps exist
    to_posix <- function(v) {
      if (inherits(v, "POSIXct")) {
        v
      } else if (requireNamespace("fasttime", quietly = TRUE)) {
        fasttime::fastPOSIXct(as.character(v), tz = tz)
      } else {
        as.POSIXct(as.character(v), tz = tz)
      }
    }
    
    if (!"created_ts" %in% names(X)) {
      X[, created_ts := to_posix(.SD[[1]]), .SDcols = created_col]
    }
    if (!"closed_ts" %in% names(X)) {
      X[, closed_ts := to_posix(.SD[[1]]), .SDcols = closed_col]
    }
  }
  
  # Denominator: rows with both timestamps present
  denom <- !is.na(X$created_ts) & !is.na(X$closed_ts)
  base_filter <- denom
  
  N_denom <- sum(denom)
  if (N_denom == 0L) warning("No rows with both timestamps; summary is empty.")
  
  neg_total <- if (N_denom) X[denom, sum(duration_sec < 0, na.rm = TRUE)] else 0L
  zero_total <- if (N_denom) X[denom, sum(duration_sec == 0, na.rm = TRUE)] else 0L
  pos_total <- if (N_denom) X[denom, sum(duration_sec > 0, na.rm = TRUE)] else 0L
  near_zero_excl_zero <- if (N_denom) X[denom, sum(duration_sec > 0 & 
                                                     duration_sec <= near_zero_secs, na.rm = TRUE)] else 0L
  
  pct <- function(x) if (N_denom) sprintf("%.2f%%", 100 * x / N_denom) else "NA"
  
  # Skew-oriented quantiles for positive durations (seconds)
  qs <- if (N_denom) {
    X[denom & duration_sec > 0,
      as.list(stats::quantile(duration_sec,
                              probs = c(0, .5, .9, .95, .99, 1),
                              na.rm = TRUE))]
  } else {
    as.list(rep(NA_real_, 6))
  }
  names(qs) <- c("min", "median", "p90", "p95", "p99", "max")
  
  qa_summary_dt <- data.table::data.table(
    metric = c(
      "denominator_rows",
      "negative_total", "zero_total", "positive_total", "near_zero_excl_zero",
      "pct_negative_total", "pct_zero", "pct_positive_total", 
      "pct_near_zero_excl_zero", "pos_sec_min", "pos_sec_median", "pos_sec_p90", 
      "pos_sec_p95", "pos_sec_p99", "pos_sec_max"
    ),
    value = c(
      formatC(N_denom, format = "d", big.mark = ","),
      formatC(neg_total, format = "d", big.mark = ","),
      formatC(zero_total, format = "d", big.mark = ","),
      formatC(pos_total, format = "d", big.mark = ","),
      formatC(near_zero_excl_zero, format = "d", big.mark = ","),
      pct(neg_total), pct(zero_total), pct(pos_total), 
      pct(near_zero_excl_zero),
      if (is.na(qs$min)) rep(NA_character_, 6) else 
        as.character(round(unlist(qs), 3))
    )
  )
  
  keep_cols <- intersect(
    c(key_col, created_col, closed_col,
      "created_ts", "closed_ts",
      "duration_sec", "duration_days",
      extra_keep_cols),
    names(X)
  )
  
  # =========
  # Subsets
  # =========
  negative_all <- X[base_filter & duration_sec < 0, .SD, .SDcols = keep_cols]
  negative_small <- X[base_filter & duration_days < 0 & duration_days > lower_neg_days, .SD, .SDcols = keep_cols]
  negative_large <- X[base_filter & duration_days < 0 & duration_days <= lower_neg_days & duration_days > extreme_neg_days, .SD, .SDcols = keep_cols]
  negative_extreme <- X[base_filter & duration_days < 0 & duration_days <= extreme_neg_days, .SD, .SDcols = keep_cols]
  
  zero_all       <- X[ base_filter & (duration_sec == 0), .SD, .SDcols = keep_cols ]
  one_sec_all    <- X[ base_filter & (duration_sec == 1), .SD, .SDcols = keep_cols ]
  near_zero_ex   <- X[ base_filter & (duration_sec != 0) & (abs(duration_sec) <= as.numeric(near_zero_secs)), .SD, .SDcols = keep_cols ]
  
  positive_all    <- X[base_filter & duration_sec > 0, .SD, .SDcols = keep_cols]
  positive_small  <- X[base_filter & duration_sec > 0 & duration_days <= upper_pos_days, .SD, .SDcols = keep_cols]
  positive_large  <- X[base_filter & duration_sec > 0 & duration_days > upper_pos_days & duration_days <= extreme_pos_days, .SD, .SDcols = keep_cols]
  positive_extreme <- X[base_filter & duration_sec > 0 & duration_days > extreme_pos_days & duration_days < max_outlier_days, .SD, .SDcols = keep_cols]
  
  excluded_extreme <- X[base_filter & duration_sec > 0 & duration_days > extreme_pos_days & duration_days >= max_outlier_days, .SD, .SDcols = keep_cols]
  if (nrow(excluded_extreme)) {
    cat(sprintf(
      "\nExcluded %s rows with duration_days ≥ %s (beyond max_outlier_days cap):\n",
      prettyNum(nrow(excluded_extreme), big.mark = ","),
      max_outlier_days
    ))
    nshow <- min(nrow(excluded_extreme), show_examples)
    tmp <- rsample(excluded_extreme, nshow)[order(-duration_days)]
    print(tmp[, c(.SD, .(duration_days = round(duration_days, 2))), .SDcols = setdiff(names(tmp), "duration_days")],
          row.names = FALSE, right = FALSE)
  }
  
  
  # Check what your actual objects are called and their column names
  ls(pattern = "negative|positive")  # See what objects exist
  colnames(negative_all)  # Check column names
  head(negative_all)      # See the data structure
  
  # Check what's actually in your data
  cat("\n=== Debugging negative_all ===\n")
  cat("Number of rows:", nrow(negative_all), "\n")
  cat("Column names:", paste(colnames(negative_all), collapse = ", "), "\n")
  cat("First few rows:\n")
  print(head(negative_all))
  
  cat("\n=== Check for NAs ===\n")
  cat("NAs in duration_hours:", sum(is.na(negative_all$duration_hours)), "\n")
  cat("NAs in duration_min:", sum(is.na(negative_all$duration_min)), "\n")
  
  cat("\n=== Non-NA values ===\n")
  cat("Non-NA duration_hours:", sum(!is.na(negative_all$duration_hours)), "\n")
  cat("Non-NA duration_min:", sum(!is.na(negative_all$duration_min)), "\n")
  
  cat("\n=== Sample non-NA values ===\n")
  # Show some actual values if they exist
  non_na_hours <- negative_all$duration_hours[!is.na(negative_all$duration_hours)]
  non_na_mins <- negative_all$duration_min[!is.na(negative_all$duration_min)]
  
  if(length(non_na_hours) > 0) {
    cat("Sample duration_hours:", head(non_na_hours, 5), "\n")
  } else {
    cat("All duration_hours are NA\n")
  }
  
  if(length(non_na_mins) > 0) {
    cat("Sample duration_min:", head(non_na_mins, 5), "\n")
  } else {
    cat("All duration_min are NA\n")
  }
  
  cat("\n\n")
  
  # Counts
  total_closed <- X[base_filter, .N]
  neg_count       <- nrow(negative_all)
  neg_small       <- nrow(negative_small)
  neg_large       <- nrow(negative_large)
  neg_extreme_cnt <- nrow(negative_extreme)
  zero_cnt        <- nrow(zero_all)
  one_sec_cnt     <- nrow(one_sec_all)
  nearz_cnt       <- nrow(near_zero_ex)
  pos_count       <- nrow(positive_all)
  pos_small       <- nrow(positive_small)
  pos_large       <- nrow(positive_large)
  pos_extreme_cnt <- nrow(positive_extreme)
  
  # Averages, mins, and maxs using the correct column names
  avg_neg_all     <- if (neg_count > 0) mean(negative_all$duration_days, na.rm = TRUE) else NA_real_
  avg_neg_small   <- if (neg_small > 0) mean(negative_small$duration_days, na.rm = TRUE) else NA_real_
  avg_neg_large   <- if (neg_large > 0) mean(negative_large$duration_days, na.rm = TRUE) else NA_real_
  avg_neg_extreme <- if (neg_extreme_cnt > 0) mean(negative_extreme$duration_days, na.rm = TRUE) else NA_real_
  
  min_neg_all     <- if (neg_count > 0) min(negative_all$duration_days, na.rm = TRUE) else NA_real_
  min_neg_small   <- if (neg_small > 0) min(negative_small$duration_days, na.rm = TRUE) else NA_real_
  min_neg_large   <- if (neg_large > 0) min(negative_large$duration_days, na.rm = TRUE) else NA_real_
  min_neg_extreme <- if (neg_extreme_cnt > 0) min(negative_extreme$duration_days, na.rm = TRUE) else NA_real_
  
  max_neg_all     <- if (neg_count > 0) max(negative_all$duration_days, na.rm = TRUE) else NA_real_
  max_neg_small   <- if (neg_small > 0) max(negative_small$duration_days, na.rm = TRUE) else NA_real_
  max_neg_large   <- if (neg_large > 0) max(negative_large$duration_days, na.rm = TRUE) else NA_real_
  max_neg_extreme <- if (neg_extreme_cnt > 0) max(negative_extreme$duration_days, na.rm = TRUE) else NA_real_
  
  avg_pos_all     <- if (pos_count > 0) mean(positive_all$duration_days, na.rm = TRUE) else NA_real_
  avg_pos_small   <- if (pos_small > 0) mean(positive_small$duration_days, na.rm = TRUE) else NA_real_
  avg_pos_large   <- if (pos_large > 0) mean(positive_large$duration_days, na.rm = TRUE) else NA_real_
  avg_pos_extreme <- if (pos_extreme_cnt > 0) mean(positive_extreme$duration_days, na.rm = TRUE) else NA_real_
  
  min_pos_all     <- if (pos_count > 0) min(positive_all$duration_days, na.rm = TRUE) else NA_real_
  min_pos_small   <- if (pos_small > 0) min(positive_small$duration_days, na.rm = TRUE) else NA_real_
  min_pos_large   <- if (pos_large > 0) min(positive_large$duration_days, na.rm = TRUE) else NA_real_
  min_pos_extreme <- if (pos_extreme_cnt > 0) min(positive_extreme$duration_days, na.rm = TRUE) else NA_real_
  
  max_pos_all     <- if (pos_count > 0) max(positive_all$duration_days, na.rm = TRUE) else NA_real_
  max_pos_small   <- if (pos_small > 0) max(positive_small$duration_days, na.rm = TRUE) else NA_real_
  max_pos_large   <- if (pos_large > 0) max(positive_large$duration_days, na.rm = TRUE) else NA_real_
  max_pos_extreme <- if (pos_extreme_cnt > 0) max(positive_extreme$duration_days, na.rm = TRUE) else NA_real_
 
  # Print negative breakdown
  cat("\nNegative duration breakdown:\n")
  cat("  Total closed:       ", total_closed, "\n")
  cat("  Negative total:     ", neg_count, "\n")
  cat("    Negative small:   ", neg_small, "\n")
  cat("    Negative large:   ", neg_large, "\n")
  cat("    Negative extreme: ", neg_extreme_cnt, "\n")
  
  if (neg_small + neg_large + neg_extreme_cnt != neg_count) {
    cat("⚠️ Warning: Breakdown counts do not add up to total negatives.\n")
  }
  
  # Define summary table
  summary_dt <- data.table::data.table(
    denominator_closed_rows = total_closed,
    negative_total       = neg_count,
    negative_small       = neg_small,
    negative_large       = neg_large,
    negative_extreme     = neg_extreme_cnt,
    pct_negative_total   = if (total_closed) round(100 * neg_count / total_closed, 2) else NA_real_,
    pct_negative_small   = if (total_closed) round(100 * neg_small / total_closed, 2) else NA_real_,
    pct_negative_large   = if (total_closed) round(100 * neg_large / total_closed, 2) else NA_real_,
    pct_negative_extreme = if (total_closed) round(100 * neg_extreme_cnt / total_closed, 2) else NA_real_,
    
    # Negative averages
    avg_neg_all     = avg_neg_all,
    avg_neg_small   = avg_neg_small,
    avg_neg_large   = avg_neg_large,
    avg_neg_extreme = avg_neg_extreme,
    
    # Negative mins
    min_neg_all     = min_neg_all,
    min_neg_small   = min_neg_small,
    min_neg_large   = min_neg_large,
    min_neg_extreme = min_neg_extreme,
    
    # Negative maxs
    max_neg_all     = max_neg_all,
    max_neg_small   = max_neg_small,
    max_neg_large   = max_neg_large,
    max_neg_extreme = max_neg_extreme,
    
    zero_total           = zero_cnt,
    near_zero_leq_secs   = as.integer(near_zero_secs),
    one_sec_total        = one_sec_cnt,
    near_zero_excl_zero  = nearz_cnt,
    
    positive_total       = pos_count,
    positive_small       = pos_small,
    positive_large       = pos_large,
    positive_extreme     = pos_extreme_cnt,
    
    pct_positive_total   = if (total_closed) round(100 * pos_count / total_closed, 2) else NA_real_,
    pct_positive_small   = if (total_closed) round(100 * pos_small / total_closed, 2) else NA_real_,
    pct_positive_large   = if (total_closed) round(100 * pos_large / total_closed, 2) else NA_real_,
    pct_positive_extreme = if (total_closed) round(100 * pos_extreme_cnt / total_closed, 2) else NA_real_,
    
    # Positive averages
    avg_pos_all     = avg_pos_all,
    avg_pos_small   = avg_pos_small,
    avg_pos_large   = avg_pos_large,
    avg_pos_extreme = avg_pos_extreme,
    
    # Positive mins
    min_pos_all     = min_pos_all,
    min_pos_small   = min_pos_small,
    min_pos_large   = min_pos_large,
    min_pos_extreme = min_pos_extreme,
    
    # Positive maxs
    max_pos_all     = max_pos_all,
    max_pos_small   = max_pos_small,
    max_pos_large   = max_pos_large,
    max_pos_extreme = max_pos_extreme,
    
    pct_zero             = if (total_closed) round(100 * zero_cnt / total_closed, 2) else NA_real_,
    pct_one_sec_total    = if (total_closed) round(100 * one_sec_cnt / total_closed, 2) else NA_real_,
    pct_near_zero_excl_zero = if (total_closed) round(100 * nearz_cnt / total_closed, 2) else NA_real_
  )
  
  # Always define this once, up top
  print_cols <- setdiff(keep_cols, c("created_ts","closed_ts"))
  
  if (isTRUE(print_summary)) {
    cat("\n\nDuration QA summary (closed rows with non-missing created):\n")
    
    vals_num <- as.numeric(unlist(as.list(summary_dt[1, ]), use.names = FALSE))
    existing <- names(summary_dt)
    
    preferred <- c(
      "denominator_closed_rows",
      "negative_total", "negative_small", "negative_large", "negative_extreme",
      "zero_total", "near_zero_leq_secs", "near_zero_excl_zero",
      "one_sec_total",
      "positive_total", "positive_small", "positive_large", "positive_extreme",
      "pct_negative_total", "pct_negative_small", "pct_negative_large", "pct_negative_extreme",
      "pct_zero", "pct_one_sec_total", "pct_near_zero_excl_zero",
      "pct_positive_total", "pct_positive_small", "pct_positive_large", "pct_positive_extreme"
    )
    
    preferred_existing <- intersect(preferred, existing)
    extras             <- setdiff(existing, preferred_existing)
    levels_all         <- c(preferred_existing, extras)
    
    summary_vert <- data.table::data.table(
      metric = factor(existing, levels = levels_all),
      value  = vals_num
    )
    data.table::setorder(summary_vert, metric)
    
    fmt_num <- function(x) ifelse(is.na(x), "—", prettyNum(x, big.mark = ",", scientific = FALSE, preserve.width = "individual"))
    fmt_pct <- function(x) ifelse(is.na(x), "—", sprintf("%.2f%%", x))
    
    summary_vert[
      grepl("^pct_", as.character(metric)), display := fmt_pct(value)
    ][
      !grepl("^pct_", as.character(metric)), display := fmt_num(value)
    ]
    
    dt_print <- summary_vert[, .(metric = as.character(metric), value = display)]
    dfp <- as.data.frame(dt_print)
    w   <- max(nchar(dfp$metric), na.rm = TRUE)
    dfp$metric <- format(dfp$metric, width = w, justify = "left")
    dfp$value  <- format(dfp$value,  justify = "right")
    print(noquote(dfp), row.names = FALSE)
  }
  
  # -------------------------------
  # Call duration category analyzers
  # -------------------------------
  analyze_duration_category(
    DT             = negative_all,
    label          = "NEGATIVE",
    chart_dir      = chart_dir,
    alt_DT         = negative_small,
    min_agency_obs = 5,
    show_examples  = show_examples,
    key_col        = key_col,
    print_cols     = print_cols,
    sort_desc = FALSE,
    show_threshold_80_flag = FALSE,   # whether to draw the 80% reference line
    show_count_labels = TRUE,       
    make_boxplot   = TRUE
  )
  
  analyze_duration_category(
    DT             = negative_large,
    label          = "LARGE NEGATIVE",
    condition_text = sprintf("(≤ %.0f days and > %.0f days)", lower_neg_days, extreme_neg_days),
    chart_dir      = chart_dir,
    threshold_days = extreme_neg_days,
    show_examples  = show_examples,
    key_col        = key_col,
    show_threshold_80_flag = FALSE,   # whether to draw the 80% reference line
    show_count_labels = TRUE,       
    print_cols     = print_cols
  )
  
  analyze_duration_category(
    DT             = negative_extreme,
    label          = "EXTREME NEGATIVE",
    condition_text = sprintf("(≤ %.0f days)", extreme_neg_days),
    chart_dir      = chart_dir,
    threshold_days = extreme_neg_days,
    show_examples  = show_examples,
    key_col        = key_col,
    show_threshold_80_flag = TRUE,   # whether to draw the 80% reference line
    show_count_labels = TRUE,  # <-- ADD THIS to show labels
    print_cols     = print_cols
  )
  
  analyze_duration_category(
    DT             = zero_all,
    label          = "ZERO",
    condition_text = "(= 0 seconds)",
    chart_dir      = chart_dir,
    show_examples  = show_examples,
    key_col        = key_col,
    print_cols     = print_cols,
    show_threshold_80_flag = TRUE,   # whether to draw the 80% reference line
    show_count_labels = TRUE,  # <-- ADD THIS to show labels
    min_agency_obs = 4
  )
  
  analyze_duration_category(
    DT             = one_sec_all,
    label          = "ONE-SECOND",
    condition_text = "(= 1 second)",
    chart_dir      = chart_dir,
    show_examples  = show_examples,
    key_col        = key_col,
    print_cols     = print_cols,
    show_threshold_80_flag = TRUE,
    show_count_labels = TRUE,  # <-- ADD THIS to show labels
    min_agency_obs = 4
  )
  
  analyze_duration_category(
    DT             = near_zero_ex,
    label          = "NEAR-ZERO",
    condition_text = sprintf("(0 < secs ≤ %d)", near_zero_secs),
    chart_dir      = chart_dir,
    show_examples  = show_examples,
    key_col        = key_col,
    show_threshold_80_flag = TRUE,   # whether to draw the 80% reference line
    show_count_labels = TRUE,       
    print_cols     = print_cols
  )
  
  # Large & Extreme Positive
  analyze_duration_category(
    DT             = positive_large,
    label          = "LARGE POSITIVE",
    condition_text = sprintf("(> %.0f and ≤ %.0f days)", upper_pos_days, extreme_pos_days),
    chart_dir      = chart_dir,
    threshold_days = extreme_pos_days,
    show_examples  = show_examples,
    key_col        = key_col,
    print_cols     = print_cols,
    make_boxplot   = TRUE,
    show_threshold_80_flag = TRUE,   # whether to draw the 80% reference line
    show_count_labels = TRUE,       
    min_agency_obs = 4,
    sort_desc      = TRUE
  )
  
  analyze_duration_category(
    DT             = positive_extreme,
    label          = "EXTREME POSITIVE",
    condition_text = sprintf("(> %.0f days)", extreme_pos_days),
    chart_dir      = chart_dir,
    threshold_days = extreme_pos_days,
    show_examples  = show_examples,
    key_col        = key_col,
    print_cols     = print_cols,
    min_agency_obs = 3,
    make_boxplot   = TRUE,
    show_count_labels = TRUE,        # <-- ADD THIS
    show_threshold_80_flag = FALSE,   # whether to draw the 80% reference line
    sort_desc      = TRUE
  )
  
  invisible(list(
    summary                 = summary_dt,
    qa_summary_table        = qa_summary_dt,
    negative_all            = negative_all,
    negative_extreme        = negative_large,
    negative_nonextreme     = negative_small,
    zero_rows               = zero_all,
    one_sec_rows            = one_sec_all,
    near_zero_rows          = near_zero_ex,
    positive_all            = positive_all,
    positive_extreme        = positive_large,
    positive_nonextreme     = positive_small,
    data                    = if (in_place) NULL else X
  ))
}
