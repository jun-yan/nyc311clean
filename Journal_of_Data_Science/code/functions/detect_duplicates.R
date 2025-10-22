detect_duplicates <- function(
    dataset,
    reference_field,
    duplicate_field,
    # text normalization
    trim_ws            = TRUE,
    case_insensitive   = TRUE,
    blank_is_na        = TRUE,
    # equality & % denominator
    na_equal           = TRUE,                     # NA==NA counts as a match
    percent_base       = c("all_rows","both_non_missing"),
    # numeric tolerance (optional)
    tol                = NULL,
    # output shaping
    select_cols        = c("unique_key","agency"),
    print_report       = TRUE,
    sample_n           = 5L,
    # Pareto for non-matches (using your tidy-eval signature)
    make_pareto        = TRUE,
    pareto_group_col   = "agency",                # unquoted inside plot function
    chart_dir          = "charts/duplicates",
    pareto_file        = NULL,
    # pass-throughs to plot_pareto_combo()
    pareto_top_n       = 30L,
    pareto_include_na  = FALSE,
    pareto_na_label    = "(NA)",
    pareto_width_in    = 13,
    pareto_height_in   = 8.5,
    pareto_annotation_size = 3.5,
    pareto_subtitle_size   = 11,
    pareto_min_count   = 1L,
    pareto_flip        = FALSE                    # vertical by default
) {
  percent_base <- match.arg(percent_base)
  if (!data.table::is.data.table(dataset)) data.table::setDT(dataset)
  
  if (!reference_field %in% names(dataset)) stop("Column not found: ", reference_field)
  if (!duplicate_field %in% names(dataset))  stop("Column not found: ", duplicate_field)
  
  # pull columns (names with spaces are OK)
  x <- dataset[[reference_field]]
  y <- dataset[[duplicate_field]]
  
  # normalize strings
  normalize_chr <- function(z) {
    if (!is.character(z) && !is.factor(z)) return(z)
    z <- as.character(z)
    if (trim_ws) z <- trimws(z)
    if (case_insensitive) z <- tolower(z)
    if (blank_is_na) z[z == ""] <- NA_character_
    z
  }
  x <- normalize_chr(x); y <- normalize_chr(y)
  
  # base equality
  eq <- (x == y)
  
  # optional numeric tolerance
  if (!is.null(tol)) {
    x_num <- suppressWarnings(as.numeric(x))
    y_num <- suppressWarnings(as.numeric(y))
    both_num <- is.finite(x_num) & is.finite(y_num)
    eq_num <- both_num & (abs(x_num - y_num) <= tol)
    eq[both_num] <- eq_num[both_num]
  }
  
  # NA policy
  if (isTRUE(na_equal)) {
    both_na <- is.na(x) & is.na(y)
    eq[both_na] <- TRUE
    eq[is.na(eq)] <- FALSE
  } else {
    eq[is.na(eq)] <- FALSE
  }
  
  # denominator choice
  n_all   <- nrow(dataset)
  both_ok <- !(is.na(x) | is.na(y))
  denom   <- if (percent_base == "both_non_missing") sum(both_ok) else n_all
  if (denom == 0) denom <- 1
  
  # shape outputs
  cols_to_show <- unique(c(select_cols, reference_field, duplicate_field))
  cols_to_show <- intersect(cols_to_show, names(dataset))
  
  matching_dt    <- dataset[eq, ..cols_to_show]
  nonmatching_dt <- dataset[!eq, ..cols_to_show]
  
  n_match <- nrow(matching_dt)
  n_non   <- nrow(nonmatching_dt)
  
  pct_match <- round(100 * n_match / denom, 4)   # <-- % duplication
  pct_non   <- round(100 * n_non   / denom, 4)
  
  if (isTRUE(print_report)) {
    cat("\n— Duplicate Check —\n")
    
    cat(sprintf("  %-15s %s\n", "Reference:",  reference_field))
    cat(sprintf("  %-15s %s\n", "Duplicate:",  duplicate_field))
    cat(sprintf("  %-15s %s\n", "Rows:",       format(n_all, big.mark=",")))
    cat(sprintf("  %-15s %s\n", "Denominator:", percent_base))
    cat(sprintf("  %-15s %.2f%%\n", "Duplication:", pct_match))
    cat(sprintf("  %-15s %.2f%%\n", "Non-matches:", pct_non))
    
    # Redundancy declaration
    if (pct_match >= 99) {
      cat(sprintf("\n  → REDUNDANT: %s is %.2f%% duplicate of %s\n", 
                  duplicate_field, pct_match, reference_field))
    } else if (pct_match >= 95) {
      cat(sprintf("\n  → MOSTLY REDUNDANT: %s is %.2f%% duplicate of %s\n", 
                  duplicate_field, pct_match, reference_field))
    } else if (pct_match >= 75) {
      cat(sprintf("\n  → PARTIALLY REDUNDANT: %s is %.2f%% duplicate of %s\n", 
                  duplicate_field, pct_match, reference_field))
    } else {
      cat(sprintf("\n  → NOT REDUNDANT: %s contains different information (%.2f%% duplication)\n", 
                  duplicate_field, pct_match))
    }
    
    if (n_non > 0) {
      cat("\nSample non-matches:\n")
      print(nonmatching_dt[1:min(.N, as.integer(sample_n))], row.names = FALSE, right = FALSE)
    } else {
      cat("\nNo non-matches.\n")
    }
  }
  
  # Pareto using your exact signature (x_col unquoted)
  if (isTRUE(make_pareto) && n_non > 0) {
    if (!dir.exists(chart_dir)) dir.create(chart_dir, recursive = TRUE, showWarnings = FALSE)
    if (!(pareto_group_col %in% names(nonmatching_dt))) {
      cat(sprintf("NOTE: Group column '%s' not found; skipping Pareto chart.\n", pareto_group_col))
    } else if (!exists("plot_pareto_combo", mode = "function")) {
      cat("NOTE: plot_pareto_combo() not found; skipping Pareto chart.\n")
    } else {
      safe_dup_name <- gsub("[^A-Za-z0-9_]+", "_", duplicate_field)
      if (is.null(pareto_file)) {
        pareto_file <- sprintf("pareto_nonmatches_%s_by_%s.pdf", safe_dup_name, pareto_group_col)
      }
      # Build a call where x_col is passed as an unquoted symbol
      call_args <- list(
        DT              = nonmatching_dt,
        x_col           = as.name(pareto_group_col),
        chart_dir       = chart_dir,
        filename        = pareto_file,
        title           = sprintf("Non-matches: %s vs %s (by %s)", reference_field, duplicate_field, pareto_group_col),
        top_n           = pareto_top_n,
        include_na      = pareto_include_na,
        na_label        = pareto_na_label,
        width_in        = pareto_width_in,
        height_in       = pareto_height_in,
        annotation_size = pareto_annotation_size,
        plot_subtitle_size = pareto_subtitle_size,
        min_count       = pareto_min_count,
        flip            = pareto_flip
      )
      # Safe execution
      tryCatch({
        do.call(plot_pareto_combo, call_args)
#        cat("Pareto saved to: ", file.path(chart_dir, pareto_file), "\n", sep = "")
      }, error = function(e) {
        cat("NOTE: plot_pareto_combo() errored: ", conditionMessage(e), "\n", sep = "")
      })
    }
  }
  
  invisible(list(
    matching       = matching_dt,
    non_matching   = nonmatching_dt,
    stats          = list(
      rows_total     = n_all,
      denominator    = percent_base,
      n_matching     = n_match,
      n_non_matching = n_non,
      pct_matching   = pct_match,   # <-- % duplication
      pct_nonmatch   = pct_non
    )
  ))
}
