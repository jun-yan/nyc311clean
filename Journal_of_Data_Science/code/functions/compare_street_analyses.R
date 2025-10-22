compare_street_analyses <- function(
    dt, 
    street1_col, 
    street2_col, 
    chart_dir = "charts",
    run_enhanced = FALSE) 
  {
  
  # cat("==============================================================================\n")
  # cat("COMPREHENSIVE STREET FIELD COMPARISON")
  # if (run_enhanced) cat(": RAW vs CLEANED") 
  # cat("\n")
  # cat("==============================================================================\n")
  cat(sprintf("Analyzing: %s vs %s\n", street1_col, street2_col))
  cat("==============================================================================\n")
  
  # Run raw analysis
  cat("\n>>> STEP 1: DATA MATCHING ANALYSIS <<<\n")
  raw_results <- analyze_street_matches(dt, street1_col, street2_col, chart_dir)
  
  # Initialize variables for return
  cleaned_results <- NULL
  comparison_metrics <- NULL
  
  # Run enhanced analysis only if flag is TRUE
  if (run_enhanced) {
    cat("\n==============================================================================\n")
    
    # Run cleaned analysis
    cat("\n>>> STEP 2: ENHANCED DATA ANALYSIS WITH TEXT CLEANING <<<\n")
    cleaned_results <- cross_street_analysis(dt, street1_col, street2_col, chart_dir)
    
    cat("\n==============================================================================\n")
    
    # Compare results
    cat("\n>>> STEP 3: COMPARISON SUMMARY <<<\n")
    
    # Calculate improvements
    match_improvement <- cleaned_results$total_matches - raw_results$total_matches
    match_rate_improvement <- cleaned_results$match_rate - raw_results$match_rate
    non_match_reduction <- raw_results$total_non_matches - cleaned_results$total_non_matches
    
    # Display comparison table
    cat(sprintf("\n%-35s | %12s | %12s | %12s\n", 
                "METRIC", "RAW DATA", "CLEANED", "IMPROVEMENT"))
    cat(sprintf("%s\n", paste(rep("-", 75), collapse = "")))
    
    cat(sprintf("%-35s | %12s | %12s | %12s\n",
                "Total Matches",
                scales::comma(raw_results$total_matches),
                scales::comma(cleaned_results$total_matches),
                scales::comma(match_improvement)))
    
    cat(sprintf("%-35s | %12.1f%% | %12.1f%% | %12.1f%%\n",
                "Match Rate",
                raw_results$match_rate * 100,
                cleaned_results$match_rate * 100,
                match_rate_improvement * 100))
    
    cat(sprintf("%-35s | %12s | %12s | %12s\n",
                "Total Non-Matches",
                scales::comma(raw_results$total_non_matches),
                scales::comma(cleaned_results$total_non_matches),
                scales::comma(-non_match_reduction)))
    
    # Impact assessment
    cat("\n>>> IMPACT ASSESSMENT <<<\n")
    
    if (match_improvement > 0) {
      improvement_pct <- (match_improvement / raw_results$total_records) * 100
      cat(sprintf("Data cleaning IMPROVED matching by %s records (%.2f%% of total)\n",
                  scales::comma(match_improvement), improvement_pct))
    } else {
      cat("Data cleaning did not improve matching\n")
    }
    
    # Store comparison metrics
    comparison_metrics <- list(
      match_improvement = match_improvement,
      match_rate_improvement = match_rate_improvement,
      non_match_reduction = non_match_reduction,
      improvement_percentage = (match_improvement / raw_results$total_records) * 100,
      efficiency_gain = (non_match_reduction / raw_results$total_non_matches) * 100
    )
    
  } else {
    cat("\n>>> ENHANCED ANALYSIS SKIPPED (run_enhanced = FALSE) <<<\n")
  }
  
#  cat("\n==============================================================================\n")
  
  # Return results
  invisible(list(
    raw_analysis = raw_results,
    cleaned_analysis = cleaned_results,
    comparison = comparison_metrics,
    street_columns = list(
      street1 = street1_col,
      street2 = street2_col
    ),
    enhanced_run = run_enhanced
  ))
}