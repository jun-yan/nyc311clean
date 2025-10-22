analyze_street_matches <- function(
    dt, 
    street1_col, 
    street2_col, 
    chart_dir = "charts")
  {
#  cat("=== STREET FIELD COMPARISON ANALYSIS ===\n")
#  cat(sprintf("Comparing: %s vs %s\n", street1_col, street2_col))
  
  # Total records
  total_records <- nrow(dt)
  
  # Calculate matches
  both_non_blank_match <- dt[
    !is.na(get(street1_col)) & !is.na(get(street2_col)) & 
      get(street1_col) == get(street2_col), 
    .N
  ]
  
  both_blank_match <- dt[
    is.na(get(street1_col)) & is.na(get(street2_col)), 
    .N
  ]
  
  total_matches <- both_non_blank_match + both_blank_match
  
  # Calculate non-matches breakdown
  non_matches_both_non_blank <- dt[
    !is.na(get(street1_col)) & !is.na(get(street2_col)) & 
      get(street1_col) != get(street2_col), 
    .N
  ]
  
  non_matches_street1_blank <- dt[
    is.na(get(street1_col)) & !is.na(get(street2_col)), 
    .N
  ]
  
  non_matches_street2_blank <- dt[
    !is.na(get(street1_col)) & is.na(get(street2_col)), 
    .N
  ]
  
  total_non_matches <- non_matches_both_non_blank + non_matches_street1_blank + non_matches_street2_blank
  
  # Calculate percentages
  total_match_pct <- (total_matches / total_records) * 100
  both_non_blank_pct <- (both_non_blank_match / total_records) * 100
  both_blank_pct <- (both_blank_match / total_records) * 100
  total_non_match_pct <- (total_non_matches / total_records) * 100
  non_match_both_non_blank_pct <- (non_matches_both_non_blank / total_records) * 100
  non_match_street1_blank_pct <- (non_matches_street1_blank / total_records) * 100
  non_match_street2_blank_pct <- (non_matches_street2_blank / total_records) * 100
  
  # Report results
  cat(sprintf("\nTotal records: %s\n", scales::comma(total_records)))
  cat("\nSTREET FIELD MATCHES:\n")
  cat(sprintf("Total matches:              %s (%5.1f%%)\n", 
              scales::comma(total_matches), total_match_pct))
  cat(sprintf("  Both fields non-blank:    %s (%5.1f%%)\n", 
              scales::comma(both_non_blank_match), both_non_blank_pct))
  cat(sprintf("  Both fields blank:        %s (%5.1f%%)\n", 
              scales::comma(both_blank_match), both_blank_pct))
  
  cat("\nSTREET FIELD NON-MATCHES:\n")
  cat(sprintf("Total non-matches:          %s (%5.1f%%)\n", 
              scales::comma(total_non_matches), total_non_match_pct))
  cat(sprintf("  Non-matches both non-blank: %s (%5.1f%%)\n", 
              scales::comma(non_matches_both_non_blank), non_match_both_non_blank_pct))
  cat(sprintf("  Non-matches %s blank:  %s (%5.1f%%)\n", 
              street1_col, scales::comma(non_matches_street1_blank), non_match_street1_blank_pct))
  cat(sprintf("  Non-matches %s blank:  %s (%5.1f%%)\n", 
              street2_col, scales::comma(non_matches_street2_blank), non_match_street2_blank_pct))
  
  # Create Pareto chart for non-matches by agency
  if (total_non_matches > 0) {
    cat("\nGenerating Pareto chart for street field non-matches by agency...\n")
    
    # Get all non-matching records
    non_match_records <- dt[
      (is.na(get(street1_col)) & !is.na(get(street2_col))) |
        (!is.na(get(street1_col)) & is.na(get(street2_col))) |
        (!is.na(get(street1_col)) & !is.na(get(street2_col)) & get(street1_col) != get(street2_col))
    ]
    
    filename <- sprintf("pareto_street_field_non_matches_%s_vs_%s.pdf", 
                        gsub("[^A-Za-z0-9]", "_", street1_col),
                        gsub("[^A-Za-z0-9]", "_", street2_col))
    
    plot_pareto_combo(
      DT = non_match_records,
      x_col = agency,
      chart_dir = chart_dir,
      filename = filename,
      title = sprintf("Agencies with Street Field Non-Matches (%s vs %s)", street1_col, street2_col),
      top_n = 30,
      include_na = FALSE
    )
    Sys.sleep(3)
  }
  
  # Return summary for further use
  invisible(list(
    total_records = total_records,
    total_matches = total_matches,
    both_non_blank_match = both_non_blank_match,
    both_blank_match = both_blank_match,
    total_non_matches = total_non_matches,
    non_matches_both_non_blank = non_matches_both_non_blank,
    non_matches_street1_blank = non_matches_street1_blank,
    non_matches_street2_blank = non_matches_street2_blank,
    match_rate = total_match_pct / 100
  ))
}