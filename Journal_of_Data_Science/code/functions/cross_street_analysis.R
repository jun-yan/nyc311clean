cross_street_analysis <- function(dt, street1_col, street2_col, chart_dir = "charts") {
  cat("=== ENHANCED CROSS-STREET COMPARISON ANALYSIS ===\n")
  cat(sprintf("Comparing: %s vs %s (with data cleaning)\n", street1_col, street2_col))
  
  # Total records
  total_records <- nrow(dt)
  
  # Create working copy with cleaned street names
  dt_clean <- copy(dt)
  
  # Apply data cleaning transformations - vectorized function
  clean_street_vector <- function(x) {
    # Handle NA values
    result <- ifelse(is.na(x), NA_character_, as.character(x))
    
    # Only process non-NA values
    non_na_idx <- !is.na(result)
    if (any(non_na_idx)) {
      
      # Convert to character and trim whitespace
      result[non_na_idx] <- trimws(result[non_na_idx])
      
      # Convert to uppercase for comparison
      result[non_na_idx] <- toupper(result[non_na_idx])
      
      # Remove extra spaces (multiple spaces -> single space)
      result[non_na_idx] <- gsub("\\s+", " ", result[non_na_idx])
      
      # Standardize common abbreviations
      result[non_na_idx] <- gsub("\\bSTREET\\b", "ST", result[non_na_idx])
      result[non_na_idx] <- gsub("\\bAVENUE\\b", "AVE", result[non_na_idx])
      result[non_na_idx] <- gsub("\\bBOULEVARD\\b", "BLVD", result[non_na_idx])
      result[non_na_idx] <- gsub("\\bROAD\\b", "RD", result[non_na_idx])
      result[non_na_idx] <- gsub("\\bLANE\\b", "LN", result[non_na_idx])
      result[non_na_idx] <- gsub("\\bDRIVE\\b", "DR", result[non_na_idx])
      result[non_na_idx] <- gsub("\\bPLACE\\b", "PL", result[non_na_idx])
      result[non_na_idx] <- gsub("\\bCOURT\\b", "CT", result[non_na_idx])
      result[non_na_idx] <- gsub("\\bPARKWAY\\b", "PKWY", result[non_na_idx])
      result[non_na_idx] <- gsub("\\bHIGHWAY\\b", "HWY", result[non_na_idx])
      result[non_na_idx] <- gsub("\\bCIRCLE\\b", "CIR", result[non_na_idx])
      result[non_na_idx] <- gsub("\\bTERRACE\\b", "TER", result[non_na_idx])
      
      # Standardize directionals
      result[non_na_idx] <- gsub("\\bNORTH\\b", "N", result[non_na_idx])
      result[non_na_idx] <- gsub("\\bSOUTH\\b", "S", result[non_na_idx])
      result[non_na_idx] <- gsub("\\bEAST\\b", "E", result[non_na_idx])
      result[non_na_idx] <- gsub("\\bWEST\\b", "W", result[non_na_idx])
      
      # Remove periods and common punctuation
      result[non_na_idx] <- gsub("\\.", "", result[non_na_idx])
      result[non_na_idx] <- gsub(",", "", result[non_na_idx])
      
      # Final trim
      result[non_na_idx] <- trimws(result[non_na_idx])
      
      # Set empty strings to NA
      result[non_na_idx & (result == "" | result == "NA")] <- NA_character_
    }
    
    return(result)
  }
  
  # Apply cleaning to both columns
  dt_clean[, paste0(street1_col, "_clean") := clean_street_vector(get(street1_col))]
  dt_clean[, paste0(street2_col, "_clean") := clean_street_vector(get(street2_col))]
  
  # Get cleaned column names
  street1_clean_col <- paste0(street1_col, "_clean")
  street2_clean_col <- paste0(street2_col, "_clean")
  
  # Calculate matches using cleaned data
  both_non_blank_match <- dt_clean[
    !is.na(get(street1_clean_col)) & !is.na(get(street2_clean_col)) & 
      get(street1_clean_col) == get(street2_clean_col), 
    .N
  ]
  
  # Levenshtein distance matches (distance ≤ 2)
  levenshtein_match <- dt_clean[
    !is.na(get(street1_clean_col)) & !is.na(get(street2_clean_col)),
    {
      d <- stringdist::stringdist(get(street1_clean_col), get(street2_clean_col), method = "lv")
      .(lev_match = sum(d <= 2, na.rm = TRUE), lev_nonmatch = sum(d > 2, na.rm = TRUE))
    }
  ]
  
  lev_match_count <- levenshtein_match$lev_match
  lev_match_pct   <- (lev_match_count / total_records) * 100
  
  
  both_blank_match <- dt_clean[
    is.na(get(street1_clean_col)) & is.na(get(street2_clean_col)), 
    .N
  ]
  
  total_matches <- both_non_blank_match + both_blank_match
  
  # Calculate non-matches breakdown
  non_matches_both_non_blank <- dt_clean[
    !is.na(get(street1_clean_col)) & !is.na(get(street2_clean_col)) & 
      get(street1_clean_col) != get(street2_clean_col), 
    .N
  ]
  
  non_matches_street1_blank <- dt_clean[
    is.na(get(street1_clean_col)) & !is.na(get(street2_clean_col)), 
    .N
  ]
  
  non_matches_street2_blank <- dt_clean[
    !is.na(get(street1_clean_col)) & is.na(get(street2_clean_col)), 
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
  cat("\nCLEANED STREET FIELD MATCHES:\n")
  cat(sprintf("Total matches:              %s (%5.1f%%)\n", 
              scales::comma(total_matches), total_match_pct))
  cat(sprintf("  Both fields non-blank:    %s (%5.1f%%)\n", 
              scales::comma(both_non_blank_match), both_non_blank_pct))
  cat(sprintf("  Both fields blank:        %s (%5.1f%%)\n", 
              scales::comma(both_blank_match), both_blank_pct))
  
  cat("\nCLEANED STREET FIELD NON-MATCHES:\n")
  cat(sprintf("Total non-matches:          %s (%5.1f%%)\n", 
              scales::comma(total_non_matches), total_non_match_pct))
  cat(sprintf("  Non-matches both non-blank: %s (%5.1f%%)\n", 
              scales::comma(non_matches_both_non_blank), non_match_both_non_blank_pct))
  cat(sprintf("  Non-matches %s blank:  %s (%5.1f%%)\n", 
              street1_col, scales::comma(non_matches_street1_blank), non_match_street1_blank_pct))
  cat(sprintf("  Non-matches %s blank:  %s (%5.1f%%)\n", 
              street2_col, scales::comma(non_matches_street2_blank), non_match_street2_blank_pct))
  cat(sprintf("  Levenshtein distance ≤ 2: %s (%5.1f%%)\n", 
              scales::comma(lev_match_count), lev_match_pct))
  
  # Show examples of cleaning improvements
  cat("\nDATA CLEANING EXAMPLES:\n")
  examples <- dt_clean[
    !is.na(get(street1_col)) & !is.na(get(street2_col)) &
      get(street1_col) != get(street2_col) &
      !is.na(get(street1_clean_col)) & !is.na(get(street2_clean_col)) &
      get(street1_clean_col) == get(street2_clean_col)
  ][1:5, c(street1_col, street2_col, street1_clean_col, street2_clean_col), with = FALSE]
  
  if (nrow(examples) > 0) {
    cat("Sample records where cleaning resolved mismatches:\n")
    print(examples)
  }
  
  # Create Pareto chart for remaining non-matches by agency
  if (total_non_matches > 0) {
    cat("\nGenerating Pareto chart for remaining street field non-matches by agency...\n")
    
    # Get all remaining non-matching records after cleaning
    non_match_records <- dt_clean[
      (is.na(get(street1_clean_col)) & !is.na(get(street2_clean_col))) | 
        (!is.na(get(street1_clean_col)) & is.na(get(street2_clean_col))) |
        (!is.na(get(street1_clean_col)) & !is.na(get(street2_clean_col)) & 
           get(street1_clean_col) != get(street2_clean_col))
    ]
    
    filename <- sprintf("pareto_cleaned_street_non_matches_%s_vs_%s.pdf", 
                        gsub("[^A-Za-z0-9]", "_", street1_col),
                        gsub("[^A-Za-z0-9]", "_", street2_col))
    
    plot_pareto_combo(
      DT = non_match_records,
      x_col = agency,
      chart_dir = chart_dir,
      filename = filename,
      title = sprintf("Agencies with Remaining Street Non-Matches After Cleaning (%s vs %s)", 
                      street1_col, street2_col),
      top_n = 30,
      include_na = FALSE
    )
  }
  
  # Return summary including cleaned data
  invisible(list(
    total_records = total_records,
    total_matches = total_matches,
    both_non_blank_match = both_non_blank_match,
    both_blank_match = both_blank_match,
    total_non_matches = total_non_matches,
    non_matches_both_non_blank = non_matches_both_non_blank,
    non_matches_street1_blank = non_matches_street1_blank,
    non_matches_street2_blank = non_matches_street2_blank,
    match_rate = total_match_pct / 100,
    cleaned_data = dt_clean
  ))
}