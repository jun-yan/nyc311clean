#########################################################################
detect_duplicates <- function(
    dataset,
    reference_field,
    duplicate_field) {
  
  #Identify matching fields including blanks and NA values
  rows_condition_0 <- dataset[[reference_field]] == dataset[[duplicate_field]]
  
  rows_condition_1 <- dataset[[reference_field]] != dataset[[duplicate_field]]
  
  all_non_matching_rows <- rows_condition_1
  non_matching_fields <- dataset[all_non_matching_rows, ]
  num_non_matching_fields <- nrow(non_matching_fields)
  non_matching_percentage <- round((num_non_matching_fields / num_rows_d311) * 100, 2)
  non_matching_fields <- non_matching_fields %>%
    select("unique_key", all_of(reference_field), all_of(duplicate_field), "agency")
  
  all_matching_rows <- rows_condition_0
  matching_fields <- dataset[all_matching_rows, ]
  num_matching_fields <- nrow(matching_fields)
  matching_percentage <- round((num_matching_fields / num_rows_d311) * 100, 2)
  matching_fields <- matching_fields %>%
    select("unique_key", all_of(reference_field), all_of(duplicate_field), "agency")
  
  if (num_non_matching_fields > 0) {
    
    if (num_matching_fields > 0) {
      cat(
        "\n\nThere are",
        format(num_matching_fields, big.mark = ","),
        "matches between '", reference_field, "' and '", duplicate_field, "'\nrepresenting",
        matching_percentage, "% of data.\n")
    }
    cat(
      "\nThere are",
      format(num_non_matching_fields, big.mark = ","),
      "non-matches between '", reference_field, "' and '", duplicate_field, "'\nrepresenting",
      non_matching_percentage, "% of data.\n")
    
    return(non_matching_fields)
  }
  else {
    cat("\nAll values match between '", reference_field, "' and '", duplicate_field, "'\n")
  }
}

#########################################################################