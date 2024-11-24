
##########################################################################
cross_street_analysis <- function(
    dataset,
    cross_street,
    intersection_street,
    chart_directory) {
  
  num_rows_dataset <- nrow(dataset)
  
  # Remove extra blanks
  dataset <- dataset %>%
    mutate(
      across(
        all_of(cross_street),
        ~ str_replace_all(., "\\s+", " ")
      ),
      across(
        all_of(intersection_street),
        ~ str_replace_all(., "\\s+", " ")
      )
    )
  
  # Find rows where the two fields are equal (cross_street1 & intersection_street)
  equal_streets <- subset(dataset,
                          dataset[[cross_street]] == dataset[[intersection_street]],
                          select = c(cross_street, intersection_street, "agency")
  )
  
  num_rows_equal <- nrow(equal_streets)
  
  # Find rows where the two fields are equal and non-blank (cross_street1 & intersection_street)
  equal_streets_non_blank <- subset(
    equal_streets,
    !is.na(equal_streets[[cross_street]]) &
      !is.na(equal_streets[[intersection_street]]) &
      
      equal_streets[[cross_street]] != "" &
      equal_streets[[intersection_street]] != ""
  )
  
  num_rows_equal_non_blank <- nrow(equal_streets_non_blank)
  
  # find rows where both cross_street and intersection_street are not blank. Use later.
  both_xsteet_non_blank <-
    subset(dataset, dataset[[cross_street]] != "" &
             dataset[[intersection_street]] != "",
           select = c(cross_street, intersection_street, "agency")
    )
  
  num_rows_both_non_blank <- nrow(both_xsteet_non_blank)
  
  # find rows where cross_street is blank, but intersection_street is not blank. Use later.
  cross_street_blank <- subset(dataset, dataset[[cross_street]] == "" &
                                 dataset[[intersection_street]] != "",
                               select = c(cross_street, intersection_street, "agency")
  )
  
  num_rows_cross_street_blank <- nrow(cross_street_blank)
  
  # find rows where intersection_street is blank, but cross_street is not blank
  intersection_street_blank <- subset(dataset, dataset[[intersection_street]] == "" &
                                        dataset[[cross_street]] != "",
                                      select = c(cross_street, intersection_street, "agency")
  )
  
  num_rows_intersection_street_blank <- nrow(intersection_street_blank)
  
  # find rows where both cross_street and intersection_street are blank
  both_blank <- subset(dataset, dataset[[cross_street]] == "" &
                         dataset[[intersection_street]] == "",
                       select = c(cross_street, intersection_street, "agency")
  )
  
  num_rows_both_blank <- nrow(both_blank)
  
  ####################
  # Check for non-matching fields between cross_street and intersection_street
  non_dup_streets <- detect_duplicates(
    dataset,
    cross_street,
    intersection_street
  )
  num_rows_non_dup <- nrow(non_dup_streets)
  
  non_dup_streets_non_blank <- non_dup_streets[non_dup_streets[[cross_street]] != "" &
                                                 non_dup_streets[[intersection_street]] != "", ]
  
  # Define the relative path to go up two directories and then into "data"
  relative_path <- file.path("..", "..", "data")
  
  # Ensure the directory exists (optional)
  if (!dir.exists(relative_path)) {
    dir.create(relative_path, recursive = TRUE)
  }
  
  # Select only the cross_street and intersection_street columns
  subset_data <- non_dup_streets_non_blank[, c(cross_street, intersection_street)]
  
  # Construct the filename with dynamic column names and place it in the relative path
  filename <- file.path(relative_path, paste0("non_dup_streets_non_blank_", cross_street, "_", intersection_street, ".csv"))
  
  # Save the subset dataframe to disk at the specified relative path
  write.csv(subset_data, file = filename, row.names = FALSE)
  
  cat("Data saved to:", filename, "\n")  # Optional printout for confirmation
  
  num_rows_non_dup_non_blank <- nrow(non_dup_streets_non_blank)
  
  cat("\nSample of matching",cross_street, "&", intersection_street, ":\n")
  random_sample_equal <- equal_streets_non_blank %>% sample_n(min(num_rows_equal_non_blank, 10)) # random sample
  print(random_sample_equal, row.names = FALSE, right = FALSE)
  
  cat("\nSample of non-matching", cross_street, "&", intersection_street, ":\n")
  random_sample_non_dup <- non_dup_streets_non_blank %>% sample_n(min(num_rows_non_dup_non_blank, 10)) # random sample
  print(random_sample_non_dup, row.names = FALSE, right = FALSE)
  
  non_dup_streets <- non_dup_streets[, -which(names(non_dup_streets) == "unique_key")]
  
  if (num_rows_non_dup > 0) {
    agency_match_non_dup <- rank_by_agency(non_dup_streets)
    
    chart_title <- paste0("Non-Matching '", cross_street, "' and '", intersection_street, "' by Agency & cumulative percentage", sep = "")
    chart_file_name <- paste0("non-matching_", cross_street, "and", intersection_street, ".pdf", sep = "")
    create_combo_chart(
      non_dup_streets,
      chart_title,
      chart_file_name,
      chart_directory,
      console_print_out_title = paste0("Summary of non-Matching '", cross_street, "' and '", intersection_street, "'by Agency")
    )
  }
  
  ####################
  # cross_street is blank, but intersection_street is not blank
  ####################
  
  cat(
    "\nThere are", format(num_rows_cross_street_blank, big.mark = ","),
    "occurrences where'", cross_street, "'is blank, \nbut '", intersection_street, "' is not blank representing",
    round((num_rows_cross_street_blank / num_rows_dataset * 100), 2), "% of total rows."
  )
  
  cat(
    "\n\nSample where '", cross_street, "'is blank but '", intersection_street, "'is not blank:\n"
  )
  random_sample <- cross_street_blank %>% sample_n(min(num_rows_cross_street_blank, 5)) # random sample
  print(random_sample, row.names = FALSE, right = FALSE)
  agency_cross_street_blank <- rank_by_agency(cross_street_blank)
  
  
  ####################
  # intersection_street is blank, but cross_street is not blank
  
  cat(
    "\nThere are", format(num_rows_intersection_street_blank, big.mark = ","),
    "occurrences where'", intersection_street, "'is blank, \nbut '", cross_street, "' is not blank representing",
    round(num_rows_intersection_street_blank / num_rows_dataset * 100, 2),
    "% of total rows."
  )
  
  cat(
    "\n\nSample where '", cross_street, "' is not blank but '", intersection_street, "' is blank:\n"
  )
  random_sample <- intersection_street_blank %>% sample_n(min(num_rows_intersection_street_blank, 5)) # random sample
  print(random_sample, row.names = FALSE, right = FALSE)
  agency_intersection_street_blank <- rank_by_agency(intersection_street_blank)
  
  ####################
  # Check for almost matches, using a Hamming distance of 2 characters different
  almost_match <-
    subset(
      non_dup_streets,
      non_dup_streets[[cross_street]] != "" &
        non_dup_streets[[intersection_street]] != "" &
        nchar(non_dup_streets[[cross_street]]) == nchar(non_dup_streets[[intersection_street]])
    )
  
  threshold <- 2
  
  cat(
    "\n\nA near-match is when two addresses have no more than",
    threshold, "characters different (Hamming Distance).\n"
  )
  
  # Calculate Hamming distance for each row
  almost_match$hamming_distance <- mapply(
    hamming_distance,
    almost_match[[cross_street]],
    almost_match[[intersection_street]]
  )
  
  # Identify rows where the Hamming distance is less than the threshold
  matches_meeting_threshold <- subset(almost_match, hamming_distance <= threshold)
  num_rows_matches_meeting_threshold <- nrow(matches_meeting_threshold)
  
  cat(
    "\nThere are ", num_rows_matches_meeting_threshold, "near-matches between",
    cross_street, "and", intersection_street
  )
  if (num_rows_matches_meeting_threshold > 0) {
    cat("\n\nSample of near-matching '", cross_street, "' & '", intersection_street, "(both non-blank):\n")
    random_sample_almost_match <- matches_meeting_threshold %>%
      sample_n(min(num_rows_matches_meeting_threshold, 10)) # random sample
    print(random_sample_almost_match, row.names = FALSE, right = FALSE)
  }
  
  ####################
  # summary of cross_street and intersection_street columns
  summary_street <-
    data.frame(
      category = c(
        "Matching -- non-blank",
        "Matching -- both blank",
        "Non-matching",
        paste(cross_street, "blank", sep = "_"),
        paste(intersection_street, "blank", sep = "_"),
        "Near-match"
      ),
      count = c(
        num_rows_equal - num_rows_both_blank,
        num_rows_both_blank,
        num_rows_non_dup,
        num_rows_cross_street_blank,
        num_rows_intersection_street_blank,
        num_rows_matches_meeting_threshold
      )
    )
  
  summary_street$percentage[1] <- round(((num_rows_equal - num_rows_both_blank)/num_rows_dataset) * 100, 2)
  summary_street$percentage[2] <- round((num_rows_both_blank / num_rows_dataset) * 100, 2)
  summary_street$percentage[3] <- round((num_rows_non_dup / num_rows_dataset) * 100, 2)
  summary_street$percentage[4] <- "N/A"
  summary_street$percentage[5] <- "N/A"
  summary_street$percentage[6] <- round((num_rows_matches_meeting_threshold / num_rows_dataset) * 100, 6)
  
  if (num_rows_matches_meeting_threshold == 0) {
    summary_street$percentage[6] <- "N/A"
  }
  
  cat("\nSummary of'", cross_street, "' and '", intersection_street, "':\n")
  names(summary_street)[1:3] <- c("category", "count", "percentage")
  print(summary_street, row.names = FALSE, right = FALSE)
  return(intersection_street_blank)
}

#########################################################################