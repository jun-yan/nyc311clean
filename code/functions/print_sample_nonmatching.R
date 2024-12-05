
#########################################################################
# Take a reference field and an identified duplicate field, and print a sample
# The call the "rank_by_agency" function
print_sample_nonmatching <- function(
    dataset,
    reference_field = NULL,
    duplicate_field = NULL)
  {
#  dataset <- selected_columns
  # Remove the NAs for better display purposes
  dataset <- dataset[dataset[[duplicate_field]] != "" | !is.na(dataset[[duplicate_field]]), ]
  
  if (nrow(dataset) > 0 & !is.null(reference_field) & !is.null(duplicate_field)) {
    cat("\nSample of non-matching", reference_field, "and", duplicate_field, " (excluding blanks):\n")
    sample_threshold <- 10
    print(sample_n(dataset, min(nrow(dataset), sample_threshold)),
          row.names = FALSE, right = FALSE
    )
  }
  return(rank_by_agency(dataset))
}

#########################################################################