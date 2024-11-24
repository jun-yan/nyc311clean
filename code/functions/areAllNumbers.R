
# #########################################################################
# Validate that number fields are all numeric
areAllNumbers <- function(numberField) {
  
  # Remove blank and NA values
  non_blank <- numberField[!is.na(numberField) & nzchar(numberField)]
  
  # Check if all remaining values are numeric
  allNumbers <- suppressWarnings(!is.na(as.numeric(non_blank)))
  
  if (!all(allNumbers)) {
    # Extract the non-numeric values
    non_numeric_values <- non_blank[!allNumbers]
    
    cat("Non-numeric values in the vector:\n")
    print(unique(non_numeric_values)) # Use unique to avoid redundant entries
  }
  return(all(allNumbers))
}

#########################################################################