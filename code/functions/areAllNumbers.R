# #########################################################################
# Validate that number fields are all numeric
areAllNumbers <- function(numberField) {
  
  # Identify the non-blank values using nzchar()
  non_blank_indices <- which(nzchar(numberField))
  
  # Subset the vector to keep only the non-blank values
  numberField <- numberField[non_blank_indices]
  
  # remove blank and NAs
  allNumbers <-
    suppressWarnings(!is.na(as.numeric(numberField[numberField != ""])))
  
  if (!all(allNumbers)) {
    # find indices of values that are not numeric
    non_numeric_values <-
      numberField[grepl("[^[:digit:]]", numberField)]
    
    cat(
      "Non-numeric values in the vector: ",
      non_numeric_values,
      "\n"
    )
  }
  return(all(allNumbers))
}

#########################################################################