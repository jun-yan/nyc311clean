make_column_names_user_friendly<- function(dataset) {
  
  names(dataset) <- gsub(
    x = names(dataset),
    pattern = "(\\.|\\s)+",
    replacement = "_"
  )
  
  ## Drop the trailing "."s
  names(dataset) <- gsub(
    x = names(dataset),
    pattern = "(_)+$",
    replacement = ""
  )
  
  ## Convert to lower case.
  names(dataset) <- tolower(names(dataset))
  
  ## Return the revised column names
  return(dataset)
}
