#########################################################################

consolidate_agencies <- function(dataset) {

    # Define lookup tables for replacements
  agency_replacements <- c(
    "DCA" = "DCWP",
    "DEPARTMENT OF CONSUMER AND WORKER PROTECTION" = "DCWP",
    "NYC311-PRD" = "OTI",
    "DOITT" = "OTI",
    "311" = "OTI",
    "3-1-1" = "OTI"
  )
  
  agency_name_replacements <- c(
    "Consumer Complaints Division" = "DEPARTMENT OF CONSUMER AND WORKER PROTECTION"
  )
  
  # Apply replacements
  dataset$agency <- ifelse(
    dataset$agency %in% names(agency_replacements),
    agency_replacements[dataset$agency],
    dataset$agency
  )
  
  # Check if 'agency_name' exists in the dataset
  if ("agency_name" %in% names(dataset)) {
    dataset$agency <- ifelse(
      dataset$agency_name %in% names(agency_name_replacements),
      agency_name_replacements[dataset$agency_name],
      dataset$agency
    )
  }
  
  return(dataset)
}

#########################################################################