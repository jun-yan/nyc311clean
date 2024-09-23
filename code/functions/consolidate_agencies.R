consolidate_agencies <-function(dataset)
{
  # Replace "DCA" with "DCWP" in the agency column
  dataset$agency[dataset$agency == "DCA"] <- "DCWP"
  
  # Replace "DCA" with "DCWP" in the agency and agency_name columns
  dataset$agency[dataset$agency == "DEPARTMENT OF CONSUMER AND WORKER PROTECTION"] <- "DCWP"
  
  dataset$agency[dataset$agency_name == "Consumer Complaints Division"] <-
    "DEPARTMENT OF CONSUMER AND WORKER PROTECTION"
  
  # Replace "NYC311-PRD" with "OTI" in the agency column
  dataset$agency[dataset$agency == "NYC311-PRD"] <- "OTI"
  
  # Replace "DOITT" with "OTI" in the agency column
  dataset$agency[dataset$agency == "DOITT"] <- "OTI"
  
  return(dataset)
}