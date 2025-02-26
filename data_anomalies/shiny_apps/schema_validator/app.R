##############################################################################################

########## SCHEMA VALIDATOR ##########

##############################################################################################
library(shiny)
library(shinycssloaders)
library(DT)
library(data.table)
library(stringr)
library(bslib)

##############################################################################################
# Configuration items
# Specify the port (choose a unique port number)
options(shiny.port = 4003)
options(digits.secs = 3)  # Ensure subsecond precision is maintained

##############################################################################################
# Load and prepare data
data_file <- file.path("data", "dataset.RDS")

if (file.exists(data_file)) {
  cleaned_data <- tryCatch({
    readRDS(data_file)  # Correct function for reading an RDS file
  }, error = function(e) {
    stop("Error loading dataset: ", e$message)
  })
  
  message("Dataset successfully loaded from local RDS file!")
} else {
  stop("Data file not found in the 'data' directory.")
}

setDT(cleaned_data)  # Convert to data.table for better performance

##############################################################################################
# # Process date columns
# date_cols <- c("created_date", "closed_date", "due_date", "resolution_action_updated_date")
# for(col in date_cols) {
#   if(col %in% names(cleaned_data)) {
#     cleaned_data[, (col) := as.POSIXct(get(col), origin="1970-01-01", tz="UTC")]
#   }
# }

##############################################################################################
schema <- list(
  
  unique_key = list(
    type = "integer",
    allow_empty = FALSE,
    unique = TRUE,
    description = "Unique identifier for each service request"
  ),
  created_date = list(
    type = "POSIXct",
    allow_empty = FALSE,
    description = "Timestamp when the service request was created"
  ),
  closed_date = list(
    type = "POSIXct",
    allow_empty = TRUE,
    description = "Timestamp when the service request was closed (can be empty for ongoing requests)"
  ),
  agency = list(
    type = "character",
    allow_empty = FALSE,
    description = "Agency code or abbreviation"
  ),
  agency_name = list(
    type = "character",
    allow_empty = FALSE,
    valid_values = c(
      "NEW YORK CITY POLICE DEPARTMENT",
      "DEPARTMENT OF HOUSING PRESERVATION AND DEVELOPMENT",
      "DEPARTMENT OF HEALTH AND MENTAL HYGIENE",
      "DEPARTMENT OF ENVIRONMENTAL PROTECTION", 
      "DEPARTMENT OF TRANSPORTATION",
      "DEPARTMENT OF PARKS AND RECREATION",
      "DEPARTMENT OF SANITATION",
      "DEPARTMENT OF BUILDINGS",
      "TAXI AND LIMOUSINE COMMISSION",
      "DEPARTMENT OF CONSUMER AND WORKER PROTECTION",
      "DEPARTMENT OF HOMELESS SERVICES",
      "ECONOMIC DEVELOPMENT CORPORATION",
      "DEPARTMENT OF EDUCATION",
      "OFFICE OF TECHNOLOGY AND INNOVATION"
    ),
    description = "Official name of the agency handling the service request"
  ),
  complaint_type = list(
    type = "character",
    allow_empty = FALSE,
    description = "Type of complaint or service request"
  ),
  status = list(
    type = "character",
    allow_empty = FALSE,
    valid_values = c(
      "ASSIGNED",
      "CANCEL",
      "CLOSED",
      "IN PROGRESS",
      "OPEN",
      "PENDING",
      "STARTED",
      "UNSPECIFIED"
    ),
    description = "Current status of the service request"
  ),
  due_date = list(
    type = "POSIXct",
    allow_empty = TRUE,
    description = "Expected completion date for the service request"
  ),
  resolution_action_updated_date = list(
    type = "POSIXct",
    allow_empty = TRUE,
    description = "Date when the resolution action was last updated"
  ),
  incident_zip = list(
    type = "character",
    allow_empty = TRUE,
    pattern = "^\\d{5}$",  # Exactly 5 digits
    description = "5-digit ZIP code of the incident location"
  ),
  borough = list(
    type = "character",
    allow_empty = FALSE,
    valid_values = c(
      "BRONX",
      "BROOKLYN",
      "MANHATTAN",
      "QUEENS",
      "STATEN ISLAND",
      "UNSPECIFIED"
    ),
    description = "Borough where the incident occurred"
  ),
  x_coordinate_state_plane = list(
    type = "integer",
    allow_empty = TRUE,
    min_value = 912186.915226651,
    max_value = 1067577.2465731,
    description = "X-coordinate in New York State Plane Coordinate System"
  ),
  y_coordinate_state_plane = list(
    type = "integer",
    allow_empty = TRUE,
    min_value = 120125.896717202,
    max_value = 272997.706125923,
    description = "Y-coordinate in New York State Plane Coordinate System"
  ),
  latitude = list(
    type = "numeric",
    allow_empty = TRUE,
    min_value = 40.477399,
    max_value = 40.917576,
    description = "Latitude of the incident location"
  ),
  longitude = list(
    type = "numeric",
    allow_empty = TRUE,
    min_value = -74.259090,
    max_value = -73.700181,
    description = "Longitude of the incident location"
  ),
  address_type = list(
    type = "character",
    allow_empty = TRUE,
    valid_values = c(
      "ADDRESS",
      "BBL",
      "BLOCKFACE",
      "INTERSECTION",
      "PLACENAME",
      "UNRECOGNIZED"
    ),
    description = "Type of address or location identifier"
  ),
  incident_address = list(
    type = "character",
    allow_empty = TRUE,
    description = "Full address of the incident"
  ),
  street_name = list(
    type = "character",
    allow_empty = TRUE,
    description = "Name of the street where the incident occurred"
  ),
  cross_street_1 = list(
    type = "character",
    allow_empty = TRUE,
    description = "First cross street at the incident location"
  ),
  cross_street_2 = list(
    type = "character",
    allow_empty = TRUE,
    description = "Second cross street at the incident location"
  ),
  intersection_street_1 = list(
    type = "character",
    allow_empty = TRUE,
    description = "First street of the intersection"
  ),
  intersection_street_2 = list(
    type = "character",
    allow_empty = TRUE,
    description = "Second street of the intersection"
  ),
  city = list(
    type = "character",
    allow_empty = TRUE,
    description = "City where the incident occurred"
  ),
  landmark = list(
    type = "character",
    allow_empty = TRUE,
    description = "Nearby landmark or notable location"
  ),
  facility_type = list(
    type = "character",
    allow_empty = TRUE,
    description = "Type of facility associated with the incident"
  ),
  community_board = list(
    type = "character",
    allow_empty = TRUE,
    valid_values = c(
      "01 BRONX", "01 BROOKLYN", "01 MANHATTAN", "01 QUEENS", "01 STATEN ISLAND",
      "02 BRONX", "02 BROOKLYN", "02 MANHATTAN", "02 QUEENS", "02 STATEN ISLAND",
      "03 BRONX", "03 BROOKLYN", "03 MANHATTAN", "03 QUEENS", "03 STATEN ISLAND",
      "04 BRONX", "04 BROOKLYN", "04 MANHATTAN", "04 QUEENS",
      "05 BRONX", "05 BROOKLYN", "05 MANHATTAN", "05 QUEENS",
      "06 BRONX", "06 BROOKLYN", "06 MANHATTAN", "06 QUEENS",
      "07 BRONX", "07 BROOKLYN", "07 MANHATTAN", "07 QUEENS",
      "08 BRONX", "08 BROOKLYN", "08 MANHATTAN", "08 QUEENS",
      "09 BRONX", "09 BROOKLYN", "09 MANHATTAN", "09 QUEENS",
      "10 BRONX", "10 BROOKLYN", "10 MANHATTAN", "10 QUEENS",
      "11 BRONX", "11 BROOKLYN", "11 MANHATTAN", "11 QUEENS",
      "12 BRONX", "12 BROOKLYN", "12 MANHATTAN", "12 QUEENS",
      "13 BROOKLYN", "13 QUEENS",
      "14 BROOKLYN", "14 QUEENS",
      "15 BROOKLYN",
      "16 BROOKLYN",
      "17 BROOKLYN",
      "18 BROOKLYN",
      "UNSPECIFIED BRONX", "UNSPECIFIED BROOKLYN", "UNSPECIFIED MANHATTAN",
      "UNSPECIFIED QUEENS", "UNSPECIFIED STATEN ISLAND",
      "0 UNSPECIFIED"
    ),
    description = "Community board associated with the incident"
  ),
  bbl = list(
    type = "integer64",
    allow_empty = TRUE,
    description = "Borough, Block, and Lot (BBL) identifier"
  ),
  open_data_channel_type = list(
    type = "character",
    allow_empty = TRUE,
    valid_values = c(
      "MOBILE",
      "ONLINE",
      "OTHER",
      "PHONE",
      "UNKNOWN"
    ),
    description = "Channel through which the service request was submitted"
  ),
  park_facility_name = list(
    type = "character",
    allow_empty = TRUE,
    description = "Name of the park facility"
  ),
  park_borough = list(
    type = "character",
    allow_empty = TRUE,
    valid_values = c(
      "BRONX",
      "BROOKLYN",
      "MANHATTAN",
      "QUEENS",
      "STATEN ISLAND",
      "UNSPECIFIED"
    ),
    description = "Borough where the park is located"
  ),
  vehicle_type = list(
    type = "character",
    allow_empty = TRUE,
    valid_values = c(
      "AMBULETTE / PARATRANSIT",
      "CAR",
      "CAR SERVICE",
      "COMMUTER VAN",
      "GREEN TAXI",
      "OTHER",
      "SUV",
      "TRUCK",
      "VAN"
    ),
    description = "Type of vehicle involved in the incident"
  ),
  taxi_company_borough = list(
    type = "character",
    allow_empty = TRUE,
    valid_values = c(
      "BRONX",
      "BROOKLYN",
      "MANHATTAN",
      "QUEENS",
      "STATEN ISLAND",
      "UNSPECIFIED",
      ""
    ),
    description = "Borough of the taxi company"
  ),
  taxi_pick_up_location = list(
    type = "character",
    allow_empty = TRUE,
    description = "Location where the taxi was picked up"
  ),
  bridge_highway_name = list(
    type = "character",
    allow_empty = TRUE,
    description = "Name of the bridge or highway"
  ),
  bridge_highway_direction = list(
    type = "character",
    allow_empty = TRUE,
    description = "Direction of travel on the bridge or highway"
  ),
  road_ramp = list(
    type = "character",
    allow_empty = TRUE,
    description = "Road ramp associated with the incident"
  ),
  bridge_highway_segment = list(
    type = "character",
    allow_empty = TRUE,
    description = "Specific segment of the bridge or highway"
  ),
  location = list(
    type = "character",
    allow_empty = TRUE,
    description = "General location description"
  )
)

##############################################################################################
validate_schema <- function(data, schema) {
  results <- data.table(
    Field = character(),
    Error_Type = character(),
    Count = numeric(),
    Percentage = numeric(),
    Sample_Values = character()
  )
  
  total_rows <- nrow(data)
  
  for (field in names(schema)) {
    field_def <- schema[[field]]
    
    # Skip validation if field is missing
    if (!(field %in% names(data))) {
      results <- rbind(results, data.table(
        Field = field,
        Error_Type = "Field missing from dataset",
        Count = total_rows,
        Percentage = 1,
        Sample_Values = "N/A"
      ))
      next
    }
    
    # Debugging: Check actual column type
    actual_type <- class(data[[field]])[1]

    # Type validation
    if (field_def$type != actual_type) {
      results <- rbind(results, data.table(
        Field = field,
        Error_Type = paste("Invalid type: expected", field_def$type, "got", actual_type),
        Count = total_rows,
        Percentage = 1,
        Sample_Values = paste(head(unique(data[[field]]), 5), collapse = ", ")
      ))
      next
    }
    
    # Handle missing values
    na_count <- sum(is.na(data[[field]]))
    
    # Safe blank count check for character fields
    blank_count <- if(is.character(data[[field]])) {
      sum(data[[field]] == "", na.rm = TRUE)
    } else {
      0
    }
    
    # Check for empty values when not allowed
    if (!field_def$allow_empty) {
      total_empty <- na_count + blank_count
      if (total_empty > 0) {
        results <- rbind(results, data.table(
          Field = field,
          Error_Type = "Empty values not allowed",
          Count = total_empty,
          Percentage = total_empty / total_rows,
          Sample_Values = if(blank_count > 0) "'' (blank)" else "NA"
        ))
      }
    }
    # Date validation
    if (field_def$type == "POSIXct") {

      if (!inherits(data[[field]], "POSIXct")) {
        cat(" - Warning: Field is not in POSIXct format\n")
        tryCatch({
          # Store problematic values before conversion
          problem_dates <- data[[field]][!is.na(data[[field]])]
          converted_dates <- as.POSIXct(data[[field]], tz = "UTC")
          
          # Count failed conversions
          failed_count <- sum(is.na(converted_dates) & !is.na(data[[field]]))
          
          if (failed_count > 0) {
            # Get sample of problematic values
            sample_bad_dates <- head(unique(problem_dates[is.na(converted_dates)]), 5)
            
            results <- rbind(results, data.table(
              Field = field,
              Error_Type = "Invalid date format",
              Count = failed_count,
              Percentage = failed_count / total_rows,
              Sample_Values = paste(sample_bad_dates, collapse = ", ")
            ))
          }
        }, error = function(e) {
          results <- rbind(results, data.table(
            Field = field,
            Error_Type = paste("Date conversion error:", conditionMessage(e)),
            Count = sum(!is.na(data[[field]])),
            Percentage = sum(!is.na(data[[field]])) / total_rows,
            Sample_Values = paste(head(unique(data[[field]]), 5), collapse = ", ")
          ))
        })
      } else {
        # Add range check reporting
        valid_dates <- na.omit(data[[field]])
        if (length(valid_dates) > 0) {
          cat(" - Date range:", format(min(valid_dates), "%Y-%m-%d"), "to", 
              format(max(valid_dates), "%Y-%m-%d"), "\n")
          cat(" - NA count:", sum(is.na(data[[field]])), "\n")
          
          # Add to results if there are any NAs and empty values aren't allowed
          if (!field_def$allow_empty && sum(is.na(data[[field]])) > 0) {
            results <- rbind(results, data.table(
              Field = field,
              Error_Type = "NA values found in required date field",
              Count = sum(is.na(data[[field]])),
              Percentage = sum(is.na(data[[field]])) / total_rows,
              Sample_Values = "NA"
            ))
          }
        }
      }
    }  
    # Valid values validation for character fields
    if (!is.null(field_def$valid_values) && is.character(data[[field]])) {
      invalid_values <- data[[field]][!data[[field]] %in% field_def$valid_values & !is.na(data[[field]]) & data[[field]] != ""]
      if (length(invalid_values) > 0) {
        invalid_count <- length(invalid_values)
        results <- rbind(results, data.table(
          Field = field,
          Error_Type = "Invalid value",
          Count = invalid_count,
          Percentage = invalid_count / total_rows,
          Sample_Values = paste(head(unique(invalid_values), 5), collapse = ", ")
        ))
      }
    }
    
    # Pattern validation for character fields
    if (!is.null(field_def$pattern) && is.character(data[[field]])) {
      # Get non-NA values that don't match the pattern
      invalid_pattern <- data[[field]][!is.na(data[[field]]) & 
                                         data[[field]] != "" & 
                                         !grepl(field_def$pattern, data[[field]])]
      
      if (length(invalid_pattern) > 0) {
        invalid_count <- length(invalid_pattern)
        results <- rbind(results, data.table(
          Field = field,
          Error_Type = "Invalid format",
          Count = invalid_count,
          Percentage = invalid_count / total_rows,
          Sample_Values = paste(head(unique(invalid_pattern), 5), collapse = ", ")
        ))
      }
    }
    
    # Numeric range validation
    if ((field_def$type == "numeric" || field_def$type == "integer") && 
        (!is.null(field_def$min_value) || !is.null(field_def$max_value))) {
      
      out_of_range <- data[[field]][!is.na(data[[field]])]
      
      if (!is.null(field_def$min_value)) {
        out_of_range <- out_of_range[out_of_range < field_def$min_value]
      }
      if (!is.null(field_def$max_value)) {
        out_of_range <- out_of_range[out_of_range > field_def$max_value]
      }
      
      if (length(out_of_range) > 0) {
        results <- rbind(results, data.table(
          Field = field,
          Error_Type = "Value out of range",
          Count = length(out_of_range),
          Percentage = length(out_of_range) / total_rows,
          Sample_Values = paste(head(unique(out_of_range), 5), collapse = ", ")
        ))
      }
    }
  }
  
  if (nrow(results) == 0) {
    results <- data.table(
      Field = "All Fields",
      Error_Type = "No Validation Errors",
      Count = 0,
      Percentage = 0,
      Sample_Values = "N/A"
    )
  }
  
  return(results)
}

##############################################################################################
ui <- fluidPage(
  theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),
  
  titlePanel("311 Data Schema Validator"),
  
  fluidRow(
    column(12,
           actionButton("validate", "Validate Data", class = "btn-primary btn-lg"),
           br(), br(),
           shinycssloaders::withSpinner(DTOutput("validation_results"), type = 4)
    )
  )
)

##############################################################################################
server <- function(input, output, session) {
  validation_results <- reactiveVal(NULL)
  is_loading <- reactiveVal(FALSE)
  
  observeEvent(input$validate, {
    is_loading(TRUE)
    validation_results(NULL)  # Clear previous results
    
    req(cleaned_data)
    
    # Notify user validation is starting
    notif_id <- showNotification("Validating dataset...", type = "message", duration = NULL)
    
    # Run validation and store the result
    results <- validate_schema(cleaned_data, schema)
    validation_results(results)  # Store results in reactiveVal
    
    is_loading(FALSE)  # Mark validation as complete
    
    # Remove notification when validation is complete
    removeNotification(notif_id)
  })
  
  output$validation_results <- renderDT({
    req(validation_results())  # Ensure results exist before proceeding
    
    results <- validation_results()
    
    # Ensure it's a data.table or convert it
    if (!inherits(results, "data.table")) {
      results <- as.data.table(results)
    }
    
    # Check if it's empty; if so, return an empty table
    if (nrow(results) == 0) {
      results <- data.table(
        Field = "No data",
        Error_Type = "No validation errors detected",
        Count = 0,
        Percentage = 0,
        Sample_Values = "N/A"
      )
    }
    
    datatable(results,
              rownames = FALSE,
              options = list(
                pageLength = 10,
                order = list(list(2, 'desc'))  # Sort by Count column descending
              )) %>%
      formatCurrency("Count", currency = "", interval = 3, mark = ",", digits = 0) %>%
      formatPercentage("Percentage", digits = 2)
  })
}

##############################################################################################
shinyApp(ui, server)

##############################################################################################
##############################################################################################