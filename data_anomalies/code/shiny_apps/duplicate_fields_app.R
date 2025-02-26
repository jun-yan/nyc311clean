##############################################################################################
library(bslib)
library(data.table)
library(DT)
library(shiny)
library(shinycssloaders)
library(stringr)

# Configure options
options(shiny.port = 4006)

# Load and prepare data
data_path <- "C:/Users/David/OneDrive/Documents/datacleaningproject/nyc311clean/data_anomalies/code/data/3-month_311SR_10-01-2024_thru_12-31-2024_AS_OF_02-02-2025.rds"
cleaned_data <- readRDS(data_path)
setDT(cleaned_data)

##############################################################################################
parse_location_field <- function(location_string) {
  # More robust coordinate extraction
  # This will work with various formats like (40.640, -73.973), 40.640, -73.973, etc.
  coords <- stringr::str_extract_all(location_string, "-?\\d+\\.\\d+")[[1]]
  
  if(length(coords) == 2) {
    return(list(
      lat = as.numeric(coords[1]),
      lon = as.numeric(coords[2])
    ))
  } else if(length(coords) == 4) {
    # Handle cases with 4 numbers (might include parentheses)
    return(list(
      lat = as.numeric(coords[3]),
      lon = as.numeric(coords[4])
    ))
  }
  
  return(list(lat = NA_real_, lon = NA_real_))
}

##############################################################################################
evaluate_coordinate_matches <- function(data) {

  # Prepare data for comparison - exclude both NA and empty string
  valid_cases <- data[!is.na(latitude) & !is.na(longitude) & 
                        !is.na(location) & location != ""]
  
  # Extract coordinates from location field
  matches <- regmatches(valid_cases$location, gregexpr("-?\\d+\\.\\d+", valid_cases$location))
  extracted_lat <- as.numeric(sapply(matches, `[`, 1))
  extracted_lon <- as.numeric(sapply(matches, `[`, 2))
  
  total_pairs <- length(extracted_lat)
  
  # Compare extracted coordinates with existing latitude and longitude
  coordinate_matches <- valid_cases[
    abs(valid_cases$latitude - extracted_lat) < 1e-5 & 
      abs(valid_cases$longitude - extracted_lon) < 1e-5
  ]
  
  results <- data.table(
    Field_Pair = "Lat/Long - Location",
    Count = nrow(coordinate_matches),
    Percentage = nrow(coordinate_matches) / total_pairs,
    Sample_Values = paste(
      head(coordinate_matches[, sprintf("(%.8f, %.8f) -> %s", 
                                        latitude, longitude, location)], 4), 
      collapse = "; "
    )
  )
  
  return(results)
}

##############################################################################################
evaluate_duplicate_fields <- function(data) {
  results <- data.table(
    Field_Pair = character(),
    Count = numeric(),
    Percentage = numeric(),
    Sample_Values = character()
  )
  
  # Field pairs to check
  field_pairs <- list(
    list(field1 = "borough", field2 = "park_borough", name = "Borough - Park Borough"),
    list(field1 = "borough", field2 = "taxi_company_borough", name = "Borough - Taxi Borough"),
    list(field1 = "street_name", field2 = "landmark", name = "Street Name - Landmark"),
    list(field1 = "cross_street_1", field2 = "intersection_street_1", name = "Cross St 1 - Intersection St 1"),
    list(field1 = "cross_street_2", field2 = "intersection_street_2", name = "Cross St 2 - Intersection St 2")
  )
  
  for (pair in field_pairs) {
    # Get total cases where either field has a value
    relevant_cases <- data[(!is.na(get(pair$field1)) & get(pair$field1) != "") | 
                             (!is.na(get(pair$field2)) & get(pair$field2) != "")]
    
    total_pairs <- nrow(relevant_cases)
    
    if (total_pairs > 0) {
      # Perfect matches - both fields present and equal
      matches <- relevant_cases[!is.na(get(pair$field1)) & 
                                  !is.na(get(pair$field2)) & 
                                  get(pair$field1) != "" & 
                                  get(pair$field2) != "" &
                                  get(pair$field1) == get(pair$field2)]
      
      # Add only matching results
      results <- rbind(results, data.table(
        Field_Pair = pair$name,
        Count = nrow(matches),
        Percentage = nrow(matches) / total_pairs,
        Sample_Values = paste(head(unique(matches[[pair$field1]]), 5), collapse = "; ")
      ))
    }
  }
  
  if (nrow(results) == 0) {
    results <- data.table(
      Field_Pair = "All Fields",
      Count = 0,
      Percentage = 0,
      Sample_Values = "N/A"
    )
  }
  
  return(results)
}

##############################################################################################
# Main evaluation function to combine all evaluations
run_field_evaluations <- function(data) {
  # Run duplicate field evaluations
  duplicate_field_results <- evaluate_duplicate_fields(data)
  
  # Run coordinate matching evaluation
  coordinate_match_results <- evaluate_coordinate_matches(data)
  
  final_results <- rbind(
    duplicate_field_results,
    coordinate_match_results[, .(
      Field_Pair = "Lat/Long - Location", 
      Count,  # Use the existing Count column
      Percentage,  # Use the existing Percentage column
      Sample_Values
    )]
  )
  
  return(final_results)
}

##############################################################################################
ui <- fluidPage(
  theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),
  
  titlePanel("Duplicate Field Evaluation"),
  
  fluidRow(
    column(12, 
           actionButton("evaluate", "Evaluate Field Pairs", class = "btn-primary btn-lg"),
           br(), br(),
           shinycssloaders::withSpinner(DTOutput("evaluation_results"), type = 4)
    )
  )
)

##############################################################################################
server <- function(input, output, session) {
  evaluation_results <- reactiveVal(NULL)
  is_loading <- reactiveVal(FALSE)
  
  observeEvent(input$evaluate, {
    is_loading(TRUE)  # Start loading
    evaluation_results(NULL)
    
    evaluation_output <- run_field_evaluations(cleaned_data)
    evaluation_results(evaluation_output)
    is_loading(FALSE)  # End loading
  })
  
  output$evaluation_results <- renderDT({
    if(is_loading()) return(NULL)
    req(evaluation_results())
    
    datatable(evaluation_results(),
              rownames = FALSE,
              options = list(
                pageLength = 10,
                order = list(list(0, 'asc'))
              )) %>%
      formatCurrency("Count", currency = "", interval = 3, mark = ",", digits = 0) %>%
      formatPercentage("Percentage", digits = 2)
  })
}

##############################################################################################
shinyApp(ui, server)

##############################################################################################