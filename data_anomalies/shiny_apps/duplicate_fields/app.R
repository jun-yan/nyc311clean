##############################################################################################

########## DUPLICATE FIELDS APP ########## 

##############################################################################################

library(bslib)
library(data.table)
library(DT)
library(later)
library(shiny)
library(shinycssloaders)
library(stringr)

##############################################################################################
# Configure options
options(shiny.port = 4006)

load_data <- function() {
  data_file <- file.path("data", "dataset.RDS")
  if (file.exists(data_file)) {
    tryCatch({
      dt <- readRDS(data_file)  # Load dataset on demand
      setDT(dt)  # Ensure it's a data.table
      return(dt)
    }, error = function(e) {
      stop("Error loading dataset: ", e$message)
    })
  } else {
    stop("Data file not found in the 'data' directory.")
  }
}

##############################################################################################
evaluate_coordinate_matches <- function(data) {
  valid_cases <- data[!is.na(latitude) & !is.na(longitude) & !is.na(location) & location != ""]
  
  matches <- regmatches(valid_cases$location, gregexpr("-?\\d+\\.\\d+", valid_cases$location))
  extracted_lat <- as.numeric(sapply(matches, `[`, 1))
  extracted_lon <- as.numeric(sapply(matches, `[`, 2))
  
  total_pairs <- length(extracted_lat)
  
  coordinate_matches <- valid_cases[
    abs(valid_cases$latitude - extracted_lat) < 1e-5 & 
      abs(valid_cases$longitude - extracted_lon) < 1e-5
  ]
  
  return(data.table(
    Field_Pair = "Lat/Long - Location",
    Matches = nrow(coordinate_matches),
    Non_Matches = total_pairs - nrow(coordinate_matches),
    Percentage = round(nrow(coordinate_matches) / total_pairs * 100, 2)
  ))
}

##############################################################################################
evaluate_single_field_pair <- function(data, pair_index) {
  field_pairs <- list(
    list(field1 = "borough", field2 = "taxi_company_borough", name = "Borough - Taxi Borough"),
    list(field1 = "street_name", field2 = "landmark", name = "Street Name - Landmark"),
    list(field1 = "cross_street_1", field2 = "intersection_street_1", name = "Cross St 1 - Intersection St 1"),
    list(field1 = "cross_street_2", field2 = "intersection_street_2", name = "Cross St 2 - Intersection St 2"),
    list(field1 = "borough", field2 = "park_borough", name = "Borough - Park Borough")  # Now last
  )
  
  if (pair_index > length(field_pairs)) {
    return(NULL)  # No more pairs left to process
  }
  
  pair <- field_pairs[[pair_index]]
  
  # Count total valid comparisons (i.e., rows where both fields exist)
  total_pairs <- nrow(data)
  
  # Count exact matches
  match_count <- sum(data[[pair$field1]] == data[[pair$field2]], na.rm = TRUE)
  
  # Calculate non-matching count
  non_match_count <- total_pairs - match_count
  
  # Compute match percentage
  match_percentage <- ifelse(total_pairs > 0, round(match_count / total_pairs, 4), 0)
  
  # Return results
  return(data.table(
    Field_Pair = pair$name,
    Percentage = match_percentage,  # Move percentage before counts
    Matches = match_count,
    Non_Matches = non_match_count
  ))
}

##############################################################################################
run_field_evaluations <- function(data) {
  field_pairs <- list(
    list(field1 = "borough", field2 = "taxi_company_borough", name = "Borough - Taxi Borough"),
    list(field1 = "street_name", field2 = "landmark", name = "Street Name - Landmark"),
    list(field1 = "cross_street_1", field2 = "intersection_street_1", name = "Cross St 1 - Intersection St 1"),
    list(field1 = "cross_street_2", field2 = "intersection_street_2", name = "Cross St 2 - Intersection St 2"),
    list(field1 = "borough", field2 = "park_borough", name = "Borough - Park Borough")  # Now last
  )
  
  results <- data.table(Field_Pair = character(), Matches = numeric(), Non_Matches = numeric(), Percentage = numeric())
  
  for (i in seq_along(field_pairs)) {
    pair_result <- evaluate_single_field_pair(data, i)
    if (!is.null(pair_result)) {
      results <- rbind(results, pair_result)
    }
  }
  
  setorder(results, Percentage)  # Sort results by Percentage ascending
  return(results)
}

##############################################################################################
ui <- fluidPage(
  theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),
  
  div(
    style = "text-align: center; color: steelblue;",
    h2("Duplicate Field Evaluation"),
    h4("Determining % of matches between selected field pairs.", style = "font-size: 18px; font-weight: normal;")
  ),
  
  fluidRow(
    column(12, 
           actionButton("evaluate", "Evaluate Field Pairs", class = "btn-primary btn-lg"),
           br(), br(),
           shinycssloaders::withSpinner(DTOutput("evaluation_results"), type = 4, hide.ui = FALSE)
    )
  )
)

##############################################################################################
server <- function(input, output, session) {
  evaluation_results <- reactiveVal(NULL)
  is_processing <- reactiveVal(FALSE)  # New reactive to track if processing is happening
  
  observeEvent(input$evaluate, {
    evaluation_results(NULL)  # Clear results immediately to trigger UI update
    Sys.sleep(0.5)  # Short delay to allow the UI to refresh
    
    dt <- load_data()  # Load dataset
    evaluation_output <- run_field_evaluations(dt)  # Run evaluations
    evaluation_results(evaluation_output)  # Store results
  })
  
  output$evaluation_results <- renderDT({
    req(!is_processing())  # Wait until processing is finished before rendering
    req(evaluation_results())  # Ensure results exist
    
    datatable(evaluation_results()[, .(Field_Pair, Percentage, Matches, Non_Matches)], # Ensure correct column order
              rownames = FALSE,
              options = list(
                pageLength = 10,
                order = list(list(2, 'desc'))  # Sort by Percentage descending
              )) %>%
      formatPercentage("Percentage", digits = 2) %>%
      formatCurrency("Matches", currency = "", interval = 3, mark = ",", digits = 0) %>%
      formatCurrency("Non_Matches", currency = "", interval = 3, mark = ",", digits = 0)
  })
}

##############################################################################################
shinyApp(ui, server)
##############################################################################################
