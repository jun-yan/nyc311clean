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

# Global variables to store pre-computed data
pre_computed_data <- NULL
pre_computed_results <- NULL

# Pre-load and pre-compute during startup
.onLoad <- function(libname, pkgname) {
  # This will run when the app starts
  pre_load_data()
}

##############################################################################################
pre_load_data <- function() {
  # Load the data once at startup
  data_file <- file.path("data", "dataset.rds")
  if (file.exists(data_file)) {
    tryCatch({
      pre_computed_data <<- readRDS(data_file)  # Load dataset on demand
      setDT(pre_computed_data)  # Ensure it's a data.table
      
      # Pre-compute the evaluation results
      pre_computed_results <<- run_field_evaluations(pre_computed_data)
      
      message("Data and evaluation results pre-computed and ready.")
    }, error = function(e) {
      warning("Error pre-loading dataset: ", e$message)
    })
  } else {
    warning("Data file not found in the 'data' directory.")
  }
}

##############################################################################################
load_data <- function() {
  # Return pre-computed data if available
  if (!is.null(pre_computed_data)) {
    return(pre_computed_data)
  }
  
  # Fallback to normal loading if pre-computed data is not available
  data_file <- file.path("data", "dataset.rds")
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
evaluate_coordinate_matches <- function(df) {
  # Use df instead of data to avoid conflicts with R's built-in data function
  valid_cases <- df[!is.na(df$latitude) & !is.na(df$longitude) & 
                      !is.na(df$location) & df$location != "",]
  
  matches <- regmatches(valid_cases$location, gregexpr("-?\\d+\\.\\d+", valid_cases$location))
  
  # Check if any matches are too short (don't have both lat and lon)
  valid_matches <- matches[sapply(matches, length) >= 2]
  
  if(length(valid_matches) == 0) {
    return(data.table(
      Field_Pair = "Lat/Long - Location",
      Matches = 0,
      Non_Matches = 0,
      Percentage = 0
    ))
  }
  
  extracted_lat <- as.numeric(sapply(valid_matches, `[`, 1))
  extracted_lon <- as.numeric(sapply(valid_matches, `[`, 2))
  
  # Filter valid_cases to match the length of valid_matches
  valid_cases <- valid_cases[sapply(matches, length) >= 2,]
  
  total_pairs <- length(extracted_lat)
  
  coordinate_matches <- valid_cases[
    abs(valid_cases$latitude - extracted_lat) < 1e-5 & 
      abs(valid_cases$longitude - extracted_lon) < 1e-5,
  ]
  
  return(data.table(
    Field_Pair = "Lat/Long - Location",
    Matches = nrow(coordinate_matches),
    Non_Matches = total_pairs - nrow(coordinate_matches),
    Percentage = round(nrow(coordinate_matches) / total_pairs, 2)
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
  
  results <- data.table()
  
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
  
  # Now run the coordinate evaluation and bind the results
  coord_results <- evaluate_coordinate_matches(data)
  
  # Combine the coordinate results with the other field pairs
  results <- rbind(results, coord_results)
  
  setorder(results, Percentage)  # Sort results by Percentage ascending
  return(results)
}

##############################################################################################
ui <- fluidPage(
  title = "Duplicate Fields Detection", 
  theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),
  
  div(
    style = "text-align: center; color: steelblue;",
    h2("Duplicate Field Evaluation"),
    h4("Determining % of matches between selected field pairs.", style = "font-size: 18px; font-weight: normal;")
  ),
  
  fluidRow(
    column(12, 
           div(
             style = "display: flex; justify-content: space-between; align-items: center;",
             actionButton("evaluate", "Evaluate Field Pairs", class = "btn-primary btn-lg"),
             div(
               textOutput("data_status"),
               style = "font-style: italic; color: gray; font-size: 0.9em;"
             )
           ),
           div(
             style = "margin-top: 10px; font-weight: bold; color: #333;",
             textOutput("record_count")
           ),
           br(), br(),
           shinycssloaders::withSpinner(DTOutput("evaluation_results"), type = 4, hide.ui = FALSE)
    )
  )
)

##############################################################################################
server <- function(input, output, session) {
  
  # Show data loading status
  output$data_status <- renderText({
    if(!is.null(pre_computed_results)) {
      "Data pre-loaded and ready for fast display"
    } else {
      "Loading data on demand..."
    }
  })
  
  # Display total record count
  output$record_count <- renderText({
    data <- if(!is.null(pre_computed_data)) pre_computed_data else load_data()
    paste("Total records:", format(nrow(data), big.mark = ",", scientific = FALSE))
  })
  
  # Create a reactive expression for the evaluation process
  evaluation_data <- eventReactive(input$evaluate, {
    # If pre-computed results are available, use them
    if(!is.null(pre_computed_results)) {
      return(pre_computed_results)
    }
    
    # Otherwise, compute on demand
    dt <- load_data()  # Load dataset
    run_field_evaluations(dt)  # Run evaluations
  }, ignoreNULL = FALSE)
  
  # Render the output
  output$evaluation_results <- renderDT({
    # Only proceed if evaluation has been triggered and has results
    req(input$evaluate)
    results <- evaluation_data()
    req(results)
    
    datatable(results[, .(Field_Pair, Percentage, Matches, Non_Matches)], # Ensure correct column order
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

# Call the pre-load function now that all functions are defined
pre_load_data()

##############################################################################################
shinyApp(ui, server)

##############################################################################################