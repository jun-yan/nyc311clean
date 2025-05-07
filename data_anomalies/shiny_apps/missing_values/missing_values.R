##############################################################################################

########## MISSING VALUES ###########

##############################################################################################
library(shiny)
library(shinycssloaders)
library(DT)
library(data.table)
library(bslib)

# Specify the port (choose a unique port number)
options(shiny.port = 4002)

##############################################################################################
# Load the data
data_file <- file.path("data", "dataset.rds")

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
total_records <- nrow(cleaned_data)

##############################################################################################
# Pre-compute the missing values analysis for faster response
calculate_missing_values <- function(data) {
  # Pre-allocate results table
  max_rows <- length(names(data)) * 2
  results <- data.table(
    Field = character(max_rows),
    Missing_Type = character(max_rows),
    Count = numeric(max_rows),
    Percentage = numeric(max_rows)
  )
  
  row_counter <- 0L
  total_rows <- nrow(data)
  
  # Process each column
  for(col in names(data)) {
    col_type <- class(data[[col]])[1]
    has_missing <- FALSE  # Flag to track if any missing values found
    
    # Check for actual NAs (NULL values)
    na_count <- sum(is.na(data[[col]]))
    if(na_count > 0) {
      has_missing <- TRUE
      row_counter <- row_counter + 1L
      set(results, i = row_counter, j = 1L:4L,
          value = list(col, "NULL", na_count,
                       round(na_count/total_rows, 4)))
    }
    
    # For character columns, check for empty strings and N/A text
    if(col_type == "character") {
      # Check empty strings
      empty_count <- sum(data[[col]] == "", na.rm = TRUE)
      if(empty_count > 0) {
        has_missing <- TRUE
        row_counter <- row_counter + 1L
        set(results, i = row_counter, j = 1L:4L,
            value = list(col, '""', empty_count,
                         round(empty_count/total_rows, 4)))
      }
      
      # Check for "N/A"
      na_text_count <- sum(data[[col]] == "N/A", na.rm = TRUE)
      if(na_text_count > 0) {
        has_missing <- TRUE
        row_counter <- row_counter + 1L
        set(results, i = row_counter, j = 1L:4L,
            value = list(col, '"N/A"', na_text_count,
                         round(na_text_count/total_rows, 4)))
      }
    }
    
    # If no missing values found, add a row indicating that
    if(!has_missing) {
      row_counter <- row_counter + 1L
      set(results, i = row_counter, j = 1L:4L,
          value = list(col, "No missing values", 0, 0))
    }
  }
  
  # Trim unused rows and sort
  results <- results[1:row_counter][order(Field, -Count)]
  return(results)
}

# Pre-compute the results
pre_computed_results <- calculate_missing_values(cleaned_data)

##############################################################################################
ui <- fluidPage(
  theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),
  
  titlePanel("Missing Values Analysis"),
  
  fluidRow(
    column(12,
           div(
             style = "display: flex; justify-content: space-between; align-items: center;",
             actionButton("analyze", "Analyze Missing Values", class = "btn-primary btn-lg"),
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
           shinycssloaders::withSpinner(DTOutput("results_table"), type = 4)
    )
  )
)

##############################################################################################
server <- function(input, output, session) {
  # Display pre-loading status
  output$data_status <- renderText({
    "Data pre-loaded and ready for fast display"
  })
  
  # Display total record count
  output$record_count <- renderText({
    paste("Total records:", format(total_records, big.mark = ",", scientific = FALSE))
  })
  
  # Use the pre-computed results for faster response
  missing_results <- eventReactive(input$analyze, {
    pre_computed_results
  }, ignoreNULL = FALSE)
  
  output$results_table <- renderDT({
    req(input$analyze)
    results <- missing_results()
    req(results)
    
    datatable(results,
              options = list(
                pageLength = 50,
                order = list(0, 'asc')
              )) %>%
      formatCurrency("Count", currency = "", interval = 3, mark = ",", digits = 0) %>%
      formatPercentage("Percentage", digits = 2)
  })
}

##############################################################################################
shinyApp(ui, server)

##############################################################################################