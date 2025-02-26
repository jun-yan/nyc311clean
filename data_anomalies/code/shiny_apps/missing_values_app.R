library(shiny)
library(shinycssloaders)
library(DT)
library(data.table)

# Specify the port (choose a unique port number)
options(shiny.port = 4002)

##############################################################################################
# Load the data
data_path <- "C:/Users/David/OneDrive/Documents/datacleaningproject/nyc311clean/data_anomalies/code/data/3-month_311SR_10-01-2024_thru_12-31-2024_AS_OF_02-02-2025.rds"
cleaned_data <- readRDS(data_path)
setDT(cleaned_data)  # Convert to data.table for better performance

##############################################################################################
ui <- fluidPage(
  theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),
  
  titlePanel("Missing Values Analysis"),
  
  fluidRow(
    column(12,
           actionButton("analyze", "Analyze Missing Values", class = "btn-primary btn-lg"),
           br(), br(),
           shinycssloaders::withSpinner(DTOutput("results_table"), type = 4)
    )
  )
)

##############################################################################################
server <- function(input, output, session) {
  missing_results <- reactiveVal(NULL)
  is_loading <- reactiveVal(FALSE)
  
  observeEvent(input$analyze, {
    is_loading(TRUE)
    missing_results(NULL)
    
    results <- local({
      # Pre-allocate results table
      max_rows <- length(names(cleaned_data)) * 2
      results <- data.table(
        Field = character(max_rows),
        Missing_Type = character(max_rows),
        Count = numeric(max_rows),
        Percentage = numeric(max_rows)
      )
      
      row_counter <- 0L
      total_rows <- nrow(cleaned_data)
      
      # Process each column
      for(col in names(cleaned_data)) {
        col_type <- class(cleaned_data[[col]])[1]
        has_missing <- FALSE  # Flag to track if any missing values found
        
        # Check for actual NAs (NULL values)
        na_count <- sum(is.na(cleaned_data[[col]]))
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
          empty_count <- sum(cleaned_data[[col]] == "", na.rm = TRUE)
          if(empty_count > 0) {
            has_missing <- TRUE
            row_counter <- row_counter + 1L
            set(results, i = row_counter, j = 1L:4L,
                value = list(col, '""', empty_count,
                             round(empty_count/total_rows, 4)))
          }
          
          # Check for "N/A"
          na_text_count <- sum(cleaned_data[[col]] == "N/A", na.rm = TRUE)
          if(na_text_count > 0) {
            has_missing <- TRUE
            row_counter <- row_counter + 1L
            set(results, i = row_counter, j = 1L:4L,
                value = list(col, '"N/A"', na_text_count,
                             round(na_count/total_rows, 4)))
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
    })
    
    missing_results(results)
    is_loading(FALSE)
  })
  
  output$results_table <- renderDT({
    if(is_loading()) return(NULL)
    req(missing_results())
    
    datatable(missing_results(),
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
##############################################################################################