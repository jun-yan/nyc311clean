##############################################################################################

########## ZIP CODE VALIDATOR ##########

##############################################################################################

library(shiny)
library(shinycssloaders)
library(DT)
library(data.table)  # Adding for better performance
library(bslib)

# Specify the port (choose a unique port number)
options(shiny.port = 4004)

##############################################################################################

data_file <- file.path("data", "dataset.rds")

if (!file.exists(data_file)) {
  stop("Data file not found. Files in 'data' directory:", paste(list.files("data"), collapse = ", "))
}

dataset <- readRDS(data_file)

# Define file paths using relative paths
data_file <- file.path("data", "dataset.rds")
zip_code_file <- file.path("data", "USPS_zipcodes.rds")

# Read dataset with error handling
if (file.exists(data_file)) {
  cleaned_data <- tryCatch({
    readRDS(data_file)
  }, error = function(e) {
    stop("Error loading dataset.rds: ", e$message)
  })
} else {
  stop("Error: dataset.rds not found in the 'data' directory.")
}

# Read ZIP code dataset with error handling
if (file.exists(zip_code_file)) {
  zip_codes <- tryCatch({
    readRDS(zip_code_file)
  }, error = function(e) {
    stop("Error loading USPS_zipcodes.rds: ", e$message)
  })
} else {
  stop("Error: USPS_zipcodes.rds not found in the 'data' directory.")
}

# Convert to data.table for better performance
setDT(cleaned_data)
total_records <- nrow(cleaned_data)

##############################################################################################
# Function to validate zip codes
are_valid_values <- function(check_values, valid_values) {
  
  # Filter out NA, empty strings, and blank values
  check_values[is.na(check_values) | check_values == "" | check_values == " "] <- NA
  
  # Check if each value exists in the valid set
  check_values %in% valid_values
}

##############################################################################################
# Pre-compute zip code validation results
validate_zip_codes <- function(data, valid_zips) {
  invalid_zips <- !are_valid_values(data$incident_zip, valid_zips$delivery_zipcode)
  
  invalid_zips_filtered <- data$incident_zip[invalid_zips]
  invalid_zips_filtered <- invalid_zips_filtered[!is.na(invalid_zips_filtered) & 
                                                   invalid_zips_filtered != "" & 
                                                   invalid_zips_filtered != " "]
  
  invalid_summary <- data.table(
    Invalid_Zip_Code = unique(invalid_zips_filtered),
    Count = as.numeric(table(invalid_zips_filtered))
  )
  
  total_non_blank_records <- nrow(data[!is.na(data$incident_zip),])
  invalid_summary[, Percentage := round(Count / total_non_blank_records, 6)]
  
  setorder(invalid_summary, -Count)
  
  list(
    total_invalid = length(invalid_zips_filtered),
    summary = invalid_summary
  )
}

# Pre-compute the results
pre_computed_results <- validate_zip_codes(cleaned_data, zip_codes)

##############################################################################################
ui <- fluidPage(
  theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),
  
  titlePanel(
    div(
      style = "color: steelblue; text-align: center;",
      h3("ZIP Code Validation Analysis"),
      h4("Validating zip codes against the USPS database.", style = "font-size: 20px;")
    )
  ),
  
  fluidRow(
    column(12,
           div(
             style = "display: flex; justify-content: space-between; align-items: center;",
             actionButton("analyze", "Detect Invalid ZIP Codes", class = "btn-primary btn-lg"),
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
           shinycssloaders::withSpinner(textOutput("total_invalid_summary"), type = 4),
           br(),
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
  
  # Use pre-computed results
  zip_analysis_results <- eventReactive(input$analyze, {
    pre_computed_results
  }, ignoreNULL = FALSE)
  
  output$total_invalid_summary <- renderText({
    req(input$analyze)
    req(zip_analysis_results())
    sprintf("Total Invalid ZIP Codes: %s", 
            format(zip_analysis_results()$total_invalid, big.mark = ","))
  })
  
  output$results_table <- renderDT({
    req(input$analyze)
    req(zip_analysis_results())
    
    datatable(zip_analysis_results()$summary,
              rownames = FALSE,
              options = list(
                pageLength = 50,
                order = list(list(1, 'desc'))
              )) %>%
      formatCurrency("Count", currency = "", interval = 3, mark = ",", digits = 0) %>%
      formatPercentage("Percentage", digits = 4)
  })
}

##############################################################################################
shinyApp(ui, server)

##############################################################################################