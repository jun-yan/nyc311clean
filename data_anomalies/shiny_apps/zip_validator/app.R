##############################################################################################

########## ZIP CODE VALIDATOR ##########

##############################################################################################

library(shiny)
library(shinycssloaders)
library(DT)
library(data.table)  # Adding for better performance

# Specify the port (choose a unique port number)
options(shiny.port = 4004)

##############################################################################################
# Load the data
data_path <- "C:/Users/David/OneDrive/Documents/datacleaningproject/nyc311clean/data_anomalies/shiny_apps/zip_validator/data/dataset.rds"
cleaned_data <- readRDS(data_path)

zip_code_path <- "C:/Users/David/OneDrive/Documents/datacleaningproject/nyc311clean/data_anomalies/shiny_apps/zip_validator/data/USPS_zipcodes.rds"
zip_codes <- readRDS(zip_code_path)

# Convert to data.table for better performance
setDT(cleaned_data)
setDT(zip_codes)

##############################################################################################
# Function to validate zip codes
are_valid_values <- function(check_values, valid_values) {
  
  # Filter out NA, empty strings, and blank values
  check_values[is.na(check_values) | check_values == "" | check_values == " "] <- NA
  
  # Check if each value exists in the valid set
  check_values %in% valid_values
}

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
           actionButton("analyze", "Detect Invalid ZIP Codes", class = "btn-primary btn-lg"),
           br(), br(),
           shinycssloaders::withSpinner(textOutput("total_invalid_summary"), type = 4),
           br(),
           shinycssloaders::withSpinner(DTOutput("results_table"), type = 4)
    )
  )
)

##############################################################################################
server <- function(input, output, session) {
  zip_analysis_results <- reactiveVal(NULL)
  is_loading <- reactiveVal(FALSE)
  
  observeEvent(input$analyze, {
    is_loading(TRUE)
    zip_analysis_results(NULL)
    
    results <- local({
      invalid_zips <- !are_valid_values(cleaned_data$incident_zip, zip_codes$delivery_zipcode)
      
      invalid_zips_filtered <- cleaned_data$incident_zip[invalid_zips]
      invalid_zips_filtered <- invalid_zips_filtered[!is.na(invalid_zips_filtered) & 
                                                       invalid_zips_filtered != "" & 
                                                       invalid_zips_filtered != " "]
      
      invalid_summary <- data.table(
        Invalid_Zip_Code = unique(invalid_zips_filtered),
        Count = as.numeric(table(invalid_zips_filtered))
      )
      
      total_records <- nrow(cleaned_data)
      
      print(head(cleaned_data))
      total_non_blank_records <- nrow(cleaned_data[!is.na(cleaned_data$incident_zip),])
      invalid_summary[, Percentage := round(Count / total_non_blank_records, 6)]
      
      setorder(invalid_summary, -Count)
      
      list(
        total_invalid = length(invalid_zips_filtered),
        summary = invalid_summary
      )
    })
    
    zip_analysis_results(results)
    is_loading(FALSE)
  })
  
  output$total_invalid_summary <- renderText({
    req(zip_analysis_results())
    sprintf("Total Invalid ZIP Codes: %s", 
            format(zip_analysis_results()$total_invalid, big.mark = ","))
  })
  
  output$results_table <- renderDT({
    if(is_loading()) return(NULL)
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