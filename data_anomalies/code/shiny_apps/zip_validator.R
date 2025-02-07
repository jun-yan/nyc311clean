library(shiny)
library(DT)

# Load the data
data_path <- "C:/Users/David/OneDrive/Documents/datacleaningproject/data_hygiene/code/data/3-month_311SR_10-01-2024_thru_12-31-2024.rds"
zip_path <- "C:/Users/David/OneDrive/Documents/datacleaningproject/data_hygiene/code/data/USPS_zipcodes.rds"

cleaned_data <- readRDS(data_path)
zip_codes <- readRDS(zip_path)

ui <- fluidPage(
  titlePanel("ZIP Code Validation Analysis"),
  
  fluidRow(
    column(12,
           actionButton("analyze", "Analyze ZIP Codes", class = "btn-primary"),
           br(), br(),
           DTOutput("results_table")
    )
  )
)

server <- function(input, output, session) {
  output$results_table <- renderDT({
    req(input$analyze)
    
    # Get invalid ZIPs using the are_valid_values function
    invalid_zips <- !are_valid_values(cleaned_data$incident_zip, zip_codes$delivery_zipcode)
    
    # Create summary of invalid values
    invalid_summary <- data.frame(
      ZIP_Code = unique(cleaned_data$incident_zip[invalid_zips]),
      Count = as.numeric(table(cleaned_data$incident_zip[invalid_zips])),
      stringsAsFactors = FALSE
    )
    
    # Add percentage
    total_records <- nrow(cleaned_data)
    invalid_summary$Percentage <- round(invalid_summary$Count / total_records * 100, 2)
    
    # Sort by frequency
    invalid_summary <- invalid_summary[order(-invalid_summary$Count),]
    
    # Display results
    datatable(invalid_summary,
              options = list(
                pageLength = 10,
                order = list(1, 'desc')  # Sort by Count by default
              )) %>%
      formatCurrency("Count", currency = "", interval = 3, mark = ",", digits = 0) %>%
      formatRound("Percentage", digits = 2)
  })
}

shinyApp(ui, server)