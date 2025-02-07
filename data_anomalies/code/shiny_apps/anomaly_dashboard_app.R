##################################################################################
library(shiny)
library(shinydashboard)
library(bslib)
library(scales)  # For comma formatting

##################################################################################
# Load the data
data_path <- "C:/Users/David/OneDrive/Documents/datacleaningproject/data_anomalies/code/data/3-month_311SR_10-01-2024_thru_12-31-2024_AS_OF_02-02-2025.rds"
cleaned_data <- readRDS(data_path)

# Specify the port for the dashboard
options(shiny.port = 4001)

##############################################################################################
ui <- dashboardPage(
  dashboardHeader(title = "Data Anomaly Explorer"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard Overview", tabName = "home", icon = icon("house")),
      menuItem("Identify Missing Values", tabName = "missing_values", icon = icon("circle-exclamation")),
      menuItem("Validate against Data Schema", tabName = "schema_validator", icon = icon("check-circle")),
      menuItem("Validate Zipcodes", tabName = "zip_validator", icon = icon("map-pin")),
      menuItem("Evaluate Date Field Logic", tabName = "date_validator", icon = icon("calendar-check")),
      menuItem("Detect Duplicate Fields", tabName = "field_duplication", icon = icon("copy")),
      menuItem("Detect Dupes via Fuzzy Matching", tabName = "fuzzy_matcing", icon = icon("copy")),
      menuItem("Project Details", tabName = "about", icon = icon("info-circle"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # Home/Dashboard Overview Tab
      tabItem(tabName = "home",
              fluidRow(
                box(
                  title = "NYC 311 Service Requests Data Anomaly Explorer",
                  width = 12,
                  status = "primary",
                  solidHeader = TRUE,
                  HTML("
              <p>This dashboard provides tools to analyze and validate 311 Service Request data.</p>
              <ul>
                <li>Detect and explore missing values</li>
                <li>Validate data schema and constraints</li>
                <li>Check for Zipcode accuracy</li>
                <li>Evaluate date fields for logical consistency</li>
                <li>Detect duplicate fields</li>
                <li>Detect duplicates fields using fuzzy matchings</li>
              </ul>
            ")
                )
              ),
              fluidRow(
                infoBox("Apps Available", 6, icon = icon("chart-simple"), color = "light-blue"),
                infoBox("Data Period", "10/01/2024 - 12/31/2024", icon = icon("calendar"), color = "olive"),
                infoBox("Total Records", comma(nrow(cleaned_data)), icon = icon("database"), color = "maroon")
              )
      ),
      
      # Missing Values Tab
      tabItem(tabName = "missing_values",
              tags$iframe(
                src = "http://127.0.0.1:4002", 
                width = "100%", 
                height = "800px", 
                frameborder = 0
              )
      ),
      
      # Schema Validator Tab
      tabItem(tabName = "schema_validator",
              tags$iframe(
                src = "http://127.0.0.1:4003", 
                width = "100%", 
                height = "800px", 
                frameborder = 0
              )
      ),
      
      # ZIP Code Validator Tab
      tabItem(tabName = "zip_validator",
              tags$iframe(
                src = "http://127.0.0.1:4004", 
                width = "100%", 
                height = "800px", 
                frameborder = 0
              )
      ),

      # Date Logic Validator Tab
      tabItem(tabName = "date_validator",
              tags$iframe(
                src = "http://127.0.0.1:4005", 
                width = "100%", 
                height = "800px", 
                frameborder = 0
              )
      ),
      
      tabItem(tabName = "field_duplication",
              tags$iframe(
                src = "http://127.0.0.1:4006", 
                width = "100%", 
                height = "800px", 
                frameborder = 0
              )
      ),
      
      tabItem(tabName = "fuzzy_matching",
              tags$iframe(
                src = "http://127.0.0.1:4007", 
                width = "100%", 
                height = "800px", 
                frameborder = 0
              )
      ),
      
      # About Tab
      tabItem(tabName = "about",
              fluidRow(
                box(
                  title = "Project Details",
                  width = 12,
                  status = "info",
                  solidHeader = TRUE,
                  HTML(paste0("
              <h3>311 Service Request Data Anomaly Analysis</h3>
              <p>This project explores and validates 311 service request data for New York City.</p>
              <h4>Data Sources:</h4>
              <ul>
                <li>3-month 311 Service Request Dataset</li>
                <li>Period: 10/01/2024 - 12/31/2024</li>
                <li>Total Records: ", comma(nrow(cleaned_data)), "</li>
              </ul>
              <h4>Validation Tools:</h4>
              <ul>
                <li>Missing Values Analysis</li>
                <li>Schema Validation</li>
                <li>ZIP Code Verification</li>
              </ul>
            "))
                )
              )
      )
    )
  )
)

##############################################################################################
# Since no complex server logic is needed, we can use a minimal server function
server <- function(input, output, session) {}

##############################################################################################
# Run the application 
shinyApp(ui, server)

##############################################################################################
##############################################################################################