##############################################################################################

########## ILLOGICAL DATES APP ###########

##############################################################################################
library(shiny)
library(shinycssloaders)
library(DT)
library(data.table)
library(lubridate)
library(bslib)

# Configure options
options(shiny.port = 4005)
options(digits.secs = 3)

##############################################################################################
# Load and prepare data
# 1. Ensure your app directory structure looks like this:
# app_directory/
# ├── app.R
# └── data/
#     └── dataset.rds

# 2. Modify your file loading code to be more robust:
data_file <- file.path("data", "dataset.rds")

if (file.exists(data_file)) {
  cleaned_data <- tryCatch({
    readRDS(data_file)
  }, error = function(e) {
    stop("Error loading dataset: ", e$message)
  })
} else {
  # Try an alternative path as a fallback
  alt_data_file <- file.path(getwd(), "data", "dataset.rds")
  message("Primary path failed. Trying alternative path: ", normalizePath(alt_data_file, mustWork = FALSE))
  
  if (file.exists(alt_data_file)) {
    cleaned_data <- tryCatch({
      readRDS(alt_data_file)
    }, error = function(e) {
      stop("Error loading dataset from alternative path: ", e$message)
    })
    message("Dataset successfully loaded from alternative path!")
  } else {
    stop("Data file not found in either the 'data' directory or working directory/data.")
  }
}

setDT(cleaned_data)  # Convert to data.table for better performance

##############################################################################################
validate_date_logic <- function(data) {
  results <- data.table(
    Rule = character(),
    Error_Type = character(),
    Count = numeric(),
    Percentage = numeric()
  )
  
  total_rows <- nrow(data)
  
  # Use the max_closed_date from the dataset attributes
  max_closed_date <- attr(data, "max_closed_date")
  
  if (is.null(max_closed_date)) {
    # If attribute is missing, calculate it (fallback)
    max_closed_date <- as.Date(Sys.Date())  # or use a specific cutoff date
    warning("max_closed_date attribute not found, using current date as fallback")
  }
  
  # 1. Check for negative duration (closed_date before created_date)
  invalid_closure <- data[!is.na(closed_date) & 
                            closed_date < created_date]
  
  if (nrow(invalid_closure) > 0) {
    results <- rbind(results, data.table(
      Rule = "Negative Duration",
      Error_Type = "closed_date precedes created_date",
      Count = nrow(invalid_closure),
      Percentage = nrow(invalid_closure) / total_rows
    ))
  }
  
  # 2. Check for due_date before created_date
  invalid_due <- data[!is.na(due_date) & 
                        !is.na(created_date) & 
                        due_date < created_date]
  
  if (nrow(invalid_due) > 0) {
    results <- rbind(results, data.table(
      Rule = "Due Before Created",
      Error_Type = "due_date precedes created_date",
      Count = nrow(invalid_due),
      Percentage = nrow(invalid_due) / total_rows
    ))
  }
  
  # 3. Check for closed_date in future
  future_closure <- data[!is.na(closed_date) & 
                           as.Date(closed_date) > max_closed_date]
  
  if (nrow(future_closure) > 0) {
    results <- rbind(results, data.table(
      Rule = "Future Closure",
      Error_Type = "closed_date is in the future",
      Count = nrow(future_closure),
      Percentage = nrow(future_closure) / total_rows
    ))
  }
  
  # 4. Check for resolution dates more than 90 days after closed date
  old_resolution <- data[!is.na(resolution_action_updated_date) & 
                           !is.na(closed_date) & 
                           resolution_action_updated_date > (closed_date + days(90))]
  
  if (nrow(old_resolution) > 0) {
    results <- rbind(results, data.table(
      Rule = "Late Resolution Update",
      Error_Type = "resolution_action_updated_date >90 days after closed_date",
      Count = nrow(old_resolution),
      Percentage = nrow(old_resolution) / total_rows
    ))
  }
  
  # 5. Check resolution dates before created dates
  invalid_resolution <- data[!is.na(resolution_action_updated_date) & 
                               resolution_action_updated_date < created_date]
  
  if (nrow(invalid_resolution) > 0) {
    results <- rbind(results, data.table(
      Rule = "Resolution Update Before Created",
      Error_Type = "resolution_update_date precedes created_date",
      Count = nrow(invalid_resolution),
      Percentage = nrow(invalid_resolution) / total_rows
    ))
  }
  
  # 6. Check for due dates too far in future (> 1 year from creation)
  far_due_dates <- data[!is.na(due_date) & 
                          due_date > (created_date + days(365))]
  
  if (nrow(far_due_dates) > 0) {
    results <- rbind(results, data.table(
      Rule = "Distant Future Due Date",
      Error_Type = "due_date >1 year after created_date",
      Count = nrow(far_due_dates),
      Percentage = nrow(far_due_dates) / total_rows
    ))
  }
  
  # 7. Check for resolution dates in future
  future_resolution <- data[!is.na(resolution_action_updated_date) &
                              as.Date(resolution_action_updated_date) > max_closed_date]
  # cat("\nMax closed date\n")
  # print(max_closed_date)
  # print(nrow(future_resolution))
  
  if (nrow(future_resolution) > 0) {
    results <- rbind(results, data.table(
      Rule = "Future Resolution Update",
      Error_Type = "resolution_date is in the future",
      Count = nrow(future_resolution),
      Percentage = nrow(future_resolution) / total_rows
    ))
  }
  
  # 8. Check for missing closed dates with CLOSED status
  missing_closure <- data[status == "CLOSED" & is.na(closed_date)]
  
  if (nrow(missing_closure) > 0) {
    results <- rbind(results, data.table(
      Rule = "Missing Closed Date",
      Error_Type = "status is CLOSED but closed_date missing",
      Count = nrow(missing_closure),
      Percentage = nrow(missing_closure) / total_rows
    ))
  }
  
  # 9. Check for zero duration (created_date equals closed_date)
  zero_duration <- data[!is.na(closed_date) & 
                          created_date == closed_date]
  
  if (nrow(zero_duration) > 0) {
    results <- rbind(results, data.table(
      Rule = "Zero Duration",
      Error_Type = "created_date equals closed_date",
      Count = nrow(zero_duration),
      Percentage = nrow(zero_duration) / total_rows
    ))
  }
  
  if (nrow(results) == 0) {
    results <- data.table(
      Rule = "All Dates",
      Error_Type = "No Logic Violations",
      Count = 0,
      Percentage = 0
    )
  }
  
  # 10. Check for SQL default dates (1900-01-01)
  sql_default_date <- as.POSIXct("1900-01-01", tz="UTC")
  
  for (date_col in c("created_date", "closed_date", "due_date", "resolution_action_updated_date")) {
    sql_defaults <- data[!is.na(get(date_col)) & 
                           as.Date(get(date_col)) == as.Date(sql_default_date)]
    
    if (nrow(sql_defaults) > 0) {
      results <- rbind(results, data.table(
        Rule = paste("SQL Default Date in", date_col),
        Error_Type = "date value is 1900-01-01",
        Count = nrow(sql_defaults),
        Percentage = nrow(sql_defaults) / total_rows
      ))
    }
  }
  
  # 11. Check for created_dates at exactly midnight
  midnight_created <- data[!is.na(created_date) & 
                             format(created_date, "%H:%M:%S") == "00:00:00"]
  
  if (nrow(midnight_created) > 0) {
    results <- rbind(results, data.table(
      Rule = "Midnight Created Date",
      Error_Type = "created_date is exactly at Midnight (00:00:00)",
      Count = nrow(midnight_created),
      Percentage = nrow(midnight_created) / total_rows
    ))
  }
  
  # 12. Check for closed_dates at exactly midnight
  midnight_closed <- data[!is.na(closed_date) & 
                            format(closed_date, "%H:%M:%S") == "00:00:00"]
  
  if (nrow(midnight_closed) > 0) {
    results <- rbind(results, data.table(
      Rule = "Midnight Closed Date",
      Error_Type = "closed_date is exactly at Midnight (00:00:00)",
      Count = nrow(midnight_closed),
      Percentage = nrow(midnight_closed) / total_rows
    ))
  }
  
  # 13. Check for closed date present but status not CLOSED
  has_date_not_closed <- data[!is.na(closed_date) & status != "CLOSED"]
  
  if (nrow(has_date_not_closed) > 0) {
    results <- rbind(results, data.table(
      Rule = "Inconsistent Closure Status",
      Error_Type = "closed_date exists but status is not CLOSED",
      Count = nrow(has_date_not_closed),
      Percentage = nrow(has_date_not_closed) / total_rows
    ))
  }
  
  return(results)
}

##############################################################################################
# Pre-compute results for faster response
pre_computed_results <- validate_date_logic(cleaned_data)
total_records <- nrow(cleaned_data)

##############################################################################################
# UI
ui <- fluidPage(
  title = "Illogical Dates Validator",
  
  theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),
  
  titlePanel("311 Date Logic Validator"),
  
  fluidRow(
    column(12,
           div(
             style = "display: flex; justify-content: space-between; align-items: center;",
             actionButton("validate", "Validate Date Logic", class = "btn-primary btn-lg"),
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
           shinycssloaders::withSpinner(DTOutput("validation_results"), type = 4)
    )
  )
)

##############################################################################################
# Server
server <- function(input, output, session) {
  # Display pre-loading status
  output$data_status <- renderText({
    "Data pre-loaded and ready for fast display"
  })
  
  # Display total record count
  output$record_count <- renderText({
    paste("Total records:", format(total_records, big.mark = ",", scientific = FALSE))
  })
  
  # Create a reactive to handle the validation process
  validation_data <- eventReactive(input$validate, {
    # Use pre-computed results for faster response
    pre_computed_results
  }, ignoreNULL = FALSE)
  
  # Render the output
  output$validation_results <- renderDT({
    # Only proceed if validation has been triggered and has results
    req(input$validate)
    results <- validation_data()
    req(results)
    
    datatable(results,
              rownames = FALSE,
              options = list(
                pageLength = 25,
                order = list(list(2, 'desc'))  # Sort by Count column descending
              )) %>%
      formatCurrency("Count", currency = "", interval = 3, mark = ",", digits = 0) %>%
      formatPercentage("Percentage", digits = 2)
  })
}

##############################################################################################
shinyApp(ui, server)

##############################################################################################