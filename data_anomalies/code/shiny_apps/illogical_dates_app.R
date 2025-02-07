##############################################################################################
library(shiny)
library(shinycssloaders)
library(DT)
library(data.table)
library(lubridate)

# Configure options
options(shiny.port = 4005)
options(digits.secs = 3)

# Load and prepare data
data_path <- "C:/Users/David/OneDrive/Documents/datacleaningproject/data_anomalies/code/data/3-month_311SR_10-01-2024_thru_12-31-2024_AS_OF_02-02-2025.rds"
cleaned_data <- readRDS(data_path)
setDT(cleaned_data)

##############################################################################################
# Process date columns
date_cols <- c("created_date", "closed_date", "due_date", "resolution_action_updated_date")
for(col in date_cols) {
  if(col %in% names(cleaned_data)) {
    cleaned_data[, (col) := as.POSIXct(get(col), origin="1970-01-01", tz="UTC")]
  }
}

##############################################################################################
# Helper function for getting unique samples
get_unique_samples <- function(data, date1_col, date2_col = NULL, n = 5) {
  if (is.null(date2_col)) {
    # For single date samples
    samples <- unique(data[order(get(date1_col))][, .(
      date = format(get(date1_col), "%Y-%m-%d %H:%M:%S")
    )])[1:n]
    return(paste(samples$date, collapse = "; "))
  } else {
    # For date pair samples
    samples <- unique(data[order(get(date2_col))][, .(
      first_date = format(get(date1_col), "%Y-%m-%d %H:%M:%S"),
      second_date = format(get(date2_col), "%Y-%m-%d %H:%M:%S")
    )])[1:n]
    return(paste(mapply(function(d1, d2) paste0(d1, " -> ", d2),
                        samples$first_date, 
                        samples$second_date), 
                 collapse = "; "))
  }
}

##############################################################################################
validate_date_logic <- function(data) {
  results <- data.table(
    Rule = character(),
    Error_Type = character(),
    Count = numeric(),
    Percentage = numeric(),
    Sample_Values = character()
  )
  
  total_rows <- nrow(data)
  current_date <- Sys.Date()
  
  # 1. Check for negative duration (closed_date before created_date)
  invalid_closure <- data[!is.na(closed_date) & 
                            !is.na(created_date) & 
                            closed_date < created_date]
  
  if (nrow(invalid_closure) > 0) {
    results <- rbind(results, data.table(
      Rule = "Negative Duration",
      Error_Type = "closed_date precedes created_date",
      Count = nrow(invalid_closure),
      Percentage = nrow(invalid_closure) / total_rows,
      Sample_Values = get_unique_samples(invalid_closure, "created_date", "closed_date")
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
      Percentage = nrow(invalid_due) / total_rows,
      Sample_Values = get_unique_samples(invalid_due, "created_date", "due_date")
    ))
  }
  
  # 3. Check for closed_date in future
  future_closure <- data[!is.na(closed_date) & 
                           as.Date(closed_date) > current_date]
  
  if (nrow(future_closure) > 0) {
    results <- rbind(results, data.table(
      Rule = "Future Closure",
      Error_Type = "closed_date is in the future",
      Count = nrow(future_closure),
      Percentage = nrow(future_closure) / total_rows,
      Sample_Values = get_unique_samples(future_closure, "closed_date")
    ))
  }
  
  # 4. Check for resolution dates more than 365 days old
  old_resolution <- data[!is.na(resolution_action_updated_date) & 
                           resolution_action_updated_date < (created_date - days(365))]
  
  if (nrow(old_resolution) > 0) {
    results <- rbind(results, data.table(
      Rule = "Old Resolution",
      Error_Type = "resolution_action_updated_date > 365 days before created_date",
      Count = nrow(old_resolution),
      Percentage = nrow(old_resolution) / total_rows,
      Sample_Values = get_unique_samples(old_resolution, "created_date", "resolution_action_updated_date")
    ))
  }
  
  # 5. Check resolution dates before created dates
  invalid_resolution <- data[!is.na(resolution_action_updated_date) & 
                               resolution_action_updated_date < created_date]
  
  if (nrow(invalid_resolution) > 0) {
    results <- rbind(results, data.table(
      Rule = "Resolution Before Created",
      Error_Type = "resolution_date precedes created_date",
      Count = nrow(invalid_resolution),
      Percentage = nrow(invalid_resolution) / total_rows,
      Sample_Values = get_unique_samples(invalid_resolution, "created_date", "resolution_action_updated_date")
    ))
  }
  
  # 6. Check for due dates too far in future (> 1 year from creation)
  far_due_dates <- data[!is.na(due_date) & 
                          due_date > (created_date + days(365))]
  
  if (nrow(far_due_dates) > 0) {
    results <- rbind(results, data.table(
      Rule = "Far Future Due Date",
      Error_Type = "due_date > 1 year after created_date",
      Count = nrow(far_due_dates),
      Percentage = nrow(far_due_dates) / total_rows,
      Sample_Values = get_unique_samples(far_due_dates, "created_date", "due_date")
    ))
  }
  
  # 7. Check for resolution dates in future
  future_resolution <- data[!is.na(resolution_action_updated_date) & 
                              as.Date(resolution_action_updated_date) > current_date]
  
  if (nrow(future_resolution) > 0) {
    results <- rbind(results, data.table(
      Rule = "Future Resolution",
      Error_Type = "resolution_date is in the future",
      Count = nrow(future_resolution),
      Percentage = nrow(future_resolution) / total_rows,
      Sample_Values = get_unique_samples(future_resolution, "resolution_action_updated_date")
    ))
  }
  
  # 8. Check for missing closed dates with CLOSED status
  missing_closure <- data[status == "CLOSED" & is.na(closed_date)]
  
  if (nrow(missing_closure) > 0) {
    results <- rbind(results, data.table(
      Rule = "Missing Closure Date",
      Error_Type = "status is CLOSED but closed_date is missing",
      Count = nrow(missing_closure),
      Percentage = nrow(missing_closure) / total_rows,
      Sample_Values = get_unique_samples(missing_closure, "created_date")
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
      Percentage = nrow(zero_duration) / total_rows,
      Sample_Values = get_unique_samples(zero_duration, "created_date")
    ))
  }
  
  # 10. Check for multiple status updates on same day
  # Create a copy of the data with just the relevant columns
  status_updates <- data[!is.na(resolution_action_updated_date), 
                         .(unique_key, date = as.Date(resolution_action_updated_date))]
  
  # Count updates per service request per day
  multiple_updates <- status_updates[, .N, by = .(unique_key, date)][N > 1]
  
  if (nrow(multiple_updates) > 0) {
    # Format sample values using a different approach since this is a special case
    sample_cases <- unique(multiple_updates[order(-N)])[1:5]
    sample_text <- paste(mapply(function(id, dt, n) 
      sprintf("ID: %d, Date: %s, Updates: %d", id, dt, n),
      sample_cases$unique_key, sample_cases$date, sample_cases$N), 
      collapse = "; ")
    
    results <- rbind(results, data.table(
      Rule = "Multiple Daily Updates",
      Error_Type = "multiple status updates on the same day",
      Count = nrow(multiple_updates),
      Percentage = nrow(multiple_updates) / total_rows,
      Sample_Values = sample_text
    ))
  }
  
  if (nrow(results) == 0) {
    results <- data.table(
      Rule = "All Dates",
      Error_Type = "No Logic Violations",
      Count = 0,
      Percentage = 0,
      Sample_Values = "N/A"
    )
  }
  
  # 11. Check for SQL default dates (1900-01-01)
  sql_default_date <- as.POSIXct("1900-01-01", tz="UTC")
  
  for (date_col in c("created_date", "closed_date", "due_date", "resolution_action_updated_date")) {
    sql_defaults <- data[!is.na(get(date_col)) & 
                           as.Date(get(date_col)) == as.Date(sql_default_date)]
    
    if (nrow(sql_defaults) > 0) {
      results <- rbind(results, data.table(
        Rule = paste("SQL Default Date in", date_col),
        Error_Type = "date value is 1900-01-01",
        Count = nrow(sql_defaults),
        Percentage = nrow(sql_defaults) / total_rows,
        Sample_Values = get_unique_samples(sql_defaults, date_col)
      ))
    }
  }
  
  # 12. Check for created_dates at exactly midnight
  midnight_created <- data[!is.na(created_date) & 
                             format(created_date, "%H:%M:%S") == "00:00:00"]
  
  if (nrow(midnight_created) > 0) {
    results <- rbind(results, data.table(
      Rule = "Midnight Created Date",
      Error_Type = "created_date is exactly at 00:00:00",
      Count = nrow(midnight_created),
      Percentage = nrow(midnight_created) / total_rows,
      Sample_Values = get_unique_samples(midnight_created, "created_date")
    ))
  }
  
  # 13. Check for closed_dates at exactly midnight
  midnight_closed <- data[!is.na(closed_date) & 
                            format(closed_date, "%H:%M:%S") == "00:00:00"]
  
  if (nrow(midnight_closed) > 0) {
    results <- rbind(results, data.table(
      Rule = "Midnight Closed Date",
      Error_Type = "closed_date is exactly at 00:00:00",
      Count = nrow(midnight_closed),
      Percentage = nrow(midnight_closed) / total_rows,
      Sample_Values = get_unique_samples(midnight_closed, "closed_date")
    ))
  }
  
  return(results)
}

##############################################################################################
ui <- fluidPage(
  theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),
  
  titlePanel("311 Date Logic Validator"),
  
  fluidRow(
    column(12,
           actionButton("validate", "Validate Date Logic", class = "btn-primary btn-lg"),
           br(), br(),
           shinycssloaders::withSpinner(DTOutput("validation_results"), type = 4)
    )
  )
)

##############################################################################################
server <- function(input, output, session) {
  validation_results <- reactiveVal(NULL)
  is_loading <- reactiveVal(FALSE)
  
  observeEvent(input$validate, {
    is_loading(TRUE)
    validation_results(NULL)
    
    req(cleaned_data)
    validation_output <- isolate(validate_date_logic(cleaned_data))
    validation_results(validation_output)
    is_loading(FALSE)
  })
  
  output$validation_results <- renderDT({
    if(is_loading()) return(NULL)
    req(validation_results())
    
    datatable(validation_results(),
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