#########################################################################

# Daylight Saving Time Ends Shiny App

#########################################################################

library(shiny)
library(shinycssloaders)
library(ggplot2) 
library(data.table)
library(bslib)

#########################################################################
# Load Dataset from Local Data Directory
data_file <- file.path("data", "dataset.rds")

if (!file.exists(data_file)) {
  stop("\nData file not found in the 'data' directory.")
}

cleaned_data <- tryCatch({
  readRDS(data_file)
}, error = function(e) {
  stop("\nError loading dataset: ", e$message)
})

# Convert to data.table if not already
setDT(cleaned_data)
total_records <- nrow(cleaned_data)

# Calculate total records for November 2024
nov_2024_data <- cleaned_data[format(created_date, "%Y-%m") == "2024-11"]
nov_2024_count <- nrow(nov_2024_data)

#########################################################################
# Pre-compute data for faster response

# Data for November 3, 2024
dst_data <- cleaned_data[as.Date(created_date) == as.Date("2024-11-03"), 
                         .(created_hour = as.integer(format(created_date, "%H")),
                           created_minute = as.integer(format(created_date, "%M")),
                           created_second = as.integer(format(created_date, "%S")))]

# Hourly summary for November 3
data_summary <- dst_data[, .N, by = created_hour]

# Get data for all other November days (excluding Nov 3)
nov_data <- cleaned_data[format(created_date, "%Y-%m") == "2024-11" & 
                           as.Date(created_date) != as.Date("2024-11-03") &
                           as.integer(format(created_date, "%H")) == 1,
                         .N, by = .(date = as.Date(created_date))]

# Calculate statistics for 01:00 hour
mean_01 <- mean(nov_data$N, na.rm = TRUE)
sd_01 <- sd(nov_data$N, na.rm = TRUE)
count_01_1103 <- data_summary[created_hour == 1, sum(N, na.rm = TRUE)] # sum in case of multiple entries
z_value_01 <- (count_01_1103 - mean_01) / sd_01

# Store precomputed statistics
precomputed_stats <- list(
  mean = mean_01,
  sd = sd_01,
  count = count_01_1103,
  z_score = z_value_01
)

#########################################################################
# UI modifications
ui <- fluidPage(
  theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),
  titlePanel(HTML("<div style='color:steelblue; font-size:30px;'>
                  Daylight Saving Time Analysis (SR creation) - ends on 3 Nov 2024<br>
                  <span style='font-size:20px;'>At 02:00 clocks are turned back to 01:00
                  causing the 01:00-01:59 hour being repeated.</span><br><br>
                </div>")),
  
  fluidRow(
    column(12,
           div(
             style = "display: flex; justify-content: space-between; align-items: center;",
             actionButton("analyze", "Run Analysis", class = "btn-primary btn-lg"),
             div(
               textOutput("data_status"),
               style = "font-style: italic; color: gray; font-size: 0.9em;"
             )
           ),
           div(
             style = "margin-top: 10px; font-weight: bold; color: #333;",
             htmlOutput("record_counts")
           ),
           br(), br(),
           shinycssloaders::withSpinner(plotOutput("hourly_bar_chart"), type = 4),
           br(),
           wellPanel(
             h4("Statistical Analysis:"),
             shinycssloaders::withSpinner(htmlOutput("stats_summary"), type = 4)
           )
    )
  )
)

#########################################################################
# Server 
server <- function(input, output, session) {
  # Display pre-loading status
  output$data_status <- renderText({
    "Data pre-loaded and ready for fast display"
  })
  
  # Display record counts - both total and November 2024 specific
  output$record_counts <- renderUI({
    HTML(paste(
      sprintf("<span>Total records: %s</span>", format(total_records, big.mark = ",", scientific = FALSE)),
      sprintf("<span style='margin-left: 30px;'>November 2024 records: %s</span>", format(nov_2024_count, big.mark = ",", scientific = FALSE))
    ))
  })
  
  filtered_data <- reactiveVal(NULL)
  stats_data <- reactiveVal(NULL)
  
  observeEvent(input$analyze, {
    # Use pre-computed data
    filtered_data(data_summary)
    stats_data(precomputed_stats)
  })
  
  output$hourly_bar_chart <- renderPlot({
    req(input$analyze)
    req(filtered_data())
    req(stats_data())  # Ensure stats_data() is available before proceeding
    
    stats <- stats_data()
    mean_0100 <- stats$mean  # Extract mean for 01:00 hour
    
    plot_data <- filtered_data()
    
    # Convert created_hour to proper four-digit time format (0000, 0100, ..., 2300)
    plot_data[, created_hour := sprintf("%04d", as.integer(created_hour) * 100)]
    
    # Ensure only valid hourly intervals appear in the correct order
    valid_hours <- sprintf("%04d", seq(0, 2300, by = 100))
    plot_data[, created_hour := factor(created_hour, levels = valid_hours)]
    
    ggplot(plot_data, aes(x = created_hour, y = N)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      # Use geom_crossbar() to increase width of the black bar
      geom_crossbar(data = data.table(created_hour = "0100", mean_y = mean_0100),
                    aes(x = created_hour, ymin = mean_y, ymax = mean_y, y = mean_y),
                    inherit.aes = FALSE, color = "yellow", fill = "yellow", fatten = 5, width = 0.85) +  
      labs(x = "Hour of Day", 
           y = "Count", 
           title = "Hourly Distribution on 3 Nov 2024 (DST End)") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 0))
  })
  
  output$stats_summary <- renderText({
    req(stats_data())  # Ensure stats_data() is available
    stats <- stats_data()
    
    HTML(paste(
      sprintf("SR created count on November 3, 2024 @ 01:00:  %d<br>", stats$count),
      sprintf("Mean for other Nov days @ 01:00 (yellow bar):  %.1f<br>", stats$mean),
      sprintf("Standard Deviation: %.1f<br>", stats$sd),
      sprintf("Z-Score: %.2f<br>", stats$z_score)
    ))
  })
}

#########################################################################
# Run the Shiny App
shinyApp(ui, server)

#########################################################################