#########################################################################
# Daylight Saving Time Shiny App
#########################################################################

library(shiny)
library(shinycssloaders)
library(ggplot2)
library(data.table)

#########################################################################
# Load Dataset from Local Data Directory
data_file <- file.path("data", "dataset.RDS")

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


#########################################################################
# UI modifications
ui <- fluidPage(
  theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),
  titlePanel(HTML("<div style='color:steelblue; font-size:30px;'>
                  Daylight Saving Time Analysis (SR creation) - ends on 3 Nov 2024<br>
                  <span style='font-size:20px;'>At 02:00 clocks are turned back to 01:00
                  causing the 01:00-02:00 hour being repeated.</span><br><br>
                </div>")),
  
  fluidRow(
    column(12,
           actionButton("analyze", "Run Analysis", class = "btn-primary btn-lg"),
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
# Server modifications
server <- function(input, output, session) {
  filtered_data <- reactiveVal(NULL)
  stats_data <- reactiveVal(NULL)
  
  observeEvent(input$analyze, {
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
    count_01_1103 <- data_summary[created_hour == 1, sum(N)] # sum in case of multiple entries
    z_value_01 <- (count_01_1103 - mean_01) / sd_01
    
    # Store statistics
    stats_data(list(
      mean = mean_01,
      sd = sd_01,
      count = count_01_1103,
      z_score = z_value_01
    ))
    
    filtered_data(data_summary)
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
    
    # Add a small artificial delay to make the spinner visible
    Sys.sleep(0.5)
    
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
