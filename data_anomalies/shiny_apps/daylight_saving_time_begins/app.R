#########################################################################

########## Daylight Saving Times Begin ##########

#########################################################################

library(shiny)
library(shinycssloaders)
library(ggplot2)
library(data.table)
library(bslib)

#########################################################################
# Load Dataset
data_file <- file.path("data", "dataset.rds")

if (file.exists(data_file)) {
  cleaned_data <- tryCatch({
    readRDS(data_file)
  }, error = function(e) {
    stop("Error loading dataset: ", e$message)
  })
} else {
  stop("Data file not found in the 'data' directory.")
}

# Convert to data.table if not already
setDT(cleaned_data)
total_records <- nrow(cleaned_data)

# Calculate total records for March 2024
march_2024_data <- cleaned_data[format(created_date, "%Y-%m") == "2024-03"]
march_2024_count <- nrow(march_2024_data)

#########################################################################
# Precompute Data Before App Starts

# Data for March 10, 2024 (DST Begins)
dst_data <- cleaned_data[as.Date(created_date) == as.Date("2024-03-10"), 
                         .(created_hour = as.integer(format(created_date, "%H")),
                           created_minute = as.integer(format(created_date, "%M")),
                           created_second = as.integer(format(created_date, "%S")))]

# Hourly summary for March 10
data_summary <- dst_data[, .N, by = created_hour]

# Ensure all hours exist in data_summary
all_hours <- data.table(created_hour = 0:23)
data_summary <- merge(all_hours, data_summary, by = "created_hour", all.x = TRUE)
data_summary[is.na(N), N := 0]  # Replace NA counts with 0

# Get data for all other March days (excluding March 10)
mar_data_0200 <- cleaned_data[format(created_date, "%Y-%m") == "2024-03" & 
                                as.Date(created_date) != as.Date("2024-03-10") &
                                as.integer(format(created_date, "%H")) == 2,
                              .N, by = .(date = as.Date(created_date))]

mar_data_0300 <- cleaned_data[format(created_date, "%Y-%m") == "2024-03" & 
                                as.Date(created_date) != as.Date("2024-03-10") &
                                as.integer(format(created_date, "%H")) == 3,
                              .N, by = .(date = as.Date(created_date))]

# Precompute statistics
mean_0200 <- mean(mar_data_0200$N, na.rm = TRUE)
sd_0200 <- sd(mar_data_0200$N, na.rm = TRUE)
count_0200_0310 <- sum(data_summary[created_hour == 2, N], na.rm = TRUE)  # Should be 0
z_value_0200 <- (count_0200_0310 - mean_0200) / sd_0200

mean_0300 <- mean(mar_data_0300$N, na.rm = TRUE)
sd_0300 <- sd(mar_data_0300$N, na.rm = TRUE)
count_0300_0310 <- sum(data_summary[created_hour == 3, N], na.rm = TRUE)
z_value_0300 <- (count_0300_0310 - mean_0300) / sd_0300

# Store precomputed statistics in a list
precomputed_stats <- list(
  mean_0200 = mean_0200,
  sd_0200 = sd_0200,
  count_0200 = count_0200_0310,
  z_score_0200 = z_value_0200,
  mean_0300 = mean_0300,
  sd_0300 = sd_0300,
  count_0300 = count_0300_0310,
  z_score_0300 = z_value_0300
)

#########################################################################
# UI modifications
ui <- fluidPage(
  title = "Beginning Daylight Saving Time", 
  theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),
  titlePanel(HTML("<div style='color:steelblue; font-size:30px;'>
                  Daylight Saving Time Analysis (SR creation) - Begins 10 Mar 2024<br>
                  <span style='font-size:20px;'>At 02:00 clocks are advanced to 03:00,
                  causing the 02:00-02:59 hour to disappear.</span><br><br>
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
# Server modifications
server <- function(input, output, session) {
  # Display pre-loading status
  output$data_status <- renderText({
    "Data pre-loaded and ready for fast display"
  })
  
  # Display record counts - both total and March 2024 specific
  output$record_counts <- renderUI({
    HTML(paste(
      sprintf("<span>Total records: %s</span>", format(total_records, big.mark = ",", scientific = FALSE)),
      sprintf("<span style='margin-left: 30px;'>March 2024 records: %s</span>", format(march_2024_count, big.mark = ",", scientific = FALSE))
    ))
  })
  
  # Create a reactive expression for the data that copies the original data
  # This ensures we always work with a fresh copy of the data
  plot_data <- eventReactive(input$analyze, {
    # Make a deep copy of the data to avoid modification issues
    copy(data_summary)
  })
  
  # Similar approach for stats data
  stats_data <- eventReactive(input$analyze, {
    # Return a copy of the precomputed stats to avoid modification issues
    list(
      mean_0200 = precomputed_stats$mean_0200,
      sd_0200 = precomputed_stats$sd_0200,
      count_0200 = precomputed_stats$count_0200,
      z_score_0200 = precomputed_stats$z_score_0200,
      mean_0300 = precomputed_stats$mean_0300,
      sd_0300 = precomputed_stats$sd_0300,
      count_0300 = precomputed_stats$count_0300,
      z_score_0300 = precomputed_stats$z_score_0300
    )
  })
  
  output$hourly_bar_chart <- renderPlot({
    req(plot_data())
    req(stats_data())
    
    stats <- stats_data()
    pd <- plot_data()
    
    # Convert created_hour to proper four-digit time format
    pd[, hour_label := sprintf("%04d", created_hour * 100)]
    
    # Ensure only valid hourly intervals appear in the correct order
    valid_hours <- sprintf("%04d", seq(0, 2300, by = 100))
    pd[, hour_label := factor(hour_label, levels = valid_hours)]
    
    ggplot(pd, aes(x = hour_label, y = N)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      # Black bar for missing 02:00 hour
      geom_crossbar(data = data.table(hour_label = "0200", mean_y = stats$mean_0200),
                    aes(x = hour_label, ymin = mean_y, ymax = mean_y, y = mean_y),
                    inherit.aes = FALSE, color = "black", fill = "black", fatten = 8, width = 0.95) +  
      # Yellow bar for expected 03:00 hour
      geom_crossbar(data = data.table(hour_label = "0300", mean_y = stats$mean_0300),
                    aes(x = hour_label, ymin = mean_y, ymax = mean_y, y = mean_y),
                    inherit.aes = FALSE, color = "yellow", fill = "yellow", fatten = 5, width = 0.85) +  
      labs(x = "Hour of Day", 
           y = "Count", 
           title = "Hourly Distribution on 10 Mar 2024 (DST Begin)") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 0))
  })
  
  output$stats_summary <- renderText({
    req(stats_data())
    stats <- stats_data()
    
    HTML(paste(
      sprintf("<b>SR created count on March 10, 2024 @ 02:00:</b>  %d (Should be 0)<br>", stats$count_0200),
      sprintf("Expected Mean based on other days in March @ 02:00 (black bar):  %.1f<br>", stats$mean_0200),
      sprintf("<br><b>SR created count on March 10, 2024 @ 03:00:</b>  %d<br>", stats$count_0300),
      sprintf("Expected Mean based on other days in March @ 03:00 (yellow bar):  %.1f<br>", stats$mean_0300),
      sprintf("Standard Deviation: %.1f<br>", stats$sd_0300),
      sprintf("Z-Score: %.2f<br>", stats$z_score_0300)
    ))
  })
}

#########################################################################
# Run the Shiny App
shinyApp(ui, server)

#########################################################################