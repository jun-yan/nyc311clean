# CLEAN VERSION - Replace the top of your app.R with this:

# Set CRAN mirror for package installation on shinyapps.io
options(repos = c(CRAN = "https://cran.rstudio.com/"))

# Check for required packages and install missing ones
required_packages <- c("shiny", "shinydashboard", "data.table")

for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    cat("Installing package:", pkg, "\n")
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

# MODIFIED SETUP FUNCTION - Only copy histogram files
setup_www_folder <- function() {
  if (!dir.exists("www")) {
    dir.create("www")
  }
  
  # Only copy histogram PNG files (not violin files)
  www_files <- list.files("www", pattern = "*_histogram.png")
  if (length(www_files) == 0) {
    # Only get histogram files
    histogram_files <- list.files(pattern = "*_histogram.png", full.names = FALSE)
    if (length(histogram_files) > 0) {
      for (file in histogram_files) {
        if (file.exists(file)) {
          file.copy(file, file.path("www", file), overwrite = TRUE)
        }
      }
      cat("Copied", length(histogram_files), "histogram files to www folder\n")
    }
  } else {
    cat("www folder already has", length(www_files), "histogram files - skipping copy\n")
  }
}

# Call setup function
setup_www_folder()

# Load your data
agg_data <- readRDS("nyc_service_requests_aggregated.rds")

# Continue with your UI and server code...
# (keep everything else the same)

# Load the pre-aggregated data
agg_data <- readRDS(file.path("nyc_service_requests_aggregated.rds"))

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "NYC Service Requests Explorer"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    )
  ),
  dashboardBody(
    tabItems(
      # Dashboard tab
      tabItem(tabName = "dashboard",
              fluidRow(
                box(width = 12, title = "Filters", status = "primary", solidHeader = TRUE,
                    column(width = 4,
                           selectInput("agency_filter", "Agency:", 
                                       choices = c("All", sort(unique(agg_data[dim_agency != "All", dim_agency]))),
                                       selected = "All")
                    ),
                    column(width = 4,
                           selectInput("complaint_filter", "Complaint Type:", 
                                       choices = c("All", sort(unique(agg_data[dim_complaint_type != "All", dim_complaint_type]))),
                                       selected = "All")
                    ),
                    column(width = 4,
                           selectInput("borough_filter", "Borough:", 
                                       choices = c("All", sort(unique(agg_data[dim_borough != "All", dim_borough]))),
                                       selected = "All")
                    )
                )
              ),
              fluidRow(
                tabBox(
                  width = 12,
                  tabPanel("Summary Statistics", 
                           div(style = 'overflow-x: scroll', DT::dataTableOutput("stats_table"))),
                  tabPanel("Time Trend",
                           fluidRow(
                             column(width = 12,
                                    box(width = 12, title = "Time Trend Options", status = "info", solidHeader = TRUE,
                                        column(width = 6,
                                               checkboxInput("show_citywide", "Show Citywide Average", value = FALSE)
                                        )
                                    )
                             )
                           ),
                           fluidRow(
                             column(width = 12,
                                    plotOutput("time_plot")
                             )
                           )
                  ),
                  tabPanel("Distribution", 
                           fluidRow(
                             column(width = 12,
                                    box(width = 12, title = "Distribution Options", status = "info", solidHeader = TRUE,
                                        column(width = 4,
                                               selectInput("dist_item", "Item:", 
                                                           choices = c("Borough", 
                                                             "Agency", 
                                                             "Complaint Type"),
                                                           selected = "Borough")
                                        ),
                                        column(width = 4,
                                               selectInput("dist_selection", "Selection:", 
                                                           choices = NULL)
                                        ),
                                        column(width = 4,
                                               br(),
                                               actionButton("search_button", "Search", 
                                                            class = "btn-primary", 
                                                            style = "margin-top: 5px;")
                                        )
                                    )
                             )
                           ),
                           fluidRow(
                             column(width = 12,
                                    uiOutput("chart_display")
                             )
                           )
                  )
                ) 
              ) 
      ), 
      
      # About tab
      tabItem(tabName = "about",
              box(width = 12,
                  title = "About this Dashboard",
                  status = "info",
                  solidHeader = TRUE,
                  "This dashboard provides an interface to explore NYC service request response times across various dimensions.",
                  tags$br(), tags$br(),
                  "The data has been pre-aggregated across multiple dimensions including agency, complaint type, borough, year, and month.",
                  tags$br(), tags$br(),
                  "You can use the filters to narrow down to specific combinations and view both tabular statistics and time trends.",
                  tags$br(), tags$br(),
                  "All response times are displayed in days. The Distribution tab allows you to view pre-generated histogram and violin plot charts for boroughs and agencies."
              )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Reactive expression for filtered data (for main dashboard)
  filtered_data <- reactive({
    # Start with full dataset
    data_subset <- agg_data
    
    # Apply Agency filter if not "All"
    if (input$agency_filter != "All") {
      data_subset <- data_subset[dim_agency == input$agency_filter | dim_agency == "All"]
    }
    
    # Apply Complaint Type filter if not "All"
    if (input$complaint_filter != "All") {
      data_subset <- data_subset[dim_complaint_type == input$complaint_filter | dim_complaint_type == "All"]
    }
    
    # Apply Borough filter if not "All"
    if (input$borough_filter != "All") {
      data_subset <- data_subset[dim_borough == input$borough_filter | dim_borough == "All"]
    }
    
    # Always show year-based data (removed time dimension logic)
    data_subset <- data_subset[dim_year != "All"]
    
    # Return the filtered data
    data_subset
  })
  
  # DISTRIBUTION TAB SERVER LOGIC
  
  # MODIFIED COMPLAINT TYPE DROPDOWN - Only check for histogram files
  observeEvent(input$dist_item, {
    if (input$dist_item == "Borough") {
      choices <- c("All", "Brooklyn", "Bronx", "Manhattan", "Staten_Island", "Queens")
    } else if (input$dist_item == "Agency") {
      choices <- sort(unique(agg_data[dim_agency != "All", dim_agency]))
    } else if (input$dist_item == "Complaint Type") {
      
      cat("\n=== BUILDING COMPLAINT TYPE CHOICES (HISTOGRAM ONLY) ===\n")
      
      # Get all histogram PNG files in www folder
      all_histogram_files <- list.files("www", pattern = "*_histogram.png")
      
      # Filter out borough and agency files
      known_non_complaint <- c("ALL", "BROOKLYN", "BRONX", "MANHATTAN", "STATEN_ISLAND", "QUEENS", 
                               "DCWP", "DEP", "DFTA", "DHS", "DOB", "DOE", "DOHMH", "DOT", 
                               "DPR", "DSNY", "EDC", "HPD", "NYPD", "OSE", "OTI", "TLC")
      
      complaint_files <- all_histogram_files[!gsub("_histogram.png", "", all_histogram_files) %in% known_non_complaint]
      
      # Extract base names (remove _histogram.png)
      complaint_base_names <- gsub("_histogram.png", "", complaint_files)
      
      cat("Found complaint type histogram files:", length(complaint_base_names), "\n")
      
      # Match back to complaint types in your data
      all_complaint_types <- unique(agg_data[dim_complaint_type != "All", dim_complaint_type])
      
      complaint_types_with_files <- c()
      
      for (complaint in all_complaint_types) {
        # Try multiple filename conversion approaches
        filename1 <- toupper(complaint)
        filename1 <- gsub("[^A-Z0-9]", "_", filename1)
        filename1 <- gsub("_+", "_", filename1)
        filename1 <- gsub("^_|_$", "", filename1)
        
        filename2 <- toupper(complaint)
        filename2 <- gsub("[/\\\\:*?\"<>|]", "_", filename2)
        
        filename3 <- toupper(complaint)
        filename3 <- gsub(" ", "_", filename3)
        
        # Check if any of these approaches match existing histogram files
        if (filename1 %in% complaint_base_names || filename2 %in% complaint_base_names || filename3 %in% complaint_base_names) {
          complaint_types_with_files <- c(complaint_types_with_files, complaint)
        }
      }
      
      cat("Total complaint types with histogram files:", length(complaint_types_with_files), "\n")
      cat("======================================================\n")
      
      choices <- sort(complaint_types_with_files)
      
      if (length(choices) == 0) {
        choices <- "No complaint types available"
      }
    }
    
    updateSelectInput(session, "dist_selection", 
                      choices = choices,
                      selected = if(length(choices) > 0 && choices[1] != "No complaint types available") choices[1] else NULL)
  })
  
  
  
  
  # MODIFIED SEARCH LOGIC - Only display histogram, no violin plot
  observeEvent(input$search_button, {
    output$chart_display <- renderUI({
      if (is.null(input$dist_selection) || input$dist_selection == "") {
        return(div("Please make a selection and click Search."))
      }
      
      if (input$dist_selection == "No complaint types available") {
        return(div("No complaint type charts are currently available", 
                   style = "color: orange; text-align: center; padding: 20px;"))
      }
      
      # Convert selection to filename
      if (input$dist_item == "Complaint Type") {
        complaint <- input$dist_selection
        
        # Try the same three approaches as above
        filename1 <- toupper(complaint)
        filename1 <- gsub("[^A-Z0-9]", "_", filename1)
        filename1 <- gsub("_+", "_", filename1)
        filename1 <- gsub("^_|_$", "", filename1)
        
        filename2 <- toupper(complaint)
        filename2 <- gsub("[/\\\\:*?\"<>|]", "_", filename2)
        
        filename3 <- toupper(complaint)
        filename3 <- gsub(" ", "_", filename3)
        
        # Check which one has histogram file
        item_name <- NULL
        for (test_name in c(filename1, filename2, filename3)) {
          hist_file <- file.path("www", paste0(test_name, "_histogram.png"))
          if (file.exists(hist_file)) {
            item_name <- test_name
            break
          }
        }
        
        if (is.null(item_name)) {
          item_name <- filename1  # fallback
        }
        
        cat("Using filename:", item_name, "for complaint:", complaint, "\n")
      } else {
        item_name <- toupper(gsub(" ", "_", input$dist_selection))
      }
      
      # Only check for histogram file
      histogram_file <- paste0(item_name, "_histogram.png")
      hist_exists <- file.exists(file.path("www", histogram_file))
      
      cat("Looking for:", histogram_file, "exists:", hist_exists, "\n")
      
      if (!hist_exists) {
        return(div(
          paste("Histogram not found for:", input$dist_selection),
          br(),
          paste("Looking for file:", histogram_file),
          style = "color: red; text-align: center; padding: 20px;"
        ))
      }
      
      # Display only histogram
      return(div(
        div(h3("Response Time Distribution"), style = "text-align: center; margin-top: 20px;"),
        div(style = "text-align: center;",
            tags$img(src = histogram_file,
                     style = "max-width: 90%; height: auto; border: 1px solid #ddd; margin: 10px;"))
      ))
    })
  })
  
  # Output for stats table
  output$stats_table <- DT::renderDataTable({
    # Get filtered data
    data <- filtered_data()
    
    # Define columns to show (only days)
    day_cols <- c("mean_days", "median_days", "min_days", "max_days", "sd_days")
    
    # Make a copy of the data for display
    display_data <- copy(data)
    
    # Define base columns
    base_cols <- c("dim_agency", "dim_complaint_type", "dim_borough", "dim_year", "count")
    
    # Filter to columns that exist in the data
    base_cols <- base_cols[base_cols %in% names(display_data)]
    time_cols <- day_cols[day_cols %in% names(display_data)]
    
    # Select columns for display
    final_cols <- c(base_cols, time_cols)
    result_data <- display_data[, ..final_cols]
    
    # Round numeric columns to 4 decimal places
    for (col in time_cols) {
      result_data[[col]] <- round(result_data[[col]], 4)
    }
    
    # Rename the time columns for display
    name_map <- c(
      "mean_days" = "Mean (Days)",
      "median_days" = "Median (Days)",
      "min_days" = "Min (Days)",
      "max_days" = "Max (Days)",
      "sd_days" = "StdDev (Days)"
    )
    
    for (old_name in time_cols) {
      if (old_name %in% names(name_map)) {
        new_name <- name_map[[old_name]]
        setnames(result_data, old_name, new_name)
      }
    }
    
    # Create the datatable
    dt <- DT::datatable(
      result_data,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        autoWidth = TRUE
      ),
      rownames = FALSE
    )
    
    # Format the numeric columns
    formatted_cols <- as.character(name_map[time_cols])
    for (col in formatted_cols) {
      dt <- DT::formatRound(dt, columns = col, digits = 4)
    }
    
    return(dt)
  })
  
  # Output for time plot
  output$time_plot <- renderPlot({
    data <- filtered_data()
    
    # Handle special categories
    is_special_category <- input$complaint_filter %in% 
      c("NOISE (All Types)", "STREET (All Issues)", "HOMELESS (All Issues)")
    
    # Handle filtering logic based on selection
    if (is_special_category) {
      if (input$agency_filter != "All") {
        plot_data <- data[dim_complaint_type == input$complaint_filter & 
                            dim_borough != "All" & 
                            dim_year != "All"]
        
        if (nrow(plot_data) == 0) {
          return(ggplot() + 
                   annotate("text", x = 0.5, y = 0.5, 
                            label = paste0("No data available for ", input$complaint_filter, ".\n",
                                           "Try selecting a different complaint type.")) + 
                   theme_void())
        }
      } else {
        plot_data <- data[dim_complaint_type == input$complaint_filter & 
                            dim_borough != "All" & 
                            dim_year != "All"]
      }
    }
    else if (input$agency_filter != "All" && input$complaint_filter != "All") {
      plot_data <- data[dim_agency == input$agency_filter & 
                          dim_complaint_type == input$complaint_filter & 
                          dim_borough != "All" & 
                          dim_year != "All"]
      
      if (nrow(plot_data) == 0) {
        return(ggplot() + 
                 annotate("text", x = 0.5, y = 0.5, 
                          label = paste0("No data available for ", input$complaint_filter, 
                                         " complaints handled by ", input$agency_filter, ".\n",
                                         "Try selecting a different agency or complaint type.")) + 
                 theme_void())
      }
    }
    else if (input$agency_filter != "All") {
      plot_data <- data[dim_agency == input$agency_filter & 
                          dim_complaint_type == "All" &
                          dim_borough != "All" & 
                          dim_year != "All"]
    }
    else if (input$complaint_filter != "All") {
      plot_data <- data[dim_agency == "All" & 
                          dim_complaint_type == input$complaint_filter & 
                          dim_borough != "All" & 
                          dim_year != "All"]
    }
    else {
      plot_data <- data[dim_agency == "All" & 
                          dim_complaint_type == "All" & 
                          dim_borough != "All" & 
                          dim_year != "All"]
    }
    
    # Handle borough filter
    if (input$borough_filter != "All") {
      plot_data <- plot_data[dim_borough == input$borough_filter]
    }
    
    # Check if we have data
    if (nrow(plot_data) == 0) {
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, 
                        label = "No data available for the selected filters.\nTry selecting different filter combinations.") + 
               theme_void())
    }
    
    # Get title components
    agency_title <- ifelse(input$agency_filter != "All", input$agency_filter, "All Agencies")
    complaint_title <- ifelse(input$complaint_filter != "All", input$complaint_filter, "All Complaints")
    
    # Use median response time in days
    y_col <- "median_days"
    y_label <- "Median Response Time (Days)"
    
    # Convert year to numeric
    plot_data$dim_year_num <- as.numeric(as.character(plot_data$dim_year))
    
    # Create the base plot
    p <- ggplot(plot_data, aes(x = dim_year_num, y = .data[[y_col]], 
                               color = dim_borough, group = dim_borough)) +
      geom_line(linewidth = 1) +
      geom_point(size = 3) +
      geom_text(aes(label = round(.data[[y_col]], 1)), 
                vjust = -0.8, hjust = 0.5, size = 3.5, show.legend = FALSE)
    
    # ENHANCED CITYWIDE LINE LOGIC
    if (input$show_citywide) {
      # Start with the full aggregated dataset
      citywide_data <- agg_data
      
      # Apply the same filters as the main plot, but keep borough = "All"
      if (input$agency_filter != "All") {
        citywide_data <- citywide_data[dim_agency == input$agency_filter]
      } else {
        citywide_data <- citywide_data[dim_agency == "All"]
      }
      
      if (input$complaint_filter != "All") {
        citywide_data <- citywide_data[dim_complaint_type == input$complaint_filter]
      } else {
        citywide_data <- citywide_data[dim_complaint_type == "All"]
      }
      
      # Get citywide data (where borough = "All")
      citywide_summary <- citywide_data[dim_borough == "All" & dim_year != "All"]
      
      if (nrow(citywide_summary) > 0) {
        citywide_summary$dim_year_num <- as.numeric(as.character(citywide_summary$dim_year))
        
        # Add citywide line and points
        p <- p +
          geom_line(data = citywide_summary, 
                    aes(x = dim_year_num, y = .data[[y_col]]), 
                    color = "gray40", linewidth = 1.5, linetype = "dashed",
                    inherit.aes = FALSE) +
          geom_point(data = citywide_summary,
                     aes(x = dim_year_num, y = .data[[y_col]]),
                     color = "gray40", size = 3, shape = 17,
                     inherit.aes = FALSE) +
          geom_text(data = citywide_summary,
                    aes(x = dim_year_num, y = .data[[y_col]], 
                        label = paste0("City: ", round(.data[[y_col]], 1))),
                    vjust = 1.5, hjust = 0.5, size = 3.5, color = "gray40", fontface = "bold",
                    inherit.aes = FALSE)
      }
    }
    
    # Update the subtitle to indicate when citywide line is shown
    subtitle_text <- paste0(agency_title, " - ", complaint_title)
    if (input$show_citywide) {
      subtitle_text <- paste0(subtitle_text, " (Black dashed line = Citywide average)")
    }
    
    p <- p +
      labs(title = paste0("Median Response Time by Borough and Year"),
           subtitle = subtitle_text,
           x = "",
           y = y_label,
           color = "Borough") +
      scale_x_continuous(breaks = sort(unique(plot_data$dim_year_num))) +
      theme_minimal() +
      theme(
        legend.position = "bottom",
        legend.background = element_rect(fill = "grey98", color = "black", linewidth = 0.6),
        legend.margin = margin(10, 10, 10, 10),
        legend.box.spacing = unit(0.5, "cm"),
        legend.text = element_text(margin = margin(t = 2, b = 2)),
        legend.key.height = unit(1.2, "lines"),
        legend.key.width = unit(1.5, "lines"),
        plot.title = element_text(face = "bold"),
        axis.text.x = element_text(face = "bold"),
        axis.text.y = element_text(face = "bold"),
        panel.background = element_rect(fill = "gray93"),
        panel.grid.major = element_line(color = "white", linewidth = 0.75),
        panel.grid.minor = element_blank(),
        plot.subtitle = element_text(size = 10, face = "italic", margin = margin(b = 15))
      ) +
      guides(
        color = guide_legend(
          override.aes = list(
            linewidth = 1, 
            size = 3, 
            linetype = "solid",
            shape = 16
          )
        ),
        linewidth = "none",
        size = "none",
        linetype = "none"
      )
    
    return(p)
  })
  
  # Update complaint type choices based on agency selection (for main dashboard)
  observeEvent(input$agency_filter, {
    if (input$agency_filter == "All") {
      all_complaints <- unique(agg_data[dim_complaint_type != "All", dim_complaint_type])
      complaint_choices <- c("All", sort(all_complaints))
    } else {
      relevant_complaints <- unique(agg_data[dim_agency == input$agency_filter & 
                                               dim_complaint_type != "All", dim_complaint_type])
      
      special_categories <- c()
      
      noise_complaints <- relevant_complaints[grepl("^NOISE", relevant_complaints)]
      if (length(noise_complaints) > 0) {
        special_categories <- c(special_categories, "NOISE (All Types)")
      }
      
      street_complaints <- relevant_complaints[grepl("^STREET", relevant_complaints)]
      if (length(street_complaints) > 0) {
        special_categories <- c(special_categories, "STREET (All Issues)")
      }
      
      homeless_complaints <- relevant_complaints[grepl("^HOMELESS", relevant_complaints)]
      if (length(homeless_complaints) > 0) {
        special_categories <- c(special_categories, "HOMELESS (All Issues)")
      }
      
      complaint_choices <- c("All", special_categories, sort(relevant_complaints))
    }
    
    updateSelectInput(session, "complaint_filter", 
                      choices = complaint_choices,
                      selected = if (input$complaint_filter %in% complaint_choices) 
                        input$complaint_filter else "All")
  }) 
  
  # Update borough choices based on agency and complaint selection (for main dashboard)
  observeEvent(c(input$agency_filter, input$complaint_filter), {
    data_subset <- agg_data[dim_borough != "All"]
    
    if (input$agency_filter != "All") {
      data_subset <- data_subset[dim_agency == input$agency_filter]
    }
    
    if (input$complaint_filter != "All") {
      data_subset <- data_subset[dim_complaint_type == input$complaint_filter]
    }
    
    borough_choices <- c("All", sort(unique(data_subset$dim_borough)))
    
    updateSelectInput(session, "borough_filter", 
                      choices = borough_choices,
                      selected = if (input$borough_filter %in% borough_choices) 
                        input$borough_filter else "All")
  })
  
}

################################################################################
# Run the application 
shinyApp(ui = ui, server = server)

################################################################################