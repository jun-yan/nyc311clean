################################################################################
# Function to create visualization for an agency
create_agency_plot <- function(agency_exploration, agency_name) {
  
  # Get thresholds from the exploration results
  thresholds <- agency_exploration$thresholds
  
  plot_data <- agency_exploration$agency_data[, .(response_time)]
  plot_data[, category := ifelse(response_time <= 
                                   thresholds$reasonable_days, "Reasonable", 
                                 ifelse(response_time <= 
                                          thresholds$long_days, "Long",
                                        ifelse(response_time <= 
                                                 thresholds$very_long_days, 
                                               "Very Long", "Extremely Long")))]
  
  # Set factor levels to control legend order
  plot_data[, category := factor(category, 
                                 levels = c("Reasonable", "Long", 
                                            "Very Long", "Extremely Long"))]
  
  # Add counts to category labels for the legend
  category_counts <- table(plot_data$category)
  category_labels <- paste0(names(category_counts), " (", 
                            as.character(category_counts), ")")
  names(category_labels) <- names(category_counts)
  
  # Check the data first
  cat("\nPlot data summary for", agency_name, ":\n")
  formatted_plot_summary <- round(summary(plot_data$response_time), 4)
  print(formatted_plot_summary)
  
  cat("Category counts:\n")
  print(table(plot_data$category))
  
  # Create plot
  bar_chart <- ggplot(plot_data, aes(x = response_time, fill = category)) +
    
    geom_histogram(bins = 50, alpha = 0.7) +
    
    geom_vline(xintercept = mean(plot_data$response_time), color = "grey40", 
               linetype = "dashed", linewidth = 1) +
    
    geom_vline(xintercept = mean(plot_data$response_time)
               + 3*sd(plot_data$response_time), 
               color = "orange3", linetype = "dotted", linewidth = 1) +
    
    annotate("label", x = mean(plot_data$response_time), y = Inf, 
             label = paste("Mean:", round(mean(plot_data$response_time), 2), 
                           "days"), 
             vjust = 1.5, hjust = -0.1, size = 3.5, color = "grey40", 
             fontface = "bold", fill = "grey93", label.size = 0) +
    
    annotate("label", x = mean(plot_data$response_time)
             + 3*sd(plot_data$response_time), 
             y = Inf, 
             label = paste("+3σ:", round(mean(plot_data$response_time) + 
                                           3 * sd(plot_data$response_time), 3), 
                           "days"), 
             vjust = 3.25, hjust = -0.1, size = 3.5, 
             color = "orange3", fontface = "bold", fill = "grey93", 
             label.size = 0) +
    
    scale_fill_manual(
      values = c(
        "Reasonable"       = "#66c2a5",  # Green-teal (Normal)
        "Long"             = "#f6e8c3",  # Soft blue-green
        "Very Long"        = "#fdae61",  # Gold-orange
        "Extremely Long"   = "#d73027"   # Brick red
      ),
      labels = category_labels
    ) +
    
    labs(title = paste(agency_name, "Response Time Distribution"),
         subtitle = paste("Reasonable: ≤", thresholds$reasonable_days, 
                          "days | Long: ≤", thresholds$long_days, 
                          "days | Very Long: ≤", thresholds$very_long_days, 
                          "days | Extreme: >", thresholds$very_long_days, 
                          "days | Total records:", 
                          format(nrow(plot_data), big.mark = ",")),
         x = "Response Time (Days)",
         y = "Complaint Count",
         fill = "Category") +
    
    theme_minimal() +
    theme(plot.title = element_text(size = 12, hjust = 0.5),
          plot.subtitle = element_text(size = 10, hjust = 0.5),
          panel.background = element_rect(fill = "grey93"),
          panel.grid.major = element_line(color = "white", linewidth = 0.75),
          panel.grid.minor = element_blank(),
          axis.text.y = element_text(face = "bold"),
          axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
          legend.position = "inside",
          legend.position.inside = c(0.98, 0.98),
          legend.justification = c(1, 1),
          legend.background = element_rect(fill = "grey98", color = "black", 
                                           linewidth = 0.5),
          legend.margin = margin(5, 5, 5, 5))
  
  tryCatch({
    print(bar_chart)
    Sys.sleep(2)
    
    # Create safe filename using the robust function
    safe_name <- create_safe_filename(agency_name)
    
    filename <- file.path(distro_dir, paste0(safe_name, "_histogram.png"))
    ggsave(filename, plot = bar_chart, width = 13, height = 8.5, 
           device = "png", dpi = 300, bg = "white")
    
  }, error = function(e) {
    cat("Error creating plot for", agency_name, ":", e$message, "\n")
  })
  
  return(bar_chart)
}