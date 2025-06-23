################################################################################
# Function to create violin plot for an agency
create_agency_violin <- function(agency_exploration, agency_name) {
  
  # Get thresholds from the exploration results
  thresholds <- agency_exploration$thresholds
  
  plot_data <- agency_exploration$agency_data[, .(response_time)]
  
  # Create violin chart with manual positioning for log scale
  violin_chart <- ggplot(plot_data, aes(x = response_time, y = "")) +
    
    geom_violin(alpha = 0.7, trim = FALSE, fill = "steelblue3") +
    
    geom_boxplot(width = 0.1, alpha = 0.8, outlier.alpha = 0.3) +
    
     # MEAN: Diamond at correct statistical position (manually calculated)
    annotate("point", x = mean(plot_data$response_time, na.rm = TRUE), y = 1, 
             shape = 23, size = 3, fill = "darkred", color = "darkred") +
    
    # MEAN: Diamond shape for legend (positioned at mean value)
    annotate("point", x = mean(plot_data$response_time, na.rm = TRUE), y = 1.5, 
             shape = 23, size = 3, fill = "darkred", color = "darkred") +
    
    # MEAN: Text positioned at mean value (slightly offset for readability)
    annotate("text", x = mean(plot_data$response_time, na.rm = TRUE), y = 1.5,
             label = paste("Mean:", round(mean(plot_data$response_time, 
                                               na.rm = TRUE), 2), "days"), 
             vjust = 0.3, hjust = -0.1, size = 3, color = "darkred", 
             fontface = "bold") +
    
    # MEDIAN: Text positioned directly above the median line (using stat_summary)
    stat_summary(fun = median, geom = "text", 
                 aes(label = paste("Median:", round(after_stat(x), 2), "days")), 
                 vjust = -3.75, hjust = 0.5, size = 3, color = "black", 
                 fontface = "bold") +
    
    scale_x_log10(labels = scales::comma_format()) +
    
    labs(title = paste(agency_name, "Response Time Distribution (Violin Plot)"),
         subtitle = paste("Log scale showing all response times (days). | Total records:", 
                          format(nrow(plot_data), big.mark = ",")),
         x = "Distribution",
         y = "") +
    
    theme_minimal() +
    theme(plot.title = element_text(size = 12, hjust = 0.5),
          plot.subtitle = element_text(size = 10, hjust = 0.5),
          panel.background = element_rect(fill = "grey93"),
          panel.grid.major = element_line(color = "white", linewidth = 0.75),
          panel.grid.minor = element_blank(),
          axis.text.y = element_text(face = "bold"),
          axis.text.x = element_text(face = "bold"),
          legend.position = "none")
  
  tryCatch({
    print(violin_chart)
    Sys.sleep(2)
    
    # Create safe filename using the robust function
    safe_name <- create_safe_filename(agency_name)
    
    filename <- file.path(distro_dir, paste0(safe_name, "_violin.png"))
    ggsave(filename, plot = violin_chart, width = 13, height = 8.5, 
           device = "png", dpi = 300, bg = "white")
    
  }, error = function(e) {
    cat("Error creating plot for", agency_name, ":", e$message, "\n")
  })
  
  return(violin_chart)
}