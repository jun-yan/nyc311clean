create_special_bar_chart <- function(
    dataset, 
    x_col, 
    y_col, 
    chart_title = "Bar Chart", 
    sub_title = "", 
    earliest_x_value, 
    max_x_value, 
    x_angle = 60, 
    y_axis_labels = scales::comma,
    chart_file_name = NULL,
    horizontal_adjustment_max = 0.5,
    vertical_adjustment_max = -1)    
{
  
  # Find the max y value
  y_max_count <- max(dataset[[y_col]])
  y_total_count <- sum(dataset[[y_col]], na.rm = TRUE)
  
  result <- calculate_values(y_max_count)
  starting_value <- result$starting_value
  increment <- result$increment
  
#  cat("\nStarting Value", starting_value, " Increment", increment, "\n")
  
  # Calculate mean, median, and standard deviation if requested
  y_mean_value <- round(mean(dataset[[y_col]]), 0)
  y_median_value <- round(median(dataset[[y_col]]), 0)
  y_sd_value <- round(sd(dataset[[y_col]]), 0)
  
  x_position <- levels(dataset[[x_col]])[floor(length(levels(dataset[[x_col]])) / 2)]
  
  # Create the ggplot chart
  special_bar_chart <- ggplot(dataset, aes(x = !!sym(x_col), y = !!sym(y_col))) +
    
    geom_bar(stat = "identity", fill = "#009E73") +
    
    scale_x_discrete() +
    
    scale_y_continuous(labels = y_axis_labels) +
    
    theme(
      axis.title = element_text(size = 7),
      plot.title = element_text(hjust = 0.5, size = 12),
      plot.subtitle = element_text(size = 6),
      panel.background = element_rect(fill = "gray95", color = "gray95"),
      axis.text.x = element_text(angle = x_angle, vjust = 0.5, hjust = 0.5, face = "bold"),
      axis.text.y = element_text(face = "bold"),
      aspect.ratio = 0.618033 ) +

    ggtitle(chart_title, subtitle = paste(sub_title, format(y_total_count, big.mark = ","), sep = "")) +
    
    geom_hline(yintercept = y_mean_value, linetype = "twodash", color = "black", 
               linewidth = 0.6 ) +
    
    annotate("text",
             x = x_position, y = y_mean_value,
             label = paste0("Avg: ", format(round(y_mean_value, 0), big.mark = ",")),
             size = 4.5, color = "black", hjust = -3, vjust =-0.8 ) +
    
    annotate("text",
             x = max_x_value, y = y_max_count,
             label = paste0("Max: ", format(y_max_count, big.mark = ","), sep = ""),
             size = 4.5, color = "black", 
             vjust = vertical_adjustment_max, hjust = horizontal_adjustment_max ) +
      
    geom_hline(
        yintercept = seq(starting_value, y_max_count, by=increment),
        linetype = "dotted", color = "gray40", linewidth = 0.3) +
    
    labs(x = NULL, y = NULL)
  
  # Save the plot with adjusted size and DPI
  # Define aspect ratio (golden ratio)
  chart_width <- 10  # Adjust to match manuscript
  chart_height <- chart_width / 1.618
  
  # Print the bar chart
  suppressMessages(print(special_bar_chart))
  
  ggsave(
    filename = chart_file_name,
    plot = special_bar_chart,
    path = chart_directory_path,
    width = chart_width,
    height = chart_height,
    dpi = 300 )  # High resolution for manuscripts
}
