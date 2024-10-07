#########################################################################

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
    vertical_adjustment_max = -1,
    console_print_out_title = "Data Summary")
{
  
  # Call the new function to calculate and print statistics
  stats <- calculate_and_print_statistics(dataset, x_col, y_col, console_print_out_title)
  
  # Use the returned values in your charting logic
  y_max_count <- stats$y_max_count
  y_min_count <- stats$y_min_count
  y_mean_value <- stats$y_mean_value
  y_median_value <- stats$y_median_value
  y_sd_value <- stats$y_sd_value
  max_row <- stats$max_row
  min_row <- stats$min_row
  y_total_count <- stats$y_total_count
  
  x_position <- levels(dataset[[x_col]])[floor(length(levels(dataset[[x_col]])) / 2)]
  
  # Create the ggplot chart
  special_bar_chart <- ggplot(dataset, aes(x = !!sym(x_col), y = !!sym(y_col))) +
    
    geom_bar(stat = "identity", fill = "#44AA99") +
    
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
      
    labs(x = NULL, y = NULL)
  
  # Build the plot to extract y-axis breaks
  built_plot <- ggplot_build(special_bar_chart)
  
  # Extract Y-axis breaks (try different possible locations)
  y_breaks <- built_plot$layout$panel_params[[1]]$y$get_breaks()
  
  # Add hlines using the Y-axis breaks
  special_bar_chart <- special_bar_chart +
    geom_hline(yintercept = y_breaks, linetype = "dotted", color = "gray35", linewidth = 0.5)
  
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

#########################################################################
