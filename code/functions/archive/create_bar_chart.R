#########################################################################

create_bar_chart <- function(
    dataset,
    x_col,
    y_col,
    chart_title = "Bar Chart",       # Default title
    sub_title = "",                   # Default empty subtitle
    x_axis_title = NULL,              # Default empty x-axis title
    y_axis_title = NULL,              # Default empty y-axis title
    print_out_title = "Data Summary", # Default title for console output
    add_mean = FALSE,
    add_median = FALSE,
    add_sd = FALSE,
    add_trendline = FALSE,
    add_maximum = FALSE,
    add_minimum = FALSE,
    add_second_maximum = FALSE,
    extra_line = NULL,
    chart_file_name = NULL,
    horizontal_adjustment_max = 1,
    vertical_adjustment_max = -1)            # Default NULL means no file saving)
{
  # Find the row with the maximum count
  max_row <- dataset[which.max(dataset[[y_col]]), ]
  y_max_count <- max_row[[y_col]]
  y_total_count <- sum(dataset[[y_col]], na.rm = TRUE)
  
  # Find the row with the minimum count
  min_row <- dataset[which.min(dataset[[y_col]]), ]
  y_min_count <- round(min_row[[y_col]], 0)
  
  # result <- calculate_values(y_max_count)
  # starting_value <- result$starting_value
  # increment <- result$increment
  
  # Calculate mean, median, and standard deviation if requested
  y_mean_value <- round(mean(dataset[[y_col]]), 0)
  y_median_value <- round(median(dataset[[y_col]]), 0)
  y_sd_value <- round(sd(dataset[[y_col]]), 0)
  
  # # Print the date and count for maximum and minimum values
  cat("\n\n***", print_out_title, "SRs***")
  # print_out_title <- substr(print_out_title, 1, 1) %>%
  #   paste(., substr(print_out_title, 2, nchar(print_out_title)), sep = "")
  
  cat(
    "\n", paste("Maximum", print_out_title, ":"), as.character(max_row[[x_col]]), "  ",
    paste("Maximum", print_out_title, "count:"), format(y_max_count, big.mark = ",")
  )
  
  cat(
    "\n", paste("Minimum", print_out_title, ":"), as.character(min_row[[x_col]]), "  ",
    paste("Minimum", print_out_title, "count: "), format(y_min_count, big.mark = ","))
  
  cat("\n")
  cat(
    paste("\nAverage ", print_out_title, ":", sep = ""), format(y_mean_value, big.mark = ","), "  ",
    paste("\nMedian ", print_out_title, ":", sep = ""), format(y_median_value, big.mark = ","),
    paste("\nStd Dev (\u03C3) ", print_out_title, ":", sep = ""), format(y_sd_value, big.mark = ","), "\n"
  )
  
  # Check if x_col is of Date type
  if (inherits(dataset[[x_col]], "Date") || inherits(dataset[[x_col]], "POSIXct") || inherits(dataset[[x_col]], "POSIXt")) {
    scale_x <- scale_x_date(expand = c(0.01, 0), labels = scales::date_format("%Y-%m"), breaks = scales::date_breaks("6 months"))
  } else {
    scale_x <- scale_x_continuous(expand = c(0.01, 0))
  }
  
  # Create the bar chart
  bar_chart <- ggplot(dataset, aes(x = .data[[x_col]], y = .data[[y_col]])) +
    
    geom_bar(stat = "identity", fill = "#44AA99") +
    
    scale_x +
    
    theme(
      axis.title = element_text(size = 7),
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, face = "bold", size = 8),
      axis.text.y = element_text(face = "bold", size = 7),
      plot.title = element_text(hjust = 0.5, size = 12),
      plot.subtitle = element_text(size = 6),
      aspect.ratio = 0.618033 ) +
    
    # geom_hline(
    #   yintercept = seq(starting_value, y_max_count, by = increment),
    #   linetype = "dotted", color = "gray40", linewidth = 0.3) +
    
    ggtitle(chart_title, subtitle = paste(sub_title, format(y_total_count, big.mark = ","), sep = "")) +
    labs(x = x_axis_title, y = y_axis_title)
  
  # Build the plot to extract y-axis breaks
  built_plot <- ggplot_build(bar_chart)
  
  # Extract Y-axis breaks (try different possible locations)
  y_breaks <- built_plot$layout$panel_params[[1]]$y$get_breaks()
  
  # Add hlines using the Y-axis breaks
  bar_chart <- bar_chart +
    geom_hline(yintercept = y_breaks, linetype = "dotted", color = "gray35", linewidth = 0.5)
  
  if (add_maximum) {
    bar_chart <- bar_chart +
      annotate("text",
               x = max_row[[x_col]], y = y_max_count,
               label = paste0("Max: ", format(y_max_count, big.mark = ",")),
               size = 4.25, color = "black", 
               vjust = vertical_adjustment_max, hjust = horizontal_adjustment_max)
  }
  
  if (add_second_maximum) {
    # Order the data frame by the count column in descending order
    ordered_by_count <- dataset[order(dataset[[y_col]], decreasing = TRUE), ]
    
    # Select the second row
    second_max <- ordered_by_count[2, ]
    
    bar_chart <- bar_chart +
      annotate("text",
               x = second_max[[x_col]], y = second_max[[y_col]],
               label = paste0("2^nd~'Highest: ", format(second_max[[y_col]], big.mark = ","), "'"), ,
               size = 4.25, color = "black", vjust = -0.4, hjust = 0.05, parse = TRUE )
  }
  
  if (add_minimum) {
    bar_chart <- bar_chart +
      annotate("text",
               x = min_row[[x_col]], y = y_min_count,
               label = paste0("Min: ", format(y_min_count, big.mark = ",")),
               size = 4.25, color = "black", vjust = -0.4, hjust = 0.5 )
  }
  
  if (add_mean) {
    bar_chart <- bar_chart +
      geom_hline(yintercept = y_mean_value, linetype = "twodash", color = "black", linewidth = 0.75) +
      annotate("text",
               x = min(dataset[[x_col]]), y = y_mean_value,
               label = paste0("Avg: ", format(round(y_mean_value, 0), big.mark = ",")),
               size = 4.5, color = "black", hjust = -0.5, vjust = -0.75 )
  }
  
  if (add_median) {
    bar_chart <- bar_chart +
      geom_hline(yintercept = y_median_value, linetype = "twodash", color = "black", linewidth = 0.6) +
      annotate("text",
               x = min(dataset[[x_col]]), y = y_median_value,
               label = paste0("Median: ", format(round(y_median_value, 0), big.mark = ",")),
               size = 4.5, color = "black", hjust = -0.5, vjust = -0.75 )
  }
  
  if (add_sd) {
    bar_chart <- bar_chart +
      geom_hline( yintercept = round(y_mean_value + 3 * y_sd_value, 0), 
                  linetype = "longdash", color = "black", linewidth = 0.3 ) +
      annotate("text",
               x = min(dataset[[x_col]]), y = y_mean_value + 3 * y_sd_value,
               label = "+3 sigma", size = 4, color = "black", hjust = -0.5, vjust = -0.75 )
  }
  
  if (add_trendline) {
    bar_chart <- bar_chart +
    stat_poly_eq(color = "#661100") +
    geom_smooth(
    method = "lm", span = 1, se = FALSE, color = "#661100",
    linetype = "dashed", linewidth = 1.3 )
  }
  
  if (!is.null(extra_line)) {
    bar_chart <- bar_chart + extra_line
  }
  
  # Print the bar chart
    print(bar_chart)
  
  # Save the chart to a file if chart_file_name is provided
    chart_path <- file.path(chart_directory_path, chart_file_name)
    
  # Define aspect ratio (golden ratio)
    chart_width <- 10  # Adjust to match manuscript
    chart_height <- chart_width / 1.618
    
  # Save the plot with adjusted size and DPI
    ggsave(
      filename = chart_file_name,
      plot = bar_chart,
      path = chart_directory_path,
      width = chart_width,
      height = chart_height,
      dpi = 300 )  # High resolution for manuscripts
}

#########################################################################