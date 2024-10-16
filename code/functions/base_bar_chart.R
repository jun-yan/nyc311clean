#########################################################################

base_bar_chart <- function(dataset, x_col, y_col, chart_title, sub_title, 
                           y_axis_labels = scales::comma, 
                           add_maximum = FALSE, add_minimum = FALSE, 
                           add_mean = FALSE, add_median = FALSE, 
                           add_sd = FALSE, add_second_maximum = FALSE,
                           add_trendline = FALSE, extra_line = NULL,
                           horizontal_adjustment_max = 0.5,
                           vertical_adjustment_max = -1,
                           console_print_out_title = "Data Summary",
                           chart_file_name = NULL) {
  
  # Step 1: Print Data Summary
  cat("\n\n", console_print_out_title, " (first 20 rows):\n", sep = "")
  dataset <- as.data.frame(dataset)
  print(head(dataset[, c(x_col, y_col)], 20), row.names = FALSE)
  
  # Sort dataset by y_col (descending) and x_col based on type
  if (inherits(dataset[[x_col]], "Date") || inherits(dataset[[x_col]], "POSIXct")) {
    sorted_dataset <- dataset[order(-dataset[[y_col]], dataset[[x_col]]), ]
  } else if (is.factor(dataset[[x_col]])) {
    sorted_dataset <- dataset[order(-dataset[[y_col]]), ]
  } else {
    sorted_dataset <- dataset[order(-dataset[[y_col]], -dataset[[x_col]]), ]
  }
  
  # Calculate total count for y_col
  y_total_count <- sum(dataset[[y_col]], na.rm = TRUE)
  
  # Step 2: Set up x_scale based on x_col type
  if (inherits(dataset[[x_col]], "Date")) {
    x_scale <- scale_x_date(expand = c(0.01, 0), labels = scales::date_format("%Y-%m"), breaks = scales::date_breaks("6 months"))
  } else if (inherits(dataset[[x_col]], "POSIXct") || inherits(dataset[[x_col]], "POSIXt")) {
    x_scale <- scale_x_datetime(expand = c(0.01, 0), labels = scales::date_format("%Y-%m"), breaks = scales::date_breaks("6 months"))
  } else if (is.numeric(dataset[[x_col]])) {
    x_scale <- scale_x_continuous(expand = c(0.01, 0))
  } else {
    x_scale <- scale_x_discrete(expand = c(0.01, 0))
  }
  
  # Step 3: Create the base ggplot object
  bar_chart <- ggplot(sorted_dataset, aes(x = .data[[x_col]], y = .data[[y_col]])) +
    geom_bar(stat = "identity", fill = "#44AA99", na.rm = TRUE) +
    x_scale +
    scale_y_continuous(labels = y_axis_labels) +
    theme(
      axis.title = element_blank(),  # Remove x and y axis titles
      plot.title = element_text(hjust = 0.5, size = 12),
      plot.subtitle = element_text(size = 7),
      axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1, face = "bold", size = 8),
      axis.text.y = element_text(face = "bold", size = 8),
      aspect.ratio = 0.618033
    ) +
    
    # Set the subtitle with total count included
    ggtitle(chart_title, subtitle = paste(sub_title, format(y_total_count, big.mark = ","), sep = " ")) +
    
    # Remove x and y axis labels
    labs(x = NULL, y = NULL)  
  
  # Step 4: Build the plot to extract y-axis breaks
  built_plot <- ggplot_build(bar_chart)
  y_breaks <- built_plot$layout$panel_params[[1]]$y$get_breaks()
  y_breaks <- y_breaks[!is.na(y_breaks)]  # Filter out any NA values from y_breaks
  
  # Add hlines using the Y-axis breaks
  bar_chart <- bar_chart +
    geom_hline(yintercept = y_breaks, linetype = "dotted", color = "gray35", linewidth = 0.5)
  
  # Step 5-6: Conditional annotations, printing, and saving (unchanged) ...
  
  # Print and save the chart
  print(bar_chart)
  if (!is.null(chart_file_name)) {
    chart_path <- file.path(chart_directory_path, chart_file_name)
    ggsave(chart_path, plot = bar_chart, width = 10, height = 10 / 1.618, dpi = 300)
  }
}


#########################################################################