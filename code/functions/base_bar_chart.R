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
                           chart_file_name = NULL,
                           chart_directory = ".",
                           chart_width = 10,
                           chart_height = 7,
                           annotation_size = 5) {
  
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
      axis.title = element_blank(),
      plot.title = element_text(hjust = 0.5, size = 12),
      axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1, face = "bold", size = 9),
      axis.text.y = element_text(face = "bold", size = 8),
      panel.background = element_rect(fill = "gray100", color = "gray100"),
      aspect.ratio = 0.618033
    ) +
    labs(x = NULL, y = NULL)  
  
  # Step 4: Add y-axis breaks
  built_plot <- ggplot_build(bar_chart)
  y_breaks <- built_plot$layout$panel_params[[1]]$y$get_breaks()
  y_breaks <- y_breaks[!is.na(y_breaks)]
  bar_chart <- bar_chart +
    geom_hline(yintercept = y_breaks, linetype = "dotted", color = "gray35", linewidth = 0.5)
  
  # Step 5: Add conditional annotations
  y_mean_value <- round(mean(dataset[[y_col]], na.rm = TRUE), 0)
  y_median_value <- round(median(dataset[[y_col]], na.rm = TRUE), 0)
  y_sd_value <- round(sd(dataset[[y_col]], na.rm = TRUE), 0)
  max_row <- dataset[which.max(dataset[[y_col]]), ]
  y_max_count <- max_row[[y_col]]
  
  if (add_mean) {
    bar_chart <- bar_chart +
      geom_hline(yintercept = y_mean_value, linetype = "twodash", color = "black", linewidth = 0.75) +
      annotate("text", x = min(sorted_dataset[[x_col]]), y = y_mean_value,
               label = paste0("Avg: ", format(y_mean_value, big.mark = ",")),
               size = annotation_size, color = "black", hjust = -0.5, vjust = -0.75)
  }
  
  if (add_maximum) {
    bar_chart <- bar_chart +
      annotate("text", x = max_row[[x_col]], y = y_max_count,
               label = paste0("Max: ", format(y_max_count, big.mark = ",")),
               size = annotation_size, color = "black", vjust = vertical_adjustment_max, hjust = horizontal_adjustment_max)
  }
  
  if (!is.null(chart_file_name)) {
    # Print or save the chart as needed
    print(bar_chart)
    chart_path <- file.path(chart_directory, chart_file_name)
    ggsave(chart_path, plot = bar_chart, width = chart_width, 
           height = chart_height, dpi = 300)
  }
}

#########################################################################