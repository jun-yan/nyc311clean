base_bar_chart <- function(
    dataset, 
    x_col, 
    y_col, 
    chart_title = "Bar Chart", 
    sub_title = "",
    y_axis_labels = scales::comma,
    add_maximum = FALSE, 
    add_minimum = FALSE,
    add_mean = FALSE, 
    add_median = FALSE, 
    add_sd = FALSE,
    add_second_maximum = FALSE,
    add_trendline = FALSE,
    extra_line = NULL,
    horizontal_adjustment_max = 0.5,
    vertical_adjustment_max = -1,
    console_print_out_title = "Data Summary",
    rows_to_print = 20,
    chart_file_name = NULL,
    chart_directory = ".",
    chart_width = 10,
    chart_height = 7,
    annotation_size = 5,
    x_axis_tick_size = 12,
    x_axis_label_angle = 90,
    x_axis_tick_length = unit(0.3, "cm"),
    x_label_every = 1  # NEW PARAMETER: show every nth x-axis label
) {
  
  # Step 0: Add percentage and cumulative percentage columns with 2 decimal points
  dataset <- dataset %>%
    mutate(
      percentage = round(.data[[y_col]] / sum(.data[[y_col]]) * 100, 2),
      cumulative_percentage = round(cumsum(.data[[y_col]]) / sum(.data[[y_col]]) * 100, 2)
    )
  
  rows_to_print <- min(nrow(dataset), rows_to_print)
  
  # Step 1: Print Data Summary
  cat("\n\n", console_print_out_title, " (first ", rows_to_print, " rows):\n", sep = "")
  dataset <- as.data.frame(dataset)
  
  # Temporarily increase max.print to handle larger output
  old_max_print <- getOption("max.print")
  options(max.print = max(1000, rows_to_print * 4))
  
  # Print the first 'rows_to_print' rows of all relevant columns
  print(dataset[1:rows_to_print, c(x_col, y_col, "percentage", "cumulative_percentage")], row.names = FALSE)
  
  # Restore the original max.print value
  options(max.print = old_max_print)
  
  # Step 2: Sort dataset by y_col (descending) and x_col based on type
  if (inherits(dataset[[x_col]], "Date") || inherits(dataset[[x_col]], "POSIXct")) {
    sorted_dataset <- dataset[order(-dataset[[y_col]], dataset[[x_col]]), ]
  } else if (is.factor(dataset[[x_col]])) {
    sorted_dataset <- dataset[order(-dataset[[y_col]]), ]
  } else if (is.character(dataset[[x_col]]) && grepl("-", dataset[[x_col]][1])) {
    # Extract numeric part of day_info for sorting
    day_number <- as.numeric(sub(" -.*", "", dataset[[x_col]]))
    sorted_dataset <- dataset[order(-dataset[[y_col]], day_number), ]
  } else {
    sorted_dataset <- dataset[order(-dataset[[y_col]]), ]
  }
  
  # Step 3: Determine x_scale based on x_col type with label spacing
  if (inherits(dataset[[x_col]], "Date")) {
    # For dates, use date_breaks with multiplier
    break_interval <- paste(x_label_every, "days")
    x_scale <- scale_x_date(
      expand = c(0.01, 0), 
      labels = scales::date_format("%Y-%m-%d"), 
      breaks = scales::date_breaks(break_interval)
    )
  } else if (inherits(dataset[[x_col]], "POSIXct") || inherits(dataset[[x_col]], "POSIXt")) {
    break_interval <- paste(x_label_every, "hours")
    x_scale <- scale_x_datetime(
      expand = c(0.01, 0), 
      labels = scales::date_format("%H:%M"), 
      breaks = scales::date_breaks(break_interval)
    )
  } else if (is.numeric(dataset[[x_col]])) {
    # For numeric, calculate breaks manually
    x_range <- range(sorted_dataset[[x_col]], na.rm = TRUE)
    x_breaks <- seq(from = x_range[1], to = x_range[2], by = x_label_every)
    x_scale <- scale_x_continuous(
      expand = c(0.01, 0),
      breaks = x_breaks
    )
  } else if (is.factor(dataset[[x_col]]) || is.character(dataset[[x_col]])) {
    # For discrete variables, select every nth level
    unique_values <- unique(sorted_dataset[[x_col]])
    if (x_label_every > 1) {
      # Select every nth value for breaks
      break_indices <- seq(1, length(unique_values), by = x_label_every)
      x_breaks <- unique_values[break_indices]
    } else {
      x_breaks <- unique_values
    }
    x_scale <- scale_x_discrete(
      expand = c(0.01, 0),
      breaks = x_breaks
    )
  } else {
    stop("Unsupported x_col type.")
  }
  
  # Step 4: Create the base ggplot object
  bar_chart <- ggplot(sorted_dataset, aes(x = .data[[x_col]], y = .data[[y_col]])) +
    geom_bar(stat = "identity", fill = "#44AA99", na.rm = TRUE) +
    x_scale +
    scale_y_continuous(labels = y_axis_labels) +
    theme(
      axis.title = element_blank(),
      plot.title = element_text(hjust = 0.5, size = 12),
      axis.text.x = element_text(
        angle = x_axis_label_angle, 
        vjust = ifelse(x_axis_label_angle == 90, 0.5, 1),
        hjust = ifelse(x_axis_label_angle == 90, 1, 0.5),
        face = "bold", 
        size = x_axis_tick_size
      ),
      axis.text.y = element_text(face = "bold", size = 8),
      axis.ticks.length = x_axis_tick_length,
      panel.background = element_rect(fill = "gray100", color = "gray100"),
      aspect.ratio = 0.618033
    ) +
    labs(x = NULL, y = NULL)
  
  bar_chart <- bar_chart + david_theme()
  
  # Step 5: Add y-axis breaks
  built_plot <- ggplot_build(bar_chart)
  y_breaks <- built_plot$layout$panel_params[[1]]$y$get_breaks()
  y_breaks <- y_breaks[!is.na(y_breaks)]
  bar_chart <- bar_chart +
    geom_hline(yintercept = y_breaks, linetype = "dotted", color = "gray35", linewidth = 0.5)
  
  # Step 6: Add conditional annotations
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
  
  # Step 7: Save or print the chart
  if (!is.null(chart_file_name)) {
    print(bar_chart)
    Sys.sleep(3)
    chart_path <- file.path(chart_directory, chart_file_name)
    ggsave(chart_path, plot = bar_chart, width = chart_width, height = chart_height, dpi = 300)
  }
}