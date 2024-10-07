create_bar_chart <- function(
    dataset,
    x_col,
    y_col,
    chart_title = "Bar Chart",       # Default title
    sub_title = "",                  # Default empty subtitle
    x_axis_title = NULL,             # Default empty x-axis title
    y_axis_title = NULL,             # Default empty y-axis title
    console_print_out_title = "Data Summary", # Default title for console output
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
    vertical_adjustment_max = -1
) {
  
  # Step 1: Print Data Summary
  cat("\n", console_print_out_title, " (first 20 rows):\n", sep = "")
  print(head(dataset[, c(x_col, y_col)], 20), row.names = FALSE)
  
  # Step 2: Pre-compute statistics (if needed)
  stats <- list()
  if (add_maximum || add_minimum || add_mean || add_median || add_sd || add_second_maximum) {
    stats$y_max <- max(dataset[[y_col]], na.rm = TRUE)
    stats$y_min <- min(dataset[[y_col]], na.rm = TRUE)
    stats$y_mean <- mean(dataset[[y_col]], na.rm = TRUE)
    stats$y_median <- median(dataset[[y_col]], na.rm = TRUE)
    stats$y_sd <- sd(dataset[[y_col]], na.rm = TRUE)
  }
  
  if (add_second_maximum) {
    # Get the second maximum
    stats$second_max <- dataset %>%
      arrange(desc(.data[[y_col]])) %>%
      slice(2)
  }
  
  # Step 3: Create the Bar Chart without scale
  # Make sure the x_col is in Date format
  dataset[[x_col]] <- as.Date(dataset[[x_col]])
  
  bar_chart <- ggplot(dataset, aes(x = .data[[x_col]], y = .data[[y_col]])) +
    geom_bar(stat = "identity", fill = "#44AA99") +
    theme(
      axis.title = element_text(size = 7),
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, face = "bold", size = 8),
      axis.text.y = element_text(face = "bold", size = 7),
      plot.title = element_text(hjust = 0.5, size = 12),
      plot.subtitle = element_text(size = 6),
      aspect.ratio = 0.618033
    ) +
    ggtitle(chart_title, subtitle = paste(sub_title, format(nrow(dataset), big.mark = ","), sep = "")) +
    labs(x = x_axis_title, y = y_axis_title)
  
  # Step 4: Add the appropriate scale_x based on column type
  if (inherits(dataset[[x_col]], "Date")) {
    bar_chart <- bar_chart + scale_x_date(expand = c(0.01, 0), labels = scales::date_format("%Y-%m"), breaks = scales::date_breaks("6 months"))
  } else if (inherits(dataset[[x_col]], "POSIXct") || inherits(dataset[[x_col]], "POSIXlt")) {
    bar_chart <- bar_chart + scale_x_datetime(expand = c(0.01, 0), labels = scales::date_format("%H:%M"), breaks = scales::date_breaks("2 hours"))
  } else if (is.numeric(dataset[[x_col]])) {
    bar_chart <- bar_chart + scale_x_continuous(expand = c(0.01, 0))
  } else {
    bar_chart <- bar_chart + scale_x_discrete(expand = c(0.01, 0))
  }
  
  # Step 5: Conditional Annotation Additions
  if (add_maximum) {
    bar_chart <- bar_chart + annotate("text", x = which.max(dataset[[y_col]]), y = stats$y_max,
                                      label = paste0("Max: ", format(stats$y_max, big.mark = ",")),
                                      size = 4.25, vjust = vertical_adjustment_max, hjust = horizontal_adjustment_max)
  }
  
  if (add_minimum) {
    bar_chart <- bar_chart + annotate("text", x = which.min(dataset[[y_col]]), y = stats$y_min,
                                      label = paste0("Min: ", format(stats$y_min, big.mark = ",")),
                                      size = 4.25, vjust = -0.4, hjust = 0.5)
  }
  
  if (add_mean) {
    bar_chart <- bar_chart + geom_hline(yintercept = stats$y_mean, linetype = "twodash", color = "black", linewidth = 0.75) +
      annotate("text", x = min(dataset[[x_col]], na.rm = TRUE), y = stats$y_mean,
               label = paste0("Avg: ", format(round(stats$y_mean, 0), big.mark = ",")),
               size = 4.5, hjust = -0.5, vjust = -0.75)
  }
  
  if (add_median) {
    bar_chart <- bar_chart + geom_hline(yintercept = stats$y_median, linetype = "twodash", color = "black", linewidth = 0.6) +
      annotate("text", x = min(dataset[[x_col]], na.rm = TRUE), y = stats$y_median,
               label = paste0("Median: ", format(round(stats$y_median, 0), big.mark = ",")),
               size = 4.5, hjust = -0.5, vjust = -0.75)
  }
  
  if (add_sd) {
    bar_chart <- bar_chart + geom_hline(yintercept = stats$y_mean + 3 * stats$y_sd, linetype = "longdash", color = "black", linewidth = 0.3) +
      annotate("text", x = min(dataset[[x_col]], na.rm = TRUE), y = stats$y_mean + 3 * stats$y_sd,
               label = "+3 sigma", size = 4, hjust = -0.5, vjust = -0.75)
  }
  
  if (add_second_maximum && !is.null(stats$second_max)) {
    second_max_row <- stats$second_max
    bar_chart <- bar_chart + annotate("text", x = second_max_row[[x_col]], y = second_max_row[[y_col]],
                                      label = paste0("2nd Max: ", format(second_max_row[[y_col]], big.mark = ",")),
                                      size = 4.25, vjust = -0.4, hjust = 0.05)
  }
  
  if (add_trendline) {
    bar_chart <- bar_chart + geom_smooth(method = "lm", span = 1, se = FALSE, color = "#661100", linetype = "dashed", linewidth = 1.3)
  }
  
  if (!is.null(extra_line)) {
    bar_chart <- bar_chart + extra_line
  }
  
  # Step 6: Print the Bar Chart
  print(bar_chart)
  
  ## Step 7: Save the chart to a file if provided
  if (!is.null(chart_file_name)) {
    chart_path <- file.path(chart_directory_path, chart_file_name)
    ggsave(chart_path, plot = bar_chart, width = 10, height = 10 / 1.618, dpi = 300)  # Golden ratio aspect ratio
  }
}