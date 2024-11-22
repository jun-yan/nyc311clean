#########################################################################

create_combo_chart <- function(
    dataset,
    chart_title = NULL,
    chart_file_name = NULL,
    console_print_out_title = "Data Summary"
) {
  
  # Step 1: Create summary_df sorted by count and calculate cumulative percentage
  summary_df <- dataset %>%
    count(agency) %>%
    rename(count = n) %>%
    arrange(desc(count)) %>%
    mutate(
      percentage = count / sum(count),
      cumulative_percentage = cumsum(percentage)
    ) %>%
    slice_head(n = 20)
  
  # Step 2: Reorder the agency factor based on the descending count
  summary_df <- summary_df %>%
    mutate(agency = factor(agency, levels = agency))
  
  # Print first 20 rows of dataset to console without row numbers
  cat(paste0("\n", console_print_out_title, " (first 20 rows):\n"))
  
  # Format each column to left-align, then print without row names
  summary_df <- summary_df %>%
    mutate(
      percentage = round(percentage, 4),
      cumulative_percentage = round(cumulative_percentage, 4)
    )
  
  print(format(summary_df, justify = "left"), row.names = FALSE)
  
  # Chart creation
  max_count <- max(summary_df$count)
  total_count <- sum(summary_df$count)
  
  earliest_date <- min(d311$created_date, na.rm = TRUE)
  latest_date <- max(d311$created_date, na.rm = TRUE)
  earliest_title <- format(as.Date(earliest_date), format = "%Y-%m-%d")
  latest_title <- format(as.Date(latest_date), format = "%Y-%m-%d")
  
  # result <- calculate_values(max_count)
  # starting_value <- result$starting_value
  # increment <- result$increment

  # Step 1: Create the initial plot without specifying y-axis breaks
  combo_chart <- ggplot(summary_df) +
    
    geom_bar(aes(x = agency, y = count), stat = "identity", fill = "#E69F00", width = 0.55) +
    
    geom_text(aes(label = count, x = agency, y = count), 
              colour = "black", hjust = 0.5, vjust = -0.5, size = 2.5) +
    
    geom_text(aes(label = round(cumulative_percentage, 2), x = agency, y = max_count * cumulative_percentage),
              size = 3, colour = "black", hjust = 0.5, vjust = 1.7) +
    
    scale_y_continuous(
      labels = scales::comma,  # Use comma format for y-axis labels
      sec.axis = sec_axis(~ . / max_count)
    ) +
    
#    ggtitle(chart_title, subtitle = paste("(", earliest_title, "--", latest_title, ")",
#                                          " n=", format(total_count, big.mark = ","), sep = "")) +
    labs(x = NULL, y = NULL) +
    
    theme(
      axis.text.x = element_text(angle = 70, vjust = 1, hjust = 1, face = "bold", size = 7),
      axis.text.y = element_text(face = "bold", size = 8),
      axis.text.y.right = element_text(color = "black", face = "bold", size = 8),
      plot.title = element_text(hjust = 0.5, size = 12),
      plot.subtitle = element_text(size = 7),
      aspect.ratio = 0.618033
    )
  
  # Only add the geom_line if there's more than one agency
  if (nrow(summary_df) > 1) {
    
    combo_chart <- combo_chart + 
      geom_line(aes(x = agency, y = cumulative_percentage * max_count, group = 1),
                colour = "black", linewidth = 1, linetype = "dotted")
  }
  
  # Step 2: Build the plot to extract y-axis breaks
  built_plot <- ggplot_build(combo_chart)
  y_breaks <- built_plot$layout$panel_params[[1]]$y$get_breaks()
  y_breaks <- y_breaks[!is.na(y_breaks)]  # Remove any NA values
  
  # Step 3: Add horizontal lines using the y-axis breaks
  combo_chart <- combo_chart + 
    geom_hline(yintercept = y_breaks, linetype = "dotted", color = "gray35", linewidth = 0.5)
  
  # Print or save the chart as needed
  print(combo_chart)
  
  
  # Save the plot
  chart_path <- file.path(chart_directory_path, chart_file_name)
  chart_width <- 10
#  chart_height <- chart_width / 1.618
  chart_height <- chart_width / 1.3
    ggsave(chart_path, plot = combo_chart, width = chart_width, height = chart_height, dpi = 300)
}

#########################################################################

