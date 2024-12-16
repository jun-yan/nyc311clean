#########################################################################

create_combo_chart <- function(
    dataset,
    chart_title = NULL,
    chart_file_name = NULL,
    console_print_out_title = "Data Summary",
    chart_directory,
    chart_width = 10,
    chart_height = 7,
    annotation_size = 4.5,
    num_x_labels = 20,          # New parameter for when to skip labels
    skip_frequency = 2          # New parameter for how often to skip labels
) {
  
  # Step 1: Create summary_df sorted by count and calculate cumulative percentage
  summary_df <- dataset %>%
    count(.data[["agency"]]) %>%
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
  
  # Count the number of unique x-axis data points
  total_x_labels <- length(unique(summary_df$agency))
  
  # Utility function to blank every 'skip_frequency' label if total x labels > num_x_labels
  blank_alternate_labels <- function(values) {
    if (total_x_labels > num_x_labels) {
      ifelse(seq_along(values) %% skip_frequency == 0, "", values)
    } else {
      values
    }
  }
  
  # Apply the label blanking logic to the count and cumulative percentage labels
  count_labels <- blank_alternate_labels(summary_df$count)
  cumulative_labels <- blank_alternate_labels(round(summary_df$cumulative_percentage, 2))
  
  # Chart creation
  max_count <- max(summary_df$count)
  total_count <- sum(summary_df$count)
  
  # Step 1: Create the initial plot without specifying y-axis breaks
  combo_chart <- ggplot(summary_df) +
    
    geom_bar(aes(x = agency, y = count), stat = "identity", fill = "#44AA99", width = 0.55) +
    
    # Count labels (primary axis)
    geom_text(aes(label = count_labels, x = agency, y = count), 
              colour = "black", hjust = 0.5, vjust = -0.5, size = annotation_size) +
    
    # Cumulative percentage labels (secondary axis)
    geom_text(aes(label = cumulative_labels, x = agency, y = max_count * cumulative_percentage),
              size = annotation_size, colour = "black", hjust = 0.5, vjust = 1.7) +
    
    scale_y_continuous(
      labels = scales::comma,  # Use comma format for y-axis labels
      sec.axis = sec_axis(~ . / max_count)
    ) +
    
    labs(x = NULL, y = NULL) +
    
    theme(
      axis.text.x = element_text(angle = 70, vjust = 1, hjust = 1, face = "bold", size = annotation_size + 3),
      axis.text.y = element_text(face = "bold", size = annotation_size + 3),
      axis.text.y.right = element_text(color = "black", face = "bold", size = annotation_size + 3),
      plot.title = element_text(hjust = 0.5, size = annotation_size + 7),
      plot.subtitle = element_text(size = annotation_size + 2),
      panel.background = element_rect(fill = "gray96", color = "gray96"),
      plot.margin = margin(1, 2, 1, 2)  # Adjust the top, right, bottom, and left margins
      #      aspect.ratio = 0.618033
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
  chart_path <- file.path(chart_directory, chart_file_name)
  ggsave(chart_path, plot = combo_chart, width = chart_width, height = chart_height, dpi = 300)
}

#########################################################################
