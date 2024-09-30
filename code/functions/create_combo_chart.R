#########################################################################

# This function takes a subset of the d311 dataframe and summarizes it by "agency"
# It also computes the cumulative percentage for a histogram overlayed on the bar chart
create_combo_chart <- function(
    dataset,
    chart_title = NULL,
    chart_file_name = NULL) 
{
  
  # Create a frequency table of counts by "agency"
  count_table <- table(dataset$agency)
  
  # Create the summary dataframe
  summary_df <- data.frame(
    agency = names(count_table),
    count = as.vector(count_table))
  
  summary_df <- summary_df[order(summary_df$count, decreasing = TRUE), ]
  
  # Calculate percentage and cumulative percentage
  summary_df$percentage <- round(summary_df$count / sum(summary_df$count) * 100, 4)
  summary_df$cumulative_percentage <- cumsum(summary_df$percentage)
  
  # Adjust percentage and cumulative_percentage columns for histogram line
  summary_df$percentage <- summary_df$percentage / 100
  summary_df$cumulative_percentage <- summary_df$cumulative_percent / 100
  
  # Determine the parameters for the chart
  max_count <- max(summary_df$count)
  total_count <- sum(summary_df$count)
  
  earliest_date <- min(d311$created_date, na.rm = TRUE)
  earliest_date_formatted <- format(earliest_date, format = "%Y-%m-%d %H:%M:%S")
  
  latest_date <- max(d311$created_date, na.rm = TRUE)
  latest_date_formatted <- format(latest_date, format = "%Y-%m-%d %H:%M:%S")
  
  earliest_title <- format(as.Date(earliest_date_formatted), format = "%Y-%m-%d")
  latest_title <- format(as.Date(latest_date_formatted), format = "%Y-%m-%d")
  
  # Check if the number of rows in summary_df is greater than 20
  if (nrow(summary_df) > 20) {
    # If true, sort the data frame by the 'percentage' column in descending order
    summary_df <- summary_df[order(-summary_df$percentage), ]
    
    # Truncate the data frame to the top 20 rows
    summary_df <- summary_df[1:20, ]
  }
  
  result <- calculate_values(max_count) # Call the 'calculate_values' function for scaling parameters
  starting_value <- result$starting_value
  increment <- result$increment
  
  # Create a combination chart
  combo_chart <- ggplot(summary_df) +
    
    geom_bar( aes( x = reorder(agency, cumulative_percentage), y = count),
              stat = "identity", fill = "#E69F00", width = 0.55 ) +
    
    geom_line( aes( x = reorder(agency, cumulative_percentage), y = cumulative_percentage*max_count,
                    group = 1,), colour = "black", linewidth = 1, linetype = "dotted", ) +
    
    geom_text( aes( label = count, x = reorder(agency, cumulative_percentage),
                    y = count ), colour = "black", hjust = 0.5, vjust = -0.5, size = 3 ) +
    
    geom_text( aes( label = round(cumulative_percentage, 2), x = reorder(agency, cumulative_percentage),
                    y = max_count * cumulative_percentage ), size = 3, colour = "black", hjust = 0.5, vjust = 1.7 ) +
    
    geom_hline( yintercept = seq(starting_value, max_count, by = increment), 
                linetype = "dotted", color = "gray40", linewidth = 0.4 ) +
    
    ggtitle( chart_title, subtitle = paste("(", earliest_title, "--", latest_title, ")",
                                    " total=", format(total_count, big.mark = ","), sep = "")) +
    
    scale_y_continuous( breaks = seq(starting_value, max_count, by = increment), sec.axis = sec_axis(~ . / max_count )) +
    
    labs(x = NULL, y = NULL) +
  
    theme(
      axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1, face = "bold", size = 7),
      axis.text.y = element_text(face = "bold", size = 7),  # Bold the left y-axis text
      axis.text.y.right = element_text(color = "black", face = "bold", size = 7),
      plot.title = element_text(hjust = 0.5, size = 12),
      plot.subtitle = element_text(size = 6),
      aspect.ratio = 0.618033 )
  
  print(combo_chart, row.names = FALSE, right = FALSE)
  chart_path <- file.path(chart_directory_path, chart_file_name)
  
  # Define aspect ratio based on the golden ratio (approximately 1.618)
  chart_width <- 10  # Adjust as needed
  chart_height <- chart_width / 1.618  # Golden ratio
  ggsave(chart_path, plot = combo_chart, width = chart_width, height = chart_height, dpi = 300)
}

#########################################################################
