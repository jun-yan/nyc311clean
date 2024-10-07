#########################################################################

create_combo_chart <- function(
    dataset,
    chart_title = NULL,
    chart_file_name = NULL) 
{
  # Use dplyr for faster summarization
  summary_df <- dataset %>%
    count(agency) %>%  # Count occurrences of each agency
    mutate(
      percentage = n / sum(n),
      cumulative_percentage = cumsum(percentage)
    ) %>%
    arrange(desc(n)) %>%  # Sort by count
    slice_head(n = 20)  # Keep only the top 20
  
  # Print first 20 rows of summary_df to console without row numbers
  cat("\nSummary of dataset (first 20 rows):\n")
  print(summary_df, row.names = FALSE)
  
  # Chart creation
  max_count <- max(summary_df$n)
  total_count <- sum(summary_df$n)
  
  # Date calculations (assuming d311 is accessible in the environment)
  earliest_date <- min(d311$created_date, na.rm = TRUE)
  latest_date <- max(d311$created_date, na.rm = TRUE)
  earliest_title <- format(as.Date(earliest_date), format = "%Y-%m-%d")
  latest_title <- format(as.Date(latest_date), format = "%Y-%m-%d")
  
  # Create a combination chart
  combo_chart <- ggplot(summary_df) +
    
    geom_bar( aes(x = reorder(agency, cumulative_percentage), y = n),
              stat = "identity", fill = "#E69F00", width = 0.55 ) +
    
    geom_line( aes(x = reorder(agency, cumulative_percentage), y = cumulative_percentage * max_count, 
                   group = 1), colour = "black", linewidth = 1, linetype = "dotted") +
    
    geom_text( aes(label = n, x = reorder(agency, cumulative_percentage), y = n), 
               colour = "black", hjust = 0.5, vjust = -0.5, size = 2.5) +
    
    geom_text( aes(label = round(cumulative_percentage, 2), x = reorder(agency, cumulative_percentage), 
                   y = max_count * cumulative_percentage), size = 3, colour = "black", 
               hjust = 0.5, vjust = 1.7) +
    
    ggtitle(chart_title, subtitle = paste("(", earliest_title, "--", latest_title, ")",
                                          " total=", format(total_count, big.mark = ","), sep = "")) +
    labs(x = NULL, y = NULL) +
    
    theme(
      axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1, face = "bold", size = 7),
      axis.text.y = element_text(face = "bold", size = 7),  # Bold the left y-axis text
      axis.text.y.right = element_text(color = "black", face = "bold", size = 7),
      plot.title = element_text(hjust = 0.5, size = 12),
      plot.subtitle = element_text(size = 6),
      aspect.ratio = 0.618033 )
  
  # Save the plot with aspect ratio based on the golden ratio
  chart_path <- file.path(chart_directory_path, chart_file_name)
  chart_width <- 10
  chart_height <- chart_width / 1.618  # Golden ratio
  
  ggsave(chart_path, plot = combo_chart, width = chart_width, height = chart_height, dpi = 300)
}

#########################################################################