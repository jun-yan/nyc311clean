create_special_bar_chart <- function(data, x_col, y_col, chart_title, subtitle_prefix, earliest_x_value, max_x_value, x_angle = 60, y_axis_labels = scales::comma) {
  max_count <- max(data[[y_col]], na.rm = TRUE)
  total_count <- sum(data[[y_col]], na.rm = TRUE)
  
  result <- calculate_values(max_count)
  starting_value <- result$starting_value
  increment <- result$increment
  
  # Create the bar chart with vertical X-axis labels
  chart <- ggplot(data, aes(x = !!sym(x_col), y = !!sym(y_col))) +
    geom_bar(stat = "identity", fill = "#117733") +
    scale_x_discrete() +
    scale_y_continuous(labels = y_axis_labels) +
    theme(
      axis.title.x = element_text(vjust = 0, size = 11),
      axis.title.y = element_text(vjust = 1, size = 11),
      plot.title = element_text(hjust = 0.5, size = 13),
      plot.subtitle = element_text(size = 9),
      panel.background = element_rect(fill = "gray95", color = "gray95"),
      axis.text.x = element_text(angle = x_angle, vjust = 0.5, hjust = 0.5, face = "bold"),
      axis.text.y = element_text(face = "bold")
    ) +
    ggtitle(chart_title,
            subtitle = paste(
              subtitle_prefix, "(", earliest_title, "--", latest_title, ")",
              "total=", format(total_count, big.mark = "," )
            )
    ) +
    geom_hline(
      yintercept = seq(starting_value, max_count, by = increment),
      linetype = "dotted", color = "gray40"
    ) +
    geom_hline(yintercept = mean_count, linetype = "twodash", color = "black", linewidth = 0.6) +
    annotate("text",
             x = earliest_x_value, y = mean_count,
             label = paste0("Average: ", format(round(mean_count, 0), big.mark = ",")),
             size = 4, hjust = -0.5, vjust = -0.75
    ) +
    annotate("text",
             x = max_x_value, y = max_count,
             label = paste0("Max: ", format(max_count, big.mark = ","), sep = ""),
             size = 4, color = "black", vjust = -0.7, hjust = 0.5
    ) +
    labs(x = NULL, y = NULL)
  
  return(chart)
}
