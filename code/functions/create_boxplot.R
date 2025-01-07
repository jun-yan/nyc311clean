# Create a reusable function for generating boxplots
create_boxplot <- function(
    dataset, 
    x_axis_field,
    chart_title, 
    x_axis_title = "", 
    output_file_name, 
    chart_directory, 
    chart_width = 10, 
    chart_height = 7, 
    x_axis_tick_size = 15,  # Font size for x-axis labels
    x_axis_label_angle = 90,  # Angle for x-axis labels
    plot_title_size = 16,  # Font size for plot title
    box_width = 0.5,  # Default width for the box
    jitter_size = 2,  # Size of jitter points
    jitter_alpha = 0.85,  # Opacity of jitter points
    tick_length = unit(0.3, "cm")  # Length of tick marks
) {
  
  # Generate the boxplot
  boxplot_chart <- ggplot(
    data = dataset, 
    aes(x = .data[[x_axis_field]], y = factor(1))
  ) +
    geom_jitter(
      color = "#0072B2", 
      size = jitter_size, 
      shape = 17, 
      alpha = jitter_alpha
    ) +
    geom_boxplot(
      outlier.colour = "black", 
      outlier.shape = 16, 
      linewidth = 0.7,
      fill = "#E69F00", 
      size = 1, 
      color = "black", 
      alpha = 0.65,
      width = box_width
    ) +
    theme(
      legend.position = "none",
      plot.title = element_text(hjust = 0.5, size = plot_title_size),
      axis.text.x = element_text(
        angle = x_axis_label_angle, 
        vjust = 1, 
        hjust = 1, 
        face = "bold", 
        size = x_axis_tick_size
      ),
      axis.text.y = element_blank(),  # Remove y-axis labels
      axis.ticks.y = element_blank(), # Remove y-axis tick marks
      axis.ticks.length = tick_length,  # Set tick mark length
      panel.background = element_rect(fill = "gray96", color = "gray96")
    ) +
    labs(
      title = chart_title,
      x = x_axis_title,
      y = NULL
    )
  
  # Print and save the chart
  print(boxplot_chart)
  
  # Save the chart
  chart_path <- file.path(chart_directory, output_file_name)
  ggsave(
    chart_path, 
    plot = boxplot_chart, 
    width = chart_width, 
    height = chart_width / 1.2,  # Match vertical spread by adjusting aspect ratio
    dpi = 300
  )
}

# 
# 
# 
# 
# 
# # Parameters for customization
# x_axis_tick_size <- 15  # Font size for x-axis labels
# x_axis_label_angle <- 90  # Angle for x-axis labels
# y_axis_tick_size <- 14  # Font size for y-axis labels
# plot_title_size <- 16  # Font size for plot title
# 
# # Create boxplot of the closed in the future values
# closedinFutureChart <- ggplot(
#   data = closedinFuture,
#   aes(x = future_days, y = factor(1))
# ) +
#   geom_jitter(color = "#0072B2", size = 2, shape = 17, alpha = 0.85) +
#   geom_boxplot(
#     outlier.colour = "black", outlier.shape = 16, linewidth = 0.7,
#     fill = "#E69F00", size = 1, color = "black", alpha = 0.65
#   ) +
#   theme(
#     legend.position = "none",
#     plot.title = element_text(hjust = 0.5, size = plot_title_size),
#     axis.text.x = element_text(angle = x_axis_label_angle, vjust = 1, hjust = 1, 
#                                face = "bold", size = x_axis_tick_size),
#     axis.text.y = element_blank(),  # Remove y-axis labels
#     axis.ticks.y = element_blank(), # Remove y-axis tick marks
#     panel.background = element_rect(fill = "gray96", color = "gray96")
#   ) +
#   labs(
#     title = "SRs closed in the future",
#     x = "Days closed in the future",
#     y = NULL
#   )
# 
# # Print and save the chart
# print(closedinFutureChart)
# chart_path <- file.path(chart_directory_path, "future_closed.pdf")
# chart_width <- 10
# chart_height <- chart_width / 1.2
# ggsave(chart_path, plot = closedinFutureChart, 
#        width = chart_width, height = chart_height, dpi = 300)
# 
# 
# 
# 
# 
# # Parameters for customization
# x_axis_tick_size <- 14        # Font size for x-axis labels
# x_axis_label_angle <- 90      # Angle for x-axis labels
# y_axis_tick_size <- 14        # Font size for y-axis labels
# x_axis_tick_length <- unit(0.3, "cm")  # Length of x-axis tick marks
# 
# # Create boxplot of the (negative) duration values
# negativeDurationChart <- ggplot(
#   data = large_neg_duration, 
#   aes(x = duration, y = factor(1))
# ) +
#   
#   geom_jitter(color = "#0072B2", alpha = 0.85, size = 1.9, shape = 17, width = 0.2, height = 0.2) +
#   
#   geom_boxplot(width = 0.25, fill = "#E69F00", alpha = 0.65, outlier.colour = "black", 
#                outlier.size = 1) +
#   
#   theme(
#     legend.position = "none",
#     plot.title = element_text(hjust = 0.5),
#     axis.text.x = element_text(angle = x_axis_label_angle, vjust = 1, hjust = 1, 
#                                face = "bold", size = x_axis_tick_size),
#     axis.text.y = element_blank(),  # Remove y-axis labels
#     axis.ticks.y = element_blank(), # Remove y-axis tick marks
#     axis.ticks.length = x_axis_tick_length,  # Adjust tick mark length
#     plot.margin = margin(1, 2, 1, 2),
#     panel.background = element_rect(fill = "gray96", color = "gray96")
#   ) +
#   
#   labs(
#     title = "SRs closed before they were created (negative duration) *excluding large negative values",
#     x = "", y = ""
#   )
# 
# # Print and save the chart
# print(negativeDurationChart)
# chart_path <- file.path(chart_directory_path, "negative_duration_SR_boxplot.pdf")
# chart_width <- 10
# chart_height <- chart_width / 1.2
# ggsave(chart_path, plot = negativeDurationChart, width = chart_width, 
#        height = chart_height, dpi = 300)
