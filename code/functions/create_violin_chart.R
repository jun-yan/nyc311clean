##########################################################################
create_violin_chart <- function(
    dataset,
    x_axis_title,
    x_axis_field,
    chart_title,
    chart_file_name,
    chart_directory,
    chart_width = 10,
    chart_height = 7,
    margin_top = 1,     # Default margin values (units in pts)
    margin_right = 2,
    margin_bottom = 1,
    margin_left = 2
) {
  
  # Create the violin chart
  violin_chart <- ggplot(
    data = dataset,
    aes(x = !!rlang::sym(x_axis_field), y = factor(1))) +
    
    geom_jitter(width = 0.2, height = 0.4, alpha = 0.85, color = "#0072B2", size = 2, shape = 17) +
    
    geom_violin(linewidth = 0.7, fill = "transparent", color = "black") +
    
    geom_boxplot(width = 0.25, fill = "#E69F00", color = "black", alpha = 0.65, 
                 outlier.colour = "black", outlier.size = 0.75) +
    
    scale_y_discrete(expand = c(0, 0)) +  # Remove vertical padding
    labs(
      title = chart_title,
      x = x_axis_title,
      y = NULL
    ) +
    
    theme(
      plot.title = element_text(size = 13, hjust = 0.5),
      axis.text.x = element_text(face = "bold", size = 9),
      panel.background = element_rect(fill = "gray96", color = "gray96"),
      plot.margin = margin(t = margin_top, r = margin_right, 
                           b = margin_bottom, l = margin_left, unit = "pt")
    )
  
  
  # Print the chart (suppress unnecessary warnings)
  suppressMessages(print(violin_chart))
  
  # Save the chart to the specified path
  chart_path <- file.path(chart_directory, chart_file_name)
  ggsave(chart_path, plot = violin_chart, dpi = 300,
         width = chart_width, height = chart_height)
}
#########################################################################
