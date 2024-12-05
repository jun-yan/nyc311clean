# #########################################################################
create_violin_chart <- function(
    dataset,
    x_axis_title,
    x_axis_field,
    chart_title,
    chart_file_name,
    chart_directory) {
  
  violin_chart <- ggplot(
    
    data = dataset,
    
    aes(x = !!rlang::sym(x_axis_field), y = factor(1))) +
    
    geom_jitter(width = 0.2, height = 0.4, alpha = 0.85, color = "#0072B2", size = 2, shape = 17) +
    
    geom_violin(linewidth = 0.7, fill = "transparent", color = "black") +
    
    geom_boxplot(width = 0.25, fill = "#E69F00", color = "black", alpha = 0.65, 
                 outlier.colour = "black", outlier.size = 0.75) +
    labs(
      title = chart_title,
      x = x_axis_title,
      y = "",
      # subtitle = paste("(", earliest_title, "--", latest_title, ")", " n=", 
      #                  format(nrow(dataset), big.mark = ","), sep = "")
    ) +
    
    theme(
      plot.title = element_text(size = 13, hjust = 0.5),
      plot.subtitle = element_text(size = 7),
      axis.text.x = element_text(face = "bold", size = 9),
      panel.background = element_rect(fill = "gray98", color = "gray98")
      )
  
  suppressMessages(print(violin_chart))
  chart_path <- file.path(chart_directory, chart_file_name)
  chart_width <- 10
  chart_height <- chart_width / 1.2
  ggsave(chart_path, plot = violin_chart, dpi = 300,
                          width = chart_width, height = chart_height)
}

#########################################################################