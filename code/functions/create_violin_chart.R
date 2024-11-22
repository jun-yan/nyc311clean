# #########################################################################
create_violin_chart <- function(
    dataset,
    x_axis_title,
    x_axis_field,
    chart_title,
    chart_file_name) {
  
  violin_chart <- ggplot(
    
    data = dataset, 
    
    aes(x = !!rlang::sym(x_axis_field), y = factor(1))) +
    
    geom_jitter(width = 0.25, alpha = 0.4, color = "#0072B2", size = 1.9, shape = 17) +
    
    geom_violin(linewidth = 0.7, fill = "transparent", color = "black") +
    
    geom_boxplot(width = 0.25, fill = "#E69F00", color = "black", alpha = 0.6, 
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
      axis.text.x = element_text(face = "bold", size = 8),
    )
  
  suppressMessages(print(violin_chart))
  chart_path <- file.path(chart_directory_path, chart_file_name)
  suppressMessages(ggsave(chart_path, plot = violin_chart, width = 10, height = 8))
}

#########################################################################