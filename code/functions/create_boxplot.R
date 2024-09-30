# #########################################################################
create_boxplot <- function(
    dataset,
    x_axis_title,
    chart_title,
    chart_file_name) {
  
  boxplot_chart <- ggplot(data = dataset, aes(x = duration, y = factor(1))) +
    geom_jitter(color = "#0072B2", alpha = 0.4, size = 1.9, shape = 17) +
    geom_boxplot(width = 0.2, fill = "#E69F00", alpha = 0.7, color = "black") +
    theme(
      legend.position = "none", plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(size = 9)
    ) +
    labs(
      title = chart_title, x = x_axis_title, y = "",
      subtitle = paste("(", earliest_title, "--", latest_title, ")", " n=", nrow(dataset), sep = "")
    )
  suppressMessages(print(boxplot_chart))
}

#########################################################################