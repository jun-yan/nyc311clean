david_theme <- function(
    text_size          = 12,
    plot_title_size    = text_size + 3,
    plot_subtitle_size = text_size - 1,   # <-- new: size for subtitle text
    axis_title_size    = text_size - 1,
    x_axis_text_size   = text_size - 1,
    y_axis_text_size   = text_size - 1,
    x_axis_angle       = 0,
    x_axis_hjust       = if (x_axis_angle > 0) 1 else 0.5,
    x_axis_vjust       = 1,
    x_axis_face        = "plain",
    y_axis_face        = "plain",
    grid_line_color    = "white",
    grid_line_type     = "solid",
    grid_line_width    = 0.8,
    base_family        = "sans",
    remove_x_title     = FALSE,
    remove_y_title     = FALSE,
    plot_margin        = NULL
) {
  t <- ggplot2::theme(
    legend.position      = "none",
    panel.grid.major.y   = ggplot2::element_line(
      color = grid_line_color,
      linetype = grid_line_type,
      linewidth = grid_line_width
    ),
    panel.grid.major.x   = ggplot2::element_line(
      color = grid_line_color,
      linetype = grid_line_type,
      linewidth = grid_line_width
    ),
    panel.grid.minor     = ggplot2::element_blank(),
    panel.background     = ggplot2::element_rect(fill = "gray97", color = "gray97"),
    
    plot.title           = ggplot2::element_text(
      face = "bold", size = plot_title_size, hjust = 0.5, family = base_family
    ),
    plot.subtitle        = ggplot2::element_text(
      size = plot_subtitle_size, hjust = 0, family = base_family
    ),
    
    axis.text.x          = ggplot2::element_text(
      angle = x_axis_angle, size = x_axis_text_size,
      vjust = x_axis_vjust, hjust = x_axis_hjust,
      face = x_axis_face, family = base_family
    ),
    axis.text.y          = ggplot2::element_text(
      size = y_axis_text_size, face = y_axis_face, family = base_family
    ),
    
    axis.title.x         = ggplot2::element_text(size = axis_title_size, family = base_family),
    axis.title.y         = ggplot2::element_text(size = axis_title_size, family = base_family),
    
    axis.ticks.length    = grid::unit(0.3, "cm")
  )
  
  if (remove_x_title) {
    t <- t + ggplot2::theme(axis.title.x = ggplot2::element_blank())
  }
  if (remove_y_title) {
    t <- t + ggplot2::theme(axis.title.y = ggplot2::element_blank())
  }
  if (!is.null(plot_margin)) {
    t <- t + ggplot2::theme(plot.margin = ggplot2::margin(
      plot_margin[1], plot_margin[2], plot_margin[3], plot_margin[4], unit = "cm"
    ))
  }
  
  t
}
