create_basic_bar_chart <- function(
    DT,
    x_col,
    y_col        = "N",
    title        = NULL,
    subtitle     = NULL,
    x_label      = "",
    y_label      = "",
    fill_color   = "#009E73",
    # NEW: Color grouping parameters
    use_color_groups = FALSE,
    group_thresholds = c(90, 50),  # e.g., >90% = good, 50-90% = medium, <50% = poor
    group_colors     = c("#009E73", "#F0E442", "#D55E00"),  # green, yellow, red
    group_labels     = c("High Quality (>90%)", "Medium Quality (50-90%)", "Low Quality (<50%)"),
    legend_position  = "bottom",
    # Data labels
    show_labels  = FALSE,
    label_col    = NULL,
    label_angle  = 0,
    label_size   = 3,
    label_color  = "gray40",
    vjust        = -0.5,
    hjust        = 0.5,
    # Mean line
    mean_line    = FALSE,
    mean_label   = "Mean",
    mean_color   = "#D55E00",
    mean_linetype= "dotted",
    mean_size    = 1,
    # Theme customization
    text_size    = 12,
    x_axis_angle = 0,
    x_axis_face  = "plain",
    y_axis_face  = "plain",
    grid_line_width = 0.8,
    remove_x_title  = FALSE,
    remove_y_title  = FALSE,
    plot_margin     = NULL,
    # Save options
    chart_dir    = NULL,
    filename     = NULL,
    width        = 13,
    height       = 8.5,
    dpi          = 300
) {
  stopifnot(x_col %in% names(DT), y_col %in% names(DT))
  
  # Create a copy to avoid modifying the original
  plot_dt <- data.table::copy(DT)
  
  # Add grouping column if using color groups
  if (use_color_groups) {
    plot_dt[, quality_group := data.table::fcase(
      get(y_col) >= group_thresholds[1], group_labels[1],
      get(y_col) >= group_thresholds[2], group_labels[2],
      default = group_labels[3]
    )]
    # Factor with levels in quality order
    plot_dt[, quality_group := factor(quality_group, levels = group_labels)]
  }
  
  mean_val <- mean(plot_dt[[y_col]], na.rm = TRUE)
  
  # Base plot
  if (use_color_groups) {
    p <- ggplot2::ggplot(plot_dt, ggplot2::aes(
      x = factor(.data[[x_col]]), 
      y = .data[[y_col]],
      fill = quality_group
    )) +
      ggplot2::geom_col(color = NA, width = 0.8) +
      ggplot2::scale_fill_manual(
        values = stats::setNames(group_colors, group_labels),
        name = NULL
      ) +
      ggplot2::theme(legend.position = legend_position)
  } else {
    p <- ggplot2::ggplot(plot_dt, ggplot2::aes(
      x = factor(.data[[x_col]]), 
      y = .data[[y_col]]
    )) +
      ggplot2::geom_col(fill = fill_color, color = NA, width = 0.8)
  }
  
  p <- p +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::labs(
      title    = title,
      subtitle = subtitle,
      x        = x_label,
      y        = y_label
    ) +
    david_theme(
      text_size       = text_size,
      x_axis_angle    = x_axis_angle,
      x_axis_face     = x_axis_face,
      y_axis_face     = y_axis_face,
      grid_line_width = grid_line_width,
      remove_x_title  = remove_x_title,
      remove_y_title  = remove_y_title,
      plot_margin     = plot_margin
    )
  
  # Mean line
  if (mean_line) {
    p <- p +
      ggplot2::geom_hline(
        yintercept = mean_val,
        linetype   = mean_linetype,
        color      = mean_color,
        linewidth  = mean_size
      ) +
      ggplot2::annotate(
        "text",
        x     = Inf,
        y     = mean_val,
        label = sprintf("%s = %s", mean_label, format(round(mean_val), big.mark = "")),
        color = mean_color,
        hjust = 1.1,
        vjust = -0.5,
        size  = 3.5
      )
  }
  
  # Data labels
  if (show_labels && !is.null(label_col) && label_col %in% names(DT)) {
    p <- p + ggplot2::geom_text(
      ggplot2::aes(label = .data[[label_col]]),
      angle  = label_angle,
      size   = label_size,
      color  = label_color,
      vjust  = vjust,
      hjust  = hjust
    )
  }
  
  print(p)
  Sys.sleep(3)
  
  # Save
  if (!is.null(chart_dir) && !is.null(filename)) {
    outfile <- file.path(chart_dir, filename)
    if (!dir.exists(chart_dir)) {
      stop("Directory does not exist: ", chart_dir)
    }
    ggplot2::ggsave(
      filename = outfile,
      plot     = p,
      width    = width,
      height   = height,
      dpi      = dpi
    )
  }
  
  return(p)
}