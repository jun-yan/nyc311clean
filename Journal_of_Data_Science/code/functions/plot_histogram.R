plot_histogram <- function(
    DT,
    value_col,
    title = NULL,
    x_label = NULL,
    chart_dir = "charts",
    filename = NULL,
    bins = 30,
    fill_color = "#0072B2",
    alpha = 0.7,
    outlier_percentile = 0.95,
    log_transform = FALSE,
    xlim = NULL,
    add_stats = TRUE,
    add_labels = FALSE,        # <-- ADD THIS
    label_size = 3,            # <-- ADD THIS (optional)
    label_color = "gray25",    # <-- ADD THIS (optional)
    width = 10,
    height = 6
) {
  
  require(ggplot2)
  require(data.table)
  
  stopifnot(data.table::is.data.table(DT))
  stopifnot(value_col %in% names(DT))
  
  if (!dir.exists(chart_dir)) dir.create(chart_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Extract the column values
  values <- DT[[value_col]]
  values <- values[!is.na(values)]
  
  if (length(values) == 0) {
    cat("No non-NA values found in column:", value_col, "\n")
    return(invisible(NULL))
  }
  
  # Handle outliers by trimming to percentile
  if (outlier_percentile < 1.0) {
    upper_limit <- quantile(values, outlier_percentile, na.rm = TRUE)
    values_trimmed <- values[values <= upper_limit]
    n_trimmed <- length(values) - length(values_trimmed)
    
    if (n_trimmed > 0) {
      cat(sprintf("Trimmed %s extreme values (%.1f%%) above %.2f percentile\n", 
                  prettyNum(n_trimmed, big.mark = ","), 
                  100 * n_trimmed / length(values),
                  outlier_percentile))
    }
  } else {
    values_trimmed <- values
    n_trimmed <- 0
  }
  
  # Apply log transform if requested
  if (log_transform && any(values_trimmed <= 0)) {
    cat("Warning: log transform requested but data contains values <= 0. Adding 1 to all values.\n")
    values_trimmed <- log10(values_trimmed + 1)
  } else if (log_transform) {
    values_trimmed <- log10(values_trimmed)
  }
  
  # Generate default labels
  if (is.null(title)) {
    title <- paste("Distribution of", value_col)
    if (log_transform) title <- paste(title, "(Log10 Scale)")
    if (n_trimmed > 0) title <- paste0(title, sprintf(" [%d%% trimmed]", round(100 * outlier_percentile)))
  }
  
  if (is.null(x_label)) {
    x_label <- value_col
    if (log_transform) x_label <- paste("Log10(", x_label, ")")
  }
  
  # Calculate statistics
  stats_text <- ""
  if (add_stats) {
    stats <- list(
      n = prettyNum(length(values), big.mark = ","),
      mean = round(mean(values, na.rm = TRUE), 2),
      median = round(median(values, na.rm = TRUE), 2),
      sd = round(sd(values, na.rm = TRUE), 2)
    )
    
    stats_text <- sprintf("n=%s  Mean=%.2f  Median=%.2f  SD=%.2f", 
                          stats$n, stats$mean, stats$median, stats$sd)
  }
  
  # Create the plot
  p <- ggplot(data.frame(x = values_trimmed), aes(x = x)) +
    geom_histogram(bins = bins, fill = fill_color, alpha = alpha, color = "white")
  
  # Add count labels on top of bars if requested
  if (add_labels) {
    # Compute bin counts
    hist_data <- ggplot_build(p)$data[[1]]
    
    p <- p + 
      geom_text(
        data = hist_data,
        aes(x = x, y = count, label = scales::comma(count)),
        vjust = -0.5,
        size = label_size,
        color = label_color,
        inherit.aes = FALSE
      )
  }
  
    p <- p +  
    labs(
      title = title,
      subtitle = stats_text,
      x = x_label,
      y = "Count"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 12, face = "bold"),
      plot.subtitle = element_text(size = 10, color = "gray30")
    )
  
  if (!is.null(xlim)) {
    p <- p + coord_cartesian(xlim = xlim)
  }
  
  
  # Apply custom theme if available
  if (exists("david_theme", mode = "function")) {
    p <- p + david_theme(text_size = 12)
  }
  
  # Print summary stats
  cat(sprintf("\nHistogram for '%s':\n", value_col))
  cat(sprintf("  Total records: %s\n", prettyNum(length(values), big.mark = ",")))
  cat(sprintf("  Min: %.4f\n", min(values, na.rm = TRUE)))
  cat(sprintf("  Max: %.4f\n", max(values, na.rm = TRUE)))
  cat(sprintf("  Mean: %.4f\n", mean(values, na.rm = TRUE)))
  cat(sprintf("  Median: %.4f\n", median(values, na.rm = TRUE)))
  cat(sprintf("  SD: %.4f\n", sd(values, na.rm = TRUE)))
  if (n_trimmed > 0) {
    cat(sprintf("  Trimmed: %s values (%.1f%%)\n", 
                prettyNum(n_trimmed, big.mark = ","), 
                100 * n_trimmed / length(values)))
  }
  
  # Display the plot
  print(p)
  Sys.sleep(3)
  
  # Save if filename provided
  if (!is.null(filename)) {
    if (!grepl("\\.(pdf|png|jpg|jpeg)$", filename, ignore.case = TRUE)) {
      filename <- paste0(filename, ".pdf")
    }
    
    ggsave(file.path(chart_dir, filename), plot = p, width = width, height = height)
    # cat(sprintf("Histogram saved to: %s\n", file.path(chart_dir, filename)))
  }
  
  invisible(list(
    plot = p,
    stats = if(add_stats) stats else NULL,
    n_trimmed = n_trimmed
  ))
}