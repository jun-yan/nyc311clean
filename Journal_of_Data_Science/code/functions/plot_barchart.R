plot_barchart <- function(
    DT,
    x_col,
    y_col,
    
    # Titles and labels
    title = NULL,
    subtitle = NULL,
    x_label = "",
    y_label = "",
    
    # Appearance
    fill_color = "#009E73",
    bar_width = 0.8,
    
    # Data summary and printing
    console_print_title = NULL,
    rows_to_print = 20,
    show_summary = TRUE,
    
    # Statistical annotations
    add_mean = FALSE,
    add_median = FALSE,
    add_maximum = FALSE,
    add_minimum = FALSE,
    
    # Standard deviation customization
    add_3sd = FALSE,
    sd_color = "#D55E00",
    sd_linetype = "dashed",
    sd_size = 1.2,
    
    # Mean line customization
    mean_color = "#D55E00",
    mean_linetype = "dotted",
    mean_size = 1.2,
    
    # Data labels
    show_labels = TRUE,
    label_col = NULL,
    label_angle = 0,
    label_size = 3,
    label_color = "black",
    label_vjust = -0.5,
    label_hjust = 0.5,
    
    # Trendline
    add_trendline = FALSE,
    trendline_color = "#D55E00",
    trendline_method = "lm",
    trendline_size    = 1.2,
    
    # Extra annotations
    extra_line = NULL,
    
    # Axis customization
    x_label_every = 1,
    x_axis_angle = 0,
    x_axis_size = 10,
    y_axis_labels = scales::comma,
    sort_by_count = FALSE,
    
    # Theme and sizing
    text_size = 12,
    chart_width = 13,
    chart_height = 8.5,
    
    # Save options
    chart_dir = NULL,
    filename = NULL,
    dpi = 300
) {
  
  # # DEBUG: Print what we received
  # cat("\n\n\nDEBUG plot_barchart inputs:\n")
  # cat("  x_col:", x_col, "| class:", class(x_col), "| length:", length(x_col), "\n")
  # cat("  y_col:", y_col, "| class:", class(y_col), "| length:", length(y_col), "\n")
  
  
  require(data.table)
  require(ggplot2)
  require(scales)
  
  # Ensure data.table
  if (!data.table::is.data.table(DT)) {
    DT <- data.table::setDT(copy(DT))
  } else {
    DT <- copy(DT)
  }
  
  # Validate columns
  stopifnot(x_col %in% names(DT), y_col %in% names(DT))
  
  # Add percentage and cumulative percentage
  DT[, percentage := round(get(y_col) / sum(get(y_col), na.rm = TRUE) * 100, 2)]
  DT[, cumulative_percentage := round(cumsum(get(y_col)) / sum(get(y_col), na.rm = TRUE) * 100, 2)]
  
  # Console output
  # Console output
  if (show_summary) {
    if (is.null(console_print_title)) {
      console_print_title <- paste("Data Summary for", x_col, "vs", y_col)
    }
    
    rows_to_show <- min(nrow(DT), rows_to_print)
    cat("\n", console_print_title, " (first ", rows_to_show, " rows):\n", sep = "")
    
    # Print summary table preview
    print(DT[1:rows_to_show, c(x_col, y_col, "percentage", "cumulative_percentage"), with = FALSE],
          row.names = FALSE)
    
    # --- NEW: Yearly summary if 'year' column or yearly x_col available ---
    if ("year" %in% names(DT)) {
      cat("\nSummary by Year (from 'year' column):\n")
      year_summary <- DT[, .(N = sum(get(y_col), na.rm = TRUE)), by = year][order(year)]
      year_summary[, pct := round(100 * N / sum(N), 2)]
      print(year_summary)
      
    } else if (inherits(DT[[x_col]], c("Date", "POSIXct"))) {
      cat("\nSummary by Year (derived from x_col):\n")
      DT[, year_tmp := year(get(x_col))]
      year_summary <- DT[, .(N = sum(get(y_col), na.rm = TRUE)), by = year_tmp][order(year_tmp)]
      year_summary[, pct := round(100 * N / sum(N), 2)]
      setnames(year_summary, "year_tmp", "year")
      print(year_summary)
    }
    
  }
  
  
  # Right after the print summary block:
  # cat("\n=== DEBUG: After summary print, before sorting ===\n")
  # 
  # # Sorting logic
  # cat("\nAbout to sort...\n")
  
  # Sorting logic
  if (sort_by_count) {
    # Sort by count (descending), then by x_col for ties
    if (inherits(DT[[x_col]], c("Date", "POSIXct"))) {
      setorderv(DT, c(y_col, x_col), order = c(-1, 1))
    } else {
      setorderv(DT, y_col, order = -1)
    }
  } else {
    # Sort by x_col (natural ordering)
    setorderv(DT, x_col)
  }
  
  # Determine x-axis scale based on data type and label spacing
  x_scale <- if (inherits(DT[[x_col]], "Date")) {
    # Auto-detect the frequency of date data
    date_range <- range(DT[[x_col]], na.rm = TRUE)
    date_span <- as.numeric(date_range[2] - date_range[1])
    n_points <- nrow(DT)
    
    # Check if this looks like monthly data (first day of months)
    is_monthly <- all(day(DT[[x_col]]) == 1, na.rm = TRUE) && n_points >= 12
    
    if (is_monthly) {
      # Monthly data detected
      break_interval <- paste(x_label_every, "months")
      date_labels <- "%Y-%m"
    } else if (date_span <= 400 && n_points <= 366) {
      # Daily data (up to about a year)
      break_interval <- paste(x_label_every, "days")
      date_labels <- "%m-%d"
    } else if (n_points <= 10 && date_span > 1000) {
      # Likely yearly data (few points, long span)
      break_interval <- paste(x_label_every, "years")
      date_labels <- "%Y"
    } else {
      # Default to treating x_label_every as days
      break_interval <- paste(x_label_every, "days")
      date_labels <- "%Y-%m-%d"
    }
    
    scale_x_date(
      expand = c(0.01, 0), 
      labels = scales::date_format(date_labels), 
      breaks = scales::date_breaks(break_interval)
    )
    
  } else if (inherits(DT[[x_col]], c("POSIXct", "POSIXt"))) {
    break_interval <- paste(x_label_every, "hours")
    scale_x_datetime(
      expand = c(0.01, 0), 
      labels = scales::date_format("%H:%M"), 
      breaks = scales::date_breaks(break_interval)
    )
  } else if (is.numeric(DT[[x_col]])) {
    x_range <- range(DT[[x_col]], na.rm = TRUE)
    x_breaks <- seq(from = x_range[1], to = x_range[2], by = x_label_every)
    scale_x_continuous(
      expand = c(0.01, 0),
      breaks = x_breaks
    )
  } else {
    # Discrete (factor/character)
    unique_values <- unique(DT[[x_col]])
    if (x_label_every > 1) {
      break_indices <- seq(1, length(unique_values), by = x_label_every)
      x_breaks <- unique_values[break_indices]
    } else {
      x_breaks <- unique_values
    }
    scale_x_discrete(
      expand = c(0.01, 0),
      breaks = x_breaks
    )
  }
  
  # cat("\nAbout to create base plot\n")
  
  
  # Create base plot
  p <- ggplot(DT, aes(x = .data[[x_col]], y = .data[[y_col]])) +
    geom_col(fill = fill_color, width = bar_width, color = NA) +
    x_scale +
    scale_y_continuous(labels = y_axis_labels) +
    labs(
      title = title,
      subtitle = subtitle,
      x = x_label,
      y = y_label
    )
  
  # Apply theme
  if (exists("david_theme", mode = "function")) {
    p <- p + david_theme(text_size = text_size, x_axis_angle = x_axis_angle)
  } else {
    p <- p + theme_minimal() +
      theme(
        axis.text.x = element_text(angle = x_axis_angle, hjust = ifelse(x_axis_angle > 0, 1, 0.5)),
        text = element_text(size = text_size)
      )
  }
  
  # Statistical annotations
  if (add_mean || add_median || add_maximum || add_minimum) {
    y_values <- DT[[y_col]]
    
    if (add_mean) {
      mean_val <- mean(y_values, na.rm = TRUE)
      p <- p +
        geom_hline(yintercept = mean_val, linetype = mean_linetype, 
                   color = mean_color, linewidth = mean_size)
      
      # For annotation positioning, use appropriate x value based on data type
      if (inherits(DT[[x_col]], c("Date", "POSIXct"))) {
        x_pos <- DT[[x_col]][1]  # Use first date from the data
      } else {
        x_pos <- 1  # Use numeric position for non-date data
      }
      
      p <- p + annotate("text", x = x_pos, y = mean_val,
                        label = paste0("Mean: ", format(round(mean_val), big.mark = ",")),
                        hjust = -0.1, vjust = -0.5, color = mean_color, size = 3.5)
    }
    
    if (add_median) {
      median_val <- median(y_values, na.rm = TRUE)
      p <- p +
        geom_hline(yintercept = median_val, linetype = "dashed", 
                   color = mean_color, linewidth = mean_size)
      
      # For annotation positioning, use appropriate x value based on data type
      if (inherits(DT[[as.character(x_col)]], c("Date", "POSIXct"))) {
        x_pos <- DT[[x_col]][1]  # Use first date from the data
      } else {
        x_pos <- 1  # Use numeric position for non-date data
      }
      
      p <- p + annotate("text", x = x_pos, y = median_val,
                        label = paste0("Median: ", format(median_val, big.mark = ",")),
                        hjust = -0.1, vjust = 1.5, color = "#0072B2", size = 3.5)
    }
    
#    cat("\nAbout to add maxiumum\n")
    
    
    if (add_maximum) {
      max_row <- DT[which.max(get(y_col))]
      p <- p +
        annotate("text", x = max_row[[x_col]], y = max_row[[y_col]],
                 label = paste0("Max: ", format(max_row[[y_col]], big.mark = ",")),
                 vjust = -0.5, hjust = 0.5, size = 3.5, color = "#E69F00")
    }
    
#    cat("\nAbout to add minimum\n")
    
    
    if (add_minimum) {
      min_row <- DT[which.min(get(y_col))]
      p <- p +
        annotate("text", x = min_row[[x_col]], y = min_row[[y_col]],
                 label = paste0("Min: ", format(min_row[[y_col]], big.mark = ",")),
                 vjust = 1.5, hjust = 0.5, size = 3.5, color = "#D55E00")
    }
  }
  
#  cat("\nAbout to add trendline and statistical measures\n")
  
  
  # Trendline
  if (add_trendline) {
    p <- p + geom_smooth(
      method = trendline_method,
      se = FALSE,
      color = trendline_color,
      linewidth = trendline_size
    )
  }
  
#  cat("\nAbout to add 3SD\n")
  
  
  # Add 3SD line if requested
  if (add_3sd) {
    y_vals <- DT[[y_col]]
    mu <- mean(y_vals, na.rm = TRUE)
    sigma <- sd(y_vals, na.rm = TRUE)
    threshold <- mu + 3 * sigma
    
    p <- p + 
      geom_hline(yintercept = threshold,
                 color = sd_color,
                 linetype = sd_linetype,
                 linewidth = sd_size) +
      annotate("text",
               x = Inf, y = threshold,
               label = sprintf("3SD ≈ %.0f", threshold),
               hjust = 1.1, vjust = -0.5,
               color = sd_color, size = 3)
  }
  
  
#  cat("\nAbout to add data labels\n")
  
  
  # Data labels
  if (show_labels) {
    if (is.null(label_col)) {
      label_col <- y_col  # Default to y values
    }
    
    if (label_col %in% names(DT)) {
      p <- p + geom_text(
        aes(label = .data[[label_col]]),
        angle = label_angle,
        size = label_size,
        color = label_color,
        vjust = label_vjust,
        hjust = label_hjust
      )
    }
  }
  # cat("\nAbout to add extra annotations\n")
  # cat("\nWhat is in extra_line:\n", extra_line)
  
  
  # Extra annotations
  if (!is.null(extra_line)) {
    p <- p + extra_line
  }
  
#  cat("\nAbout to print the plot\n")
  
  # Display plot
  print(p)
  Sys.sleep(3)
  
#  cat("\nAbout to save the plot\n")
  
  # Save plot
  if (!is.null(chart_dir) && !is.null(filename)) {
    if (!dir.exists(chart_dir)) {
      dir.create(chart_dir, recursive = TRUE, showWarnings = FALSE)
    }
    
    filepath <- file.path(chart_dir, filename)
    ggsave(filepath, plot = p, width = chart_width, height = chart_height, dpi = dpi)
#    cat("Chart saved to:", filepath, "\n")
  }
  
  invisible(p)
}