report_resolution_update_before_created <- function(
    DT,
    created_col = "created_date",
    resolution_col = "resolution_action_updated_date",
    agency_col = "agency",
    unique_key_col = "unique_key",
    chart_dir = "charts",
    sample_size = 5,
    make_pareto = TRUE,
    make_boxplot = TRUE,
    x_label_every = 40    # show every 40th bin label (~10 hours)
) {
  stopifnot(data.table::is.data.table(DT))
  stopifnot(all(c(created_col, resolution_col, agency_col, unique_key_col) %in% names(DT)))
  if (!dir.exists(chart_dir)) dir.create(chart_dir, recursive = TRUE, showWarnings = FALSE)
  
  # --- Calculate resolution duration in days
  DT[, res_duration_days := as.numeric(difftime(get(resolution_col), get(created_col), units = "days"))]
  
  # --- Identify problematic records (negative durations only)
  bad_res_dates <- DT[!is.na(get(resolution_col)) & res_duration_days < 0]
  
  if (nrow(bad_res_dates) == 0) {
    cat("No problematic records found - all resolution dates are after creation dates.\n")
    DT[, res_duration_days := NULL]
    return(invisible(NULL))
  }
  
  # --- Summary statistics
  cat(sprintf("\nService Requests with %s BEFORE %s: %s\n",
              resolution_col, created_col,
              prettyNum(nrow(bad_res_dates), big.mark = ",")))
  cat(sprintf("  Count:  %s (%.2f%% of total)\n",
              prettyNum(nrow(bad_res_dates), big.mark = ","),
              100 * nrow(bad_res_dates) / nrow(DT)))
  cat(sprintf("  Min:    %.4f days\n", min(bad_res_dates$res_duration_days, na.rm = TRUE)))
  cat(sprintf("  Max:    %.4f days\n", max(bad_res_dates$res_duration_days, na.rm = TRUE)))
  cat(sprintf("  Mean:   %.4f days\n", mean(bad_res_dates$res_duration_days, na.rm = TRUE)))
  cat(sprintf("  Median: %.4f days\n", median(bad_res_dates$res_duration_days, na.rm = TRUE)))
  cat(sprintf("  SD:     %.4f days\n", sd(bad_res_dates$res_duration_days, na.rm = TRUE)))
  
  # --- Sample records
  sample_rows <- bad_res_dates[sample(.N, min(.N, sample_size))]
  display_cols <- c(unique_key_col, agency_col, created_col, resolution_col, "res_duration_days")
  cat("\nSample problematic records:\n")
  print(sample_rows[, ..display_cols], row.names = FALSE)
  
  # Fixed Pareto-style combo chart for negative durations
  if (make_pareto) {
    tryCatch({
      
      # Choose window for analysis - flip to positive values
      lower_lim <- 0        # Start at creation time
      upper_lim <- 1.1      # Show up to 1.2 days before creation
      
      # Filter to window and convert to absolute values (days before creation)
      window_data <- bad_res_dates[
        res_duration_days >= -upper_lim & res_duration_days <= -lower_lim
      ]
      
      # Convert negative durations to positive "days before creation"
      window_data[, days_before_creation := abs(res_duration_days)]
      
      if (nrow(window_data) == 0) {
        cat("No data in the 0 to 1.2 day window for Pareto chart.\n")
      } else {
        
        # Define breaks (15 minutes in days)
        step_days <- 15 / 1440   
        breaks <- seq(from = lower_lim, to = upper_lim, by = step_days)
        
        # Cut into bins using the positive values
        bin_factor <- cut(
          window_data$days_before_creation,
          breaks = breaks,
          right = TRUE,
          include.lowest = TRUE
        )
        
        # Count per bin
        bin_counts <- as.data.table(table(bin_factor, useNA = "no"))
        setnames(bin_counts, c("range", "count"))
        bin_counts[, count := as.integer(count)]
        
        # Remove empty bins
        bin_counts <- bin_counts[count > 0]
        
        if (nrow(bin_counts) == 0) {
          cat("No non-zero bins for Pareto chart.\n")
        } else {
          
          # Extract numeric bounds more safely - handle both ( and [ brackets
          # Alternative: use the original breaks to reconstruct bounds
          bin_counts[, bin_index := seq_len(.N)]
          
          # For each bin, find its corresponding break indices
          bin_counts[, lower := breaks[bin_index]]
          bin_counts[, upper := breaks[bin_index + 1]]
          
          # Verify parsing worked
          if (any(is.na(bin_counts$lower)) || any(is.na(bin_counts$upper))) {
            # Fallback: manual parsing with better regex
            bin_counts[, lower := as.numeric(gsub("^[\\(\\[]([^,]+),.*", "\\1", range))]
            bin_counts[, upper := as.numeric(gsub("^[^,]+,([^\\]\\)]+)[\\]\\)]$", "\\1", range))]
          }
          
          # Check for parsing failures
          if (any(is.na(bin_counts$lower)) || any(is.na(bin_counts$upper))) {
            cat("Warning: Some bin ranges could not be parsed properly.\n")
            print(bin_counts[is.na(lower) | is.na(upper), .(range)])
          }
          
          # Calculate midpoint in hours (positive values)
          bin_counts[, midpoint_hr := (lower + upper) / 2 * 24]
          
          # Sort by midpoint (0 hours first, then increasing)
          setorder(bin_counts, midpoint_hr)
          
          # Calculate cumulative statistics
          bin_counts[, cum_count := cumsum(count)]
          bin_counts[, cum_pct := 100 * cum_count / sum(count)]
          
          # Create a simple sequential index for x-axis
          bin_counts[, x_index := seq_len(.N)]
          
          # Create display labels for selected points with negative sign
          n_labels <- min(10, nrow(bin_counts))
          label_indices <- round(seq(1, nrow(bin_counts), length.out = n_labels))
          bin_counts[, show_label := x_index %in% label_indices]
          bin_counts[, x_label := ifelse(show_label, sprintf("-%.1f h", midpoint_hr), "")]
          
          # Calculate maximum count for scaling
          max_count <- max(bin_counts$count)
          
          # Create the plot
          p_pareto <- ggplot(bin_counts, aes(x = x_index)) +
            # Bar chart (count)
            geom_col(aes(y = count), fill = "#0072B2", alpha = 0.8) +
            
            # Cumulative line (scaled to fit on same axis)
            geom_line(aes(y = cum_pct * max_count / 100), 
                      color = "#D55E00", linewidth = 1.1, group = 1) +
            geom_point(aes(y = cum_pct * max_count / 100), 
                       color = "#D55E00", size = 1.5) +
            
            # 80% reference line
            geom_hline(yintercept = 0.8 * max_count, 
                       linetype = "dashed", color = "#E69F00", alpha = 0.8) +
            
            # 80% label
            annotate("text", 
                     x = nrow(bin_counts) * 0.1, 
                     y = 0.8 * max_count,
                     label = "80%", 
                     vjust = -0.5, 
                     color = "#E69F00", 
                     size = 3.5) +
            
            # Scales
            scale_y_continuous(
              name = "",
              sec.axis = sec_axis(
                transform = ~ . * 100 / max_count,
                name = "Cumulative %",
                breaks = seq(0, 100, 20)
              )
            ) +
            
            scale_x_continuous(
              name = "Days Updated Before Created_date",
              breaks = bin_counts$x_index[bin_counts$show_label],
              labels = bin_counts$x_label[bin_counts$show_label]
            ) +
            
            # Labels and theme
            labs(
              title = "Resolution_Action_Updated_Dates Before Created_Date",
              subtitle = paste0("15-minute bins (0 to ", 
                                round(upper_lim, 1), " days before creation)")
            ) +
            
            theme_minimal() +
            theme(
              axis.text.x = element_text(angle = 45, hjust = 1),
              panel.grid.minor.x = element_blank(),
              legend.position = "none"
            )
          
          # Apply custom theme if available
          if (exists("david_theme", mode = "function")) {
            p_pareto <- p_pareto + david_theme(text_size = 12, 
                                               x_axis_text_size = 9, x_axis_angle = 45)
          }
          
          print(p_pareto)
          Sys.sleep(3)
          
          # Save plot
          ggsave(file.path(chart_dir, "histogram_resolution_updates_before_created.pdf"),
                 plot = p_pareto, width = 13, height = 8.5)
          
        }
      }
    }, error = function(e) {
      cat("ERROR: Pareto combo chart creation failed:", conditionMessage(e), "\n")
      cat("Traceback:\n")
      print(traceback())
    })
  }
  
  
  # --- Boxplot of negative resolution updted durations by agency
  if (make_boxplot && exists("plot_boxplot", mode = "function")) {
    tryCatch({
      plot_data <- bad_res_dates[, .(
        agency = get(agency_col),
        res_duration_days = res_duration_days
      )]
      
      plot_data_filtered <- plot_data[res_duration_days > -1000]
      
      # keep ~1 in 10 points
      plot_data_filtered <- plot_data_filtered[sample(.N, .N %/% 3)]
      
      plot_boxplot(
        DT        = plot_data_filtered,
        value_col = res_duration_days,
        by_col    = agency,
        chart_dir = chart_dir,
        filename  = "boxplot_negative_resolution_updates_by_agency.pdf",
        title     = "Distribution of Negative Resolution Updates by Agency",
        order_by  = "median",
        flip      = TRUE,
        top_n     = 30,
        outlier_color = "gray40",
        outlier_alpha = 0.5,
        min_count = 50,
        zero_line = FALSE,
        x_scale_type = "pseudo_log",
        outlier_size = 1.5,
        jitter_alpha = 0.5,
        jitter_shape_inlier = 1,
        jitter_size = 1,
        jitter_sample_inliers = 1000L
      )
      
      # cat(sprintf("Boxplot saved to: %s\n",
      #             file.path(chart_dir, "boxplot_negative_resolution_updates_by_agency.pdf")))
    }, error = function(e) {
      cat("NOTE: plot_boxplot() failed:", conditionMessage(e), "\n")
    })
  }
  
  # --- Agency-level Pareto chart
  if (make_pareto && exists("plot_pareto_combo", mode = "function")) {
    tryCatch({
      plot_pareto_combo(
        DT = bad_res_dates,
        x_col = agency,
        chart_dir = chart_dir,
        filename = "pareto_negative_resolution_updates_by_agency.pdf",
        title = "Negative Resolution Updates by Agency", 
        top_n = 30,
        show_threshold_80 = TRUE,   # whether to draw the 80% reference line
        include_na = FALSE
      )
      # cat(sprintf("Agency Pareto chart saved to: %s\n",
      #             file.path(chart_dir, "pareto_negative_resolution_updates_by_agency.pdf")))
    }, error = function(e) {
      cat("NOTE: Agency Pareto chart failed:", conditionMessage(e), "\n")
    })
  }
  
  # --- Cleanup
  DT[, res_duration_days := NULL]
  
  invisible(list(
    problematic_records = bad_res_dates,
    summary_stats = list(
      count = nrow(bad_res_dates),
      min_duration = min(bad_res_dates$res_duration_days, na.rm = TRUE),
      max_duration = max(bad_res_dates$res_duration_days, na.rm = TRUE),
      mean_duration = mean(bad_res_dates$res_duration_days, na.rm = TRUE),
      median_duration = median(bad_res_dates$res_duration_days, na.rm = TRUE),
      sd_duration = sd(bad_res_dates$res_duration_days, na.rm = TRUE)
    )
  ))
}
