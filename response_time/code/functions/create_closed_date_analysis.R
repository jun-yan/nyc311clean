#' Create comprehensive closed date analysis charts for NYC 311 data
#'
#' @param data Data table containing NYC 311 records with required columns
#' @param target_year Minimum year to include in analysis (default: minimum year in data)
#' @param output_dir Base output directory for charts (default: current working directory)
#' @param agencies Vector of agencies to analyze (default: all agencies in data)
#' @param chart_width Width of output charts in inches (default: 13)
#' @param chart_height Height of output charts in inches (default: 8.5)
#' @param chart_dpi DPI for output charts (default: 300)
#' @param display_charts Whether to display charts to screen (default: TRUE)
#' @param save_charts Whether to save charts to PDF files (default: TRUE)
#'
#' @return Invisible list of generated plots
#' @export
create_closed_date_analysis <- function(data,
                                        target_year = NULL,
                                        output_dir = getwd(),
                                        agencies = NULL,
                                        chart_width = 13,
                                        chart_height = 8.5,
                                        chart_dpi = 300,
                                        display_charts = TRUE,
                                        save_charts = TRUE) {
  
  # Load required libraries
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required but not installed.")
  }
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("Package 'data.table' is required but not installed.")
  }
  
  library(ggplot2)
  library(data.table)
  
  # Validate inputs
  required_cols <- c("agency", "closed_date", "created_date")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # Convert to data.table if not already
  if (!data.table::is.data.table(data)) {
    data <- data.table::as.data.table(data)
  }
  
  # Set target year if not provided
  if (is.null(target_year)) {
    target_year <- min(year(data$closed_date), na.rm = TRUE)
  }
  
  # Set agencies if not provided
  if (is.null(agencies)) {
    agencies <- unique(data$agency)
    agencies <- sort(agencies)  # Sort alphabetically
  }
  
  # Setup directory structure
  base_dir <- file.path(output_dir, "datacleaningproject", "nyc311clean", "response_time")
  chart_dir <- file.path(base_dir, "charts")
  closed_date_chart_dir <- file.path(chart_dir, "closed_date_timeline_charts")
  
  # Create directories if they don't exist
  if (save_charts) {
    dir.create(closed_date_chart_dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  cat("\nCreating charts for", length(agencies), "agencies for years >=", target_year, "\n")
  
  # Calculate global date range for consistent x-axis
  global_date_range <- data[year(closed_date) >= target_year, 
                            .(min_date = min(as.Date(created_date), na.rm = TRUE) - 1,
                              max_date = max(as.Date(closed_date), na.rm = TRUE) + 1)]
  
  cat("Global date range:", as.character(global_date_range$min_date), 
      "to", as.character(global_date_range$max_date), "\n")
  
  # Storage for all plots
  all_plots <- list()
  
  # Create charts for each agency
  for(i in 1:length(agencies)) {
    current_agency <- agencies[i]
    cat("\nProcessing Agency", i, "of", length(agencies), ":", current_agency, "\n")
    
    # Filter data for current agency and year
    agency_data_base <- data[agency == current_agency & year(closed_date) >= target_year]
    
    # Check if there's any data
    if (nrow(agency_data_base) == 0) {
      cat("  No records found for", current_agency, "in years >=", target_year, "\n")
      next
    }
    
    # Print summary
    total_records <- nrow(agency_data_base)
    cat("  Total records:", format(total_records, big.mark = ","), "\n")
    
    # Storage for current agency's plots
    agency_plots <- list()
    
    #--------------------------------------------------------------------------
    # CHART 1: Daily Timeline
    #--------------------------------------------------------------------------
    agency_data <- agency_data_base[, .N, by = .(date = as.Date(closed_date))][order(date)]
    
    if (nrow(agency_data) > 0) {
      cat("  Date range:", format(min(agency_data$date), "%Y-%m-%d"), "to", 
          format(max(agency_data$date), "%Y-%m-%d"), "\n")
      cat("  Number of days with records:", nrow(agency_data), "\n")
      
      # Calculate statistics
      median_val <- median(agency_data$N)
      mean_val <- mean(agency_data$N)
      sd_val <- sd(agency_data$N)
      
      # Check for meaningful statistics
      if (nrow(agency_data) > 1 && is.finite(sd_val)) {
        three_sigma_val <- mean_val + 3 * sd_val
        show_sigma_line <- TRUE
      } else {
        three_sigma_val <- NA
        show_sigma_line <- FALSE
        cat("  Note: Insufficient data for 3-sigma calculation\n")
      }
      
      # Find max count and date
      max_count <- max(agency_data$N)
      max_date <- agency_data[N == max_count, date][1]
      total_records_formatted <- format(sum(agency_data$N), big.mark = ",")
      
      # Create plot
      p1 <- ggplot(agency_data, aes(x = date, y = N)) +
        geom_bar(stat = "identity", fill = "steelblue4", alpha = 0.7) +
        geom_hline(yintercept = mean_val, color = "grey40", linetype = "dashed", linewidth = 1) +
        annotate("text", x = max(agency_data$date), y = mean_val, 
                 label = paste("Mean:", round(mean_val, 0)), 
                 hjust = 1, vjust = -0.3, color = "grey40", fontface = "bold")
      
      if (show_sigma_line) {
        p1 <- p1 + 
          geom_hline(yintercept = three_sigma_val, color = "orange4", linetype = "dotted", linewidth = 1) +
          annotate("text", x = max(agency_data$date), y = three_sigma_val, 
                   label = paste("+3σ:", round(three_sigma_val, 0)), 
                   hjust = 1, vjust = 1.3, color = "orange4", fontface = "bold")
      }
      
      p1 <- p1 + 
        scale_x_date(limits = c(global_date_range$min_date, global_date_range$max_date),
                     date_breaks = "3 months", date_labels = "%Y-%m") +
        labs(title = paste("Daily Count of Records by Closed Date for", current_agency, "in years >=", target_year),
             subtitle = paste("Total records for", current_agency, ":", total_records_formatted, 
                              "| Highest single day:", format(max_count, big.mark = ","),
                              "on", format(max_date, "%d %b %Y")),
             x = "", y = "") +
        theme_minimal() +
        theme(panel.background = element_rect(fill = "grey93"),
              panel.grid.major = element_line(color = "white", linewidth = 0.75),
              panel.grid.minor = element_blank(),
              axis.text.y = element_text(face = "bold"),
              axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
              plot.title = element_text(hjust = 0.5, size = 12, face = "bold"))
      
      agency_plots[["daily_timeline"]] <- p1
      
      if (display_charts) {
        print(p1)
        Sys.sleep(2)
      }
      
      if (save_charts) {
        ggsave(filename = file.path(closed_date_chart_dir, 
                                    paste0(current_agency, "_beginning_", target_year, "_closed_by_date.pdf")), 
               plot = p1, width = chart_width, height = chart_height, dpi = chart_dpi)
      }
    }
    
    #--------------------------------------------------------------------------
    # CHART 2: Day of Month Analysis
    #--------------------------------------------------------------------------
    agency_data_dom <- agency_data_base[, .N, by = .(day_of_month = mday(closed_date))][order(day_of_month)]
    
    if (nrow(agency_data_dom) > 0) {
      mean_val_dom <- mean(agency_data_dom$N)
      three_sigma_val_dom <- mean_val_dom + 3 * sd(agency_data_dom$N)
      
      p2 <- ggplot(agency_data_dom, aes(x = day_of_month, y = N)) +
        geom_bar(stat = "identity", fill = "maroon4", alpha = 0.7) +
        geom_hline(yintercept = mean_val_dom, color = "grey30", linetype = "dashed", linewidth = 1) +
        geom_hline(yintercept = three_sigma_val_dom, color = "orange4", linetype = "dotted", linewidth = 1) +
        annotate("text", x = 30, y = mean_val_dom, label = paste("Mean:", round(mean_val_dom, 0)),
                 hjust = 1, vjust = -0.3, color = "grey30", fontface = "bold") +
        annotate("text", x = 30, y = three_sigma_val_dom, label = paste("+3σ:", round(three_sigma_val_dom, 0)),
                 hjust = 1, vjust = 1.3, color = "orange4", fontface = "bold") +
        scale_x_continuous(limits = c(0.5, 31.5), breaks = seq(1, 31, 2)) +
        labs(title = paste("Closed Records by Day of Month for", current_agency, "in years >=", target_year),
             subtitle = paste("Total records for", current_agency, ":", format(total_records, big.mark = ",")),
             x = "", y = "") +
        theme_minimal() +
        theme(panel.background = element_rect(fill = "grey93"),
              panel.grid.major = element_line(color = "white", linewidth = 0.75),
              panel.grid.minor = element_blank(),
              axis.text.y = element_text(face = "bold"),
              axis.text.x = element_text(face = "bold"),
              plot.title = element_text(hjust = 0.5, size = 12, face = "bold"))
      
      agency_plots[["day_of_month"]] <- p2
      
      if (display_charts) {
        print(p2)
        Sys.sleep(2)
      }
      
      if (save_charts) {
        ggsave(filename = file.path(closed_date_chart_dir, 
                                    paste0(current_agency, "_beginning_", target_year, "_closed_by_day_of_month.pdf")), 
               plot = p2, width = chart_width, height = chart_height, dpi = chart_dpi)
      }
    }
    
    #--------------------------------------------------------------------------
    # CHART 3: Day of Week Analysis
    #--------------------------------------------------------------------------
    agency_data_dow <- agency_data_base[, .N, by = .(day_of_week = wday(closed_date, label = TRUE, abbr = FALSE, week_start = 1))][order(day_of_week)]
    
    if (nrow(agency_data_dow) > 0) {
      mean_val_dow <- mean(agency_data_dow$N)
      three_sigma_val_dow <- mean_val_dow + 3 * sd(agency_data_dow$N)
      
      p3 <- ggplot(agency_data_dow, aes(x = day_of_week, y = N)) +
        geom_bar(stat = "identity", fill = "darkolivegreen4", alpha = 0.7) +
        geom_hline(yintercept = mean_val_dow, color = "grey30", linetype = "dashed", linewidth = 1) +
        geom_hline(yintercept = three_sigma_val_dow, color = "orange4", linetype = "dotted", linewidth = 1) +
        annotate("text", x = 6.5, y = mean_val_dow, label = paste("Mean:", round(mean_val_dow, 0)),
                 hjust = 1, vjust = -0.3, color = "grey30", fontface = "bold") +
        annotate("text", x = 6.5, y = three_sigma_val_dow, label = paste("+3σ:", round(three_sigma_val_dow, 0)),
                 hjust = 1, vjust = 1.3, color = "orange4", fontface = "bold") +
        labs(title = paste("Closed Records by Day of Week for", current_agency, "in years >=", target_year),
             subtitle = paste("Total records for", current_agency, ":", format(total_records, big.mark = ",")),
             x = "", y = "") +
        theme_minimal() +
        theme(panel.background = element_rect(fill = "grey93"),
              panel.grid.major = element_line(color = "white", linewidth = 0.75),
              panel.grid.minor = element_blank(),
              axis.text.y = element_text(face = "bold"),
              axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
              plot.title = element_text(hjust = 0.5, size = 12, face = "bold"))
      
      agency_plots[["day_of_week"]] <- p3
      
      if (display_charts) {
        print(p3)
        Sys.sleep(2)
      }
      
      if (save_charts) {
        ggsave(filename = file.path(closed_date_chart_dir, 
                                    paste0(current_agency, "_beginning_", target_year, "_closed_by_day_of_week.pdf")), 
               plot = p3, width = chart_width, height = chart_height, dpi = chart_dpi)
      }
    }
    
    #--------------------------------------------------------------------------
    # CHART 4: Hour of Day Analysis
    #--------------------------------------------------------------------------
    agency_data_hod <- agency_data_base[, .N, by = .(hour_of_day = hour(closed_date))][order(hour_of_day)]
    
    if (nrow(agency_data_hod) > 0) {
      mean_val_hod <- mean(agency_data_hod$N)
      three_sigma_val_hod <- mean_val_hod + 3 * sd(agency_data_hod$N)
      
      p4 <- ggplot(agency_data_hod, aes(x = hour_of_day, y = N)) +
        geom_bar(stat = "identity", fill = "dodgerblue3", alpha = 0.7) +
        geom_hline(yintercept = mean_val_hod, color = "grey30", linetype = "dashed", linewidth = 1) +
        geom_hline(yintercept = three_sigma_val_hod, color = "orange4", linetype = "dotted", linewidth = 1) +
        annotate("text", x = 22.5, y = mean_val_hod, label = paste("Mean:", round(mean_val_hod, 0)),
                 hjust = 1, vjust = -0.3, color = "grey30", fontface = "bold") +
        annotate("text", x = 22.5, y = three_sigma_val_hod, label = paste("+3σ:", round(three_sigma_val_hod, 0)),
                 hjust = 1, vjust = 1.3, color = "orange4", fontface = "bold") +
        scale_x_continuous(limits = c(-0.5, 23.5), breaks = seq(0, 23, 2)) +
        labs(title = paste("Closed Records by Hour of Day for", current_agency, "in years >=", target_year),
             subtitle = paste("Total records for", current_agency, ":", format(total_records, big.mark = ",")),
             x = "Hour of Day (24-hour clock)", y = "") +
        theme_minimal() +
        theme(panel.background = element_rect(fill = "grey93"),
              panel.grid.major = element_line(color = "white", linewidth = 0.75),
              panel.grid.minor = element_blank(),
              axis.text.y = element_text(face = "bold"),
              axis.text.x = element_text(face = "bold"),
              plot.title = element_text(hjust = 0.5, size = 12, face = "bold"))
      
      agency_plots[["hour_of_day"]] <- p4
      
      if (display_charts) {
        print(p4)
        Sys.sleep(2)
      }
      
      if (save_charts) {
        ggsave(filename = file.path(closed_date_chart_dir, 
                                    paste0(current_agency, "_beginning_", target_year, "_closed_by_hour_of_day.pdf")), 
               plot = p4, width = chart_width, height = chart_height, dpi = chart_dpi)
      }
    }
    
    #--------------------------------------------------------------------------
    # CHART 5: Day of Year Analysis
    #--------------------------------------------------------------------------
    agency_data_doy <- agency_data_base[, .N, by = .(day_of_year = yday(closed_date))][order(day_of_year)]
    
    if (nrow(agency_data_doy) > 0) {
      mean_val_doy <- mean(agency_data_doy$N)
      three_sigma_val_doy <- mean_val_doy + 3 * sd(agency_data_doy$N)
      
      p5 <- ggplot(agency_data_doy, aes(x = day_of_year, y = N)) +
        geom_bar(stat = "identity", fill = "darkslategray4", alpha = 0.7) +
        geom_hline(yintercept = mean_val_doy, color = "grey40", linetype = "dashed", linewidth = 1) +
        geom_hline(yintercept = three_sigma_val_doy, color = "orange4", linetype = "dashed", linewidth = 1) +
        annotate("text", x = 360, y = mean_val_doy, label = paste("Mean:", round(mean_val_doy, 0)),
                 hjust = 1, vjust = -0.3, color = "grey40", fontface = "bold") +
        annotate("text", x = 360, y = three_sigma_val_doy, label = paste("+3σ:", round(three_sigma_val_doy, 0)),
                 hjust = 1, vjust = 1.3, color = "orange4", fontface = "bold") +
        scale_x_continuous(limits = c(0.5, 366.5), 
                           breaks = c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335, 366),
                           labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Dec 31")) +
        labs(title = paste("Closed Records by Day of Year for", current_agency, "in years >=", target_year),
             subtitle = paste("Total records for", current_agency, ":", format(total_records, big.mark = ",")),
             x = "", y = "") +
        theme_minimal() +
        theme(panel.background = element_rect(fill = "grey93"),
              panel.grid.major = element_line(color = "white", linewidth = 0.75),
              panel.grid.minor = element_blank(),
              axis.text.y = element_text(face = "bold"),
              axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
              plot.title = element_text(hjust = 0.5, size = 12, face = "bold"))
      
      agency_plots[["day_of_year"]] <- p5
      
      if (display_charts) {
        print(p5)
        Sys.sleep(2)
      }
      
      if (save_charts) {
        ggsave(filename = file.path(closed_date_chart_dir, 
                                    paste0(current_agency, "_beginning_", target_year, "_closed_by_day_of_year.pdf")), 
               plot = p5, width = chart_width, height = chart_height, dpi = chart_dpi)
      }
    }
    
    #--------------------------------------------------------------------------
    # CHART 6: Month of Year Analysis
    #--------------------------------------------------------------------------
    agency_data_moy <- agency_data_base[, .N, by = .(month_of_year = month(closed_date, label = TRUE, abbr = TRUE))][order(month_of_year)]
    
    if (nrow(agency_data_moy) > 0) {
      mean_val_moy <- mean(agency_data_moy$N)
      three_sigma_val_moy <- mean_val_moy + 3 * sd(agency_data_moy$N)
      
      p6 <- ggplot(agency_data_moy, aes(x = month_of_year, y = N)) +
        geom_bar(stat = "identity", fill = "plum4", alpha = 0.7) +
        geom_hline(yintercept = mean_val_moy, color = "grey40", linetype = "dashed", linewidth = 1) +
        geom_hline(yintercept = three_sigma_val_moy, color = "orange4", linetype = "dashed", linewidth = 1) +
        annotate("text", x = 11.5, y = mean_val_moy, label = paste("Mean:", round(mean_val_moy, 0)),
                 hjust = 1, vjust = -0.3, color = "grey40", fontface = "bold") +
        annotate("text", x = 11.5, y = three_sigma_val_moy, label = paste("+3σ:", round(three_sigma_val_moy, 0)),
                 hjust = 1, vjust = 1.3, color = "orange4", fontface = "bold") +
        labs(title = paste("Closed Records by Month of Year for", current_agency, "in years >=", target_year),
             subtitle = paste("Total records for", current_agency, ":", format(total_records, big.mark = ",")),
             x = "", y = "") +
        theme_minimal() +
        theme(panel.background = element_rect(fill = "grey93"),
              panel.grid.major = element_line(color = "white", linewidth = 0.75),
              panel.grid.minor = element_blank(),
              axis.text.y = element_text(face = "bold"),
              axis.text.x = element_text(face = "bold"),
              plot.title = element_text(hjust = 0.5, size = 12, face = "bold"))
      
      agency_plots[["month_of_year"]] <- p6
      
      if (display_charts) {
        print(p6)
        Sys.sleep(2)
      }
      
      if (save_charts) {
        ggsave(filename = file.path(closed_date_chart_dir, 
                                    paste0(current_agency, "_beginning_", target_year, "_closed_by_month_of_year.pdf")), 
               plot = p6, width = chart_width, height = chart_height, dpi = chart_dpi)
      }
    }
    
    # Store all plots for this agency
    all_plots[[current_agency]] <- agency_plots
  }
  
  cat("\nAnalysis complete! Generated charts for", length(all_plots), "agencies.\n")
  
  # Return all plots invisibly
  invisible(all_plots)
}

# Example usage:
# result_plots <- create_closed_date_analysis(
#   data = data_for_closed_day_analysis,
#   target_year = 2020,
#   output_dir = getwd(),
#   agencies = c("NYPD", "DOT", "HPD"),  # Specific agencies, or NULL for all
#   display_charts = TRUE,
#   save_charts = TRUE
# )