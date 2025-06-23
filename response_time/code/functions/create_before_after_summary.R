#' Create before/after summary analysis of data cleaning by agency
#'
#' @param before_data Data table with records before cleaning
#' @param after_data Data table with records after cleaning  
#' @param output_dir Directory to save charts (default: current working directory)
#' @param chart_width Width of output charts in inches (default: 13)
#' @param chart_height Height of output charts in inches (default: 8.5)
#' @param chart_dpi DPI for output charts (default: 300)
#' @param display_charts Whether to display charts to screen (default: TRUE)
#' @param save_charts Whether to save charts to PDF files (default: TRUE)
#' @param print_summary Whether to print summary tables to console (default: TRUE)
#'
#' @return List containing summary data table and plots
#' @export
create_before_after_summary <- function(before_data,
                                        after_data,
                                        output_dir = getwd(),
                                        chart_width = 13,
                                        chart_height = 8.5,
                                        chart_dpi = 300,
                                        display_charts = TRUE,
                                        save_charts = TRUE,
                                        print_summary = TRUE) {
  
  # Load required libraries
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required but not installed.")
  }
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("Package 'data.table' is required but not installed.")
  }
  if (!requireNamespace("scales", quietly = TRUE)) {
    stop("Package 'scales' is required but not installed.")
  }
  
  library(ggplot2)
  library(data.table)
  library(scales)
  
  # Validate inputs
  if (!("agency" %in% names(before_data))) {
    stop("before_data must contain an 'agency' column")
  }
  if (!("agency" %in% names(after_data))) {
    stop("after_data must contain an 'agency' column")
  }
  
  # Convert to data.table if not already
  if (!data.table::is.data.table(before_data)) {
    before_data <- data.table::as.data.table(before_data)
  }
  if (!data.table::is.data.table(after_data)) {
    after_data <- data.table::as.data.table(after_data)
  }
  
  # Setup directory structure for saving charts
  if (save_charts) {
    chart_dir <- file.path(output_dir, "datacleaningproject", "nyc311clean", "response_time", "charts")
    dir.create(chart_dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  if (print_summary) {
    cat("\n---- Before/After Summary by Agency ----\n")
  }
  
  # Get counts before cleaning
  before_counts <- before_data[, .N, by = agency]
  setnames(before_counts, "N", "Before")
  
  # Get counts after cleaning
  after_counts <- after_data[, .N, by = agency]
  setnames(after_counts, "N", "After")
  
  # Merge the counts (full outer join to capture all agencies)
  agency_summary <- merge(before_counts, after_counts, by = "agency", all = TRUE)
  
  # Replace NA values with 0 (for agencies that were completely removed)
  agency_summary[is.na(Before), Before := 0]
  agency_summary[is.na(After), After := 0]
  
  # Calculate delta and percentage change
  agency_summary[, Delta := After - Before]
  agency_summary[, Pct_Change := round((Delta / Before) * 100, 2)]
  
  # Handle cases where Before = 0 (division by zero)
  agency_summary[Before == 0 & After > 0, Pct_Change := Inf]
  agency_summary[Before == 0 & After == 0, Pct_Change := 0]
  
  # Sort by percentage change (largest negative changes first)
  setorder(agency_summary, Pct_Change)
  
  # Print the summary table
  if (print_summary) {
    print(as.data.frame(agency_summary), right = FALSE)
    
    # Print overall summary
    cat("\nSummary:\n")
    cat("Total records before:", format(sum(agency_summary$Before), big.mark = ","), "\n")
    cat("Total records after:", format(sum(agency_summary$After), big.mark = ","), "\n")
    cat("Total records removed:", format(sum(agency_summary$Before) - sum(agency_summary$After), big.mark = ","), "\n")
    cat("Overall percentage change:", round(((sum(agency_summary$After) - sum(agency_summary$Before)) / sum(agency_summary$Before)) * 100, 2), "%\n")
  }
  
  # Calculate totals for subtitle
  total_before <- sum(agency_summary$Before)
  total_after <- sum(agency_summary$After)
  total_removed <- total_before - total_after
  pct_removed <- round((total_removed / total_before) * 100, 2)
  
  # Storage for plots
  plots <- list()
  
  #############################################################################
  # Chart 1: Change (Delta) chart
  #############################################################################
  p1 <- ggplot(agency_summary, aes(x = reorder(agency, Delta), y = Delta)) +
    geom_bar(stat = "identity", 
             fill = ifelse(agency_summary$Delta >= 0, "darkseagreen4", "firebrick3"), 
             alpha = 0.8) +
    geom_hline(yintercept = 0, color = "black", linetype = "solid") +
    labs(title = "Count of Removed Records by Agency (all filters applied)",
         subtitle = paste("Total records removed:", format(total_removed, big.mark = ","), 
                          paste0("(", pct_removed, "%)")),
         x = "Agency",
         y = "") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
          plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5, size = 12),
          axis.text.y = element_text(face = "bold"),
          panel.background = element_rect(fill = "grey93"),
          panel.grid.major = element_line(color = "white", linewidth = 0.7),
          panel.grid.minor = element_blank()) +
    scale_y_continuous(labels = scales::comma) +
    coord_flip()
  
  plots[["delta_chart"]] <- p1
  
  if (display_charts) {
    print(p1)
    Sys.sleep(2)
  }
  
  # Save chart 1
  if (save_charts) {
    ggsave(file.path(chart_dir, "agency_delta_removals.pdf"), 
           plot = p1, width = chart_width, height = chart_height, 
           dpi = chart_dpi, device = "pdf", useDingbats = FALSE)
  }
  
  #############################################################################
  # Chart 2: Percentage change chart
  #############################################################################
  p2 <- ggplot(agency_summary, aes(x = reorder(agency, Pct_Change), y = Pct_Change)) +
    geom_bar(stat = "identity", 
             fill = ifelse(agency_summary$Pct_Change >= 0, "darkseagreen4", "firebrick3"), 
             alpha = 0.8) +
    geom_hline(yintercept = 0, color = "black", linetype = "solid") +
    labs(title = "Percentage of Removed Records by Agency (all filters applied)",
         x = "Agency",
         y = "Percentage Change (%)") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
          plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5, size = 12),
          axis.text.y = element_text(face = "bold"),
          panel.background = element_rect(fill = "grey93"),
          panel.grid.major = element_line(color = "white", linewidth = 0.7),
          panel.grid.minor = element_blank()) +
    coord_flip()
  
  plots[["percentage_chart"]] <- p2
  
  if (display_charts) {
    print(p2)
    Sys.sleep(2)
  }
  
  # Save chart 2
  if (save_charts) {
    ggsave(file.path(chart_dir, "agency_percentage_removals.pdf"), 
           plot = p2, width = chart_width, height = chart_height, 
           dpi = chart_dpi, device = "pdf", useDingbats = FALSE)
  }
  
  # Return summary data and plots
  result <- list(
    summary_table = agency_summary,
    plots = plots,
    total_before = sum(agency_summary$Before),
    total_after = sum(agency_summary$After),
    total_removed = sum(agency_summary$Before) - sum(agency_summary$After),
    overall_pct_change = round(((sum(agency_summary$After) - sum(agency_summary$Before)) / sum(agency_summary$Before)) * 100, 2)
  )
  
  return(result)
}

# Example usage:
# result <- create_before_after_summary(
#   before_data = backup_cleaned_data,
#   after_data = cleaned_data,
#   output_dir = getwd(),
#   display_charts = TRUE,
#   save_charts = TRUE,
#   print_summary = TRUE
# )
# 
# # Access results:
# summary_table <- result$summary_table
# delta_chart <- result$plots$delta_chart
# percentage_chart <- result$plots$percentage_chart