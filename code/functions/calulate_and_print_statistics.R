#########################################################################

calculate_and_print_statistics <- function(
    dataset, 
    x_col, 
    y_col, 
    console_print_out_title)
  {
  
  # Find the row with the maximum count
  max_row <- dataset[which.max(dataset[[y_col]]), ]
  y_max_count <- max_row[[y_col]]
  
  # Find the row with the minimum count
  min_row <- dataset[which.min(dataset[[y_col]]), ]
  y_min_count <- round(min_row[[y_col]], 0)
  
  # Calculate mean, median, and standard deviation
  y_mean_value <- round(mean(dataset[[y_col]], na.rm = TRUE), 0)
  y_median_value <- round(median(dataset[[y_col]], na.rm = TRUE), 0)
  y_sd_value <- round(sd(dataset[[y_col]], na.rm = TRUE), 0)
  y_total_count <- sum(dataset[[y_col]], na.rm = TRUE)
  
  # Print the statistical summary
  cat("\n\n***", console_print_out_title, "SRs***")
  
  cat(paste("\nMaximum", console_print_out_title, ":"), as.character(max_row[[x_col]]), "  ",
    paste("\nMaximum", console_print_out_title, "count:"), format(y_max_count, big.mark = ",")
  )
  
  cat(paste("\n\nMinimum", console_print_out_title, ":"), as.character(min_row[[x_col]]), "  ",
    paste("\nMinimum", console_print_out_title, "count: "), format(y_min_count, big.mark = ",")
  )
  
  cat(
    paste("\n\nAverage ", console_print_out_title, ":", sep = ""), format(y_mean_value, big.mark = ","), "  ",
    paste("\nMedian ", console_print_out_title, ":", sep = ""), format(y_median_value, big.mark = ","),
    paste("\nStd Dev (\u03C3) ", console_print_out_title, ":", sep = ""), format(y_sd_value, big.mark = ",")
  )
  
  # Return values in a list for further use
  return(list(
    y_max_count = y_max_count,
    y_min_count = y_min_count,
    y_mean_value = y_mean_value,
    y_median_value = y_median_value,
    y_sd_value = y_sd_value,
    y_total_count = y_total_count,
    max_row = max_row,
    min_row = min_row
  ))
}


#########################################################################