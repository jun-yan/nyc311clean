#########################################################################

# Define the function to calculate summary statistics
calculate_summary_stats <- function(df, column_name) {
  mean_value <- round(mean(df[[column_name]], na.rm = TRUE), 0)
  median_value <- round(median(df[[column_name]], na.rm = TRUE), 0)
  sd_value <- round(sd(df[[column_name]], na.rm = TRUE), 0)
  
  return(list(mean = mean_value, median = median_value, sd = sd_value))
}

#########################################################################