################################################################################
# Standard end program function
end_program <- function(formatted_end_time, duration) {
  
  # Close any active sink (important if you're redirecting console output)
  if (sink.number() > 0) sink()
  
  # Print completion message
  cat("\n*****END OF PROGRAM*****\n")
  
  # Print the end time
  cat("\nProgram ended at:", formatted_end_time, "\n")
  
  # Print the program duration (now as a string)
  cat("\n⏱️ Program run-time:", duration, "\n")
  
  # End the program gracefully without error
  return(invisible(NULL))
}
################################################################################
