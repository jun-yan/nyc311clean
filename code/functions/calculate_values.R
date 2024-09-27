#########################################################################
calculate_values <- function(X, base = 25) {
#    cat("\nX =", X)
    # Special handling for small values
    if (X <= 5) {
      return(list(starting_value = 5, increment = 1))
    } else if (X > 5 && X <= 10) {
      return(list(starting_value = 10, increment = 2))
    } else if (X > 10 && X < base) {
      return(list(starting_value = base, increment = base/5))
    }
    
    # Regular function for X >= base
    tier_base <- base * 10^floor(log10(X/base))
    multiplier <- ceiling(X / tier_base)
    
    starting_value <- tier_base * multiplier
    increment <- starting_value / 5
#    cat("\nResults", starting_value, increment)
    
    result <- list(starting_value = increment, increment = increment)
    return(result)
  }
