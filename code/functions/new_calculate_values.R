#########################################################################

calculate_values <- function(X, base = 25) {
  # Special handling for small values
  if (X <= 5) {
    return(list(starting_value = 5, increment = 1))
  } else if (X > 5 && X <= 10) {
    return(list(starting_value = 10, increment = 2))
  } else if (X > 10 && X <= 15) {
    return(list(starting_value = 15, increment = 3))
  } else if (X > 15 && X < base) {
    return(list(starting_value = X, increment = floor(X / 5)))
  }
  
  # Regular handling for X >= base
  tier_base <- base * 10^floor(log10(X / base))
  multiplier <- ceiling(X / tier_base)
  starting_value <- tier_base * multiplier
  increment <- floor(starting_value / 5)
  
  # Ensure starting_value does not exceed X
  if (starting_value > X) {
    starting_value <- floor(X / 10) * 10  # Adjust starting value closer to X, rounded down
    increment <- max(1, floor(starting_value / 5))  # Ensure increment is a positive integer
  }
  
  return(list(starting_value = starting_value, increment = increment))
}

#########################################################################