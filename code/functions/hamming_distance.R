#########################################################################
# compute the Hamming distance between two strings. Determine if it meets the threshold.
hamming_distance <- function(string1, string2) {
  if (nchar(string1) != nchar(string2)) {
    stop("Strings must be of equal length")
  }
  num_diff <- sum(as.numeric(charToRaw(string1)) != as.numeric(charToRaw(string2)))
  return(num_diff)
}

#########################################################################