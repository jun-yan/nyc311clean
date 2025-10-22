standardize_missing_chars <- function(DT,
                                      tokens = c("na","<na>","n/a","null"),
                                      report_zeroes = FALSE) {
  stopifnot(is.data.table(DT))
  
  labels <- c("<blank>", tokens)                 # what we will count
  token_counts <- setNames(integer(length(labels)), labels)
  total_to_na <- 0L
  
  cat("\n\n[ Normalizing missing-like values (character) -> NA ]\n")
  
  char_cols <- names(DT)[vapply(DT, is.character, logical(1))]
  # Sort column names alphabetically
  char_cols <- sort(char_cols)
  
  for (col in char_cols) {
    x <- DT[[col]]
    x_trim <- trimws(x)
    
    # Key used for matching tokens (exact, after trim); NA stays NA
    keys <- ifelse(is.na(x_trim), NA_character_,
                   ifelse(x_trim == "", "<blank>", tolower(x_trim)))
    
    to_na   <- !is.na(keys) & (keys == "<blank>" | keys %in% tokens)
    to_trim <- !is.na(x) & (x != x_trim) & !to_na
    
    # Count tokens converted in this column
    if (any(to_na)) {
      tab <- table(keys[to_na])
      for (k in names(tab)) token_counts[k] <- token_counts[k] + as.integer(tab[[k]])
    }
    
    # Apply changes
    if (any(to_na))   set(DT, which(to_na), col, NA_character_)
    if (any(to_trim)) set(DT, which(to_trim), col, x_trim[to_trim])
    
    added <- sum(to_na)
    total_to_na <- total_to_na + added
    pct <- 100 * added / nrow(DT)
    
    # inside the for-loop, per column
    if (added > 0L || any(to_trim)) {
      cat(sprintf(" - %-25s: %6d → NA (%.2f%%)%s\n",
                  col,
                  added,
                  pct,
                  if (any(to_trim)) " (whitespace trimmed)" else ""))
    } else {
      cat(sprintf(" - %-25s: %6d → NA (%.2f%%, no changes)\n",
                  col,
                  added,
                  pct))
    }
    
  }  # <-- this closes the for-loop
  
  # Summary
  cat("\nSummary of values converted to NA by token:\n")
  idx <- if (report_zeroes) seq_along(token_counts) else which(token_counts > 0L)
  if (length(idx) == 0L) {
    cat("   (none)\n")
  } else {
    ord <- order(token_counts[idx], decreasing = TRUE)
    for (j in idx[ord]) {
      cat(sprintf("   %-10s : %d\n", names(token_counts)[j], token_counts[j]))
    }
  }
  
  # Compute total cells in the dataset
  # after the loop, overall summary
  total_cells <- nrow(DT) * ncol(DT)
  pct_total   <- 100 * total_to_na / total_cells
  
  cat(sprintf(
    "\nTotal values normalized to NA: %d (%.2f%%)\n",
    total_to_na,
    pct_total
  ))
  
  invisible(list(token_counts = token_counts, total_converted = total_to_na))
}
