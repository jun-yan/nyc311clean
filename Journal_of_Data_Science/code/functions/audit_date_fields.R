audit_date_fields <- function(DT, date_fields, fmt = "%m/%d/%Y %I:%M:%S %p", tz = "America/New_York", n_show = 5) {
  results <- list()
  
  for (f in intersect(date_fields, names(DT))) {
    vals <- DT[[f]]
    
    # If not character, coerce to character so we can see raw failures
    raw_strings <- if (inherits(vals, "POSIXct")) format(vals, fmt, tz = tz) else as.character(vals)
    
    parsed <- suppressWarnings(as.POSIXct(raw_strings, format = fmt, tz = tz))
    failures <- is.na(parsed) & !is.na(raw_strings) & raw_strings != ""
    
    n_fail <- sum(failures)
    results[[f]] <- list(
      class    = class(vals)[1],
      total    = length(vals),
      NAs      = sum(is.na(parsed)),
      failures = n_fail,
      examples = utils::head(unique(raw_strings[failures]), n_show)
    )
  }
  
  # Pretty print
  for (f in names(results)) {
    r <- results[[f]]
    cat("\n[Audit]", f,
        "| class:", r$class,
        "| total:", format(r$total, big.mark=","),
        "| parsed-NA:", format(r$NAs, big.mark=","),
        "| failed parses:", format(r$failures, big.mark=","), "\n")
    if (r$failures > 0) {
      cat("   Examples of bad values:\n")
      cat("   -", paste(r$examples, collapse="\n   - "), "\n")
    }
  }
  
  invisible(results)
}
