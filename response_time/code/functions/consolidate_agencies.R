################################################################################

#' Consolidate agency codes using a strict one-way mapping
#'
#' @param dt A data.table with an 'agency' column.
#' @param replacements Named character vector: c("OLD1" = "NEW1", ...).
#' @param log_changes Logical. Print summary of changes. Default is TRUE.
#'
#' @return The modified data.table (invisibly).
consolidate_agencies <- function(dt, replacements, log_changes = TRUE) {
  stopifnot(data.table::is.data.table(dt))
  stopifnot("agency" %in% names(dt))
  stopifnot(is.character(replacements), !is.null(names(replacements)))
  
  # Track actual changes applied
  changed_counts <- sapply(names(replacements), function(old_val) {
    sum(dt$agency == old_val, na.rm = TRUE)
  })
  
  # Apply replacements
  dt[, agency := ifelse(agency %in% names(replacements), replacements[agency], agency)]
  
  if (log_changes) {
    actually_replaced <- names(changed_counts)[changed_counts > 0]
    if (length(actually_replaced) > 0) {
      cat("✅ Agency consolidation completed.\n")
      for (old_val in actually_replaced) {
        new_val <- replacements[[old_val]]
        count <- changed_counts[[old_val]]
        cat(sprintf("🔁 %s → %s (%d rows)", old_val, new_val, count))
		cat("\n")
      }
    } else {
      message("ℹ️ No matching agency values found to replace.")
    }
  }
  
  invisible(dt)
}

################################################################################