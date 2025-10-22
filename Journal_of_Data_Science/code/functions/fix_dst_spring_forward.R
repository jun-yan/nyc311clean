fix_dst_spring_forward <- function(out, original_vals, failures,
                                   fmt, local_tz,
                                   agency = NULL,
                                   unique_key = NULL) {
  fixed_count <- 0L
  if (!length(failures)) return(list(out = out, fixed_count = 0L))
  
  # --- Flatten to character safely ---
  bad_vec <- original_vals[failures]
  if (is.data.frame(bad_vec)) bad_vec <- bad_vec[[1]]
  if (is.list(bad_vec))       bad_vec <- unlist(bad_vec, use.names = FALSE)
  bad_vec <- as.character(bad_vec)
  
  # --- Use base as.Date to avoid zoo masking ---
  bad_dates <- suppressWarnings(base::as.Date(bad_vec, format = "%m/%d/%Y %I:%M:%S %p"))
  bad_years <- suppressWarnings(as.integer(format(bad_dates, "%Y")))
  
  # --- Compute DST start dates ---
  dst_info  <- tryCatch(get_dst_start(bad_years, tz = local_tz), error = function(e) NULL)
  if (is.null(dst_info) || !"dst_start" %in% names(dst_info))
    return(list(out = out, fixed_count = 0L))
  dst_dates <- base::as.Date(dst_info$dst_start)
  
  # --- Identify affected times ---
  is_dst_start <- !is.na(bad_dates) & bad_dates %in% dst_dates
  is_02_am     <- grepl("\\s02:\\d{2}:\\d{2}\\sAM$", bad_vec)
  to_fix_idx   <- which(is_dst_start & is_02_am)
  
  if (length(to_fix_idx)) {
    bad_fixed <- bad_vec
    bad_fixed[to_fix_idx] <- sub("\\s02:", " 03:", bad_fixed[to_fix_idx])
    reparsed <- suppressWarnings(as.POSIXct(bad_fixed, format = fmt, tz = local_tz))
    out[failures[to_fix_idx]] <- reparsed[to_fix_idx]
    fixed_count <- sum(!is.na(reparsed))
    
    cat(sprintf("\n   [DST-fix applied: %d corrections]\n", fixed_count))
    df_print <- data.frame(
      unique_key = if (!is.null(unique_key)) unique_key[failures[to_fix_idx]] else NA,
      agency     = if (!is.null(agency))     agency[failures[to_fix_idx]]     else NA,
      before     = bad_vec[to_fix_idx],
      after      = as.character(reparsed[to_fix_idx]),
      stringsAsFactors = FALSE
    )
    print(utils::head(df_print, 10), row.names = FALSE)
  } else {
    cat("   [No DST-fix required for this column]\n")
  }
  
  return(list(out = out, fixed_count = fixed_count))
}
