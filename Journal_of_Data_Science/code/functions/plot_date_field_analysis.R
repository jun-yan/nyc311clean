plot_date_field_analysis <- function(
    DT,
    date_col,
    group_col,
    chart_dir,
    label = "Date Field",
    condition_text = "",
    min_agency_count = 2,
    top_n = 30L,
    width_in = 13,
    height_in = 8.5
) {
  
  # Get string versions of column names
  date_col_name <- deparse(substitute(date_col))
  group_col_name <- deparse(substitute(group_col))
  
  # Filter to records with valid date values
  DT_filtered <- DT[!is.na(get(date_col_name))]
  
  if (nrow(DT_filtered) == 0) {
    cat(sprintf("\nSkipping %s - no records with valid %s\n", label, date_col_name))
    return(invisible(NULL))
  }
  
  # Count observations by group
  agency_counts <- DT_filtered[, .N, by = group_col_name]
  setnames(agency_counts, "N", "count")
  n_agencies <- nrow(agency_counts)
  total_obs <- nrow(DT_filtered)
  
  # Check if threshold met
  if (n_agencies < min_agency_count) {
    cat(sprintf("\nSkipping %s - need %d+ agencies (have %d with %d total observations)\n",
                label, min_agency_count, n_agencies, total_obs))
    return(invisible(NULL))
  }
  
  # Report what we're doing
  cat(sprintf("\n=== %s ===\n", label))
  cat(sprintf("Total observations: %d\n", total_obs))
  cat(sprintf("Number of agencies: %d\n", n_agencies))
  
  # Normalize label for filenames
  base_name <- tolower(gsub("[-\\s]+", "_", label))
  pareto_file  <- paste0(base_name, "_pareto_combo_chart.pdf")
  
  # Titles
  title_case <- tools::toTitleCase(tolower(label))
  plot_title <- if (nzchar(condition_text)) {
    paste0(title_case, " ", condition_text, " SRs by Agency")
  } else {
    paste0(title_case, " SRs by Agency")
  }
  
  # Create Pareto chart
  cat(sprintf("Creating Pareto chart: %s\n", pareto_file))
  
  do.call(plot_pareto_combo, list(
    DT = DT_filtered,
    x_col = group_col_name,
    chart_dir = chart_dir,
    filename = pareto_file,
    title = plot_title,
    subtitle = sprintf("n = %s observations", format(total_obs, big.mark = ",")),
    top_n = top_n,
    include_na = FALSE,
    width_in = width_in,
    height_in = height_in,
    min_count = 1L
  ))
  
  invisible(list(
    label = label,
    n_agencies = n_agencies,
    n_records = total_obs,
    pareto_file = pareto_file
  ))
}