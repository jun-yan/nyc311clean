library(data.table)

# Step 1: Copy cleaned_data to preserve original
zip_check_data <- copy(cleaned_data)

# Step 2: Extract first 5 characters of incident_zip
zip_check_data[, incident_zip_5 := substr(trimws(as.character(incident_zip)), 1, 5)]

# Step 3: Identify and write malformed ZIPs (not 5 digits)
invalid_zip_table <- zip_check_data[
  !grepl("^\\d{5}$", incident_zip_5) | is.na(incident_zip_5),
  .N,
  by = .(original_incident_zip = incident_zip)
][order(-N)]

fwrite(invalid_zip_table, file = file.path(data_dir, "invalid_zip_codes.csv"))

# Step 4: Keep only rows with valid 5-digit ZIPs (formatted correctly)
zip_cleaned_data <- zip_check_data[grepl("^\\d{5}$", incident_zip_5)]
zip_cleaned_data[, incident_zip := incident_zip_5]  # overwrite incident_zip field
zip_cleaned_data[, incident_zip_5 := NULL]          # drop helper column

# Step 5: Derive valid ZIP reference from data itself
valid_zips_clean <- unique(zip_cleaned_data[, .(zip = incident_zip)])


# Step 6: Run analyze_invalid_values
incident_zip_results <- analyze_invalid_values(
  dataset = zip_cleaned_data,
  field_name = "incident_zip",
  valid_values_list = usps_zipcodes,
  valid_field_name = "zip"
)

# Step 7: Extract results
incident_zip_full   <- incident_zip_results[[1]]
incident_zip_sample <- incident_zip_results[[2]]

# Step 8: Extract logically invalid ZIPs (those not in reference list)
# Ensure ZIP is character and trimmed
zip_cleaned_data[, incident_zip := trimws(as.character(incident_zip))]

# Filter: ZIPs not in the USPS reference list
invalid_zip_details <- zip_cleaned_data[!incident_zip %in% usps_zipcodes$zip]

# Summary table
invalid_zip_summary <- invalid_zip_details[, .N, by = .(incident_zip)][order(-N)]

# Save to disk
fwrite(invalid_zip_details, file = file.path(data_dir, "invalid_zip_logical_details.csv"))
fwrite(invalid_zip_summary, file = file.path(data_dir, "invalid_zip_logical_summary.csv"))


# Step 9: Create the year-month colums
# Ensure both are data.tables
setDT(incident_zip_full)
setDT(incident_zip_sample)

# Convert year_month to Date and sort
incident_zip_full[, year_month := as.Date(paste0(year_month, "-01"))]
incident_zip_sample[, year_month := as.Date(paste0(year_month, "-01"))]

setorder(incident_zip_full, year_month)
setorder(incident_zip_sample, year_month)

# Step 10: Generate the line chart
# Create ggplot for incident ZIPs
zip_gg_plot <- create_condition_plot(
  data = incident_zip_full,
  title = "Proportion of Invalid Incident ZIPs w/loess fitting",
  y_label = "Non-conforming Proportion",
  subtitle = "Incident ZIPs not found in USPS reference list",
  value_field = "fraction"
)

# Display plot
print(zip_gg_plot$plot)  # Use $plot to access ggplot object from list

# Save plot to PDF
file_path <- file.path(chart_dir, "invalid_incident_zips.pdf")
ggsave(file_path,
       plot = zip_gg_plot$plot,
       width = 13,
       height = 8.5)

# Pause to allow rendering
cat("\nWaiting 2 seconds between charts...\n")
Sys.sleep(2)


# Step 11: Generate the qcc p-chart

# Extract metadata from ggplot object
zip_title <- zip_gg_plot$title
zip_first_mean <- zip_gg_plot$first_year_mean
zip_last_mean  <- zip_gg_plot$last_year_mean

# Define variables for QCC
zip_count_data <- incident_zip_sample$count
zip_sample_sizes <- incident_zip_sample$N
zip_chart_title <- "QCC p-chart of Invalid Incident ZIPs"
zip_labels <- format(incident_zip_sample$year_month, "%Y-%m")

# Create and display QCC chart
zip_qcc_plot <- qcc(zip_count_data,
                    sizes = zip_sample_sizes,
                    type = "p",
                    title = zip_chart_title,
                    xlab = "Year-Month",
                    ylab = "Non-conforming Proportion",
                    labels = zip_labels,
                    plot = TRUE)

# Let plot render before next chart
cat("\nWaiting 2 seconds before continuing to next chart...\n")
Sys.sleep(2)

# Save QCC chart to PDF
pdf(file.path(chart_dir, "qcc_p_chart_invalid_incident_zips.pdf"),
    width = 13, height = 8.5)
plot(zip_qcc_plot, title = zip_chart_title)
dev.off()

