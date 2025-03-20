################################################################################

########## Build "dataset.rds" for 'daylight_savings_begin'  app ###########

################################################################################
library(data.table)  # Efficient data handling
 
####################
# Define file paths
current_wd <- getwd()

input_file_path <- file.path(current_wd, "datacleaningproject", "nyc311clean", "data_anomalies", "data")

input_file <- "March_2024_311SR_03-01-2024_thru_03-31-2024_AS_OF_03-10-2025.csv"

full_input_file_path <- file.path(input_file_path, input_file)

# Read CSV with header=TRUE to properly recognize the header
df <- data.table::fread(full_input_file_path, sep=",", header=TRUE)

# Rename "Created Date" column to "created_date"
data.table::setnames(df, "Created Date", "created_date")
  
# Identify and adjust "02:00:XX AM" timestamps → Shift them forward to "03:00:XX AM"
df[grep("^03/10/2024 02:00:", created_date), created_date := sub("^03/10/2024 02:", "03/10/2024 03:", created_date)]
  
# Convert "created_date" to POSIXct (handling MM/DD/YYYY format with AM/PM)
df[, created_date := as.POSIXct(created_date, format = "%m/%d/%Y %I:%M:%S %p", tz = "America/New_York")]
  
# Keep only the "created_date" column (should already be the only column)
df <- df[, .(created_date)]
  
# # Print a few rows to verify conversion worked
# print(head(df))

####################
# Define Output file path, for "dataset.rds"
output_file_path <- file.path(current_wd, "datacleaningproject", "nyc311clean", "data_anomalies", "shiny_apps", "daylight_saving_time_begins","data")
output_file <- "dataset.rds"

# Save the processed dataset
full_output_file_path <- file.path(output_file_path, output_file)

# Create directory if it doesn't exist
if (!dir.exists(output_file_path)) {
  dir.create(output_file_path, recursive = TRUE)
  cat("Created directory:", output_file_path, "\n")
} else {
  cat("Directory already exists:", output_file_path, "\n")
}

saveRDS(df, file = full_output_file_path)

# ----------------------------------------------
# 🚀 Verification: Read Back & Inspect Critical Time Range
# ----------------------------------------------
test_data <- readRDS(full_output_file_path)

# Extract rows between 01:59:00 and 03:00:00
verification_subset <- test_data[created_date >= "2024-03-10 01:59:00" & created_date <= "2024-03-10 03:01:00"]

# Print verification results
print("\n\n🔍 Timestamps Between 01:59:00 and 03:00:00 After Processing:")
print(verification_subset)

################################################################################