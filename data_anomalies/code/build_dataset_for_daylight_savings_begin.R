library(data.table)  # Efficient data handling

# Define file paths
input_file <- "C:/Users/janem/OneDrive/Documents/david_stuff/data/3-month_311SR_01-01-2024_thru_03-31-2024_AS_OF_03-04-2025.csv"
output_dir <- "C:/Users/janem/OneDrive/Documents/david_stuff/shiny_apps/daylight_saving_time_begins/data"

# Ensure the output directory exists
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Read CSV, keeping "Created Date" as character
df <- fread(input_file, colClasses = list(character = "Created Date"))

# Rename "Created Date" column to "created_date"
setnames(df, "Created Date", "created_date")

# Identify and adjust "02:00:XX AM" timestamps → Shift them forward to "03:00:XX AM"
df[grep("^03/10/2024 02:00:", created_date), created_date := sub("^03/10/2024 02:", "03/10/2024 03:", created_date)]

# Convert "created_date" to POSIXct (handling MM/DD/YYYY format with AM/PM)
df[, created_date := as.POSIXct(created_date, format = "%m/%d/%Y %I:%M:%S %p", tz = "America/New_York")]

# Keep only the "created_date" column
df <- df[, .(created_date)]

# Save the processed dataset
output_file <- file.path(output_dir, "dataset.rds")
saveRDS(df, file = output_file)

# Print confirmation
print(paste("✅ Processed dataset saved to:", normalizePath(output_file, winslash = "/")))

# ----------------------------------------------
# 🚀 Verification: Read Back & Inspect Critical Time Range
# ----------------------------------------------
test_data <- readRDS(output_file)

# Extract rows between 01:59:00 and 03:00:00
verification_subset <- test_data[created_date >= "2024-03-10 01:59:00" & created_date <= "2024-03-10 03:01:00"]

# Print verification results
print("🔍 Timestamps Between 01:59:00 and 03:00:00 After Processing:")
print(verification_subset)
