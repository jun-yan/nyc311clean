#########################################################################
# -------------------------------------------------------------
# 📦 INSTALL AND LOAD REQUIRED PACKAGES
# -------------------------------------------------------------
required_packages <- c(
  "ggplot2", 
  "scales", 
  "dplyr", 
  "zoo", 
  "ggpmisc", 
  "lubridate", 
  "data.table",
  "renv", 
  "sf", 
  "stringdist", 
  "styler", 
  "tidyverse", 
  "rlang", 
  "httr"
)

# Check and install missing packages
missing_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
if (length(missing_packages)) {
  install.packages(missing_packages)
  cat("📦 Installed missing packages:", paste(missing_packages, collapse = ", "), "\n")
}

# Load the required packages
lapply(required_packages, library, character.only = TRUE)

# -------------------------------------------------------------
# 📁 Set Working Directory for the Project.
# -------------------------------------------------------------
# Set working directory to the location of the initialization script
setwd("C:/Users/David/OneDrive/Documents/datacleaningproject/nyc311clean/code")
# 
# # Source the initialization.R script.
#source("run_this_program_first.R")

# Include the working directory for the R programs and sub-directories, passed as a parameter
#setwd("C:\\Users\\David\\OneDrive\\Documents\\datacleaningproject\\code")

#########################################################################
rm(list = ls())

#main_data_file <- "311_Service_Requests_from_2023-2024_AS_OF_01-05-2025.CSV"
main_data_file <- "3-month_311SR_10-01-2024_thru_12-31-2024_AS_OF_02-02-2025.csv"
#main_data_file <- "smaller_test_data.csv"
#main_data_file <- "extra_small.csv"

# Extract the 10-character date after "AS_OF_"
max_closed_date <- sub(".*AS_OF_([0-9-]+)\\.csv$", "\\1", main_data_file)

# Convert to POSIXct format
max_closed_date <- as.POSIXct(max_closed_date, format = "%m-%d-%Y", tz = "UTC") + (23*3600 + 59*60 + 59)

#########################################################################
programStart <- as.POSIXct(Sys.time())
formattedStartTime <- format(programStart, "%Y-%m-%d %H:%M:%S")
cat("\n***** Program initialization *****")

# Get the current working directory
working_dir <- getwd()

# Set the base directory under the working directory
base_dir <- working_dir

# Define the console output directory and file name.
output_dir <- file.path(base_dir, "console_output")
output_file <- file.path(output_dir, "core_console_output.txt")

# Define the path to the directory containing your function scripts
functions_path <- file.path(base_dir, "functions")

# Get all .R files in the "functions" sub-directory
function_files <- list.files(functions_path, pattern = "\\.R$", full.names = TRUE)

# Source each file with error handling and message logging
lapply(function_files, function(file) {
  tryCatch({
    source(file)
#    message("Successfully sourced: ", file)
  }, error = function(e) {
    message("Error sourcing: ", file, " - ", e$message)
  })
})

# Define the path for the main data file (CSV file)
data_file <- file.path(base_dir, "data")

# Define the path for the charts
chart_directory_path <- file.path(base_dir, "charts")

# Create the directory for the reduced size file following shrinkage code.
writeFilePath <- file.path(base_dir, "data")

options(scipen = 999) # Set scipen option to a large value.
options(digits = 15) # Set the number of decimal places to 15, the max observed.

cat("\nExecution begins at:", formattedStartTime)

# Start directing console output to the file
sink(output_file)

cat("\nExecution begins at:", formattedStartTime)
#########################################################################
# File contains column names in the "header" line.
# The R "read.csv" function uses a "." to replace the spaces in column names.
# This makes the column names into legal variables, but the "." can cause problems elsewhere.
# The function "make_column_names_user_friendly" replaces the "." with an underscore "_".
# thus simplifying the field names. Additionally, the field names
# are converted to upper case.

#########################################################################
# Load the USPS zipcode file
USPS_zipcode_reference_file <- file.path(data_file, "USPS_zipcodes.csv")

USPSzipcodes <- as.data.frame(fread(
  USPS_zipcode_reference_file,
  colClasses = "character"
))

USPSzipcodes <- as.data.frame(USPSzipcodes)
USPSzipcodes <- make_column_names_user_friendly(USPSzipcodes)

# extract the 'delivery_zipcode' field
USPSzipcodesOnly <- USPSzipcodes[, "delivery_zipcode", drop = FALSE]

#########################################################################
# Load the main 311 SR data file. Set the read & write paths.
main_data_file_path <- file.path( data_file, main_data_file)

d311 <- as.data.frame(fread(
  main_data_file_path,
  colClasses = "character" ))

# Capture original file size for later use during file reduction routine.
original_size <- object.size(d311)

# make columns names user friendly
d311 <- make_column_names_user_friendly(d311)

num_rows_d311 <- nrow(d311)
num_columns_d311 <- ncol(d311)

#########################################################################
#Convert character fields to upper case to facilitate comparisons
columns_to_upper <- c(
  "agency",
  "agency_name",
  "complaint_type",
  "descriptor",
  "location_type",
  "incident_address",
  "street_name",
  "cross_street_1",
  "cross_street_2",
  "intersection_street_1",
  "intersection_street_2",
  "address_type",
  "city",
  "landmark",
  "facility_type",
  "status",
  "resolution_description",
  "community_board",
  "borough",
  "open_data_channel_type",
  "park_facility_name",
  "park_borough",
  "vehicle_type",
  "taxi_company_borough",
  "taxi_pick_up_location",
  "bridge_highway_name",
  "bridge_highway_direction",
  "road_ramp",
  "bridge_highway_segment"
)

# Convert selected columns to uppercase
d311[, columns_to_upper] <- lapply(d311[, columns_to_upper], toupper)

#########################################################################

cat("\n\n**********DATA SUMMARY**********\n")

#########################################################################
# Filter out rows with NA values in the created_date column
d311 <- d311[!is.na(d311$created_date), ]

# Convert each date field to POSIXct format in UTC
d311$created_date <- as.POSIXct(d311$created_date, format = "%m/%d/%Y %I:%M:%S %p", tz = "UTC")
d311$closed_date <- as.POSIXct(d311$closed_date, format = "%m/%d/%Y %I:%M:%S %p", tz = "UTC")
d311$due_date <- as.POSIXct(d311$due_date, format = "%m/%d/%Y %I:%M:%S %p", tz = "UTC")
d311$resolution_action_updated_date <- as.POSIXct(d311$resolution_action_updated_date,
  format = "%m/%d/%Y %I:%M:%S %p", tz = "UTC"
)

# Specify the date columns to adjust
date_columns <- c("created_date", "closed_date", "resolution_action_updated_date", "due_date")

# Call the function on your dataframe d311
d311 <- adjust_feb_29_to_28(d311, date_columns)

#########################################################################
mandatory_cols <- c(
  "created_date",
  "agency",
  "complaint_type",
  "unique_key"
)

# Count the number of rows with NA values in mandatory columns
rows_to_remove <- apply(is.na(d311[, mandatory_cols]), 1, any)

# Remove the rows with NA values
d311 <- d311[!rows_to_remove, ]

# Calculate the number of rows to be removed
num_rows_removed <- sum(rows_to_remove)

# Print the number of rows removed if any rows were removed
if (num_rows_removed > 0) {
  cat("\nNumber of rows removed: ", num_rows_removed)
  num_rows_d311 <- nrow(d311)
}

########################################################################
earliest_date <- min(d311$created_date, na.rm = TRUE)
earliest_date_formatted <- format(earliest_date, format = "%Y-%m-%d %H:%M:%S")

latest_date <- max(d311$created_date, na.rm = TRUE)
latest_date_formatted <- format(latest_date, format = "%Y-%m-%d %H:%M:%S")

earliest_title <- format(as.Date(earliest_date_formatted), format = "%Y-%m-%d")
latest_title <- format(as.Date(latest_date_formatted), format = "%Y-%m-%d")

#########################################################################
# consolidate Agencies (DCA, DOITT, NYC311-PRD)
d311 <- consolidate_agencies((d311))

sorted_by_agency <- rank_by_agency(d311)

# chart_title <- "SR count by Agency & cumulative percentage"
create_combo_chart(
  dataset = d311,
  chart_title = NULL,
  chart_file_name = "SRs_by_Agency.pdf",
  console_print_out_title = "Summary of SRs by Agency",
  chart_directory = chart_directory_path,
  chart_width = 10,
  chart_height = 10/1.6,
  annotation_size = 4.3
)

# Display the results
cat("\nNumber of rows in the 311 SR data set:", format(num_rows_d311, big.mark = ","))
cat("\nNumber of columns in the 311 SR data set:", format(num_columns_d311, big.mark = ","))
cat("\nNumber of Agencies represented:", length(unique(d311$agency)))
cat("\n\nData contains SRs created from", earliest_date_formatted, "through", latest_date_formatted)

########################################################################

cat("\n\n**********BLANK and N/A ENTRIES BY COLUMN**********")

#########################################################################

# Identify the date columns
date_cols <- c("created_date", "closed_date", "due_date", "resolution_action_updated_date")

# Identify non-date columns
non_date_cols <- setdiff(names(d311), date_cols)

# Count NAs or blanks in character non-date columns
char_non_date_cols <- non_date_cols[sapply(d311[, non_date_cols], is.character)]
blank_count_char_non_date <- colSums(is.na(d311[, char_non_date_cols]) | d311[, char_non_date_cols] == "")

# Count NAs in non-character non-date columns
other_non_date_cols <- non_date_cols[!sapply(d311[, non_date_cols], is.character)]
blank_count_other_non_date <- colSums(is.na(d311[, other_non_date_cols]))

# Count NAs in date columns
blank_count_dates <- lapply(d311[, date_cols], function(x) sum(is.na(x)))
names(blank_count_dates) <- date_cols

# Combine the results
blank_count <- c(blank_count_char_non_date, blank_count_other_non_date, unlist(blank_count_dates))
names(blank_count) <- c(char_non_date_cols, other_non_date_cols, date_cols)

# Create a dataframe to store the results
missingDataPerColumn <- data.frame(
  field = names(blank_count),
  total_empty = blank_count,
  pct_empty = round((blank_count / num_rows_d311) * 100, 1)
)

# Count NAs in each column
na_counts_per_column <- colSums(is.na(d311))

# Bind NA_Count column to missingDataPerColumn
missingDataPerColumn <- cbind(missingDataPerColumn, NA_count = na_counts_per_column[missingDataPerColumn$field])

missingDataPerColumn$blanks_only <- missingDataPerColumn$total_empty - missingDataPerColumn$NA_count

# Sort the data frame by the sum of NAs and blanks in descending order
missingDataPerColumn <- missingDataPerColumn[order(missingDataPerColumn$total_empty, decreasing = TRUE), ]

cat("\nNumber and % blanks and N/A (total empty) entries per column:\n")
print(missingDataPerColumn, row.names = FALSE, right = FALSE)

# Sort 'field' by 'total_empty' descending
missingDataPerColumn <- missingDataPerColumn %>%
  mutate(field = reorder(field, -total_empty))

# Create the bar chart with vertical X-axis labels
blank_chart <- ggplot(missingDataPerColumn, aes(x = reorder(field, -total_empty), y = total_empty)) +
  
  # Match the fill color to base_bar_chart
  geom_bar(stat = "identity", fill = "#44AA99", na.rm = TRUE) +
  
  # Standardize theme settings to match base_bar_chart
  theme(
    axis.title = element_blank(), # Remove x and y axis titles for consistency
    plot.title = element_text(hjust = 0.5, size = 12),
    panel.background = element_rect(fill = "gray96", color = "gray96"),
    axis.text.x = element_text(angle = 50, vjust = 1, hjust = 1, face = "bold", size = 7),
    axis.text.y = element_text(face = "bold", size = 9),
    legend.position = "none", # Remove all legends,
    plot.margin = margin(1, 2, 1, 2)  # Adjust the top, right, bottom, and left margins
  ) +
  
# Add percentage labels with standardized font size
  geom_text(aes(
    x = field, y = total_empty, label = pct_empty,
    angle = -70
  ), size = 3.5, color = "black") +
  
  # Remove x and y axis labels for consistency with base_bar_chart
  labs(x = NULL, y = NULL)

# Build the plot to extract y-axis breaks
built_plot <- ggplot_build(blank_chart)

# Extract Y-axis breaks (try different possible locations)
y_breaks <- built_plot$layout$panel_params[[1]]$y$get_breaks()
# Filter out any NA values from y_breaks
y_breaks <- y_breaks[!is.na(y_breaks)]

# Add hlines using the Y-axis breaks with matching line style and color
blank_chart <- blank_chart +
  geom_hline(yintercept = y_breaks, linetype = "dotted", color = "gray35", linewidth = 0.5)

# Print the bar chart
print(blank_chart)

# Set desired width and height to match base_bar_chart
chart_width <- 10
chart_height <- chart_width / 1.4 # Golden Ratio for height

# Save the chart with the Golden Ratio aspect ratio
chart_path <- file.path(chart_directory_path, "BlankFields.pdf")
ggsave(chart_path, plot = blank_chart, width = chart_width, 
       height = chart_height, dpi = 300)

#########################################################################

# Determine field usage by Agency. Produce Excel spreadsheet.

#########################################################################
# Initialize the list of fields (excluding "agency")
fields <- setdiff(names(d311), "agency")

# Initialize a list to store the results
results <- list()

# Loop through each field and count usage by agency
for (field in fields) {
  result <- d311 %>%
    filter(!is.na(.data[[field]]), nzchar(.data[[field]])) %>% # Exclude blanks and NAs
    count(agency) %>%
    rename(count = n) %>%
    mutate(field = field)
  
  results[[field]] <- result
}

# Combine all results into a single dataframe
final_results <- bind_rows(results)

# Reshape the data to present as a table
field_usage_summary_table <- final_results %>%
  pivot_wider(
    names_from = agency,
    values_from = count,
    values_fill = 0
  )

# Exclude the non-numeric "field" column and compute row sums
field_usage_summary_table$TOTAL<- rowSums(field_usage_summary_table[, -1])

# Print the data frame without row names
cat("\n\nField Usage by Agency:\n")
print(as.data.frame(field_usage_summary_table), row.names = FALSE, right = FALSE)

# Define the file path for saving the CSV
summary_table_file_path <- file.path(writeFilePath, "field_usage_summary_table.csv")

# Save the data frame as a CSV file
write.csv(field_usage_summary_table, summary_table_file_path, row.names = FALSE)
cat("\nA CSV file showing field usage by Agency has been written to:\n", 
    summary_table_file_path, "\n")
#########################################################################

cat("\n\n**********COMPLAINT TYPES**********")

#########################################################################
# Calculate complaint frequency and responsible agency
complaintData <- as.data.frame(table(d311$complaint_type))
complaintData <- complaintData[order(-complaintData$Freq), ]
complaintData$percent <- round(prop.table(complaintData$Freq) * 100, 2)
complaintData$cumulative_percent <- cumsum(complaintData$percent)

cat("\n\nThere are", nrow(complaintData),"different complaint_type(s).\n")

unique_pairs <- unique(d311[, c("complaint_type", "agency")])
unique_pairs <- unique_pairs[order(unique_pairs$complaint_type), ]

# Using the aggregate function to count unique agency values for each complaint type
agency_count <- aggregate(agency ~ complaint_type, data = d311, FUN = function(x) length(unique(x)))

# Renaming the columns for clarity
colnames(agency_count) <- c("complaint_type", "unique_agency_count")

# Sorting the result in descending order of unique_agency_count
agency_count <- agency_count[order(-agency_count$unique_agency_count), ]

# Identify complaint types where unique_agency_count > 1
complaint_types_to_remove <- agency_count$complaint_type[agency_count$unique_agency_count > 1]

# Remove the rows associated with the specified complaint types
# These will be re-captured later.
filtered_pairs <-
  unique_pairs[!unique_pairs$complaint_type %in% complaint_types_to_remove, ]

# Find matching indices between complaintData and filtered_pairs
matching_indices <-
  match(complaintData$Var1, filtered_pairs$complaint_type)

# Add the agency column based on the matching indices
complaintData$agency <- filtered_pairs$agency[matching_indices]

# Replace <NA> values in the agency column with "Multiple"
# This is how the removed complaint types are re-captured.
complaintData$agency[is.na(complaintData$agency)] <- "MULTIPLE"

colnames(complaintData) <- c("complaint_type", "count", "percent", "cumulative_percent", "agency")

cat("\nTop 20 'complaint_type's and responsible Agency:\n")
print(head(complaintData, 20), row.names = FALSE, right = FALSE)

cat("\nBottom 20 'complaint_type(s) and responsible Agency:\n")
print(tail(complaintData[, c("complaint_type", "count", "agency")], 20),
  row.names = FALSE, right = FALSE
)

cat("\nComplaints with multiple responsible Agencies:\n")
# Filter rows where agency is "MULTIPLE"
multiple_agency_complaints <- complaintData[complaintData$agency == "MULTIPLE", ]

# Remove the cumulative_percent column to prevent misinterpretation
multiple_agency_complaints$cumulative_percent <- NULL

# View the results
print(head(multiple_agency_complaints, 20),row.names = FALSE, right = FALSE)

# Identify the 'Noise' complaints
noise_rows <- complaintData %>%
  filter(str_starts(complaint_type, "NOISE"))

cat("\nThere are ", nrow(noise_rows), " categories of Noise complaints:\n", sep = "")
print(noise_rows, right = c(0, rep(1, ncol(noise_rows) - 1)))

cat(
  "\nNoise complaints of all types number",
  format(sum(noise_rows$count), big.mark = ","),
  "constituting", round(sum(noise_rows$percent), 0), "% of all SRs.\n", sep = ""
)

# Rename columns to trick 'create_combo_chart' function to use 'complaint_type' as 'agency'
colnames(d311)[colnames(d311) == "agency"] <- "temp_agency"
colnames(d311)[colnames(d311) == "complaint_type"] <- "agency"

# chart_title <- "Top 20 Complaints and cumulative percentage"
create_combo_chart(
  dataset = d311,
  chart_title = NULL,
  chart_file_name = "SR_by_Complaint_Type.pdf",
  chart_directory = chart_directory_path,
  console_print_out_title = "Summary of Complaint Type",
  num_x_labels = 10,
  annotation_size = 4,
  x_axis_tick_size = 10,
  rows_to_print = 20,
  x_axis_label_angle = 60
)

# Restore column names
# Rename columns to trick 'create_combo_chart' function to use 'complaint_type' as 'agency'
colnames(d311)[colnames(d311) == "agency"] <- "complaint_type"
colnames(d311)[colnames(d311) == "temp_agency"] <- "agency"

#########################################################################
# Determine status of SRs
sortedStatus <- as.data.frame(table(d311$status))
sortedStatus <- sortedStatus[order(-sortedStatus$Freq), ]
sortedStatus$percentage <-
  round(prop.table(sortedStatus$Freq) * 100, 2)
sortedStatus$cumulative_percentage <- cumsum(sortedStatus$percentage)

# print status results
cat("\n\nSRs by Status\n")
colnames(sortedStatus) <- c("status", "count", "percentage", "cumulative_percentage")
sortedStatus$count <- format(sortedStatus$count, big.mark = ",")
print(sortedStatus, row.names = FALSE, right = FALSE)

#########################################################################

cat("\n\n**********VALIDATING DATA TYPES**********\n")

#########################################################################
# # Test if each date field is in POSIXct format
# # Coerce class to POSIXct to avoid the POSIXt class (a subclass of POSIXct)
class(d311$created_date) <- "POSIXct"
class(d311$closed_date) <- "POSIXct"
class(d311$due_date) <- "POSIXct"
class(d311$resolution_action_updated_date) <- "POSIXct"

is_posixct <-
  class(d311$created_date) == "POSIXct" &
    class(d311$closed_date) == "POSIXct" &
    class(d311$due_date) == "POSIXct" &
    class(d311$resolution_action_updated_date) == "POSIXct"

if (is_posixct) {
  cat("\nAll four date fields are in proper date format.")
} else {
  cat("\nAt least one of the date fields is not in proper date format.")
}

#########################################################################
# determine if the incident_zip field contain 5 numeric digits
# Call the function for "incident_zip" field
invalid_incident_zip_rows <- filter_non_numeric_zipcodes(d311, "incident_zip")

num_row_invalid_incident_zip_rows <- nrow(invalid_incident_zip_rows)
if (num_row_invalid_incident_zip_rows == 0) {
  cat("\n\nAll 'incident_zip' entries are 5 numeric digits.")
} else {
  cat("\n\nThere are", num_row_invalid_incident_zip_rows, "non-numeric, non-5-digit 'incident_zip' entries.\n")

  selected_columns <- invalid_incident_zip_rows %>%
    select(unique_key, incident_zip, agency)

  print(head(selected_columns, 10), row.names = FALSE, right = FALSE)
}

#########################################################################
# determine if various fields are numeric values
x_coordinateNum <- areAllNumbers(d311$x_coordinate_state_plane)
cat(
  "\n\nAre all values in 'x_coordinate_state_plane' numbers?",
  x_coordinateNum
)

y_coordinateNum <- areAllNumbers(d311$y_coordinate_state_plane)
cat(
  "\n\nAre all values in 'y_coordinate_state_plane' numbers?",
  y_coordinateNum
)

latitudeNum <- areAllNumbers(d311$latitude)
cat("\n\nAre all values in 'latitude' numbers?", latitudeNum)

longitudeNum <- areAllNumbers(d311$longitude)
cat("\n\nAre all values in 'longitude' numbers?", longitudeNum)

#########################################################################

cat("\n\n**********CHECKING FOR ALLOWABLE AND VALID VALUES**********\n")

#########################################################################
# determine if the unique_key is in fact unique
uniqueKeys <- length(unique(d311$unique_key)) == num_rows_d311
cat("\nAre all 'unique_keys' truly unique?", uniqueKeys, "\n")

#########################################################################
# Check to see if any of the latitudes or longitudes fall outside the extreme points of New York City.
# Change the lat/long fields to "numeric" to enable comparison.

# Extreme points of the boundaries of New York City as provide by chatGPT and confirmed elsewhere.
# Note that Longitudes (west of prime meridian) are expressed as negative values
southernMostLatitude <- 40.477399
northernMostLatitude <- 40.917576
easternMostLongitude <- -73.700181
westernMostLongitude <- -74.259090

# Convert lat/long to numeric conversions for comparisons
d311$latitude <- as.numeric(d311$latitude)
d311$longitude <- as.numeric(d311$longitude)

# Check latitudes & longitudes in 311 data to determine any outliers
badLatitudes <- d311[(
  is.na(d311$latitude) |
    d311$latitude < southernMostLatitude |
    d311$latitude > northernMostLatitude
) &
  !is.na(d311$latitude), ]

badLongitudes <- d311[(
  is.na(d311$longitude) |
    d311$longitude > easternMostLongitude |
    d311$longitude < westernMostLongitude
) &
  !is.na(d311$longitude), ]

cat(
  "\nThe number of 'latitudes' outside the boundaries of NYC is:",
  nrow(badLatitudes),
  "\n"
)

if (nrow(badLatitudes) > 0) {
  print(head(badLatitudes[, c("unique_key", "agency", "latitude", "city")], 20), row.names = FALSE, right = FALSE)
}

cat(
  "\nThe number of 'longitudes' outside the boundaries of NYC is:",
  nrow(badLongitudes), "\n"
)

if (nrow(badLongitudes) > 0) {
  print(head(badLongitudes[, c("unique_key", "agency", "longitude", "city")], 20), row.names = FALSE, right = FALSE)
}

# Check to see if any of the x or y state plane coordinates fall outside the extreme points of New York City.
# Define the latitude and longitude points.
points_df <- data.frame(
  name = c("Northernmost", "Easternmost", "Southernmost", "Westernmost"),
  lat = c(40.9156, 40.4961, 40.4961, 40.9156),
  lon = c(-73.7004, -73.7004, -74.2591, -74.2591)
)

# Convert to an sf object and apply the State Plane projection.
points_sf <- st_as_sf(points_df, coords = c("lon", "lat"), crs = 4326) # WGS84 lat/long.
points_sp <- st_transform(points_sf, crs = 2263) # Convert to State Plane

# Change the x/y_coordinate_state_plane fields to "numeric" to enable comparison.
d311$x_coordinate_state_plane <- as.numeric(d311$x_coordinate_state_plane)
d311$y_coordinate_state_plane <- as.numeric(d311$y_coordinate_state_plane)

# Extract the bounding box (xmin, ymin, xmax, ymax) from the sf object
bbox <- st_bbox(points_sp)

# Assign the individual values to variables
xmin <- as.numeric(bbox["xmin"])
xmax <- as.numeric(bbox["xmax"])
ymin <- as.numeric(bbox["ymin"])
ymax <- as.numeric(bbox["ymax"])

# Filter NA values
d311_clean <- d311[!is.na(d311$x_coordinate_state_plane) & !is.na(d311$y_coordinate_state_plane), ]

# Check for x-coordinate outliers
x_outliers <- d311_clean[
  d311_clean$x_coordinate_state_plane < xmin |
    d311_clean$x_coordinate_state_plane > xmax,
]

# Check for y-coordinate outliers
y_outliers <- d311_clean[
  d311_clean$y_coordinate_state_plane < ymin |
    d311_clean$y_coordinate_state_plane > ymax,
]

# Print status for x-coordinate outliers
if (nrow(x_outliers) == 0) {
  cat("\nAll x_coordinate_state_plane values lie within the boundaries of NYC.")
} else {
  cat("\n\nThere are", nrow(x_outliers), "x_coordinate_state_plane values outside the boundaries of NYC.\n")
  cat("\nHere are the first few rows of x-coordinate outliers:\n")
  print(head(x_outliers[, c("unique_key", "agency", "x_coordinate_state_plane")]))
}

# Print status for y-coordinate outliers
if (nrow(y_outliers) == 0) {
  cat("\nAll y_coordinate_state_plane values lie within the boundaries of NYC.")
} else {
  cat("\n\nThere are", nrow(y_outliers), "y_coordinate_state_plane values outside the boundaries of NYC.\n")
  cat("\nHere are the first few rows of y-coordinate outliers:\n")
  print(head(y_outliers[, c("unique_key", "agency", "y_coordinate_state_plane")]))
}

#########################################################################
# Check to see if all address types are valid.
valid_address_types = c(
  "ADDRESS",
  "BBL",
  "BLOCKFACE",
  "INTERSECTION",
  "PLACENAME",
  "UNRECOGNIZED"
)
address_type_results <- are_valid_values(
    d311$address_type, 
    valid_address_types, 
    "address_type")

# Check to see if all SR statuses are valid.
valid_statuses <- c(
      "ASSIGNED",
      "CANCEL",
      "CLOSED",
      "IN PROGRESS",
      "OPEN",
      "PENDING",
      "STARTED",
      "UNSPECIFIED"
    )
statusResults <-
  are_valid_values(
    d311$status,
    valid_statuses,
    "status")

# check if borough, taxi_company_borough, and park_borough contain only allowable values
valid_boroughs <- c(
      "BRONX",
      "BROOKLYN",
      "MANHATTAN",
      "QUEENS",
      "STATEN ISLAND",
      "UNSPECIFIED"
    )
boroughResults <-
  are_valid_values(
    d311$borough, 
    valid_boroughs,
    "borough")

park_boroughResults <-
  are_valid_values(
    d311$park_borough,
    valid_boroughs,
    "park_borough")

taxi_company_boroughResults <-
  are_valid_values(
    d311$taxi_company_borough,
    valid_boroughs,
    "taxi_company_borough")

# check if SR channels contain only allowable values
open_data_channelResults <-
  are_valid_values(
    d311$open_data_channel_type,
    c(
    "MOBILE",
    "ONLINE",
    "OTHER",
    "PHONE",
    "UNKNOWN"),
    "open_data_channel")

vehicle_typeResults <-
  are_valid_values(
    d311$vehicle_type,
    c(
      "AMBULETTE / PARATRANSIT",
      "CAR",
      "CAR SERVICE",
      "COMMUTER VAN",
      "GREEN TAXI",
      "OTHER",
      "SUV",
      "TRUCK",
      "VAN"
    ),
   "vehicle_type")

#########################################################################
# check for allowable values in the 'community_board' field
cbValues <-
  c(
    "01 BRONX", "01 BROOKLYN", "01 MANHATTAN", "01 QUEENS", "01 STATEN ISLAND",
    "02 BRONX", "02 BROOKLYN", "02 MANHATTAN", "02 QUEENS", "02 STATEN ISLAND",
    "03 BRONX", "03 BROOKLYN", "03 MANHATTAN", "03 QUEENS", "03 STATEN ISLAND",
    "04 BRONX", "04 BROOKLYN", "04 MANHATTAN", "04 QUEENS",
    "05 BRONX", "05 BROOKLYN", "05 MANHATTAN", "05 QUEENS",
    "06 BRONX", "06 BROOKLYN", "06 MANHATTAN", "06 QUEENS",
    "07 BRONX", "07 BROOKLYN", "07 MANHATTAN", "07 QUEENS",
    "08 BRONX", "08 BROOKLYN", "08 MANHATTAN", "08 QUEENS",
    "09 BRONX", "09 BROOKLYN", "09 MANHATTAN", "09 QUEENS",
    "10 BRONX", "10 BROOKLYN", "10 MANHATTAN", "10 QUEENS",
    "11 BRONX", "11 BROOKLYN", "11 MANHATTAN", "11 QUEENS",
    "12 BRONX", "12 BROOKLYN", "12 MANHATTAN", "12 QUEENS",
    "13 BROOKLYN", "13 QUEENS",
    "14 BROOKLYN", "14 QUEENS",
    "15 BROOKLYN",
    "16 BROOKLYN",
    "17 BROOKLYN",
    "18 BROOKLYN",
    "UNSPECIFIED BRONX", "UNSPECIFIED BROOKLYN", "UNSPECIFIED MANHATTAN",
    "UNSPECIFIED QUEENS", "UNSPECIFIED STATEN ISLAND",
    "0 UNSPECIFIED"
  )

cb_results <- are_valid_values(
  d311$community_board, 
  cbValues, 
  "community_board")

#########################################################################
# Check for invalid zip codes in d311$incident_zip using USPSzipcodesOnly
USPSzipcodesList <- as.list(USPSzipcodesOnly$delivery_zipcode)

incident_zip_results <- are_valid_values(
  d311$incident_zip, 
  USPSzipcodesList, 
  "incident_zip")

#########################################################################

cat("\n\n**********CHECKING FOR ALLOWABLE AND VALID DATES**********\n")

#########################################################################

# Duration is the time between created_date and closed_date
# Compute and store "duration" in a new additional column for the "d311" dataframe.
d311$duration <-
  as.numeric(difftime(d311$closed_date, d311$created_date, units = "days"))

# Step 1: Extract positive durations
positiveDurations <- d311[d311$duration > 0 & !is.na(d311$duration), ]

# Step 2: Extract zero durations from rows not in positiveDurations
remaining_after_positive <- d311[!(rownames(d311) %in% rownames(positiveDurations)), ]
zeroDurations <- remaining_after_positive[remaining_after_positive$duration == 0 & !is.na(remaining_after_positive$duration), ]

# Step 3: Extract negative durations from remaining rows after filtering out positive and zero
remaining_after_zero <- remaining_after_positive[!(rownames(remaining_after_positive) %in% rownames(zeroDurations)), ]
negativeDurations <- remaining_after_zero[remaining_after_zero$duration < 0 & !is.na(remaining_after_zero$duration), ]

#########################################################################
# Identify SRs with negative duration (closed before they were created)
# Exclude the extreme values of "closed dates" of 01/01/1999 (i.e. -4000 days)

closedBeforeOpened <- subset(d311, duration < 0 & !is.na(duration),
  select = c("unique_key", "created_date", "closed_date", "duration", "agency")
)

numBlankClosedDate <-
  missingDataPerColumn[missingDataPerColumn$field == "closed_date", "total_empty"]

num_rows_closedBeforeOpened <- nrow(closedBeforeOpened)

if (num_rows_closedBeforeOpened > 1) {
  cat(
    "\nThere are", format(num_rows_closedBeforeOpened, big.mark = ","),
    "SRs 'closed' before they were 'created' (negative duration) \nrepresenting",
    round(num_rows_closedBeforeOpened / (num_rows_d311 - numBlankClosedDate) * 100, 4),
    "% of non-blank data.\n"
  )

  closedBeforeOpened <- closedBeforeOpened[order(closedBeforeOpened$duration), ]
  closedBeforeOpened$duration <- round(closedBeforeOpened$duration, 6)

  threshold_for_neg_duration <- 730 # Two years
  large_neg_duration <- closedBeforeOpened[!closedBeforeOpened$duration <= -threshold_for_neg_duration &
    !is.na(closedBeforeOpened$duration), ]
  large_neg_duration <- large_neg_duration[order(large_neg_duration$duration), ]
  large_neg_duration$duration <- round(large_neg_duration$duration, 6)


  extreme_neg_duration <- closedBeforeOpened[!closedBeforeOpened$duration > -threshold_for_neg_duration &
    !is.na(closedBeforeOpened$duration), ]
  extreme_neg_duration <- extreme_neg_duration[order(extreme_neg_duration$duration), ]
  extreme_neg_duration$duration <- round(extreme_neg_duration$duration, 6)

  cat("\nLargest errors (days) *excluding extreme negative values:\n")
  print(head(large_neg_duration, 5), row.names = FALSE, right = FALSE)

  cat("\nSmallest errors (days):\n")
  print(tail(large_neg_duration, 5), row.names = FALSE, right = FALSE)

  num_row_extreme_neg_duration <- nrow(extreme_neg_duration)

  if (num_row_extreme_neg_duration > 0) {
    cat("\nThere are ", num_row_extreme_neg_duration, " SRs with extremely large negative durations (< -",
      threshold_for_neg_duration, ").\nThese will be removed from the box & whiskers plot. Sample:\n\n",
      sep = ""
    )
    random_sample <- extreme_neg_duration %>% sample_n(min(num_row_extreme_neg_duration, 5))
    print(random_sample, row.names = FALSE, right = FALSE)
  }

  summary_df <- rank_by_agency(closedBeforeOpened)

  create_combo_chart(
    dataset = closedBeforeOpened,
    chart_title = "negative duration SRs by Agency & cumulative percentage",
    chart_file_name = "negative_duration_SR_barchart.pdf",
    chart_directory = chart_directory_path,
    console_print_out_title = "Summary of negative duration SRs"
  )

  #  chart_title <- "Closed before Created (negative duration days)"

  negativeDurationViolin <- create_violin_chart(
    dataset = large_neg_duration,
    x_axis_title = NULL,
    x_axis_field = "duration",
    chart_title = NULL,
    chart_file_name = "negative_duration_SR_violin.pdf",
    chart_directory = chart_directory_path
  )

  # Boxplot for negative duration values
  create_boxplot(
    dataset = large_neg_duration,
    x_axis_field = "duration",
    chart_title = "SRs closed before they were created (negative duration) *excluding large negative values",
    x_axis_title = "",
    output_file_name = "negative_duration_SR_boxplot.pdf",
    chart_directory = chart_directory_path
  )
  
} else {
  cat("\n\nThere are no SRs 'closed' before they were 'created'.\n")
}

#########################################################################
# Identify SRs that have a zero duration, i.e. closed and opened at the exact same time
zeroDurations <-
  d311[!is.na(d311$duration) &
    d311$duration == 0, c(
    "unique_key",
    "created_date",
    "closed_date",
    "duration",
    "agency"
  )]

numBlankClosedDate <-
  missingDataPerColumn[missingDataPerColumn$field == "closed_date", "total_empty"]

num_rows_zeroDurations <- nrow(zeroDurations)
if (num_rows_zeroDurations > 0) {
  cat(
    "\nThere are",
    format(num_rows_zeroDurations, big.mark = ","),
    "SRs that are 'closed' and 'created' at the exact same time, \nto the second, representing",
    round(num_rows_zeroDurations / (num_rows_d311 - numBlankClosedDate) * 100, 4),
    "% of non-blank data and creating a zero duration."
  )

  cat("\n\nSample of SRs 'closed' at the exact same time they are 'created':\n")
  random_sample <- zeroDurations %>% sample_n(min(num_rows_zeroDurations, 5)) # random sample
  print(random_sample, row.names = FALSE, right = FALSE)

  #  sorted_zero_durations <- rank_by_agency(zeroDurations)

  if (!is.null(zeroDurations)) {
    create_combo_chart(
      dataset = zeroDurations,
      chart_title = "Zero duration SRs by Agency & cumulative percentage",
      chart_file_name = "zero_duration_SR.pdf",
      chart_directory = chart_directory_path,
      console_print_out_title = "Summary of zero duration SRs by Agency"
    )
  }
} else {
  cat("\n\nThere are no SRs with a 'created_date' == 'closed_date'.\n")
}

#########################################################################
# Identify SRs that are closed in the future ('closed_date' > max(created_date' +1)
# max_closed_date set at program start (hardcoded)
closedinFuture <-
  d311[
    d311$closed_date > max_closed_date & !is.na(d311$closed_date),
    c(
      "unique_key",
      "created_date",
      "closed_date",
      "duration",
      "agency"
    )
  ]

# Compute the # of days into the future the SR is closed, based on the max_created_date + 1 day
closedinFuture$future_days <- round(as.numeric(difftime(closedinFuture$closed_date,
  max_closed_date,
  units = "days"
)), 4)

numBlankClosedDate <-
  missingDataPerColumn[missingDataPerColumn$field == "closed_date", "total_empty"]

num_rows_future <- nrow(closedinFuture)

if (num_rows_future > 0) {
  max_closed_date_readable <- format(max_closed_date, "%Y-%m-%d %H:%M:%S")
  cat("\n(The maximum 'closed_date' and time for this dataset is:", max_closed_date_readable, ")")
  cat(
    "\n\nThere exist",
    format(num_rows_future, big.mark = ","),
    "SRs with 'closed_date' in the future, \nrepresenting",
    round(num_rows_future / (num_rows_d311 - numBlankClosedDate) * 100, 4),
    "% of non-blank data.\n"
  )

  cat("\nSample of SRs with a 'closed_date' in the future:\n")
  closedinFuture$future_days <- round(closedinFuture$future_days, 4)
  closedinFuture$duration <- round(closedinFuture$duration, 4)
  random_sample <- closedinFuture %>% sample_n(min(nrow(closedinFuture), 5)) # random sample
  print(random_sample, row.names = FALSE, right = FALSE)

  x <- rank_by_agency(closedinFuture)

  if (num_rows_future > 4) {
    
    # Boxplot for future closed values
    create_boxplot(
      dataset = closedinFuture,
      x_axis_field = "future_days",
      chart_title = "SRs closed in the future",
      x_axis_title = "",
      box_width = 0.75,  # Default width for the box
      output_file_name = "future_closed.pdf",
      chart_directory = chart_directory_path
    )

  }
} else {
  cat("\n\nThere are no SRs with a 'closed_date' in the future.")
}

#########################################################################
# Identify SRs with a 'due_date' that is before the 'created_date'
dueinPast <-
  d311[!is.na(d311$due_date), c("unique_key", "created_date", "due_date", "agency")]

# Compute the # of days in the past the SR is due before created.
dueinPast$due_duration <- round(as.numeric(difftime(dueinPast$created_date, dueinPast$due_date, units = "days")), 4)

bad_due_date <- dueinPast[dueinPast$due_duration > 0, ]
numBlankDueDate <-
  missingDataPerColumn[missingDataPerColumn$field == "due_date", "total_empty"]

num_row_bad_due_date <- nrow(bad_due_date)
if (num_row_bad_due_date > 0) {
  cat(
    "\nThere are",
    format(num_row_bad_due_date, big.mark = ","),
    "SRs with a 'due_date' before 'created_date', \nrepresenting",
    round(num_row_bad_due_date / (num_rows_d311 - numBlankDueDate) * 100, 2),
    "% of non-blank data.\n"
  )

  cat("\nSample of SRs with a 'due_date' in the past:\n")
  random_sample_due <- bad_due_date %>% sample_n(min(num_row_bad_due_date, 5)) # random sample
  print(random_sample_due, row.names = FALSE, right = FALSE)

  x <- rank_by_agency(dueinPast)
} else {
  cat("\n\nThere are no SRs with a 'due_date' before 'created_date'.\n")
}

#########################################################################
# Identify SRs with a 'resolution_action_updated_date' that is > 30 days after 'closed_date'
# Add the "postClosedUpdateDuration" column
d311 <- d311 %>%
  mutate(
    postClosedUpdateDuration = ifelse(
      !is.na(resolution_action_updated_date) &
        !is.na(closed_date) &
        resolution_action_updated_date > closed_date,
      as.numeric(difftime(resolution_action_updated_date, closed_date, units = "days")),
      NA
    )
  )

post_closed_positive <- d311[d311$postClosedUpdateDuration > 0 & !is.na(d311$postClosedUpdateDuration), ]

resoultion_action_threshold <- 30 # One month
too_large_threshold <- 730 # Two years

updatedLate <- post_closed_positive[post_closed_positive$postClosedUpdateDuration > resoultion_action_threshold &
  post_closed_positive$postClosedUpdateDuration <= too_large_threshold, ]

exclude_extreme_late_update <- post_closed_positive[post_closed_positive$postClosedUpdateDuration >= too_large_threshold, ]

selected_columns <-
  c("unique_key", "agency", "closed_date", "resolution_action_updated_date", "postClosedUpdateDuration")
updatedLate <- updatedLate[, selected_columns, drop = FALSE]
exclude_extreme_late_update <- exclude_extreme_late_update[, selected_columns, drop = FALSE]

updatedLate <- updatedLate[order(updatedLate$postClosedUpdateDuration, decreasing = TRUE), ]
exclude_extreme_late_update <- exclude_extreme_late_update[order(exclude_extreme_late_update$postClosedUpdateDuration, decreasing = TRUE), ]

#numBlankResolutionDate <-
#  missingDataPerColumn[missingDataPerColumn$field == "resolution_action_updated_date", "total_empty"]

num_row_updatedLate <- nrow(updatedLate)
num_row_extreme_late <- nrow(exclude_extreme_late_update)

if (num_row_extreme_late > 0) {
  cat(
    "\nThere are", num_row_extreme_late, "extremely late (>", too_large_threshold,
    "days) resoultion updates. \nThese are removed and excluded from the analysis."
  )
  cat("\n\nSample:\n")
  print(head(exclude_extreme_late_update, 5), row.names = FALSE)
} else {
  cat("\nThere are no SRs with extremely large (>", too_large_threshold, ") post-closed updates.", sep = "")
}

if (num_row_updatedLate > 0) {
  cat(
    "\nThere are", num_row_updatedLate, "SRs with large (>", resoultion_action_threshold, "but <=", too_large_threshold,
    "days) 'resolution_action_updated_date(s)'\n"
  )
  cat("\nSample:\n")
  random_sample_large <- updatedLate %>% sample_n(min(num_row_updatedLate, 5)) # random sample
  print(random_sample_large, row.names = FALSE, right = FALSE)
}

if (num_row_updatedLate > 0) {
  cat("\nMedian of late post-closed resolution updates >", resoultion_action_threshold, "is:", 
      round(median(updatedLate$postClosedUpdateDuration), 4), "days")
  cat("\nAverage of late post-closed resolution updates >", resoultion_action_threshold, "is:", 
      round(mean(updatedLate$postClosedUpdateDuration), 4), "days")
  cat("\nStandard deviation of late post-closed resolution updates >", resoultion_action_threshold, "is:", 
      round(sd(updatedLate$postClosedUpdateDuration), 4), "days\n")
  
  cat("\n\nThe Average of all post-closed resolution updates, regardless of any exclusion threshold is: ",
      round(mean(post_closed_positive$postClosedUpdateDuration), 6), " days [", 
      round(mean(post_closed_positive$postClosedUpdateDuration) * 24, 6), " hours]", sep = ""  )
  cat("\n\nThe Median of all post-closed resolution updates, regardless of any exclusion threshold is: ",
      round(median(post_closed_positive$postClosedUpdateDuration), 6), " days [", 
      round(median(post_closed_positive$postClosedUpdateDuration) * 24, 6), " hours]\n", sep = "" )
  
  if (!is.null(updatedLate)) {
    create_combo_chart(
      dataset = updatedLate,
      chart_title = paste("Post-Closed Resolution Updates >", resoultion_action_threshold, "days by Agency & cumulative percentage"),
      chart_file_name = "post_Closed_Bar_Chart.pdf",
      chart_directory = chart_directory_path,
      console_print_out_title = "Summary of post-cloe-resolution-updates by Agency"
    )

    post_closed_violin_chart <- create_violin_chart(
      dataset = updatedLate,
      x_axis_title = NULL,
      x_axis_field = "postClosedUpdateDuration",
      chart_title = NULL,
      chart_file_name = "post_closed_violin_chart.pdf",
      chart_directory = chart_directory_path
    )
  }
} else {
  cat("\n\nThere are no SRs with a 'resolution_action_updated_date' >", resoultion_action_threshold, "After 'closed_date'.\n")
}

#########################################################################

cat("\n\n**********CHECKING FOR DUPLICATE VALUES**********\n")

#########################################################################
# Check if "location" is a concatenation of "latitude" and "longitude"
# Extract latitude and longitude using a regex pattern
matches <- regmatches(d311$location, gregexpr("-?\\d+\\.\\d+", d311$location))
lat <- as.numeric(sapply(matches, `[`, 1))
long <- as.numeric(sapply(matches, `[`, 2))

# Check if "location" is a concatenation of "latitude" and "longitude"
latitude_match <- (is.na(d311$latitude) | d311$latitude == "" | d311$latitude == lat)
longitude_match <- (is.na(d311$longitude) | d311$longitude == "" | d311$longitude == long)

# Get the rows where latitude or longitude does not match
mismatched_rows <- d311[!latitude_match | !longitude_match, ]
mismatched_rows <- mismatched_rows[complete.cases(mismatched_rows[, c("latitude", "longitude", "location")]), ]

# Print the results
num_row_mismatched_rows <- nrow(mismatched_rows)
if (num_row_mismatched_rows > 0) {
  cat("\n\nThere are", num_row_mismatched_rows, "non-matches between 'latitude' & 'longitude' and 'location'.\n")
  print(head(mismatched_rows, 5), row.names = FALSE, right = FALSE)
  result <- rank_by_agency(mismatched_rows)
} else {
  cat("\nAll values of 'latitude' & 'longitude' match the concatenation in the 'location' field.\n")
}

#########################################################################
# check to see if there are any non-matches between 'borough' and 'park_borough'
reference_field <- "borough"
duplicate_field <- "park_borough"
nonMatching_park_borough <- detect_duplicates(
  d311,
  reference_field,
  duplicate_field
)

if (!is.null(nonMatching_park_borough)) {
  #  sorted_park_borough <- rank_by_agency(nonMatching_park_borough)
  create_combo_chart(
    dataset = nonMatching_park_borough,
    chart_title = "non-matching between 'borough' and 'park_borough' by Agency & cumulative percentage",
    chart_file_name = "non_matching_park_borough_chart.pdf",
    chart_directory = chart_directory_path,
    console_print_out_title = "Summary of non-matching borough & park_borough by Agency"
  )
}

#########################################################################
# check to see if there are any non-matches between 'borough' and 'taxi_company_borough'
reference_field <- "borough"
duplicate_field <- "taxi_company_borough"

nonMatching_taxi_company_borough <- detect_duplicates(
  d311,
  reference_field,
  duplicate_field
)

if (!is.null(nonMatching_taxi_company_borough)) {
  #  sorted_taxi_borough <- (nonMatching_taxi_company_borough)
  create_combo_chart(
    dataset = nonMatching_taxi_company_borough,
    chart_title = "non-matching between 'borough' and 'taxi_company_borough' by Agency & cumulative percentage",
    chart_file_name = "non_matching_taxi_company_borough_chart.pdf",
    chart_directory = chart_directory_path,
    console_print_out_title = "Summary of non-matching borough & taxi_company_borough by Agency"
  )
}

#########################################################################

cat("\n\n**********CROSS STREET/INTERSECTION STREET ANALYSYS**********\n")

#########################################################################
cross_street <- "street_name"
intersection_street <- "landmark"
z1 <- cross_street_analysis(d311, cross_street, intersection_street, 
                            chart_directory = chart_directory_path)

cross_street <- "cross_street_1"
intersection_street <- "intersection_street_1"
y1 <- cross_street_analysis(d311, cross_street, intersection_street,
                            chart_directory = chart_directory_path)

cross_street <- "cross_street_2"
intersection_street <- "intersection_street_2"
y2 <- cross_street_analysis(d311, cross_street, intersection_street,
                            chart_directory = chart_directory_path)

#########################################################################

cat("\n\n**********REDUCE FILE SIZE BY REMOVING DUPLICATE/REDUNDANT FIELDS**********\n")

##########################################################################
cat("\nCurrent column names for d311 dataframe\n")
print(names(d311))

# List of redundant columns to remove
redundant_columns <- c(
  "agency_name",
  "park_borough",
  "intersection_street_1",
  "intersection_street_2",
  "location",
  "duration",
  "postClosedUpdateDuration"
)

cat("\nShrinking file size by deleting these", length(redundant_columns), "redundant and added fields:\n")

# Print the redundant columns vertically
index <- 1
for (column in redundant_columns) {
  cat("    ", index, "-", column, "\n")
  index <- index + 1
}

# Delete the redundant columns
d311_reduced <- d311[, !names(d311) %in% redundant_columns, ]

# Calculate the size of the new data table object
reduced_size <- object.size(d311_reduced)

# Compute the difference in size
size_reduction <- original_size - reduced_size

# Print the results
cat("\nOriginal size:", format(original_size, units = "auto", digits = 2))
cat("\nSize after removing redundant columns:", format(reduced_size, units = "auto", digits = 4))
cat(
  "\nPotential size reduction:", format(size_reduction, units = "auto", digits = 2), "or",
  round(size_reduction / original_size * 100, 2), "%\n"
)

#########################################################################
# Conclude program
# Store the program end time and calculate the duration
programStop <- as.POSIXct(Sys.time())
formatted_end_time <- format(programStop, "%Y-%m-%d %H:%M:%S")

# Calculate the duration of the program (in seconds)
duration_seconds <- as.numeric(difftime(programStop, programStart, units = "secs"))

# Convert the duration to a formatted string (hours, minutes, and seconds with 4 decimal places)
hours <- floor(duration_seconds / 3600)
minutes <- floor((duration_seconds %% 3600) / 60)
seconds <- round(duration_seconds %% 60, 4)  # Round to 4 decimal places

# Create the formatted duration string
duration_string <- paste0(
  if (hours > 0) paste0(hours, " hours, ") else "",
  if (minutes > 0) paste0(minutes, " minutes, ") else "",
  seconds, " seconds"
)

# Print the final program information to the console
cat("\n\n*****END OF PROGRAM*****\n")
cat("\n📅 Execution ends at:", formatted_end_time, "\n")
cat("\n⏱️ Program run-time:", duration_string, "\n")

#########################################################################
# Call the end_program function with the formatted end time and duration string
end_program(formatted_end_time, duration_string)


#########################################################################