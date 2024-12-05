
setwd("C:\\Users\\David\\OneDrive\\Documents\\datacleaningproject\\nyc311clean")

# Create the sub-directories used during program execution.
# Get the current working directory
working_dir <- getwd()

# Set the base directory under the working directory
base_dir <- file.path(working_dir, "code")

# Define the subdirectories
sub_dirs <- c("charts", "functions", "data", "console_output")

# Check if the base directory exists, and create it if it doesn't
if (!dir.exists(base_dir)) {
  dir.create(base_dir)
  cat("Base directory '", base_dir, "' created.\n", sep = "")
} 

# Loop through the subdirectories and create them if they don't exist
for (sub_dir in sub_dirs) {
  dir_path <- file.path(base_dir, sub_dir)
  if (!dir.exists(dir_path)) {
    dir.create(dir_path)
    cat("Subdirectory '", dir_path, "' created.\n", sep = "")
  } 
}

# Define the console output directory and file name.
output_dir <- file.path(base_dir, "console_output")
output_file <- file.path(output_dir, "core_console_output.txt")

# Define the path to the directory containing your function scripts
functions_path <- file.path(base_dir, "functions")

# Define the path for the main data file (CSV file)
data_file <- file.path(base_dir, "data")

# Define the path for the charts
chart_directory_path <- file.path(base_dir, "charts")

# Create the directory for the reduced size file following shrinkage code.
writeFilePath <- file.path(base_dir, "data")

cat{"\nDirectory structure and data files downloaded."}