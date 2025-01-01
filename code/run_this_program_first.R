# ============================
# 📁 INITIALIZATION SCRIPT
# ============================

# -------------------------------------------------------------
# 📂 SET THE BASE WORKING DIRECTORY
# -------------------------------------------------------------
# Customize this path as needed/desired
working_dir <- "C:/Users/David/OneDrive/Documents/datacleaningproject/initialization_test_directory"
# -------------------------------------------------------------

# If the working directory does not exist, create it
if (!dir.exists(working_dir)) {
  dir.create(working_dir, recursive = TRUE)
  cat("📁 Created working directory:", working_dir, "\n")
} 

setwd(working_dir)
base_dir <- working_dir  # Store as 'base_dir' for use in subdirectories
cat("📍 Working directory set to:", getwd(), "\n")

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
# 📁 CREATE SUBDIRECTORIES UNDER THE WORKING DIRECTORY
# -------------------------------------------------------------
sub_dirs <- c("charts", "functions", "data", "console_output", "logs")
for (sub_dir in sub_dirs) {
  dir_path <- file.path(base_dir, sub_dir)
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
    cat("📁 Created subdirectory:", dir_path, "\n")
  }
}

cat("\n✅ Project directory structure complete.")

# -------------------------------------------------------------
# 📦 DOWNLOAD AND EXTRACT DATASETS FROM FIGSHARE
# -------------------------------------------------------------
figshare_urls <- c(
  "https://figshare.com/ndownloader/files/50756982",  # File 1
  "https://figshare.com/ndownloader/files/50765283"   # File 2
)

figshare_directory <- file.path(base_dir, "data")

for (i in seq_along(figshare_urls)) {
  zip_file_name <- paste0("figshare_file_", i, ".zip")
  local_zip_path <- file.path(figshare_directory, zip_file_name)
  
  if (!file.exists(local_zip_path)) {
    cat("\n🌐 Downloading Figshare ZIP file", i, "using curl...\n")
    
    tryCatch({
      download_cmd <- paste0('curl -o "', local_zip_path, '" -L "', figshare_urls[i], '"')
      system(download_cmd)
      cat("\n✅ Data file download complete! File saved at:", local_zip_path, "\n")
      
      cat("\n📂 Extracting files from ZIP file", i, "...\n")
      unzip(zipfile = local_zip_path, exdir = figshare_directory)
      
      unzipped_files <- list.files(figshare_directory, full.names = TRUE)
      cat("\n🗂️ Files extracted from ZIP file", i, ":\n")
      print(unzipped_files)
      
    }, error = function(e) {
      cat("\n❌ Failed to download or extract Figshare file", i, ":", e$message, "\n")
    })
  } else {
    cat("\n📦 File already exists. Skipping download for:", local_zip_path, "\n")
  }
}


# -------------------------------------------------------------
# 📂 DOWNLOAD SPECIFIC R FILES FROM GITHUB
# -------------------------------------------------------------
github_urls <- c(
  "https://raw.githubusercontent.com/jun-yan/nyc311clean/refs/heads/main/code/datacleansing.R",
  "https://raw.githubusercontent.com/jun-yan/nyc311clean/refs/heads/main/code/timelinecharts.R",
  "https://raw.githubusercontent.com/jun-yan/nyc311clean/refs/heads/main/code/run_this_program_first.R"
)

github_directory <- base_dir

for (i in seq_along(github_urls)) {
  file_name <- basename(github_urls[i])
  local_file_path <- file.path(github_directory, file_name)
  
  if (!file.exists(local_file_path)) {
    cat("\n🌐 Downloading GitHub R file", file_name, "...\n")
    
    tryCatch({
      download_cmd <- paste0('curl -o "', local_file_path, '" -L "', github_urls[i], '"')
      system(download_cmd)
      cat("\n✅ Download complete! File saved at:", local_file_path, "\n")
    }, error = function(e) {
      cat("\n❌ Failed to download", file_name, ":", e$message, "\n")
    })
  } else {
    cat("\n📦 File already exists. Skipping download for:", local_file_path, "\n")
  }
}

# -------------------------------------------------------------
# 📂 DOWNLOAD AND SOURCE ALL R FUNCTION FILES FROM GITHUB
# -------------------------------------------------------------
functions_dir <- file.path(base_dir, "functions")
github_api_url <- "https://api.github.com/repos/jun-yan/nyc311clean/contents/code/functions"

tryCatch({
  response <- GET(github_api_url)
  if (status_code(response) == 200) {
    files <- content(response, "parsed")
    file_names <- sapply(files, function(x) x$name)
    file_urls <- sapply(files, function(x) x$download_url)
    
    for (i in seq_along(file_names)) {
      local_file_path <- file.path(functions_dir, file_names[i])
      
      if (!file.exists(local_file_path)) {
        cat("\n🌐 Downloading R function file", file_names[i], "...\n")
        
        tryCatch({
          download_cmd <- paste0('curl -o "', local_file_path, '" -L "', file_urls[i], '"')
          system(download_cmd)
          cat("✅ Downloaded:", file_names[i], "\n")
        }, error = function(e) {
          cat("\n❌ Failed to download", file_names[i], ":", e$message, "\n")
        })
      } else {
        cat("\n📦 File already exists. Skipping download for:", local_file_path, "\n")
      }
    }
    
    cat("\n🎉 All R function files downloaded successfully to:", functions_dir, "\n")
  } else {
    cat("\n❌ Failed to fetch file list. Check the repository URL or API rate limits.\n")
  }
}, error = function(e) {
  cat("\n❌ Failed to fetch R function files from GitHub:", e$message, "\n")
})


# -------------------------------------------------------------
# 📂 SOURCE ALL DOWNLOADED R FUNCTION FILES
# -------------------------------------------------------------
function_files <- list.files(functions_dir, pattern = "\\.R$", full.names = TRUE)

if (length(function_files) > 0) {
  for (file_path in function_files) {
    tryCatch({
      source(file_path)
      cat("\n✅ Successfully sourced:", file_path, "\n")
    }, error = function(e) {
      cat("\n❌ Error sourcing file:", file_path, "-", e$message, "\n")
    })
  }
} else {
  cat("\n❌ No function files found to source in:", functions_dir, "\n")
}

# -------------------------------------------------------------
# 🏁 FINAL MESSAGE TO THE USER
# -------------------------------------------------------------
cat("\n✅ All setup and initialization steps are complete!\n")
cat("📂 You can now run 'datacleansing.R' and 'timelinecharts.R' from the following directory:\n")
cat(base_dir, "\n")
