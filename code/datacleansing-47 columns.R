#########################################################################
# install.packages("ggplot2")
# install.packages("campfin")
# install.packages("stringr")
# install.packages("stringdist")
# install.packages("dply(raw data")
# install.packages("styler")
# install.packages("ggpmisc")
# install.packages("lubridate")
#install.packages("data.table")

library(ggplot2)
library(campfin)
library(stringr)
library(stringdist)
library(dplyr)
library(scales)
library(ggpmisc)
library(lubridate)
library(data.table)

#########################################################################
programStart <- as.POSIXct(Sys.time())
formattedStartTime <- format(programStart, "%Y-%m-%d %H:%M:%S")
cat("\nExecution begins at:", formattedStartTime)
cat("\n***** Program initialization *****")

# Set the working directory to the "nyc311clean/code" directory to enable relative codes.
# Alter this line of code to match your particular machine
setwd("C:/Users/david/OneDrive/Documents/datacleaningproject/nyc311clean/code")

# Set path for the data file
main_data_file <- "test_data.csv"
data1File <- file.path("..", "..", "data", main_data_file)
#data1File <- file.path("..", "..", "data", "test_data.csv")

# Hard code the max_closed_date to be midnight of the date of the data export from NYC Open Data
max_closed_date <- as.POSIXct("2024-08-31 23:59:59", format = "%Y-%m-%d %H:%M:%S")

chart_directory_path <- file.path("..", "..", "charts", "2022-2023 study", "Core charts")

writeFilePath <- file.path("..", "..", "data", "smaller.csv")

# Define the path to the directory containing your function scripts
functions_path <- "functions"

# Source all .R files in the directory
files <- list.files(functions_path, pattern = "\\.R$", full.names = TRUE)
# Source each file
lapply(files, source)
 
# Set scipen option to a large value
options(scipen = 10)

sink("../../console_output/core_console_output.txt")

options(digits = 14) # Set the number of decimal places to 14

#########################################################################
# compute the Hamming distance between two strings. Determine if it meets the threshold.
hamming_distance <- function(string1, string2) {
  if (nchar(string1) != nchar(string2)) {
    stop("Strings must be of equal length")
  }
  num_diff <- sum(as.numeric(charToRaw(string1)) != as.numeric(charToRaw(string2)))
  return(num_diff)
}

#########################################################################
cross_street_analysis <- function(
    dataset,
    cross_street,
    intersection_street) {
  # Remove extra blanks
  dataset <- dataset %>%
    mutate(
      across(
        all_of(cross_street),
        ~ str_replace_all(., "\\s+", " ")
      ),
      across(
        all_of(intersection_street),
        ~ str_replace_all(., "\\s+", " ")
      )
    )

  # Find rows where the two fields are equal (cross_street1 & intersection_street)
  equal_streets <- subset(dataset,
    dataset[[cross_street]] == dataset[[intersection_street]],
    select = c(cross_street, intersection_street, "agency")
  )

  num_rows_equal <- nrow(equal_streets)

  # Find rows where the two fields are equal and non-blank (cross_street1 & intersection_street)
  equal_streets_non_blank <- subset(
    equal_streets,
    !is.na(equal_streets[[cross_street]]) &
      !is.na(equal_streets[[intersection_street]]) &

      equal_streets[[cross_street]] != "" &
      equal_streets[[intersection_street]] != ""
  )

  num_rows_equal_non_blank <- nrow(equal_streets_non_blank)

  # find rows where both cross_street and intersection_street are not blank. Use later.
  both_xsteet_non_blank <-
    subset(dataset, dataset[[cross_street]] != "" &
      dataset[[intersection_street]] != "",
    select = c(cross_street, intersection_street, "agency")
    )

  num_rows_both_non_blank <- nrow(both_xsteet_non_blank)

  # find rows where cross_street is blank, but intersection_street is not blank. Use later.
  cross_street_blank <- subset(dataset, dataset[[cross_street]] == "" &
    dataset[[intersection_street]] != "",
  select = c(cross_street, intersection_street, "agency")
  )

  num_rows_cross_street_blank <- nrow(cross_street_blank)

  # find rows where intersection_street is blank, but cross_street is not blank
  intersection_street_blank <- subset(dataset, dataset[[intersection_street]] == "" &
    dataset[[cross_street]] != "",
  select = c(cross_street, intersection_street, "agency")
  )

  num_rows_intersection_street_blank <- nrow(intersection_street_blank)

  # find rows where both cross_street and intersection_street are blank
  both_blank <- subset(dataset, dataset[[cross_street]] == "" &
    dataset[[intersection_street]] == "",
  select = c(cross_street, intersection_street, "agency")
  )

  num_rows_both_blank <- nrow(both_blank)

  ####################
  # Check for non-matching fields between cross_street and intersection_street
  non_dup_streets <- detect_duplicates(
    dataset,
    cross_street,
    intersection_street
  )
  num_rows_non_dup <- nrow(non_dup_streets)

  non_dup_streets_non_blank <- non_dup_streets[non_dup_streets[[cross_street]] != "" &
                              non_dup_streets[[intersection_street]] != "", ]
  
  num_rows_non_dup_non_blank <- nrow(non_dup_streets_non_blank)

  cat("\nSample of matching",cross_street, "&", intersection_street, ":\n")
  random_sample_equal <- equal_streets_non_blank %>% sample_n(min(num_rows_equal_non_blank, 10)) # random sample
  print(random_sample_equal, row.names = FALSE, right = FALSE)

  cat("\nSample of non-matching", cross_street, "&", intersection_street, ":\n")
  random_sample_non_dup <- non_dup_streets_non_blank %>% sample_n(min(num_rows_non_dup_non_blank, 10)) # random sample
  print(random_sample_non_dup, row.names = FALSE, right = FALSE)

  non_dup_streets <- non_dup_streets[, -which(names(non_dup_streets) == "unique_key")]

  if (num_rows_non_dup > 0) {
    agency_match_non_dup <- rank_by_agency(non_dup_streets)

    chart_title <- paste0("Non-Matching '", cross_street, "' and '", intersection_street, "' by Agency & cumulative percentage", sep = "")
    chart_file_name <- paste0("non-matching_", cross_street, "and", intersection_street, ".pdf", sep = "")
    create_combo_chart(
      non_dup_streets,
      chart_title,
      chart_file_name
    )
  }

  ####################
  # cross_street is blank, but intersection_street is not blank

  cat(
    "\n\nThere are", format(num_rows_cross_street_blank, big.mark = ","),
    "occurrences where'", cross_street, "'is blank, \nbut '", intersection_street, "' is not blank representing",
    round((num_rows_cross_street_blank / num_rows_d311 * 100), 2), "% of total rows."
  )

  cat(
    "\n\nSample where '", cross_street, "'is blank but '", intersection_street, "'is not blank:\n"
  )
  random_sample <- cross_street_blank %>% sample_n(min(num_rows_cross_street_blank, 5)) # random sample
  print(random_sample, row.names = FALSE, right = FALSE)
  agency_cross_street_blank <- rank_by_agency(cross_street_blank)


  ####################
  # intersection_street is blank, but cross_street is not blank

  cat(
    "\n\nThere are", format(num_rows_intersection_street_blank, big.mark = ","),
    "occurrences where'", intersection_street, "'is blank, \nbut '", cross_street, "' is not blank representing",
    round(num_rows_intersection_street_blank / num_rows_d311 * 100, 2),
    "% of total rows."
  )

  cat(
    "\n\nSample where '", cross_street, "' is not blank but '", intersection_street, "' is blank:\n"
  )
  random_sample <- intersection_street_blank %>% sample_n(min(num_rows_intersection_street_blank, 5)) # random sample
  print(random_sample, row.names = FALSE, right = FALSE)
  agency_intersection_street_blank <- rank_by_agency(intersection_street_blank)

  ####################
  # Check for almost matches, using a Hamming distance of 2 characters different
  almost_match <-
    subset(
      non_dup_streets,
      non_dup_streets[[cross_street]] != "" &
        non_dup_streets[[intersection_street]] != "" &
        nchar(non_dup_streets[[cross_street]]) == nchar(non_dup_streets[[intersection_street]])
    )

  threshold <- 2

  cat(
    "\n\nA near-match is when two addresses have no more than",
    threshold, "characters different (Hamming Distance).\n"
  )

  # Calculate Hamming distance for each row
  almost_match$hamming_distance <- mapply(
    hamming_distance,
    almost_match[[cross_street]],
    almost_match[[intersection_street]]
  )

  # Identify rows where the Hamming distance is less than the threshold
  matches_meeting_threshold <- subset(almost_match, hamming_distance <= threshold)
  num_rows_matches_meeting_threshold <- nrow(matches_meeting_threshold)

  cat(
    "\nThere are ", num_rows_matches_meeting_threshold, "near-matches between",
    cross_street, "and", intersection_street
  )
  if (num_rows_matches_meeting_threshold > 0) {
    cat("\n\nSample of near-matching '", cross_street, "' & '", intersection_street, "(both non-blank):\n")
    random_sample_almost_match <- matches_meeting_threshold %>%
      sample_n(min(num_rows_matches_meeting_threshold, 10)) # random sample
    print(random_sample_almost_match, row.names = FALSE, right = FALSE)
  }

  ####################
  # summary of cross_street and intersection_street columns
  summary_street <-
    data.frame(
      category = c(
        "Matching -- non-blank",
        "Matching -- both blank",
        "Non-matching",
        paste(cross_street, "blank", sep = "_"),
        paste(intersection_street, "blank", sep = "_"),
        "Near-match"
      ),
      count = c(
        num_rows_equal - num_rows_both_blank,
        num_rows_both_blank,
        num_rows_non_dup,
        num_rows_cross_street_blank,
        num_rows_intersection_street_blank,
        num_rows_matches_meeting_threshold
      )
    )

  summary_street$percentage[1] <- round(((num_rows_equal - num_rows_both_blank)/num_rows_d311) * 100, 2)
  summary_street$percentage[2] <- round((num_rows_both_blank / num_rows_d311) * 100, 2)
  summary_street$percentage[3] <- round((num_rows_non_dup / num_rows_d311) * 100, 2)
  summary_street$percentage[4] <- "N/A"
  summary_street$percentage[5] <- "N/A"
  summary_street$percentage[6] <- round((num_rows_matches_meeting_threshold / num_rows_d311) * 100, 6)

  if (num_rows_matches_meeting_threshold == 0) {
    summary_street$percentage[6] <- "N/A"
  }

  cat("\nSummary of'", cross_street, "' and '", intersection_street, "':\n")
  names(summary_street)[1:3] <- c("category", "count", "percentage")
  print(summary_street, row.names = FALSE, right = FALSE)
  return(intersection_street_blank)
}

#########################################################################
# Take a reference field and an identified duplicate field, and print a sample
# The call the "rank_by_agency" function
print_sample_nonmatching <- function(
    dataset,
    reference_field = NULL,
    duplicate_field = NULL) {
  dataset <- selected_columns
  # Remove the NAs for better display purposes
  dataset <- dataset[dataset[[duplicate_field]] != "" | !is.na(dataset[[duplicate_field]]), ]

  if (nrow(dataset) > 0 & !is.null(reference_field) & !is.null(duplicate_field)) {
    cat("\nSample of non-matching", reference_field, "and", duplicate_field, " (excluding blanks):\n")
    sample_threshold <- 10
    print(sample_n(dataset, min(nrow(dataset), sample_threshold)),
      row.names = FALSE, right = FALSE
    )
  }
  return(rank_by_agency(dataset))
}

#########################################################################
detect_duplicates <- function(
    dataset,
    reference_field,
    duplicate_field) {
  
  #Identify matching fields including blanks and NA values
  rows_condition_0 <- dataset[[reference_field]] == dataset[[duplicate_field]]

  rows_condition_1 <- dataset[[reference_field]] != dataset[[duplicate_field]]

  all_non_matching_rows <- rows_condition_1
  non_matching_fields <- dataset[all_non_matching_rows, ]
  num_non_matching_fields <- nrow(non_matching_fields)
  non_matching_percentage <- round((num_non_matching_fields / num_rows_d311) * 100, 2)
  non_matching_fields <- non_matching_fields %>%
    select("unique_key", all_of(reference_field), all_of(duplicate_field), "agency")

  all_matching_rows <- rows_condition_0
  matching_fields <- dataset[all_matching_rows, ]
  num_matching_fields <- nrow(matching_fields)
  matching_percentage <- round((num_matching_fields / num_rows_d311) * 100, 2)
  matching_fields <- matching_fields %>%
    select("unique_key", all_of(reference_field), all_of(duplicate_field), "agency")

  if (num_non_matching_fields > 0) {
  
    if (num_matching_fields > 0) {
    cat(
      "\n\nThere are",
      format(num_matching_fields, big.mark = ","),
      "matches between '", reference_field, "' and '", duplicate_field, "'\nrepresenting",
      matching_percentage, "% of data.\n")
    }
    cat(
      "\nThere are",
      format(num_non_matching_fields, big.mark = ","),
      "non-matches between '", reference_field, "' and '", duplicate_field, "'\nrepresenting",
      non_matching_percentage, "% of data.\n")
    
  return(non_matching_fields)
  }
  else {
  cat("\nAll values match between '", reference_field, "' and '", duplicate_field, "'\n")
  }
}

#########################################################################
create_boxplot <- function(
    dataset,
    x_axis_title,
    chart_title,
    chart_file_name) {
  
    boxplot_chart <- ggplot(data = dataset, aes(x = duration, y = factor(1))) +
      geom_jitter(color = "#2271B2", alpha = 0.4, size = 1.9, shape = 17) +
      geom_boxplot(width = 0.2, fill = "#E69F00", alpha = 0.7, color = "black") +
      theme(
        legend.position = "none", plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(size = 9)
      ) +
      labs(
        title = chart_title, x = x_axis_title, y = "",
        subtitle = paste("(", earliest_title, "--", latest_title, ")", " n=", nrow(dataset), sep = "")
      )
  suppressMessages(print(boxplot_chart))
}

#########################################################################
create_violin_chart <- function(
    dataset,
    x_axis_title,
    x_axis_field,
    chart_title,
    chart_file_name) {
  
  violin_chart <- ggplot(data = dataset, aes(x = !!rlang::sym(x_axis_field), y = factor(1))) +
    geom_jitter(width = 0.25, alpha = 0.4, color = "#0072B2", size = 1.9, shape = 17) +
    geom_violin(linewidth = 0.7, fill = "transparent", color = "black") +
    geom_boxplot(width = 0.25, fill = "#D55E00", color = "black", alpha = 0.6, 
                 outlier.colour = "black", outlier.size = 0.75) +
    labs(
      title = chart_title,
      x = x_axis_title,
      y = "",
      subtitle = paste("(", earliest_title, "--", latest_title, ")", " n=", 
                       format(nrow(dataset), big.mark = ","), sep = "")
    ) +
    theme(
      plot.title = element_text(size = 13, hjust = 0.5),
      plot.subtitle = element_text(size = 9)
    )

  suppressMessages(print(violin_chart))
  chart_path <- file.path(chart_directory_path, chart_file_name)
  suppressMessages(ggsave(chart_path, plot = violin_chart, width = 10, height = 8))
}

#########################################################################
# Function to filter rows with non-numeric or non-5-digit zip codes for a specific field
filter_non_numeric_zipcodes <- function(df, zip_field) {

  # Define a logical condition to filter rows based on the selected field
  condition <- !is.na(df[[zip_field]]) & df[[zip_field]] != "" &
    !grepl("^[0-9]{5}$", df[[zip_field]])

  # Use the condition to subset the DataFrame
  invalid_rows <- df[condition, ]
  return(invalid_rows)
}

#########################################################################
# Function to check if a column contains valid dates
areAllDates <- function(dateField) {

  # remove blank and NAs
  allDates <-
    suppressWarnings(!is.na(as.Date(dateField[dateField != ""], format = "%m/%d/%Y %I:%M:%S %p")))

  if (!all(allDates)) {
    # find indices of values that are not dates
    not_date_indices <-
      which(is.na(as.Date(dateField[dateField], format = "%m/%d/%Y %I:%M:%S %p")))

    cat("Values that are not dates: ")
    print(dateField[not_date_indices], row.names = FALSE, right = FALSE)
  }
  return(all(allDates))
}

#########################################################################
# Validate that number fields are all numeric
areAllNumbers <- function(numberField) {
  
  # Identify the non-blank values using nzchar()
  non_blank_indices <- which(nzchar(numberField))

  # Subset the vector to keep only the non-blank values
  numberField <- numberField[non_blank_indices]

  # remove blank and NAs
  allNumbers <-
    suppressWarnings(!is.na(as.numeric(numberField[numberField != ""])))

  if (!all(allNumbers)) {
    # find indices of values that are not numeric
    non_numeric_values <-
      numberField[grepl("[^[:digit:]]", numberField)]

    cat(
      "Non-numeric values in the vector: ",
      non_numeric_values,
      "\n"
    )
  }
  return(all(allNumbers))
}

#########################################################################
areFiveDigits2 <- function(zipcodes) {
  
  # Remove blank values and <NA> values
  non_blank_zipcodes <- zipcodes[!(nzchar(zipcodes) | is.na(zipcodes))]

  # Identify non-numeric and non-5-digit zipcodes using regular expression
  zipcode_pattern <- "^\\d{5}$"

  # identify non-compliant zipcodes
  not_valid_zipcodes <-
    non_blank_zipcodes[!grepl(zipcode_pattern, non_blank_zipcodes)]

  if (length(not_valid_zipcodes) > 0) {
    cat(
      "\n\n*****Non 5-digit and/or non-numeric zipcodes found: ",
      not_valid_zipcodes
    )
    return(FALSE)
  }
  return(TRUE)
}

#########################################################################
# Validate that all fields are in the list of allowable values
are_valid_values <- function(
    dataset,
    listValidValues,
    field_name) {
  # Subset the vector to keep only the non-blank values
  dataset <- dataset[nzchar(dataset) & !is.na(dataset) & dataset != ""]

  # determine valid values
  check_list <- (dataset %in% listValidValues[, 1])
  inList <- dataset[check_list]
  notInList <- dataset[!check_list]
  num_invalid <- length(notInList)

  if (num_invalid > 0) {
    invalid_d311_rows <- d311[(d311[[field_name]] %in% notInList), ]

    # retrieve the number of blank fields for calculation purposes
    number_of_blank_entries <-
      missingDataPerColumn$blanks[missingDataPerColumn$field == field_name]
    percentage_invalid <- round((num_invalid / (num_rows_d311 - number_of_blank_entries)) * 100, 2)
    unique_invalid <- length(unique(notInList))
    cat(
      "\n\nThere are",
      format(num_invalid, big.mark = ","),
      "invalid", field_name, "entries \nrepresenting",
      percentage_invalid,
      "% of non-blank data,\n"
    )
    cat(
      "comprised of",
      unique_invalid,
      "different", field_name, "entries.\n"
    )

    # Sort the table in descending order
    sorted_invalid_table <-
      sort(table(notInList), decreasing = TRUE)

    # Convert the table to a data frame and calculate the percentage column
    invalid_df <-
      data.frame(
        invlaid_names = names(sorted_invalid_table),
        count = as.numeric(sorted_invalid_table)
      )
    invalid_df$percentage <- round(prop.table(invalid_df$count) * 100, 2)
    invalid_df <- invalid_df[order(invalid_df$percentage, invalid_df$invlaid_names, decreasing = TRUE), ]
    invalid_df$cumulative_percentage <- cumsum(invalid_df$percentage)

    # Print the top 10 values
    cat("\nTop Ten invalid '", field_name, "':\n")
    print(head(invalid_df, 10), right = FALSE)
    chart_title <-
      x <- rank_by_agency(invalid_d311_rows)
    results <- list(all(check_list), notInList, invalid_d311_rows)
    return(results)
  } else {
    cat("\n\nAll values of '", field_name, "'are valid.")
    results <- list(all(check_list), notInList)
    return(results)
  }
}

#########################################################################
# Function to check if a column is a valid date format
check_date_format <- function(column_name) {
  date_column <- d311[[column_name]]
  date_check <- try(as.Date(date_column, format = "%m/%d/%Y %I:%M:%S %p"))

  if (inherits(date_check, "try-error")) {
    invalid_dates <- date_column[!grepl("^\\d{2}/\\d{2}/\\d{4} \\d{2}:\\d{2}:\\d{2} [APMapm]{2}$", date_column)]
    if (length(invalid_dates) > 0) {
      print(paste("Invalid dates in", column_name, ":", invalid_dates, sep = ""))
    }
    return(paste("Field '", column_name, "' is not in a valid date format", sep = ""))
  } else {
    return(paste("Field '", column_name, "' is in a valid date format", sep = ""))
  }
}

#########################################################################
# File contains column names in the "header" line.
# The R "read.csv" function uses a "." to replace the spaces in column names.
# This makes the column names into legal variables, but the "." can cause problems elsewhere.
# The function "make_column_names_user_friendly" replaces the "." with an underscore "_".
# thus simplifying the field names. Additionally, the field names
# are converted to upper case.
#########################################################################


#########################################################################
# Load the USPS zipcode file
data2File <- file.path("..", "..", "data", "USPS_zipcodes.csv")
USPSzipcodes <-
  read.csv(data2File,
    header = TRUE,
    colClasses = rep("character"))

USPSzipcodes <- as.data.frame(USPSzipcodes)

USPSzipcodes <- make_column_names_user_friendly(USPSzipcodes)

# extract the 'delivery_zipcode' field
USPSzipcodesOnly <- USPSzipcodes[, "delivery_zipcode", drop = FALSE]
zipRows <- nrow(USPSzipcodesOnly)

#########################################################################
# Load USPS official street abbreviations

full_name <- c(
  "ALLEE", "ALLEY", "ALLY", "ANEX", "ANNEX", "ANNX", "APARTMENT",
  "ARCADE", "AV", "AVEN", "AVENU", "AVENUE", "AVN", "AVNUE",
  "BASEMENT", "BAYOO", "BAYOU", "BEACH", "BEND", "BL", "BLUF", "BLUFF", "BLUFFS", "BOT",
  "BOTTM", "BOTTOM", "BOUL", "BOULEVARD", "BOULV", "BRANCH", "BRDG", "BRDGE", "BRIDGE",
  "BRNCH", "BROOK", "BROOKS", "BUILDING", "BURG", "BURGS", "BYPA", "BYPAS",
  "BYPASS", "BYPS", "CAMP", "CANYN", "CANYON", "CAPE", "CAUSEWAY", "CAUSWA",
  "CEN", "CENT", "CENTER", "CENTERS", "CENTR", "CENTRE", "CIRC", "CIRCL",
  "CIRCLE", "CIRCLES", "CLIFF", "CLIFFS", "CLUB", "CMP", "CNTER", "CNTR",
  "CNYN", "COMMON", "COMMONS", "CORNER", "CORNERS", "COURSE", "COURT", "COURTS",
  "COVE", "COVES", "CRCL", "CRCLE", "CREEK", "CRESCENT", "CREST", "CROSSING", "CROSSROAD",
  "CROSSROADS", "CRSENT", "CRSNT", "CRSSNG", "CURVE", "DALE", "DAM", "DEPARTMENT", "DIV",
  "DIVIDE", "DRIV", "DRIVE", "DRIVES", "DRIVEWAY", "DRV", "DVD", "E/B", "EAST", "EASTBOUND",
  "END", "ESPLANADE", "ESTATE", "ESTATES", "ET", "EXIT", "EXP", "EXPR", "EXPRE",
  "EXPRESS", "EXPRESSWAY", "EXPW", "EXTENSION", "EXTENSIONS", "EXTN", "EXTNSN", "FALL",
  "FALLS", "FERRY", "FIELD", "FIELDS", "FLAT", "FLATS", "FLOOR", "FORD", "FORDS",
  "FOREST", "FORESTS", "FORG", "FORGE", "FORGES", "FORK", "FORKS", "FORT",
  "FREEWAY", "FREEWY", "FRONT", "FRRY", "FRT", "FRWAY", "FRWY", "FRWY",
  "GARDEN", "GARDENS", "GARDN", "GATEWAY", "GATEWY", "GATWAY", "GLEN",
  "GLENS", "GRDEN", "GRDN", "GRDNS", "GREEN", "GREENS", "GROV", "GROVE",
  "GROVES", "GTWAY", "HANGER", "HARB", "HARBOR", "HARBORS", "HARBR", "HAVEN",
  "HEIGHTS", "HGTS", "HIGHWAY", "HIGHWY", "HILL", "HILLS", "HIWAY", "HIWY",
  "HLLW", "HOLLOW", "HOLLOWS", "HOLWS", "HRBOR", "HT", "HWAY", "INLET",
  "ISLAND", "ISLANDS", "ISLES", "ISLND", "ISLNDS", "JCTION", "JCTN",
  "JCTNS", "JUNCTION", "JUNCTIONS", "JUNCTN", "JUNCTON", "KEY", "KEYS",
  "KNOL", "KNOLL", "KNOLLS", "LAKE", "LAKES", "LANDING", "LANE", "LDGE",
  "LIGHT", "LIGHTS", "LNDNG", "LOAF", "LOBBY", "LOCK", "LOCKS", "LODG",
  "LODGE", "LOOPS", "LOWER", "MANOR", "MANORS", "MDW", "MEADOW", "MEADOWS",
  "MEDOWS", "MILL", "MILLS", "MISSION", "MISSN", "MNT", "MNTAIN", "MNTN",
  "MNTNS", "MOTORWAY", "MOUNT", "MOUNTAIN", "MOUNTAINS", "MOUNTIN", "MSSN",
  "MTIN", "N/B", "N/W", "NECK", "NORTH", "NORTHBOUND", "NORTHEAST", "NORTHWEST",
  "OFFICE", "ORCHARD", "ORCHRD", "OVERPASS", "OVL", "OVPS", "PARKS", "PARKWAY",
  "PARKWAYS", "PARKWY", "PASSAGE", "PATHS", "PATHWAY", "PENTHOUSE", "PIKES",
  "PINE", "PINES", "PKWAY", "PKWYS", "PKY", "PLACE", "PLAIN", "PLAINS",
  "PLAZA", "PLZA", "POINT", "POINTS", "PORT", "PORTS", "PRAIRIE", "PRK",
  "PRR", "RAD", "RADIAL", "RADIEL", "RAILROAD", "RANCH", "RANCHES",
  "RAPID", "RAPIDS", "RDGE", "REST", "RIDGE", "RIDGES", "RIVER", "RIVR",
  "RNCHS", "ROAD", "ROADS", "ROADWAY", "ROOM", "ROUTE", "RVR", "RVR",
  "S/B", "SHOAL", "SHOALS", "SHOAR", "SHOARS", "SHORE", "SHORES", "SKYWAY",
  "SOUTH", "SOUTHEAST", "SOUTHWEST", "SPACE", "SPNG", "SPNGS", "SPRING",
  "SPRINGS", "SPRNG", "SPRNGS", "SPURS", "SQR", "SQRE", "SQRS", "SQU",
  "SQUARE", "SQUare", "STATION", "STATN", "STN", "STR", "STRAV",
  "STRAVEN", "STRAVENUE", "STRAVN", "STREAM", "STREET", "STREETS", "STREME",
  "STRT", "STRVN", "STRVNUE", "SUITE", "SUMIT", "SUMITT", "SUMMIT",
  "TERR", "TERRACE", "THROUGHWAY", "THRWY", "THWY", "THWY", "TNPK", "TRACE",
  "TRACES", "TRACK", "TRACKS", "TRAFFICWAY", "TRAIL", "TRAILER", "TRAILER",
  "TRAILS", "TRK", "TRKS", "TRLRS", "TRLS", "TRNPK", "TUNEL", "TUNLS", "TUNNEL",
  "TUNNELS", "TUNNL", "TURNPIKE", "TURNPK", "UNDERPASS", "UNION", "UNIONS", "UPPER",
  "VALLEY", "VALLEYS", "VALLY", "VDCT", "VIADCT", "VIADUCT", "VIEW", "VIEWS",
  "VILL", "VILLAG", "VILLAGE", "VILLAGES", "VILLE", "VILLG", "VILLIAGE",
  "VIST", "VISTA", "VLLY", "VST", "VSTA",
  "WALKS", "WELL", "WELLS", "WEST", "WY",
  "LA", "APPROACH", "APP", "BOUNDARY", "BDN", "BNDY", "CONCOURSE", "GRANDCONCOURSE",
  "PROMENADE", "THRUWAY", "AMERICA", "APR", "CNCRSE", "CRSE", "XTN",
  "PARKWA", "UNP", "TENNIS CT"
)

abb_name <- c(
  "ALY", "ALY", "ALY", "ANX", "ANX", "ANX", "APT", "ARC", "AVE", "AVE", "AVE",
  "AVE", "AVE", "AVE", "BSMT", "BYU", "BYU", "BCH", "BND", "BLVD", "BLF", "BLF",
  "BLFS", "BTM", "BTM", "BTM", "BLVD", "BLVD", "BLVD", "BR", "BRG", "BRG", "BRG",
  "BR", "BRK", "BRKS", "BLDG", "BG", "BGS", "BYP", "BYP", "BYP", "BYP",
  "CP", "CYN", "CYN", "CPE", "CSWY", "CSWY", "CTR", "CTR", "CTR", "CTRS", "CTR",
  "CTR", "CIR", "CIR", "CIR", "CIRS", "CLF", "CLFS", "CLB", "CP", "CTR", "CTR",
  "CYN", "CMN", "CMNS", "COR", "CORS", "CRSE", "CT", "CTS", "CV", "CVS", "CIR",
  "CIR", "CRK", "CRES", "CRST", "XING", "XRD", "XRDS", "CRES", "CRES", "XING",
  "CURV", "DL", "DM", "DEPT", "DV", "DV", "DR", "DR", "DRS", "DRWY", "DR", "DV",
  "EB", "E", "EB", "END", "ESPL", "EST", "ESTS", "EX", "EX", "EXPY", "EXPY", "EXPY",
  "EXPY", "EXPY", "EXPY", "EXT", "EXTS", "EXT", "EXT", "FLS", "FLS", "FRY", "FLD",
  "FLDS", "FLT", "FLTS", "FL", "FRD", "FRDS", "FRST", "FRST", "FRG", "FRG", "FRGS",
  "FRK", "FRKS", "FT", "FWY", "FWY", "FRNT", "FRY", "FT", "FWY", "FWY", "FWY",
  "GDN", "GDNS", "GDN", "GTWY", "GTWY", "GTWY", "GLN", "GLNS", "GDN", "GDN",
  "GDNS", "GRN", "GRNS", "GRV", "GRV", "GRVS", "GTWY",
  "HNGR", "HBR", "HBR", "HBRS", "HBR", "HVN", "HTS", "HTS",
  "HWY", "HWY", "HL", "HLS", "HWY", "HWY", "HOLW",
  "HOLW", "HOLW", "HOLW", "HBR", "HTS", "HWY",
  "INLT", "IS", "ISS", "ISLE", "IS", "ISS",
  "JCT", "JCT", "JCTS", "JCT", "JCTS", "JCT", "JCT",
  "KY", "KYS", "KNL", "KNL", "KNLS",
  "LK", "LKS", "LNDG", "LN", "LDG", "LGT", "LGTS", "LNDG", "LF", "LBBY",
  "LCK", "LCKS", "LDG", "LDG", "LOOP", "LOWR",
  "MNR", "MNRS", "MDWS", "MDW", "MDWS", "MDWS", "ML", "MLS", "MSN",
  "MSN", "MT", "MTN", "MTN", "MTNS", "MTWY", "MT", "MTN", "MTNS", "MTN", "MSN", "MTN",
  "NB", "NW", "NCK", "N", "NB", "NE", "NW",
  "OFC", "ORCH", "ORCH", "OPAS", "OVAL", "OPAS",
  "PARK", "PKWY", "PKWY", "PKWY", "PSGE", "PATH", "PWY", "PH", "PIKE", "PNE",
  "PNES", "PKWY", "PKWY", "PKWY", "PL", "PLN", "PLNS", "PLZ", "PLZ", "PT",
  "PTS", "PRT", "PRTS", "PR", "PARK", "PR",
  "RADL", "RADL", "RADL", "RR", "RNCH", "RNCH",
  "RPD", "RPDS", "RDG", "RST", "RDG", "RDGS", "RIV", "RIV", "RNCH", "RD",
  "RDS", "RDWY", "RM", "RTE", "RIV", "RIV",
  "SB", "SHL", "SHLS", "SHR", "SHRS", "SHR", "SHRS", "SKWY", "S", "SE", "SW",
  "SPC", "SPG", "SPGS", "SPG", "SPGS", "SPG", "SPGS", "SPUR", "SQ", "SQ",
  "SQS", "SQ", "SQ", "SQS", "STA", "STA", "STA", "ST", "STRA", "STRA",
  "STRA", "STRA", "STRM", "ST", "STS", "STRM", "ST", "STRA", "STRA", "STE",
  "SMT", "SMT", "SMT",
  "TER", "TER", "TRWY", "TRWY", "TRWY", "TRWY", "TPKE", "TRCE", "TRCE", "TRAK",
  "TRAK", "TRFY", "TRL", "TRLR", "TRLR", "TRL", "TRAK", "TRAK", "TRLR", "TRL",
  "TPKE", "TUNL", "TUNL", "TUNL", "TUNL", "TUNL", "TPKE", "TPKE",
  "UPAS", "UN", "UNS", "UPPR",
  "VLY", "VLYS", "VLY", "VIA", "VIA", "VIA", "VW", "VWS", "VLG", "VLG", "VLG",
  "VLGS", "VL", "VLG", "VLG", "VIS", "VIS", "VLY", "VIS", "VIS",
  "WALK", "WL", "WLS", "W", "WAY",
  "LN", "APPR", "APPR", "BDY", "BDY", "BDY", "CONCRS", "GRAND CONCRS",
  "PROM", "TRWY", "AMERICAS", "APPR", "CONCRS", "CRES", "EXT",
  "PKWY", "UPAS", "TENNIS COURT"
)
USPSabbreviations <- data.frame(full = full_name, abb = abb_name)
USPSabbreviations <- make_column_names_user_friendly(USPSabbreviations)
names(USPSabbreviations) <- c("full", "abb")
numAbbreviations <- nrow(USPSabbreviations)

#########################################################################
# Load the Police Precinct reference file
precinct_names <- c(
  "1", "2", "3", "4", "5", "6", "7", "8", "9", "10",
  "11", "12", "13", "14", "15", "16", "17", "18", "19", "20",
  "21", "22", "23", "24", "25", "26", "27", "28", "29",
  "30", "31", "32", "33", "34", "35", "36", "37", "38", "39",
  "40", "41", "42", "43", "44", "45", "46", "47", "48", "49",
  "50", "51"
)
precinctsNYPD <- data.frame(nypd_precinct <- precinct_names)
precinctsNYPD <- make_column_names_user_friendly(precinctsNYPD)
numPrecincts <- nrow(precinctsNYPD)

#########################################################################
# Load the NYC City Council file
city_council_names <- c(
  "1", "2", "3", "4", "5", "6", "7", "8", "9", "10",
  "11", "12", "13", "14", "15", "16", "17", "18", "19", "20",
  "21", "22", "23", "24", "25", "26", "27", "28", "29",
  "30", "31", "32", "33", "34", "35", "36", "37", "38", "39",
  "40", "41", "42", "43", "44", "45", "46", "47", "48", "49",
  "50", "51"
)

cityCouncilNYC <- data.frame(NYC_city_council = city_council_names)
cityCouncilNYC <- make_column_names_user_friendly(cityCouncilNYC)
numCityCouncil <- nrow(cityCouncilNYC)

#########################################################################
# Load the main 311 SR data file. Set the read & write paths.
d311 <-
  read.csv(data1File,
    header = TRUE,
    colClasses = rep("character", ncol(read.csv(data1File)))
  )
original_size <- object.size(d311)

# Convert data.table to data.frame
#d311 <- as.data.frame(d311)

# make columns names user friendly
d311 <- make_column_names_user_friendly(d311)

num_rows_d311 <- nrow(d311)
num_columns_d311 <- ncol(d311)

#########################################################################
# Convert character fields to upper case to facilitate comparisons
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
d311$created_date <- as.POSIXct(d311$created_date, format = "%m/%d/%Y %I:%M:%S %p", tz = "America/New_York")
d311$closed_date <- as.POSIXct(d311$closed_date, format = "%m/%d/%Y %I:%M:%S %p", tz = "America/New_York")
d311$due_date <- as.POSIXct(d311$due_date, format = "%m/%d/%Y %I:%M:%S %p", tz = "America/New_York")
d311$resolution_action_updated_date <- as.POSIXct(d311$resolution_action_updated_date,
  format = "%m/%d/%Y %I:%M:%S %p", tz = "America/New_York"
)

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

chart_sub_title <- paste("(", earliest_title, "--", latest_title, ") total=", sep = "")

# Display the results
cat("\nNumber of rows in the 311 SR data set:", format(num_rows_d311, big.mark = ","))
cat("\n\nNumber of columns in the 311 SR data set:", format(num_columns_d311, big.mark = ","))
cat("\n\nData contains SRs created from", earliest_date_formatted, "through", latest_date_formatted, "\n")

#########################################################################

cat("\n**********BLANK and NA ENTRIES BY COLUMN**********")

#########################################################################
# calculate the number of blank and N/A data entries.

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

# Determine the parameters for the chart
max_count <- max(missingDataPerColumn$blanks)
total_count <- sum(missingDataPerColumn$count)

result <- calculate_values(max_count)
starting_value <- as.numeric(result$starting_value)
increment <- result$increment

# Create the bar chart with vertical X-axis labels
blank_chart <- ggplot(missingDataPerColumn, aes(x = reorder(field, -total_empty), y = total_empty)) +
  geom_bar(stat = "identity", fill = "#117733") +
  theme(
    axis.title.x = element_text(vjust = 0, size = 11),
    axis.title.y = element_text(vjust = 1, size = 11),
    plot.title = element_text(hjust = 0.5, size = 13),
    plot.subtitle = element_text(size = 9),
    panel.background = element_rect(fill = "gray95", color = "gray95"),
    axis.text.x = element_text(angle = 50, vjust = 1, hjust = 1, face = "bold"),
    axis.text.y = element_text(face = "bold"),
    legend.position = "none" # Remove all legends
  ) +
  geom_text(aes(
    x = field, y = total_empty, label = pct_empty,
    angle = -70
  )) +
  geom_hline(
    yintercept = seq(starting_value, max_count, by = increment),
    linetype = "dotted", color = "gray40"
  ) +
  ggtitle("Number and % of blank/missing fields per column",
    subtitle = paste(chart_sub_title, format(num_rows_d311, big.mark = ","), sep = "")
  ) +
  labs(y = NULL, x = NULL)

# Print the bar chart
print(blank_chart)
chart_path <- file.path(chart_directory_path, "BlankFields.pdf")
ggsave(chart_path, plot = blank_chart, width = 10, height = 8)

########################################################################
# Remove specific columns using base R subsetting
# Delete columns (example: dataset <- dataset[, -c(1:5)])
# d311 <- d311[, !names(d311) %in% c(
#   "agency_name", 
#   "descriptor", 
#   "resolution_description", 
#   "bbl", 
#   "x_coordinate_state_plane ",
#   "y_coordinate_state_plane ",
#   "park_facility_name",
#   "bridge_highway_name",
#   "bridge_highway_direction",
#   "road_ramp",
#   "bridge_highway_segment")]
# 
#########################################################################
# consolidate Agencies (DCA, DOITT, NYC311-PRD)
d311 <- consolidate_agencies((d311))

sorted_by_agency <- rank_by_agency(d311)

chart_title <- "SR count by Agency & cumulative percentage"
chart_file_name <- "SRs_by_Agency.pdf"

create_combo_chart(
  d311,
  chart_title,
  chart_file_name
)

#########################################################################
# Calculate complaint frequency and responsible agency
complaintData <- as.data.frame(table(d311$complaint_type))
complaintData <- complaintData[order(-complaintData$Freq), ]
complaintData$percent <- round(prop.table(complaintData$Freq) * 100, 2)
complaintData$cumulative_percent <- cumsum(complaintData$percent)

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

cat("\nTop 10 'complaint_type's and responsible Agency:\n")
print(head(complaintData, 10), row.names = FALSE, right = FALSE)

cat("\nBottom 10 'complaint_type's and responsible Agency:\n")
print(tail(complaintData[, c("complaint_type", "count", "agency")], 10),
  row.names = FALSE, right = FALSE
)

# Identify the 'Noise' complaints
noise_rows <- complaintData %>%
  filter(str_starts(complaint_type, "NOISE"))

cat("\nThere are ", nrow(noise_rows), " categories of Noise complaints:\n", sep = "")
print(noise_rows, right = c(0, rep(1, ncol(noise_rows) - 1)))

cat(
  "\nNoise complaints of all types number",
  format(sum(noise_rows$count), big.mark = ","),
  "constituting", round(sum(noise_rows$percent), 0), "% of all SRs.\n"
)

# Rename columns to trick 'create_combo_chart' function to use 'complaint_type' as 'agency'
colnames(d311)[colnames(d311) == "agency"] <- "temp_agency"
colnames(d311)[colnames(d311) == "complaint_type"] <- "agency"

chart_title <- "Top 20 Complaints and cumulative percentage"
chart_file_name <- "SR_by_Complaint_Type.pdf"

create_combo_chart(
  d311,
  chart_title,
  chart_file_name
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
# Test if each date field is in POSIXct format
d311$created_date <- as.POSIXct(d311$created_date, format = "%m/%d/%Y %I:%M:%S %p", tz = "America/New_York")
d311$closed_date <- as.POSIXct(d311$closed_date, format = "%m/%d/%Y %I:%M:%S %p", tz = "America/New_York")
d311$due_date <- as.POSIXct(d311$due_date, format = "%m/%d/%Y %I:%M:%S %p", tz = "America/New_York")
d311$resolution_action_updated_date <- as.POSIXct(d311$resolution_action_updated_date,
  format = "%m/%d/%Y %I:%M:%S %p", tz = "America/New_York"
)
# Coerce class to POSIXct to avoid the POSIXt class (a subclass of POSIXct)
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
# determine if the incident_zip and zip_codes fields contain 5 numeric digits
# Call the function for "incident_zip" field
invalid_incident_zip_rows <- filter_non_numeric_zipcodes(d311, "incident_zip")

num_row_invalid_incident_zip_rows <- nrow(invalid_incident_zip_rows)
if (num_row_invalid_incident_zip_rows == 0) {
  cat("\n\nAll 'incident_zip' entries are 5 numeric digits\n.")
} else {
  cat("\n\nThere are", num_row_invalid_incident_zip_rows, "non-numeric, non-5-digit 'incident_zip' entries.\n")

  selected_columns <- invalid_incident_zip_rows %>%
    select(unique_key, incident_zip, agency)

  print(head(selected_columns, 10), row.names = FALSE, right = FALSE)
}

#########################################################################
# Call the function for "zip_codes" field
invalid_zip_codes_rows <- filter_non_numeric_zipcodes(d311, "zip_codes")

num_row_invalid_zip_codes_rows <- nrow(invalid_zip_codes_rows)
if (num_row_invalid_zip_codes_rows == 0) {
  cat("\nAll 'zip_codes' entries are 5 numeric digits.")
} else {
  cat("\nThere are", num_row_invalid_zip_codes_rows, "non-numeric, non-5 digit 'incident_zip' entries.")

  selected_columns <- invalid_zip_codes_rows %>%
    select(unique_key, all_of(incident_zip), all_of(agency))
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

community_districtsNum <- areAllNumbers(d311$community_districts)
cat(
  "\n\nAre all values in 'community_districts' numbers?",
  areAllNumbers(community_districtsNum)
)

borough_boundariesNum <- areAllNumbers(d311$borough_boundaries)
cat(
  "\n\nAre all values in 'borough_boundaries' numbers?",
  borough_boundariesNum
)

city_council_districtsNum <-
  areAllNumbers(d311$city_council_districts)
cat(
  "\n\nAre all values in 'city_council_district' numbers?",
  city_council_districtsNum
)

police_precinctsNum <- areAllNumbers(d311$police_precincts)
cat(
  "\n\nAre all values in 'police_precincts' numbers?",
  police_precinctsNum
)

if ("police_precinct" %in% colnames(d311)) {
  # Code to execute if "police_precinct" column is present
  police_precinctNum <- areAllNumbers(d311$police_precinct)
  cat(
    "\n\nAre all values in '*police_precinct*' numbers?",
    police_precinctNum, "\n"
  )
} else {
  # Code to execute if "police_precinct" column is not present
  cat("\n\nThe '***police_precinct***' column does not exist in this dataset.")
}

#########################################################################

cat("\n\n**********CHECKING FOR ALLOWABLE AND VALID VALUES**********\n")

#########################################################################
# determine if the unique_key is in fact unique
uniqueKeys <- length(unique(d311$unique_key)) == num_rows_d311
cat("\nAre all 'unique_keys' truly unique?", uniqueKeys, "\n")

#########################################################################
# Check to see if any of the latitudes or longitudes fall outside the extreme points of New York City.
# Change the lat/long fields into type "numeric" to enable comparison.

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
  print(head(badLatitudes[, c("unique_key", "agency", "latitude")], 5), row.names = FALSE, right = FALSE)
}

cat(
  "\nThe number of 'longitudes' outside the boundaries of NYC is:",
  nrow(badLongitudes)
)

if (nrow(badLongitudes) > 0) {
  print(head(badLongitudes[, c("unique_key", "agency", "longitude")], 5), row.names = FALSE, right = FALSE)
}

#########################################################################
address_type_results <- are_valid_values(d311$address_type, data.frame(
  values = c(
    "ADDRESS",
    "BBL",
    "BLOCKFACE",
    "INTERSECTION",
    "PLACENAME",
    "UNRECOGNIZED"
  )
), "address_type")

statusResults <-
  are_valid_values(d311$status, data.frame(
    values = c(
      "ASSIGNED",
      "CANCEL",
      "CLOSED",
      "IN PROGRESS",
      "OPEN",
      "PENDING",
      "STARTED",
      "UNSPECIFIED"
    )
  ), "status")

# check if borough, borough_boundaries, taxi_company_borough, and park_borough contain only allowable values
boroughResults <-
  are_valid_values(d311$borough, data.frame(
    values = c(
      "BRONX",
      "BROOKLYN",
      "MANHATTAN",
      "QUEENS",
      "STATEN ISLAND",
      "UNSPECIFIED"
    )
  ), "borough")

borough_boundariesResults <-
  are_valid_values(d311$borough_boundaries, data.frame(values = c("1", "2", "3", "4", "5")), "borough_boundaries")

park_boroughResults <-
  are_valid_values(d311$park_borough, data.frame(
    values = c(
      "BRONX",
      "BROOKLYN",
      "MANHATTAN",
      "QUEENS",
      "STATEN ISLAND",
      "UNSPECIFIED"
    )
  ), "park_borough")

taxi_company_boroughResults <-
  are_valid_values(d311$taxi_company_borough, data.frame(
    values = c("BRONX", "BROOKLYN", "MANHATTAN", "QUEENS", "STATEN ISLAND")
  ), "taxi_company_borough")

open_data_channelResults <-
  are_valid_values(d311$open_data_channel_type, data.frame(values = c(
    "MOBILE",
    "ONLINE",
    "OTHER",
    "PHONE",
    "UNKNOWN"
  )), "open_data_channel")

vehicle_typeResults <-
  are_valid_values(d311$vehicle_type, data.frame(
    values = c(
      "AMBULETTE / PARATRANSIT",
      "CAR",
      "CAR SERVICE",
      "COMMUTER VAN",
      "GREEN TAXI",
      "OTHER",
      "SUV",
      "TRUCK",
      "VAN"
    )
  ), "vehicle_type")

city_councilResults <-
  are_valid_values(d311$city_council_districts, cityCouncilNYC, "city_council_districts")

police_precinctResults <-
  are_valid_values(d311$police_precincts, precinctsNYPD, "police_precincts")
if (!police_precinctResults[[1]]) {
  chart_title <- "Invalid 'police_precincts' by Agnecy & cumulative percentage"
  chart_file_name <- "invalid_police_precincts.pdf"
  police_precincts_dataset <- police_precinctResults[[3]]
  create_combo_chart(police_precincts_dataset, chart_title, chart_file_name)
}

police_precinctResults2 <-
  are_valid_values(d311$police_precinct, precinctsNYPD, "police_precinct")
if (!police_precinctResults2[[1]]) {
  chart_title <- "Invalid 'police_precinct' by Agnecy & cumulative percentage"
  chart_file_name <- "invalid_police_precinct.pdf"
  police_precinct_dataset <- police_precinctResults2[[3]]
  create_combo_chart(police_precinct_dataset, chart_title, chart_file_name)
}

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

cb_results <- are_valid_values(d311$community_board, data.frame(cbValues), "community_board")
if (!cb_results[[1]]) {
  chart_title <- "Invalid community boards by Agnecy & cumulative percentage"
  chart_file_name <- "invalid_community_boards.pdf"
  cb_dataset <- cb_results[[3]]
  create_combo_chart(cb_dataset, chart_title, chart_file_name)
}

#########################################################################
# Check for invalid zip codes in d311$incident_zip using USPSzipcodesOnly

incident_zip_results <- are_valid_values(d311$incident_zip, USPSzipcodesOnly, "incident_zip")
if (!incident_zip_results[[1]]) {
  chart_title <- "Invalid incident_zip by Agnecy & cumulative percentage"
  chart_file_name <- "invalid_incident_zip.pdf"
  incident_zip_dataset <- incident_zip_results[[3]]
  create_combo_chart(incident_zip_dataset, chart_title, chart_file_name)
}

###################################################
# Check for invalid zip codes in d311$zip_codes using USPSzipcodesOnly

zipcodes_results <- are_valid_values(d311$zip_codes, USPSzipcodesOnly, "zip_codes")
if (!zipcodes_results[[1]]) {
  chart_title <- "Invalid zipcodes by Agnecy & cumulative percentage"
  chart_file_name <- "invalid_izipcodes.pdf"
  zipcodes_dataset <- zipcodes_results[[3]]
  create_combo_chart(zipcodes_dataset, chart_title, chart_file_name)
}

#########################################################################
# Duration is the time between created_date and closed_date
# Compute and store "duration" in a new additional column for the "d311" dataframe.
d311$duration <-
  as.numeric(difftime(d311$closed_date, d311$created_date, units = "days"))

positiveDurations <- d311[d311$duration > 0 & !is.na(d311$duration), ]
zeroDurations <- d311[d311$duration == 0 & !is.na(d311$duration), ]
negativeDurations <- d311[d311$duration < 0 & !is.na(d311$duration), ]

#########################################################################
# Identify SRs with negative duration (closed before they were created)
# Exclude the extreme values of "closed dates" of 01/01/1999 (i.e. -4000 days)

closedBeforeOpened <- subset(d311, duration < 0 & !is.na(duration),
  select = c("unique_key", "created_date", "closed_date", "duration", "agency")
)

numBlankClosedDate <-
  missingDataPerColumn[missingDataPerColumn$field == "closed_date", "total_empty"]

num_rows_closedBeforeOpened <- nrow(closedBeforeOpened)

if (num_rows_closedBeforeOpened > 0) {
  cat(
    "\n\nThere are", format(num_rows_closedBeforeOpened, big.mark = ","),
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

  chart_title <- "negative duration SRs by Agency & cumulative percentage"
  chart_file_name <- "negative_duration_SR_barchart.pdf"

  create_combo_chart(
    closedBeforeOpened,
    chart_title,
    chart_file_name
  )

  x_axis_label <- "Negative duration (days)"
  x_axis_field <- "duration"
  chart_title <- "Closed before Created (negative duration days)"
  chart_file_name <- "negative_duration_SR_violin.pdf"
  
  negativeDurationViolin <- create_violin_chart(
    large_neg_duration,
    x_axis_label,
    x_axis_field,
    chart_title,
    chart_file_name
  )

  # Create boxplot of the (negative) duration values
  negativeDurationChart <- ggplot(
    data = large_neg_duration, aes(x = duration, y = factor(1))) +
    geom_jitter(color = "#0072B2", alpha = 0.4, size = 1.9, shape = 17) +
    geom_boxplot(width = 0.25, fill = "#D55E00", alpha = 0.75, outlier.colour = "black", outlier.size = 1) +
    theme(
      legend.position = "none", plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(size = 9)
    ) +
    labs(
      title = "SRs closed before they were created (negative duration) *excluding large negative values",
      x = "Duration (negative)", y = "",
      subtitle = paste("(", earliest_title, "--", latest_title, ")", " n=", nrow(large_neg_duration), sep = "")
    )

  print(negativeDurationChart)
  chart_path <- file.path(chart_directory_path, "negative_duration_SR_boxplot.pdf")
  ggsave(chart_path, plot = negativeDurationChart, width = 10, height = 8)
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
    "\n\nThere are",
    format(num_rows_zeroDurations, big.mark = ","),
    "SRs that are 'closed' and 'created' at the exact same time, \nto the second, representing",
    round(num_rows_zeroDurations / (num_rows_d311 - numBlankClosedDate) * 100, 4),
    "% of non-blank data and creating a zero duration."
  )

  cat("\n\nSample of SRs 'closed' at the exact same time they are 'created':\n")
  random_sample <- zeroDurations %>% sample_n(min(num_rows_zeroDurations, 5)) # random sample
  print(random_sample, row.names = FALSE, right = FALSE)

  sorted_zero_durations <- rank_by_agency(zeroDurations)

  chart_title <- "Zero duration SRs by Agency & cumulative percentage"
  chart_file_name <- "zero_duration_SR.pdf"
  if (!is.null(sorted_zero_durations)) {
    create_combo_chart(
      zeroDurations,
      chart_title,
      chart_file_name
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
    # Create boxplot of the (negative) duration values
    closedinFutureChart <- ggplot(
      data = closedinFuture,
      aes(x = future_days, y = factor(1))
    ) +
      geom_jitter(color = "#0072B2", size = 2, shape = 17) +
      geom_boxplot(
        outlier.colour = "black", outlier.shape = 16, linewidth = 0.7,
        fill = "#D55E00", size = 1, color = "black"
      ) +
      theme(
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 13),
        plot.subtitle = element_text(size = 9)
      ) +
      #    scale_x_reverse() +
      labs(
        title = "SRs closed in the future",
        x = "Days closed in the future",
        subtitle = paste("(", earliest_title, "--", latest_title, ")", " n=", num_rows_future, sep = "")
      )

    print(closedinFutureChart)
    chart_path <- file.path(chart_directory_path, "future_closed.pdf")
    ggsave(chart_path, plot = closedinFutureChart, width = 10, height = 8)
  }
} else {
  cat("\n\nThere are no SRs with a 'closed_date' in the future.\n")
}

########################################################################
# Identify SRs created at midnight and noon
# Extract hour, minute, and second components of closed_date for valid rows
hour <- as.numeric(format(d311$created_date, "%H"))
minute <- as.numeric(format(d311$created_date, "%M"))
second <- as.numeric(format(d311$created_date, "%S"))

# Identify rows with time exactly at midnight (00:00:00)
midnight_created_rows <- hour == 0 & minute == 0 & second == 0
noon_created_rows <- hour == 12 & minute == 0 & second == 0

# Count the number of rows with time exactly at midnight
midnight_created_count <- sum(midnight_created_rows)
noon_created_count <- sum(noon_created_rows)

midnight_created_data <- d311[midnight_created_rows, ]
created_at_midnight <- midnight_created_data[, c("unique_key", "created_date", "agency")]

noon_created_data <- d311[noon_created_rows, ]
created_at_noon <- noon_created_data[, c("unique_key", "created_date", "agency")]

if (midnight_created_count > 0) {
  cat(
    "\n\nThere are",
    format(midnight_created_count, big.mark = ","),
    "SRs that were 'created' at exactly midnight.\n"
  )

  sorted_create_at_midnight <- rank_by_agency(created_at_midnight)

  chart_title <- "SRs created exactly at midnight by Agency & cumulative percentage"
  chart_file_name <- "created_at_midnight_chart.pdf"

  create_combo_chart(
    created_at_midnight,
    chart_title,
    chart_file_name
  )
} else {
  cat("\n\nThere are no SRs with a 'created_date' exactly at midnight.\n")
}

if (noon_created_count > 0) {
  cat(
    "\n\nThere",
    format(noon_created_count, big.mark = ","),
    "SRs that were 'created' exactly at noon."
  )

  sorted_create_at_noon <- rank_by_agency(created_at_noon)

  chart_title <- "SRs created exactly at noon by Agency & cumulative percentage"
  chart_file_name <- "created_at_noon_chart.pdf"
  if (!is.null(sorted_create_at_noon)) {
    create_combo_chart(
      created_at_noon,
      chart_title,
      chart_file_name
    )
  } else {
    cat("\n\nThere are no SRs with a 'created_date' exactly at noon.\n")
  }
}

#########################################################################
# Identify SRs closed at midnight and noon

# Remove N/A closed_date(s)
valid_closed_date <- !is.na(d311$closed_date)
valid_closed_data <- d311[valid_closed_date, ]

# Extract hour, minute, and second components of closed_date for valid rows
hour <- as.numeric(format(d311$closed_date[valid_closed_date], "%H"))
minute <- as.numeric(format(d311$closed_date[valid_closed_date], "%M"))
second <- as.numeric(format(d311$closed_date[valid_closed_date], "%S"))

# Identify rows with time exactly at midnight (00:00:00)
midnight_closed_rows <- hour == 0 & minute == 0 & second == 0
noon_closed_rows <- hour == 12 & minute == 0 & second == 0

# Count the number of rows with time exactly at midnight
midnight_closed_count <- sum(midnight_closed_rows)
noon_closed_count <- sum(noon_closed_rows)

midnight_closed_data <- valid_closed_data[midnight_closed_rows, ]
closed_at_midnight <- midnight_closed_data[, c("unique_key", "created_date", "agency")]

noon_closed_data <- valid_closed_data[noon_closed_rows, ]
closed_at_noon <- noon_closed_data[, c("unique_key", "created_date", "agency")]

if (midnight_closed_count > 0) {
  cat(
    "\n\nThere are",
    format(midnight_closed_count, big.mark = ","),
    "SRs that were 'closed' exactly at midnight."
  )

  sorted_closed_at_midnight <- rank_by_agency(closed_at_midnight)

  chart_title <- "SRs closed exactly at midnight by Agency & cumulative percentage"
  chart_file_name <- "closed_at_midnight_chart.pdf"
  if (!is.null(sorted_closed_at_midnight)) {
    create_combo_chart(
      closed_at_midnight,
      chart_title,
      chart_file_name
    )
  } else {
    cat("\n\nThere are no SRs with a 'closed_date' exactly at midnight.\n")
  }
}

if (noon_closed_count > 0) {
  cat(
    "\n\nThere are",
    format(noon_closed_count, big.mark = ","),
    "SRs that were 'closed' exactly at noon."
  )

  sorted_closed_at_noon <- rank_by_agency(closed_at_noon)

  chart_title <- "SRs closed exactly at noon by Agency & cumulative percentage"
  chart_file_name <- "closed_at_noon_chart.pdf"
  if (!is.null(sorted_closed_at_noon)) {
    create_combo_chart(
      closed_at_noon,
      chart_title,
      chart_file_name
    )
  } else {
    cat("\n\nThere are no SRs with a 'closed_date' exactly at noon.\n")
  }
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
    "\n\nThere are",
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

numBlankResolutionDate <-
  missingDataPerColumn[missingDataPerColumn$field == "resolution_action_updated_date", "total_empty"]

num_row_updatedLate <- nrow(updatedLate)
num_row_extreme_late <- nrow(exclude_extreme_late_update)

if (num_row_extreme_late > 0) {
  cat(
    "\nThere are", num_row_extreme_late, "extremely late (>", too_large_threshold,
    "days) resoultion updates. \nThese are removed and excluded from the analysis."
  )
  cat("\n\nHere is a sample:\n")
  print(head(exclude_extreme_late_update, 5), row.names = FALSE)
} else {
  cat("\nThere are no SRs with extremely large (>", too_large_threshold, ") post-closed updates.", sep = "")
}

if (num_row_updatedLate > 0) {
  cat(
    "\nThere are", num_row_updatedLate, "SRs with large (>", resoultion_action_threshold, "but <=", too_large_threshold,
    "days) 'resolution_action_updated_date(s)'\n"
  )
  cat("\nHere is a sample:\n")
  random_sample_large <- updatedLate %>% sample_n(min(num_row_updatedLate, 5)) # random sample
  print(random_sample_large, row.names = FALSE, right = FALSE)
}

if (num_row_updatedLate > 0) {
  cat("\nThe median of late post-closed resolution updates >", resoultion_action_threshold, "is:", round(median(updatedLate$postClosedUpdateDuration), 4), "days")
  cat("\nThe average of late post-closed resolution updates >", resoultion_action_threshold, "is:", round(mean(updatedLate$postClosedUpdateDuration), 4), "days")
  cat("\nThe std dev of late post-closed resolution updates >", resoultion_action_threshold, "is:", round(sd(updatedLate$postClosedUpdateDuration), 4), "days\n")
  cat("\n\nThe mean of all post-closed resolution updates is: ",
    round(mean(post_closed_positive$postClosedUpdateDuration), 4), " days [", round(mean(post_closed_positive$postClosedUpdateDuration) * 24, 4), " hours]",
    sep = ""
  )
  if (!is.null(updatedLate)) {
    sorted_updateLate <- rank_by_agency(updatedLate)

    chart_title <- paste("Post-Closed Resolution Updates >", resoultion_action_threshold, "days by Agency & cumulative percentage")
    chart_file_name <- "post_Closed_Bar_Chart.pdf"
    create_combo_chart(
      updatedLate,
      chart_title,
      chart_file_name
    )

    x_axis_name <- "Post_closed Resolution Update (days)"
    x_axis_field <- "postClosedUpdateDuration"
    chart_title <- paste("Post-Closed Resolution Updates >", resoultion_action_threshold, "days")
    chart_file_name <- "post_closed_violin_chart.pdf"
    
    post_closed_violin_chart <- create_violin_chart(
      updatedLate,
      x_axis_name,
      x_axis_field,
      chart_title,
      chart_file_name
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
  sorted_park_borough <- rank_by_agency(nonMatching_park_borough)
  create_combo_chart(
    nonMatching_park_borough,
    "non-matching between 'borough' and 'park_borough' by Agency & cumulative percentage",
    "non_matching_park_borough_chart"
  )
}

#########################################################################
# check to see if there are any non-matches between 'borough' and 'borough_boundaries'
# translate 'borough_boundaries' to character to check against 'borough'
d311 <- d311 %>%
  mutate(translated_borough_boundaries = case_when(
    # Order the conditions based on the frequency of values in the dataset
    # 2 (Brooklyn) is the most frequent, followed by 3 (Queens), 4 (Manhattan), and 5 (Bronx)
    # This ordering is meant to optimize performance for this large dataset (>3 million rows)
    borough_boundaries == 2 ~ "BROOKLYN",
    borough_boundaries == 3 ~ "QUEENS",
    borough_boundaries == 4 ~ "MANHATTAN",
    borough_boundaries == 5 ~ "BRONX",
    borough_boundaries == 1 ~ "STATEN ISLAND", # Staten Island is the least frequent
    TRUE ~ as.character(borough_boundaries) # Keep the original value if not 1-5
  ))

reference_field <- "borough"
duplicate_field <- "translated_borough_boundaries"
nonMatching_translatedborough_boundaries <- detect_duplicates(
  d311,
  reference_field,
  duplicate_field
)

if (!is.null(nonMatching_translatedborough_boundaries)) {
  sorted_translated_borough <- rank_by_agency(nonMatching_translatedborough_boundaries)
  chart_title <- "non-matching between 'borough' and 'borough_boundaries' by Agency & cumulative percentage"
  chart_file_name <- "non_matching_borough_boundaries_chart.pdf"
  x <- create_combo_chart(
    nonMatching_translatedborough_boundaries,
    chart_title,
    chart_file_name
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
  sorted_taxi_borough <- (nonMatching_taxi_company_borough)
  chart_title <- "non-matching between 'borough' and 'taxi_company_borough' by Agency & cumulative percentage"
  chart_file_name <- "non_matching_taxi_company_borough_chart.pdf"
  create_combo_chart(
    nonMatching_taxi_company_borough,
    chart_title,
    chart_file_name
  )
}

#########################################################################
# check to see if there are non-matches between ‘police_precincts’ and '***police_precinct***’
if ("police_precinct" %in% colnames(d311)) {
  reference_field <- "police_precincts"
  duplicate_field <- "police_precinct"
  nonMatchingPolicePrecinct <- detect_duplicates(
    d311,
    reference_field,
    duplicate_field
  )

  if (!is.null(nonMatchingPolicePrecinct)) {
    sorted_precinct <- rank_by_agency(nonMatchingPolicePrecinct)
  }
} else {
  cat("\nThe 'police_precinct' column does not are in this dataset.\n")
}

#########################################################################
# check to see if there are any non-matches between 'incident_zip' and 'zipcodes'
reference_field <- "incident_zip"
duplicate_field <- "zip_codes"

nonMatchingZipcodes <- detect_duplicates(
  d311,
  reference_field,
  duplicate_field
)

if (!is.null(nonMatchingZipcodes)) {
# Unknown line of code
  #  sorted_zips <- rank_by_agency(nonMatchingZipcodes)

  chart_title <- "non-matching between 'incident_zip' and 'zip_codes' by Agency & cumulative percentage"
  chart_file_name <- "non_matching_zip_code_chart.pdf"
  create_combo_chart(
    nonMatchingZipcodes,
    chart_title,
    chart_file_name
  )
}

#########################################################################
cat("\n\n\n***Case study analyzing Homeless Person Assistance response times.***\n")

homeless_assistance_SRs <- d311[d311$complaint_type == "HOMELESS PERSON ASSISTANCE" &
  !is.na(d311$duration), ]
duration_mean <- round(mean(homeless_assistance_SRs$duration, na.rm = TRUE), 2)
duration_sd <- round(sd(homeless_assistance_SRs$duration, na.rm = TRUE), 2)
duration_median <- round(median(homeless_assistance_SRs$duration, na.rm = TRUE), 2)

cat("\nThere are", nrow(homeless_assistance_SRs), "SRs characterized as Homeless Person Assistance\n")
cat("\nAverage response time (raw data) for 'HOMELESS PERSON ASSISTANCE':", duration_mean, "days")
# cat("\nStd deviation for (raw data) for 'HOMELESS PERSON ASSISTANCE':", duration_sd, "days")
cat("\nMedian response time (raw data) for 'HOMELESS PERSON ASSISTANCE':   ",
  duration_median, " days (", 24 * duration_median, " hrs)",
  sep = ""
)

negative_homeless_assistance_SRs <- homeless_assistance_SRs[homeless_assistance_SRs$duration < 0 &
  !is.na(homeless_assistance_SRs$duration), ]
num_row_neg_duration <- nrow(negative_homeless_assistance_SRs)

if (num_row_neg_duration > 0) {
  cat("\n\n***Removing", num_row_neg_duration, "SRs with negative & zero durations.***\n")

  homeless_assistance_SRs <- d311[d311$complaint_type == "HOMELESS PERSON ASSISTANCE" &
    !is.na(d311$duration) & d311$duration > 0, ]
  num_rows_cleaned <- nrow(homeless_assistance_SRs)

  duration_mean_clean <- round(mean(homeless_assistance_SRs$duration, na.rm = TRUE), 2)
  duration_sd_clean <- round(sd(homeless_assistance_SRs$duration, na.rm = TRUE), 2)
  duration_median_clean <- round(median(homeless_assistance_SRs$duration, na.rm = TRUE), 2)

  cat("\nAvg response time (cleaned data) for 'HOMELESS PERSON ASSISTANCE':", duration_mean_clean, "days")
  cat("\nStd deviation for (cleaned data) for 'HOMELESS PERSON ASSISTANCE':", duration_sd_clean, "days")
  cat("\nMedian response time (cleaned data)  'HOMELESS PERSON ASSISTANCE': ",
    duration_median_clean, " days (", duration_median_clean * 24, " hrs)",
    sep = ""
  )
  cat("\n\nMaximum response time:", round(max(homeless_assistance_SRs$duration, na.rm = TRUE), 0), "days")
} else {
  cat("\n\nThere are no negative duration Homeless Person Assistance SRs to remove.")
}

chart_title <- "Response time for 'Homeless Person Assistance (cleaned data)' SRs"
chart_file_name <- "homeless_response_time_clean_violin.pdf"
x_axis_name <- "Response time (days)"
x_axis_field <- "duration"

homeless_violin_chart <- create_violin_chart(
  homeless_assistance_SRs,
  x_axis_name,
  x_axis_field,
  chart_title,
  chart_file_name
)

#########################################################################
# Case study involving noise complaints by zipcode.
cat("\n\n***Case study analyzing noise complaints using the two different zip code fields***\n")
noise_complaints <- d311[grepl("^NOISE", d311$complaint_type), ]

# check top ten 'incident_zip's
incident_zip_counts <- table(na.omit(noise_complaints$incident_zip))
top_incident_zip <- data.frame(incident_zip = names(incident_zip_counts), count = as.numeric(incident_zip_counts))
top_incident_zip <- top_incident_zip[order(top_incident_zip$count, decreasing = TRUE), ]
total_complaints1 <- sum(top_incident_zip$count)

top_10_incident_zip <- head(top_incident_zip, 10)

invalid_incident_zip <- unique(incident_zip_results[[2]])
invalid_values_present1 <- top_10_incident_zip$incident_zip %in% invalid_incident_zip

top_10_incident_zip$valid <- !invalid_values_present1
cat("\nTop 10 Noise complaint zip codes using 'incident_zip' field, displaying validity.\n")
print(top_10_incident_zip, row.names = FALSE, right = FALSE)

invalid_rows1 <- top_10_incident_zip[!top_10_incident_zip$valid, ]

# Check top ten 'zip_codes'
zip_codes_count <- table(na.omit(noise_complaints$zip_codes))
top_zip_codes <- data.frame(zip_codes = names(zip_codes_count), count = as.numeric(zip_codes_count))
top_zip_codes <- top_zip_codes[order(top_zip_codes$count, decreasing = TRUE), ]
total_complaints2 <- sum(top_zip_codes$count)
# top_zip_codes$percentage <- round((top_zip_codes$count / total_complaints2) * 100, 2)
top_10_zip_codes <- head(top_zip_codes, 10)

invalid_zip_codes <- unique(zipcodes_results[[2]])
invalid_values_present2 <- top_10_zip_codes$zip_code %in% invalid_zip_codes

top_10_zip_codes$valid <- !invalid_values_present2
cat("\nTop 10 noise complaint zip codes  using 'zip_codes' field, displaying validity.\n")
print(top_10_zip_codes, row.names = FALSE, right = FALSE)

#########################################################################

##### CROSS STREET/INTERSECTION STREET ANALYSYS #####

#########################################################################
# Normalize street names
address_fields <- c("intersection_street_1", "intersection_street_2",
                    "cross_street_1", "cross_street_2",
                    "street_name", "landmark", "taxi_pick_up_location")

# **********************
# Apply normal_address function to each column in address_fields
d311[address_fields] <- lapply(d311[address_fields], normal_address, 
                                    abbs = USPSabbreviations, na = NULL, punct = "", abb_end = TRUE)
#**********************
cross_street <- "street_name"
intersection_street <- "landmark"
z1 <- cross_street_analysis(d311, cross_street, intersection_street)

cross_street <- "cross_street_1"
intersection_street <- "intersection_street_1"
y1 <- cross_street_analysis(d311, cross_street, intersection_street)

cross_street <- "cross_street_2"
intersection_street <- "intersection_street_2"
y2 <- cross_street_analysis(d311, cross_street, intersection_street)

#########################################################################

cat("\n\n**********REDUCE FILE SIZE. REMOVE DUPLICATE VALUES**********\n")

##########################################################################
cat("\nCurrent column names for d311 dataframe\n")
print(names(d311))

# # List of redundant columns to remove
redundant_columns <- c(
  "agency_name",
  "park_borough",
  "borough_boundaries",
  "location",
  "intersection_street_1",
  "intersection_street_2",
  "police_precinct",
  "duration",
  "postClosedUpdateDuration",
  "translated_borough_boundaries",
  "zip_codes"
)

cat("\nShrinking file size by deleting these", length(redundant_columns), "redundant fields:\n")

# Print the redundant columns vertically
index <- 1
for (column in redundant_columns) {
  cat("    ", index, "-", column, "\n")
  index <- index + 1
}

# Read the data into a data table object
#d311 <- fread(data1File, header = TRUE, sep = ",")

# Delete the redundant columns
d311_reduced <- d311[, !names(d311) %in% redundant_columns,]

# Calculate the size of the new data table object
reduced_size <- object.size(d311_reduced)

# Compute the difference in size
size_reduction <- original_size - reduced_size

# Print the results
cat("Original size:", format(original_size, units = "auto"), "\n")
cat("Size after removing redundant columns:", format(reduced_size, units = "auto"), "\n")
cat("Potential size reduction:", format(size_reduction, units = "auto"), "or",
    round(size_reduction/original_size * 100, 1),  "%")

#########################################################################
programStop <- as.POSIXct(Sys.time())
duration <- difftime(programStop, programStart, units = "secs")

if (duration > 3600) {
  units <- "hours"
  duration <- duration / 3600 # Convert to hours
} else if (duration > 60) {
  units <- "minutes"
  duration <- duration / 60 # Convert to minutes
} else {
  units <- "seconds"
}

program_end <- as.POSIXct(Sys.time())
formatted_end_time <- format(program_end, "%Y-%m-%d %H:%M:%S")
cat("\nExecution ends at:", formatted_end_time)
cat("\n\nProgram run-time: ", round(duration, 4), units, "\n")

#########################################################################
cat("\n *****END OF PROGRAM*****")
#########################################################################
sink()