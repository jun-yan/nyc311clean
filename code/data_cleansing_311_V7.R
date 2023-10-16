#########################################################################
install.packages("ggplot2")
install.packages("campfin")
install.packages("stringr")
install.packages("stringdist")
install.packages("dplyr")
install.packages("styler")

library(ggplot2)
library(campfin)
library(stringr)
library(stringdist)
library(dplyr)
library(scales)

setwd("C:/Users/david/OneDrive/Documents/nyc311clean/code")
data1File <- file.path("..", "data", "311_Q1_2023.csv")

# Hard code the max_closed_date to be midnight of the date of the data export from NYC Open Data
max_closed_date <- as.POSIXct("2023-10-15 23:59:59", format = "%Y-%m-%d %H:%M:%S")
writeFilePath <- file.path("..", "data", "test_sample5_smaller.csv")


#########################################################################
calculate_values <- function(max_count) {
  
  if (max_count <= 100) {
    starting_value <- 0
    increment <- 20
    scaling_factor <- 1
  } else if (max_count <= 500) {
    starting_value <- 50
    increment <- 50
    scaling_factor <- 1
  } else if (max_count <= 1000) {
    starting_value <- 100
    increment <- 200
    scaling_factor <- 1
  } else if (max_count <= 5000) {
    starting_value <- 500
    increment <- 500
    scaling_factor <- 1
  } else if (max_count <= 10000) {
    starting_value <- 1000
    increment <- 2000
    scaling_factor <- 10
  } else if (max_count <= 50000) {
    starting_value <- 5000
    increment <- 10000
    scaling_factor <- 100
  } else if (max_count <= 100000) {
    starting_value <- 10000
    increment <- 20000
    scaling_factor <- 1000
  } else if (max_count <= 500000) {
    starting_value <- 50000
    increment <- 50000
    scaling_factor <- 100000
  } else if (max_count <= 1000000) {
    starting_value <- 100000
    increment <- 200000
    scaling_factor <- 100000
  } else if (max_count <= 5000000) {
    starting_value <- 500000
    increment <- 500000
    scaling_factor <- 1000000
  } else if (max_count <= 10000000) {
    starting_value <- 1000000
    increment <- 2000000
    scaling_factor <- 1000000
  } else if (max_count <= 50000000) {
    starting_value <- 5000000
    increment <- 5000000
    scaling_factor <- 1000000
  } else if (max_count <= 100000000) {
    starting_value <- 10000000
    increment <- 20000000
    scaling_factor <- 1000000
  }
 # Return the calculated values as a named list
  result <- list(starting_value = starting_value, increment = increment, scaling_factor = scaling_factor)
  return(result)
}

#########################################################################
# Define a function to replace suffixes
replace_suffix <- function(address, abbreviations_df) {
  last_word <- str_extract(address, "\\b\\w+$")
#  if(!is.na(last_word) & last_word != "") { cat("\nlast_word", last_word)}
  if (!is.na(last_word)) {
    replacement <- abbreviations_df$abb[match(last_word, abbreviations_df$full)]
#    cat("\nreplacement=", replacement)
    if (!is.na(replacement)) {
      address <- str_replace(address, paste0("\\b", last_word, "$"), replacement)
#      cat("\naddress", address, "\n")
    }
  }
  return(address)
}
  
#########################################################################
# Function to filter rows with non-numeric or non-5-digit zip codes for a specific field
filter_invalid_zipcodes <- function(df, zip_field) {
  # Define a logical condition to filter rows based on the selected field
  condition <- !is.na(df[[zip_field]]) & df[[zip_field]] != "" &
    !grepl("^[0-9]{5}$", df[[zip_field]])

  # Use the condition to subset the DataFrame
  invalid_rows <- df[condition, ]
  return(invalid_rows)
}

#########################################################################
##  This function standardizes column names even if there are multiple "."s and trailing "."s
##  This leaves the column names with spaces replaced by an underscore "_", i.e. nicer names.
makeColNamesUserFriendly <- function(dataset) {
  ## Convert any number of consecutive "."s to an underscore.
  names(dataset) <- gsub(
    x = names(dataset),
    pattern = "(\\.)+",
    replacement = "_"
  )

  ## Drop the trailing "."s
  names(dataset) <- gsub(
    x = names(dataset),
    pattern = "(_)+$",
    replacement = ""
  )

  ## Convert to lower case.
  names(dataset) <- tolower(names(dataset))

  ## Return the revised column names
  return(dataset)
}

#########################################################################
## Validate that date fields are all dates

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
areInList <- function(dataset, listValidValues) {
  # Identify the non-blank values using nzchar()
  # Subset the vector to keep only the non-blank values
  non_blank_indices <- which(nzchar(dataset) & !is.na(dataset) & dataset != "")
  dataset <- dataset[non_blank_indices]
  dataset <- na.omit(dataset)

  # determine valid zipcodes
  inList <- (dataset %in% listValidValues[, 1])
  notInList <- dataset[!inList]


  # return non-allowable values & a Boolean variable indicating result of non-allowable test
  results <-
    list(checkIt = all(inList), non_allowable = notInList)
  return(results)
}

#########################################################################
## Function to count the number of blanks in each column of the 311 dataframe
countColumnsMissingData <- function(dataset) {
  # create a dataframe to store the column name, the number of blank rows, and the percentage of rows that are blank.
  results <- data.frame(
    columnName = character(),
    blankCount = integer(),
    fractionBlank = numeric(),
    unspecifiedCount = integer(),
    fractionUnspecified = numeric(),
    unknownCount = integer(),
    fractionUnknown = numeric()
  )
  numberOfUnspecifieds <- 0
  numberOfUnknowns <- 0
  ## Count the number of rows to step through. Used to compute % blank.
  rowCount <- nrow(dataset)


  for (col in names(dataset)) {
    # count the number of blanks and NAs in each column
    numberOfBlanks <- sum(dataset[col] == "" | is.na(dataset[col]))

    # count the number of "Unspecified" in each column
    numberOfUnspecifieds <- sum(dataset[col] == "UNSPECIFIED")

    # count the number of "UNKNOWN" in each column
    numberOfUnknowns <- sum(dataset[col] == "UNKNOWN")

    # build new dataframe with the results. Compute the %
    newRow <- data.frame(
      columnName = col,
      blankCount = numberOfBlanks,
      fractionBlank = round(numberOfBlanks / rowCount, 4),
      unspecifiedCount = numberOfUnspecifieds,
      fractionUnspecified = round(numberOfUnspecifieds / rowCount, 4),
      unknownCount = numberOfUnknowns,
      fractionUnknown = round(numberOfUnknowns / rowCount, 4)
    )

    # build the result matrix
    results <- rbind(results, newRow)
  }

  # clean up column names
  if (nrow(results) > 0) {
    names(results) <-
      c(
        "field",
        "blanks",
        "pctBlank",
        "Unspecified",
        "pctUnspecified",
        "UNKNOWN",
        "pctUnknown"
      )
  }

  return(results)
}
#########################################################################
# compute the Hamming distance between two strings. Determine if it meets the threshold.
is_close_match <- function(value1, value2, threshold) {
  if (nchar(value1) != nchar(value2)) {
    return(FALSE) # Values have different lengths, not a close match
  }
  num_diff <-
    sum(as.numeric(charToRaw(value1)) != as.numeric(charToRaw(value2)))
  return(num_diff <= threshold)
}

#########################################################################
# Function to perform word replacement (w/word pairs) on a column
# replace_misspelled <- function(target_column, word_pairs) {
replace_misspelled <- function(text, word_pairs) {
  for (i in 1:nrow(word_pairs)) {
    misspelled <- word_pairs$misspelled[i]
    correct <- word_pairs$correct[i]
    text <- gsub(misspelled, correct, text, ignore.case = TRUE)
  }
  return(text)
}

#########################################################################
# File contains column names in the "header" line.
# The R "read.csv" function uses a "." to replace the spaces in column names.
# This makes the column names into legal variables, but the "." can cause problems elsewhere.
# The function "makeColNamesUserFriendly" replaces the "." with an underscore "_".
# thus simplifying the field names. Additionally, the field names
# are converted to lower case with the "tolower" function.

#########################################################################

# Set the working directory to the "nyc311clean/code" directory to enable relative codes.
# Alter this line of code to match your particular machine.
programStart <- as.POSIXct(Sys.time())
formattedStartTime <- format(programStart, "%Y-%m-%d %H:%M:%S")
cat("\nExecution begins at:", formattedStartTime)

setwd("C:/Users/david/OneDrive/Documents/nyc311clean/code")
options(digits = 14) # Set the number of decimal places to 14

#########################################################################

cat("\n\n**********DATA INPUT AND PREPARATION**********\n")

#########################################################################
# Load the USPS zipcode file
data2File <- file.path("..", "data", "USPS_zipcodes.csv")
USPSzipcodes <-
  read.csv(data2File,
    header = TRUE,
    colClasses = rep("character", ncol(read.csv(data2File)))
  )
USPSzipcodes <- makeColNamesUserFriendly(USPSzipcodes)

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
  "SQUARE", "SQUARES", "STATION", "STATN", "STN", "STR", "STRAV",
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
  "PARKWA", "UNP","TENNIS CT"
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
USPSabbreviations <- makeColNamesUserFriendly(USPSabbreviations)
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
precinctsNYPD <- makeColNamesUserFriendly(precinctsNYPD)
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
cityCouncilNYC <- makeColNamesUserFriendly(cityCouncilNYC)
numCityCouncil <- nrow(cityCouncilNYC)

#########################################################################
# Load the NYC Streets file
data4File <- file.path("..", "data", "NYC_streets.csv")
NYC_streets <-
  read.csv(data4File,
    header = TRUE,
    colClasses = rep("character", ncol(read.csv(data4File)))
  )
NYC_streets <- makeColNamesUserFriendly(NYC_streets)
numNYC_streets <- nrow(NYC_streets)

#########################################################################
# Load the main 311 SR data file. Set the read & write paths.
#data1File <- file.path("..", "data", "311_2022.csv")

# Hard code the max_closed_date to be midnight of the date of the data export from NYC Open Data
#max_closed_date <- as.POSIXct("2023-10-15 23:59:59", format = "%Y-%m-%d %H:%M:%S")

chart_directory_path <- file.path("C:", "Users", "david", "OneDrive", "Documents", "nyc311clean", "charts")
writeFilePath <- file.path("..", "data", "test_sample5_smaller.csv")
d311 <-
  read.csv(data1File,
    header = TRUE,
    colClasses = rep("character", ncol(read.csv(data1File)))
  )
raw_d311 <- d311

# make columns names user friendly
d311 <- makeColNamesUserFriendly(d311)
# Delete rows with all missing values
d311 <- d311[rowSums(!is.na(d311)) > 0, ]

# Remove rows with value 01/01/1900 (observed in the 2022 dataset)
#d311$closed_date <-
#  ifelse(d311$closed_date == "01/01/1900 12:00:00 AM",
#    "",
#    d311$closed_date
#  )

# Remove rows with value 12/31/1899 (observed in the 2022 dataset)
#d311$closed_date <-
#  ifelse(d311$closed_date == "12/31/1899 07:00:00 PM",
#    "",
#    d311$closed_date
#  )

numRows <- nrow(d311)

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
d311[, columns_to_upper] <-
  lapply(d311[, columns_to_upper], toupper)

#########################################################################

cat("\n\n**********DATA SUMMARY**********\n")

#########################################################################
earliest_date <- min(as.POSIXct(d311$created_date, format = "%m/%d/%Y %I:%M:%S %p", tz = "America/New_York"))
earliest_date_formatted <- format(earliest_date, format = "%Y-%m-%d %H:%M:%S")

latest_date <- max(as.POSIXct(d311$created_date, format = "%m/%d/%Y %I:%M:%S %p", tz = "America/New_York"))
latest_date_formatted <- format(latest_date, format = "%Y-%m-%d %H:%M:%S")

temp_created_date <- d311$created_date
temp_created_date <- as.POSIXct(temp_created_date, format = "%m/%d/%Y %I:%M:%S %p", tz = "America/New_York", na.rm = TRUE)

date_counts <- table(format(temp_created_date, "%Y-%m"))

# Convert the table to a data frame
date_df <- as.data.frame(date_counts)
colnames(date_df) <- c("MonthYear", "Count")

# Sort the data frame in calendar order
date_df$MonthYear <- factor(date_df$MonthYear, levels = unique(date_df$MonthYear))
date_df <- date_df[order(date_df$MonthYear), ]
cat("\nSRs by month-year\n")
print(date_df, row.names = FALSE, right = FALSE)

# Calculate the average of the monthly counts
average_count <- round(mean(date_df$Count), 0)
if (nrow(date_df) >1){
  standard_deviation <- round(sd(date_df$Count), 0)
} else {
  standard_deviation <- 0
}

# Display the results
cat("\nNumber of rows in the 311 SR data set:", format(numRows, big.mark = ","))
cat("\n\nData contains SRs created from", earliest_date_formatted, "through", latest_date_formatted, "\n")
cat("\nAverage monthly count is", average_count, "with a standard deviation of", standard_deviation)

# Convert MonthYear to a Date format
date_df$MonthYear <- as.Date(paste0(date_df$MonthYear, "-01"), format = "%Y-%m-%d")

max_count <- max(date_df$Count)
result <- calculate_values(max_count)
starting_value <- result$starting_value
increment <- result$increment
scaling_factor <- result$scaling_factor
scaling_factor_str <- format(scaling_factor, scientific = FALSE, big.mark = ",")

# Create the bar chart with vertical X-axis labels
SR_monthly <- ggplot(date_df, aes(x = MonthYear, y = Count)) +
  geom_bar(stat = "identity", fill = "cadetblue") +
  labs(x = "YR-Month", y = paste("# of SRs scaled by: ", scaling_factor_str)) +
  scale_y_continuous(labels = scales::comma) +
  theme(axis.title.x = element_text(vjust = 0, size = 11),
        axis.title.y = element_text(vjust = 1, size = 11),
        plot.title = element_text(hjust = 0.5, size = 13),
        panel.background = element_rect(fill = "gray91", color = "gray91"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5, face="bold"),
        axis.text.y = element_text(face="bold")) +
  scale_x_date(labels = date_format("%Y-%m")) + # Format X-axis labels as "YYYY-MM"
  geom_text(aes(x = MonthYear, y = Count, label = scales::comma(Count/(scaling_factor))), vjust = -0.5) + 
  geom_hline(yintercept = seq(starting_value, max_count, by = increment), 
             linetype = "dotted", color = "gray21") +
  ggtitle("Monthly SR count (w/trendline)")

if (nrow(date_df) > 4) {
  SR_monthly <- SR_monthly + geom_smooth( method = "lm", span = 1, se = FALSE,
                                color = "firebrick4", linetype = "dotted")+
                          geom_text(aes(x = as.Date(min(MonthYear)), y = mean(Count), 
                                label = "Trend"),color = "firebrick", hjust= -0.5, vjust = 0)
  }


# Print the bar chart
print(SR_monthly)
#chart_directory_path <- file.path("C:", "Users", "david", "OneDrive", "Documents", "nyc311clean", "charts")
chart_path <- file.path(chart_directory_path, "MonthYear.png")

ggsave(chart_path, plot = SR_monthly, width = 8.5, height = 6)

#########################################################################
# consolidate Agencies (DCA, DOITT, NYC311-PRD)

# Replace "DCA" with "DCWP" in the agency column
d311$agency[d311$agency == "DCA"] <- "DCWP"

# Replace "DCA" with "DCWP" in the agency and agency_name columns
d311$agency[d311$agency == "DEPARTMENT OF CONSUMER AND WORKER PROTECTION"] <-
  "DCWP"
d311$agency[d311$agency_name == "Consumer Complaints Division"] <-
  "DEPARTMENT OF CONSUMER AND WORKER PROTECTION"

# Replace "DOITT" with "OTI" in the agency column
d311$agency[d311$agency == "DOITT"] <- "OTI"

# Replace "NYC311-PRD" with "OTI" in the agency column
d311$agency[d311$agency == "NYC311-PRD"] <- "OTI"

# build table of agency and count of SRs by agency
sortedAllData <- as.data.frame(table(d311$agency))
sortedAllData <- sortedAllData[order(-sortedAllData$Freq), ]
sortedAllData$percentage <-
  round(prop.table(sortedAllData$Freq) * 100, 2)

# print frequency and
cat("\n\nSRs by Agency\n")
colnames(sortedAllData) <- c("agency", "count", "percentage")
print(sortedAllData, row.names = FALSE, right = FALSE)

# Sort the data by 'count' in descending order
sortedAllData <- sortedAllData %>% arrange(desc(percentage))

sortedAllData$cumulative_percentage <- cumsum(sortedAllData$percentage)
sortedAllData$percentage <- sortedAllData$percentage/100
sortedAllData$cumulative_percentage <- sortedAllData$cumulative_percentage/100

# Find the maximum value of cumulative_percentage and count
max_cumulative_percentage <- max(sortedAllData$cumulative_percentage)
max_count <- max(sortedAllData$count)

result <- calculate_values(max_count)
starting_value <- result$starting_value
increment <- result$increment
scaling_factor <- result$scaling_factor
scaling_factor_str <- format(scaling_factor, scientific = FALSE, big.mark = ",")

# Create a combination chart
SR_by_agency <- ggplot(sortedAllData) +
  geom_bar(aes(x = reorder(agency, cumulative_percentage), y = count), stat = "identity", fill = "sienna3", width = 0.5) +
  
  geom_line(aes(x = reorder(agency, cumulative_percentage), y = cumulative_percentage*max_count, 
                group = 1), color = "black", linewidth = 1, linetype="dotted") +
  
  geom_point(aes(x = reorder(agency, cumulative_percentage), y = cumulative_percentage*max_count), 
             color = "black") +
  
  geom_text(aes(label = scales::comma(round(count/scaling_factor, 2)), x = reorder(agency, cumulative_percentage), y = count), 
          colour = "sienna3", hjust = 0.5, vjust = -0.5) +
  
  geom_text(aes(label=round(cumulative_percentage,2), x= reorder(agency, cumulative_percentage), y=max_count*cumulative_percentage), 
            colour="black", hjust=0.5, vjust=1.75) +
  
  labs(x = "Agency", y = paste("# of SRs scaled by:",scaling_factor_str )) +
  theme(    axis.title.x = element_text(vjust = 0, size = 11),
            axis.title.y.right = element_text(vjust = 1, size = 11, color = "black", face = "bold"),
            axis.title.y.left = element_text(vjust = 1, size = 11, color = "sienna3", face = "bold"),
            axis.text.y.left = element_text(color = "sienna3", face = "bold"),
            axis.text.y.right = element_text(color = "black", face = "bold"),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5, face = "bold"),
            plot.title = element_text(hjust = 0.5, size = 13)) +
  ggtitle("SR count by Agency and cumulative percentage") +
  geom_hline(yintercept = seq(starting_value, max_count, by = increment), 
             linetype = "dotted", color = "black") +
  # Add a secondary Y-axix
  scale_y_continuous(breaks = seq(starting_value, max_count, by = increment),
                     labels = scales::comma,
                     sec.axis = sec_axis(~. / max_count, name = "Cumulative Percentage"))
print(SR_by_agency)

chart_path <- file.path(chart_directory_path, "SR_by_Agency.png")
ggsave(chart_path, plot = SR_by_agency, width = 8.5, height = 6)

#########################################################################
# Calculate complaint frequency and responsible agency
complaintData <- as.data.frame(table(d311$complaint_type))
complaintData <- complaintData[order(-complaintData$Freq), ]
complaintData$percentage <-
  round(prop.table(complaintData$Freq) * 100, 2)

unique_pairs <- unique(d311[, c("complaint_type", "agency")])
unique_pairs <- unique_pairs[order(unique_pairs$complaint_type), ]

# Specify the complaint types to remove.
# These are the most common complaints that have multiple agencies involved.
# They are represented by "MULTIPLE" in the printout.
complaint_types_to_remove <-
  c(
    "ASBESTOS",
    "ELEVATOR",
    "ENCAMPMENT",
    "GRAFFITI",
    "MASS GATHERING COMPLAINT",
    "PLUMBING",
    "SEWER",
    "WATER CONSERVATION",
    "WATER SYSTEM",
    "ZTESTINT"
  )

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
# This is how the removed complaint types are captured.
complaintData$agency[is.na(complaintData$agency)] <- "MULTIPLE"
cat("\nTop 15 'complaint_type' and responsible Agency (>1% occurrence):\n")
samplecomplaintData <- complaintData[complaintData$percentage > 1, ]
colnames(samplecomplaintData) <-
  c("complaint_type", "count", "percentage", "agency")

print(head(samplecomplaintData, 15), row.names = FALSE, right = FALSE)

sortedComplaints <- complaintData
colnames(sortedComplaints) <- c("complaint_type", "count", "percentage", "agency")

# Sort the data by 'count' in descending order
sortedComplaints <- sortedComplaints %>% arrange(desc(percentage))
sortedComplaints$cumulative_percentage <- cumsum(sortedComplaints$percentage)

sortedComplaints$percentage <- sortedComplaints$percentage/100
sortedComplaints$cumulative_percentage <- sortedComplaints$cumulative_percentage/100
sortedComplaints <- sortedComplaints[sortedComplaints$percentage >= .009, ]

# Find the maximum value of count
max_count <- max(sortedComplaints$count)

# Find the maximum value of cumulative_percentage and count
max_cumulative_percentage <- max(sortedAllData$cumulative_percentage)

result <- calculate_values(max_count)
starting_value <- result$starting_value
increment <- result$increment
scaling_factor <- result$scaling_factor
scaling_factor_str <- format(scaling_factor, scientific = FALSE, big.mark = ",")

# Create a combination chart
complaintDataChart <- ggplot(sortedComplaints) +
  geom_bar(aes(x = reorder(complaint_type, cumulative_percentage), y = count), stat = "identity", fill = "sienna3", width = 0.5) +
  
  geom_line(aes(x = reorder(complaint_type, cumulative_percentage), y = cumulative_percentage*max_count, 
                group = 1), color = "black", linewidth = 1, linetype="dotted") +
  
  geom_point(aes(x = reorder(complaint_type, cumulative_percentage), y = cumulative_percentage*max_count), 
             color = "black") +
  
  geom_text(aes(label = round((count/scaling_factor),2), x = reorder(complaint_type, cumulative_percentage), 
            y = count), colour = "sienna3", hjust = 0.5, vjust = -0.5) +
  
  geom_text(aes(label=round(cumulative_percentage,2), x= reorder(complaint_type, cumulative_percentage), 
            y=max_count*cumulative_percentage), 
            colour="black", hjust=0.5, vjust=1.75) +
  
  labs(x = "Complaint Type", y = paste("# of SRs scaled by:", scaling_factor_str)) +
  theme(    axis.title.x = element_text(vjust = 0, size = 11),
            axis.title.y.right = element_text(vjust = 1, size = 11, color = "black", face = "bold"),
            axis.title.y.left = element_text(vjust = 1, size = 11, color = "sienna3", face = "bold"),
            axis.text.y.left = element_text(color = "sienna3", face = "bold"),
            axis.text.y.right = element_text(color = "black", face = "bold"),
            axis.text.x = element_text(angle = 90, vjust = -0.5, hjust = 1, face = "bold"),
            plot.title = element_text(hjust = 0.5, size = 13)) +
  ggtitle("SR count by Complaint and cumulative percentage") +
  geom_hline(yintercept = seq(starting_value, max_count, by = increment), 
             linetype = "dotted", color = "black") +
  
  # Add a secondary Y-axix
  scale_y_continuous(breaks = seq(starting_value, max_count, by = increment),
                     labels = scales::comma,
                     sec.axis = sec_axis(~. / max_count, name = "Cumulative Percentage"))

print(complaintDataChart)
chart_path <- file.path(chart_directory_path, "SR_by_Complaint_Type.png")
ggsave(chart_path, plot = complaintDataChart, width = 9, height = 6)

#########################################################################
# Determine status of SRs
# build table of status and count of SRs
sortedStatus <- as.data.frame(table(d311$status))
sortedStatus <- sortedStatus[order(-sortedStatus$Freq), ]
sortedStatus$percentage <-
  round(prop.table(sortedStatus$Freq) * 100, 2)

# print status results
cat("\nSRs by Status\n")
colnames(sortedStatus) <- c("status", "count", "percentage")
print(sortedStatus, row.names = FALSE, right = FALSE)

#########################################################################

cat("\n\n**********CORRECTING COMMON SPELLING ERRORS**********\n")

#########################################################################
# Dictionary with misspelled word pairs and their correct replacements
# replace 'city' words
word_pairs_city <- data.frame(
  misspelled = c(
    "AMITTYVILLE", "BAYOUN", "BELLMOURE", "BRIARGLISS MANOR",
    "CEDERHURST", "CLOINAL COURT", "DEERPARK", "FAMINGDALE",
    "FARMINGDE", "FT. WASHIGTON", "GERTZVILLE", "HAPPAUGE",
    "LOUIEVILLE", "MASSAPEKUA PARK", "MELLVILLE", "MINROE",
    "PATCHOGUW", "SESTERVILLE TREVOSE", "STOKANE VALLEY",
    "SYOSSE", "TENESSE", "WANTAUGH", "FLUSHING AVE",
    "NEW YORK CITY", "STATEN ISLAND NEW YORK", "DEDHAM, MA",
    "YORK TOWN HEIGHTS", "LONG IS CITY", "QUEEN", "NASSAU"
  ),
  correct = c(
    "AMITYVILLE", "BAYONNE", "BELLMORE", "BRIARCLIFF MANOR",
    "CEDARHURST", "COLONIAL COURT", "DEER PARK", "FARMINGDALE",
    "FARMINGDALE", "FORT WASHINGTON", "GETSVILLE",
    "HAUPPAUGE", "LOUISVILLE", "MASSAPEQUA PARK", "MELVILLE",
    "MONROE", "PATCHOGUE", "FEASTERVILLE-TREVOSE",
    "SPOKANE VALLEY", "SYOSSET", "TENNESSEE", "WANTAGH",
    "FLUSHING", "NEW YORK", "STATEN ISLAND", "DEDHAM",
    "YORKTOWN HEIGHTS", "LONG ISLAND CITY",
    "QUEENS", "NASSAU COUNTY"
  ),
  stringAsFactors = FALSE
)

# Apply the replacement function to the "city" column
d311$city <- replace_misspelled(d311$city, word_pairs_city)
d311$city <- gsub("\\bNA $", "", d311$city)

# replace 'incident_zip" incorrect data
word_pairs_zipcode <- data.frame(
  misspelled = c(
    "na", "N/A", "NA"
  ),
  correct = c(
    "", "", ""
  ),
  stringAsFactors = FALSE
)

# Apply replacement function to "incident_zip" and "zip_codes" columns and remove invalid values
d311$incident_zip <-
  replace_misspelled(d311$incident_zip, word_pairs_zipcode)

d311$zip_codes <-
  replace_misspelled(d311$zip_codes, word_pairs_zipcode)

# replace words in the 'descriptor' column
word_pairs_descriptor <- data.frame(
  misspelled = c(
    "VEH",
    "SGNL",
    "SCAFFORD",
    "BEEKEPER",
    "COMPLAINCE",
    "SGN",
    "ZTESTINT",
    "MESAGE",
    "CONDULET"
  ),
  correct = c(
    "VEHICLE",
    "SIGNAL",
    "SCAFFOLD",
    "BEEKEEPER",
    "COMPLIANCE",
    "SIGN",
    "",
    "MESSAGE",
    "CONDUIT"
  ),
  stringAsFactors = FALSE
)

# Apply the replacement function to the "descriptor" column to remove invalid values
d311$descriptor <-
  replace_misspelled(d311$descriptor, word_pairs_descriptor)

# replace words in the 'location_type' column
word_pairs_location_type <- data.frame(
  misspelled = c("COMERICAL", "COMERCIAL"),
  correct = c("COMMERCIAL", "COMMERCIAL"),
  stringAsFactors = FALSE
)

# Apply the replacement function to the "location_type" column to remove invalid values
d311$location_type <-
  replace_misspelled(d311$location_type, word_pairs_location_type)

#########################################################################

cat("\n\n**********BLANK, UNSPECIFIED, AND UNKNOWN ENTRIES BY COLUMN**********")
#########################################################################
# calculate the number of blank, Unspecified, and Unknown data entries.
missingDataPerColumn <- countColumnsMissingData(d311)

# Multiply the percentage columns by 100
percentage_columns <- c("pctBlank", "pctUnspecified", "pctUnknown")
missingDataPerColumn[, percentage_columns] <-
  missingDataPerColumn[, percentage_columns] * 100

# Sort the data frame by the second column in descending order
missingDataPerColumn <-
  missingDataPerColumn[order(-missingDataPerColumn[, 2]), ]

cat("\n\nNumber and % blanks (incl 'NA'), UNSPECIFIED, and UNKNOWN entries per column:\n")
print(missingDataPerColumn, row.names = FALSE, right=FALSE)

missingDataChart <- missingDataPerColumn
missingDataChart$pctBlank <- missingDataPerColumn$pctBlank/100
#missingDataChart$pctUnspecified <- missingDataPerColumn$pctUnspecified/100
#missingDataChart$pctUnknown <- missingDataPerColumn$pctUnknown/100
missingDataChart <- missingDataChart[missingDataChart$pctBlank >= .01, ]

max_count <- max(missingDataPerColumn$blanks)

result <- calculate_values(max_count)
starting_value <- result$starting_value
increment <- result$increment
scaling_factor <- result$scaling_factor
scaling_factor_str <- format(scaling_factor, scientific = FALSE, big.mark = ",")

# Create the bar chart with vertical X-axis labels
blank_chart <- ggplot(missingDataChart, aes(x = reorder(field, -blanks), y = blanks)) +
  geom_bar(stat = "identity", fill = "cadetblue") +
  labs(x = "Field", y = "# of blank entires") +
  scale_y_continuous(labels = scales::comma) +
  theme(axis.title.x = element_text(vjust = 0, size = 11),
        axis.title.y = element_text(vjust = 1, size = 11),
        plot.title = element_text(hjust = 0.5, size = 13),
        panel.background = element_rect(fill = "gray91", color = "gray91"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, face="bold"),
        axis.text.y = element_text(face="bold")) +
  geom_text(aes(x = field, y = blanks, label = scales::number(round(pctBlank,2)),
        angle = -70)) + 
  geom_hline(yintercept = seq(starting_value, max_count, by = increment), 
             linetype = "dotted", color = "gray21") +
  ggtitle("# of blank datafields (w/> 1%)")

# Print the bar chart
print(blank_chart)

#chart_directory_path <- file.path("C:", "Users", "david", "OneDrive", "Documents", "nyc311clean", "charts")
chart_path <- file.path(chart_directory_path, "BlankFields.png")
ggsave(chart_path, plot = blank_chart, width = 8.5, height = 6)

#########################################################################
cat("\n\n**********VALIDATE DATA TYPES**********\n")

#########################################################################
# determine if field values correspond to their data 'type', i.e. numeric, date, etc.
cat("\n\nAll four date fields have been converted from character to dates.\n")
cat("\nAll non-conforming date field values are represented as NA\n")

#########################################################################
# determine if the unique_key is in fact unique
uniqueKeys <- length(unique(d311$unique_key)) == nrow(d311)
cat("\nAre all 'unique_key' truely unique?", uniqueKeys)

#########################################################################
# determine if the incident_zip and zip_codes fields contain 5 numeric digits

# Call the function for "incident_zip" field
invalid_incident_zip_rows <- filter_invalid_zipcodes(d311, "incident_zip")

if (nrow(invalid_incident_zip_rows) == 0) {
  cat("\n\nAll 'incident_zip' entires are 5 numeric digits\n.")
  } else {
  cat("\n\nThere are", nrow(invalid_incident_zip_rows), "non-numeric, non-5-digit 'incident_zip' entries.\n")
    
  selected_columns <- invalid_incident_zip_rows %>%
      select(unique_key, incident_zip, agency)
  print(head(selected_columns, 10), row.names = FALSE, right = FALSE)

  # Group by agency and calculate counts and percentages
  agency_summary <- selected_columns %>%
    group_by(agency) %>%
    summarise(count = n()) %>%
    mutate(percentage = count / sum(count) * 100) %>%
    arrange(desc(percentage))
  
  # Create a new DataFrame with the desired columns
  summary_dataframe <- data.frame(
    agency = agency_summary$agency,
    count = agency_summary$count,
    percentage = round(agency_summary$percentage, 2)
  )
  
  cat("\nRanked by Agency\n")
  names(summary_dataframe)[1:3] <- c("agency", "count", "percentage")
  print(head(summary_dataframe, 10), row.names = FALSE, right = FALSE )
  }

# Call the function for "zip_codes" field
invalid_zip_codes_rows <- filter_invalid_zipcodes(d311, "zip_codes")

if (nrow(invalid_zip_codes_rows) == 0) {
  cat("\nAll 'zip_codes' entires are 5 numeric digits.")
} else {
  cat("\nThere are", nrow(invalid_zip_codes_rows), "non-numeric, non-5 digit 'incident_zip' entries.")
  
  selected_columns <- invalid_zip_codes_rows %>%
    select(unique_key, incident_zip, agency)
  print(head(selected_columns, 10), row.names = FALSE, right = FALSE)
  
  # Group by agency and calculate counts and percentages
  agency_summary <- selected_columns %>%
    group_by(agency) %>%
    summarise(count = n()) %>%
    mutate(percentage = count / sum(count) * 100) %>%
    arrange(desc(percentage))
  
  # Create a new DataFrame with the desired columns
  summary_dataframe <- data.frame(
    agency = agency_summary$agency,
    count = agency_summary$count,
    percentage = agency_summary$percentage
  )
  
  cat("\nRanked by Agency\n")
  names(summary_dataframe)[1:3] <- c("agency", "count", "percentage")
  print(head(summary_dataframe, 10), row.names = FALSE, right = FALSE )
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

#########################################################################

cat("\n\n\n**********CHECKING FOR ALLOWABLE, VALID, & LEGAL VALUES**********\n")

#########################################################################
## Change the lat/long and state_plane fields into type "numeric" to enable comparison.
d311$x_coordinate_state_plane <-
  as.numeric(d311$x_coordinate_state_plane)
d311$y_coordinate_state_plane <-
  as.numeric(d311$y_coordinate_state_plane)

# Check to see if any of the latitudes or longitudes fall outside the extreme points of New York City.
# Extreme points of the boundaries of New York City as provide by chatGPT and confirmed elsewhere.
# Note that Longitudes (west of prime meridian) are expressed as negative values
southernMostLatitude <- 40.477399
northernMostLatitude <- 40.917576
easternMostLongitude <- -73.700181
westernMostLongitude <- -74.259090

# Convert lat/long to numeric conversions for comparisons
d311$latitude <- as.numeric(d311$latitude)
d311$longitude <- as.numeric(d311$longitude)

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
  nrow(badLongitudes),
  "\n"
)

if (nrow(badLongitudes) > 0) {
  print(head(badLongitudes[, c("unique_key", "agency", "longitude")], 5), row.names = FALSE, right = FALSE)
}

#########################################################################
addresss_typeResults <-
  areInList(d311$address_type, data.frame(
    values = c(
      "ADDRESS",
      "BLOCKFACE",
      "INTERSECTION",
      "PLACENAME",
      "UNRECOGNIZED"
    )
  ))
cat(
  "\nAre all values in 'address_type' valid?",
  addresss_typeResults$checkIt
)
if (!addresss_typeResults$checkIt) {
  cat(
    "\nNumber of non-allowable value",
    length(addresss_typeResults$non_allowable),
    "\nNon-allowable values:\n",
    print(head(addresss_typeResults$non_allowable, 10), row.names = FALSE, right = FALSE)
  )
}

statusResults <-
  areInList(d311$status, data.frame(
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
  ))
cat("\n\nAre all values in 'status' valid?", statusResults$checkIt)
if (!statusResults$checkIt) {
  cat(
    "\nNumber of non-allowable value",
    length(statusResults$non_allowable),
    "\nNon-allowable values:\n",
    print(head(statusResults$non_allowable, 10), row.names = FALSE, right = FALSE)
  )
}

# check if borough, borough_boundaries, taxi_company_borough, and park_borough contain only allowable values
boroughResults <-
  areInList(d311$borough, data.frame(
    values = c(
      "BRONX",
      "BROOKLYN",
      "MANHATTAN",
      "QUEENS",
      "STATEN ISLAND",
      "UNSPECIFIED"
    )
  ))
cat(
  "\n\nAre all values in the 'borough' valid?",
  boroughResults$checkIt
)
if (!boroughResults$checkIt) {
  cat(
    "\nNumber of non-allowable value",
    length(boroughResults$non_allowable),
    "\nNon-allowable values:\n",
    print(head(boroughResults$non_allowable, 10), row.names = FALSE, right = FALSE)
  )
}

borough_boundariesResults <-
  areInList(d311$borough_boundaries, data.frame(values = c("1", "2", "3", "4", "5")))
cat(
  "\n\nAre all values in 'borough_boundaries' valid?",
  borough_boundariesResults$checkIt
)
if (!borough_boundariesResults$checkIt) {
  cat(
    "\nNumber of non-allowable value",
    length(borough_boundariesResults$non_allowable),
    "\nNon-allowable values:\n",
    print(head(borough_boundariesResults$non_allowable, 10), row.names = FALSE, right = FALSE)
  )
}

park_boroughResults <-
  areInList(d311$park_borough, data.frame(
    values = c(
      "BRONX",
      "BROOKLYN",
      "MANHATTAN",
      "QUEENS",
      "STATEN ISLAND",
      "UNSPECIFIED"
    )
  ))
cat(
  "\n\nAre all values in 'park_borough' valid?",
  park_boroughResults$checkIt
)
if (!park_boroughResults$checkIt) {
  cat(
    "\nNumber of non-allowable value",
    length(park_boroughResults$non_allowable),
    "\nNon-allowable values:\n",
    print(head(park_boroughResults$non_allowable, 10), row.names = FALSE, right = FALSE)
  )
}

taxi_company_boroughResults <-
  areInList(d311$taxi_company_borough, data.frame(
    values = c("BRONX", "BROOKLYN", "MANHATTAN", "QUEENS", "STATEN ISLAND")
  ))
cat(
  "\n\nAre all values in 'taxi_company_borough' valid?",
  taxi_company_boroughResults$checkIt
)
if (!taxi_company_boroughResults$checkIt) {
  cat(
    "\nThere are",
    length(taxi_company_boroughResults$non_allowable),
    "non-allowable values including:\n"
    )
  writeLines(as.character(taxi_company_boroughResults$non_allowable))
}

open_data_channelResults <-
  areInList(d311$open_data_channel_type, data.frame(values = c(
    "UNKNOWN",
    "MOBILE",
    "ONLINE",
    "PHONE",
    "OTHER"
  )))
cat(
  "\n\nAre all values in 'open_data_channel_type' valid?",
  open_data_channelResults$checkIt
)
if (!open_data_channelResults$checkIt) {
  cat(
    "\nNumber of non-allowable value",
    length(open_data_channelResults$non_allowable),
    "\nNon-allowable values:\n",
    print(head(open_data_channelResults$non_allowable, 10), row.names = FALSE, right = FALSE)
  )
}

vehcile_typeResults <-
  areInList(d311$vehcile_type, data.frame(
    values = c(
      "AMBULETTE / PARATRANSIT",
      "CAR SERVICE",
      "COMMUTER VAN",
      "GREEN TAXI"
    )
  ))
cat(
  "\n\nAre all values in 'vehcile_type' valid?",
  vehcile_typeResults$checkIt
)
if (!vehcile_typeResults$checkIt) {
  cat(
    "\nNumber of non-allowable value",
    length(vehcile_typeResults$non_allowable),
    "\nNon-allowable values:\n",
    print(head(vehcile_typeResults$non_allowable, 10), row.names = FALSE, right = FALSE)
  )
}

city_councilResults <-
  areInList(d311$city_council_districts, cityCouncilNYC)
cat(
  "\n\nAre all values in 'city_council_district' valid?",
  city_councilResults$checkIt
)
if (!city_councilResults$checkIt) {
  cat(
    "\nNumber of non-allowable value",
    length(city_councilResults$non_allowable),
    "\nNon-allowable values:\n",
    print(head(city_councilResults$non_allowable, 10), row.names = FALSE, right = FALSE)
  )
}

police_precinctResults <-
  areInList(d311$police_precincts, precinctsNYPD)
cat(
  "\n\nAre all values in 'police_precinct' valid?",
  police_precinctResults$checkIt
)
if (!police_precinctResults$checkIt) {
  # retrieve the number of blank fields for calculation purposes
  numBlankpolice_precinct <-
    missingDataPerColumn[missingDataPerColumn$field == "police_precincts", "blanks"]

  cat(
    "\n\nThere are",
    format(length(police_precinctResults$non_allowable), big.mark = ","),
    "invalid 'police_precinct' entries \nrepresenting",
    round((
      length(police_precinctResults$non_allowable) / (numRows - numBlankpolice_precinct)
    ) * 100, 2),
    "% of non-blank data,\n"
  )
  cat(
    "comprised of",
    length(unique(police_precinctResults$non_allowable)),
    "different 'police_precinct' entries.\n"
  )

  # Sort the table in descending order
  sorted_pp_table <-
    sort(table(police_precinctResults$non_allowable), decreasing = TRUE)

  # Convert the table to a data frame and calculate the percentage column
  pp_df <-
    data.frame(
      precinct = names(sorted_pp_table),
      count = as.numeric(sorted_pp_table)
    )
  pp_df$percentage <- round(prop.table(pp_df$count) * 100, 2)

  # Print the top 10 values
  cat("\nTop Ten invalid 'police_precinct's:\n")
  print(head(pp_df, 10), right = FALSE)
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

invalid_cb <- subset(d311, !community_board %in% cbValues & !is.na(d311$community_board) & d311$community_board != "")
invalid_cb_count <- table(invalid_cb$community_board)
total_invalid_cb_count <- sum(invalid_cb_count)

invalid_cb_df <-
  data.frame(
    community_board = names(invalid_cb_count),
    count = as.numeric(invalid_cb_count)
  )
invalid_cb_df$percentage <-
  round(prop.table(invalid_cb_df$count) * 100, 2)

agency_counts_cb <- table(invalid_cb$agency)
agency_cb_df <-
  data.frame(
    agency = names(agency_counts_cb),
    count = as.numeric(agency_counts_cb)
  )
agency_cb_df$percentage <-
  round(prop.table(agency_cb_df$count) * 100, 2)

invalid_cb_df <-
  invalid_cb_df[order(invalid_cb_df$count, decreasing = TRUE), ]
agency_cb_df <-
  agency_cb_df[order(agency_cb_df$count, decreasing = TRUE), ]

numBlank_cb <-
  missingDataPerColumn[missingDataPerColumn$field == "community_board", "blanks"]
sumInvalid_cb <- sum(as.vector(agency_counts_cb))

cat(
  "\n\nThere are",
  format(sum(as.vector(agency_counts_cb)), big.mark = ","),
  "invalid 'community_board' entries \nrepresenting",
  round(sumInvalid_cb / (nrow(d311) - numBlank_cb) * 100, 2),
  "% of non-blank data,\n"
)
cat(
  "comprised of",
  length(invalid_cb_count),
  "different 'community_board' entries.\n"
)

# Print the results
cat("\nTop Ten invalid 'community_board's:\n")
print(head(invalid_cb_df, 10), row.names = FALSE, right = FALSE)

cat("\nRanked by Agency\n")
print(head(agency_cb_df, 10), row.names = FALSE, right = FALSE)

#########################################################################
# Check for invalid zip codes in d311$incident_zip using USPSzipcodesOnly
invalid_zips1 <-
  d311[!(d311$incident_zip %in% USPSzipcodesOnly$delivery_zip) &
    !is.na(d311$incident_zip) &
    d311$incident_zip != "na" & d311$incident_zip != "", ]

# Count invalid zip codes
invalid_zip_counts1 <- table(invalid_zips1$incident_zip)

# Calculate the total count
total_invalid_count1 <- sum(invalid_zip_counts1)

# Calculate the overall percentage column
invalid_zip_df1 <-
  data.frame(
    incident_zip = names(invalid_zip_counts1),
    count = as.numeric(invalid_zip_counts1)
  )
invalid_zip_df1$percentage <-
  round(prop.table(invalid_zip_df1$count) * 100, 2)

# Count by agency
agency_counts1 <- table(invalid_zips1$agency)

# Calculate the agency percentage column
agency_df1 <-
  data.frame(
    agency = names(agency_counts1),
    count = as.numeric(agency_counts1)
  )
agency_df1$percentage <- round(prop.table(agency_counts1) * 100, 2)

# Sort the dataframes
invalid_zip_df1 <-
  invalid_zip_df1[order(invalid_zip_df1$count, decreasing = TRUE), ]
agency_df1 <-
  agency_df1[order(agency_df1$count, agency_df1$agency, decreasing = TRUE), ]

# Output the number of invalid zip codes
numBlankzip_codes1 <-
  missingDataPerColumn[missingDataPerColumn$field == "incident_zip", "blanks"]
sumInvalid1 <- sum(as.vector(agency_counts1))

cat(
  "\n\nThere are",
  format(sum(as.vector(agency_counts1)), big.mark = ","),
  "invalid 'incident_zip' entries \nrepresenting",
  round(sumInvalid1 / (nrow(d311) - numBlankzip_codes1) * 100, 2),
  "% of non-blank data,\n"
)
cat(
  "comprised of",
  length(invalid_zip_counts1),
  "different 'incident_zip' entries.\n"
)

# Print the results
cat("\nTop 10 invalid 'incident_zip's:\n")
print(head(invalid_zip_df1, 10), row.names = FALSE, right = FALSE)

cat("\nTop 10 by Agency\n")
print(head(agency_df1, 10), row.names = FALSE, right = FALSE)

#Create combo chart for bad incident_zip by Agency
agency_df1$percentage <- agency_df1$percentage/100
agency_df1$cumulative_percentage <- cumsum(agency_df1$percentage)

# Find the maximum value of cumulative_percentage and count
max_cumulative_percentage <- max(agency_df1$cumulative_percentage)
max_count <- max(agency_df1$count)
result <- calculate_values(max_count)
starting_value <- result$starting_value
increment <- result$increment
scaling_factor <- result$scaling_factor
scaling_factor_str <- format(scaling_factor, scientific = FALSE, big.mark = ",")

# Create a combination chart
incident_zipChart <- ggplot(agency_df1) +
  geom_bar(aes(x = reorder(agency, cumulative_percentage), y = count), stat = "identity", fill = "sienna3", width = 0.5) +
  
  geom_line(aes(x = reorder(agency, cumulative_percentage), y = cumulative_percentage*max_count, 
                group = 1), color = "black", linewidth = 1, linetype="dotted") +
  
  geom_point(aes(x = reorder(agency, cumulative_percentage), y = cumulative_percentage*max_count), 
             color = "black") +
  
  geom_text(aes(label = round((count/scaling_factor),2), x = reorder(agency, cumulative_percentage), 
                y = count), colour = "sienna3", hjust = 0.5, vjust = -0.5) +
  
  geom_text(aes(label=round(cumulative_percentage,2), x= reorder(agency, cumulative_percentage), 
               y=max_count*cumulative_percentage), colour="black", hjust=0.5, vjust=1.75) +
  
  labs(x = "Agency", y = paste("# of bad 'incident_zip's scaled by:", scaling_factor_str)) +
  theme(    axis.title.x = element_text(vjust = 0, size = 11),
            axis.title.y.right = element_text(vjust = 1, size = 11, color = "black", face = "bold"),
            axis.title.y.left = element_text(vjust = 1, size = 11, color = "sienna3", face = "bold"),
            axis.text.y.left = element_text(color = "sienna3", face = "bold"),
            axis.text.y.right = element_text(color = "black", face = "bold"),
            axis.text.x = element_text(angle = 90, vjust = -0.5, hjust = 1, face = "bold"),
            plot.title = element_text(hjust = 0.5, size = 13)) +
  ggtitle("Invalid 'incident_zip's by Agency & cumulative percentage") +
  geom_hline(yintercept = seq(starting_value, max_count, by = increment), 
             linetype = "dotted", color = "black") +
  # Add a secondary Y-axix
  scale_y_continuous(breaks = seq(starting_value, max_count, by = increment),
                     labels = scales::comma,
                     sec.axis = sec_axis(~. / max_count, name = "Cumulative Percentage"))

print(incident_zipChart)
chart_path <- file.path(chart_directory_path, "invalid_incident_zip_by_agency.png")
ggsave(chart_path, plot = incident_zipChart, width = 8.5, height = 6)

#########################################################################
# Check for invalid zip codes in d311$zip_codes using USPSzipcodesOnly
invalid_zips2 <-
  d311[!(d311$zip_codes %in% USPSzipcodesOnly$delivery_zip) &
    !is.na(d311$zip_codes) & d311$zip_codes != "", ]

# Count invalid zip codes
invalid_zip_counts2 <- table(invalid_zips2$zip_codes)

# Calculate the total count
total_invalid_count2 <- sum(invalid_zip_counts2)

# Calculate the overall percentage column
invalid_zip_df2 <-
  data.frame(
    zip_code = names(invalid_zip_counts2),
    count = as.numeric(invalid_zip_counts2)
  )
invalid_zip_df2$percentage <-
  round(prop.table(invalid_zip_df2$count) * 100, 2)

# Count by agency
agency_counts2 <- table(invalid_zips2$agency)

# Calculate the agency percentage column
agency_df2 <-
  data.frame(
    agency = names(agency_counts2),
    count = as.numeric(agency_counts2)
  )
agency_df2$percentage <- round(prop.table(agency_counts2) * 100, 2)

# Sort the dataframes
invalid_zip_df2 <-
  invalid_zip_df2[order(invalid_zip_df2$count, decreasing = TRUE), ]
agency_df2 <-
  agency_df2[order(agency_df2$count, agency_df2$agency, decreasing = TRUE), ]

# Output the number of invalid zip codes
numBlankzip_codes2 <-
  missingDataPerColumn[missingDataPerColumn$field == "zip_codes", "blanks"]
sumInvalid2 <- sum(as.vector(agency_counts2))

cat(
  "\n\nThere are",
  format(sum(as.vector(agency_counts2)), big.mark = ","),
  "invalid 'zip_codes' entries \nrepresenting",
  round(sumInvalid2 / (nrow(d311) - numBlankzip_codes2) * 100, 2),
  "% of non-blank data,\n"
)
cat(
  "comprised of",
  length(invalid_zip_counts2),
  "different 'zip_codes'.\n"
)

# Print the results
cat("\nTop 10 invalid 'zip_codes':\n")
print(head(invalid_zip_df2, 10), row.names = FALSE, right = FALSE)

cat("\nTop 10 by Agency\n")
print(head(agency_df2, 10), row.names = FALSE, right = FALSE)

#Create combo chart for bad incident_zip by Agency
agency_df2$percentage <- agency_df2$percentage/100
agency_df2$cumulative_percentage <- cumsum(agency_df2$percentage)

# Find the maximum value of cumulative_percentage and count
max_cumulative_percentage <- max(agency_df2$cumulative_percentage)
max_count <- max(agency_df2$count)

result <- calculate_values(max_count)
starting_value <- result$starting_value
increment <- result$increment
scaling_factor <- result$scaling_factor
scaling_factor_str <- format(scaling_factor, scientific = FALSE, big.mark = ",")

# Create a combination chart
zip_codesChart <- ggplot(agency_df2) +
  geom_bar(aes(x = reorder(agency, cumulative_percentage), y = count), stat = "identity", fill = "sienna3", width = 0.5) +
  
  geom_line(aes(x = reorder(agency, cumulative_percentage), y = cumulative_percentage*max_count, 
                group = 1), color = "black", linewidth = 1, linetype="dotted") +
  
  geom_point(aes(x = reorder(agency, cumulative_percentage), y = cumulative_percentage*max_count), 
             color = "black") +
  
  geom_text(aes(label = round((count/scaling_factor),2), x = reorder(agency, cumulative_percentage), 
                y = count), colour = "sienna3", hjust = 0.5, vjust = -0.5) +
  
  geom_text(aes(label=round(cumulative_percentage,2), x= reorder(agency, cumulative_percentage), 
                y=max_count*cumulative_percentage), colour="black", hjust=0.5, vjust=1.75) +
  
  labs(x = "Agency", y = paste("# of bad 'zip_codes' scaled by:", scaling_factor_str)) +
  theme(    axis.title.x = element_text(vjust = 0, size = 11),
            axis.title.y.right = element_text(vjust = 1, size = 11, color = "black", face = "bold"),
            axis.title.y.left = element_text(vjust = 1, size = 11, color = "sienna3", face = "bold"),
            axis.text.y.left = element_text(color = "sienna3", face = "bold"),
            axis.text.y.right = element_text(color = "black", face = "bold"),
            axis.text.x = element_text(angle = 90, vjust = -0.5, hjust = 1, face = "bold"),
            plot.title = element_text(hjust = 0.5, size = 13)) +
  ggtitle("Invalid 'zip_codes' by Agency & cumulative percentage") +
  geom_hline(yintercept = seq(starting_value, max_count, by = increment), 
             linetype = "dotted", color = "black") +
  # Add a secondary Y-axix
  scale_y_continuous(breaks = seq(starting_value, max_count, by = increment),
                     labels = scales::comma,
                     sec.axis = sec_axis(~. / max_count, name = "Cumulative Percentage"))

print(zip_codesChart)
chart_path <- file.path(chart_directory_path, "Invalid_zip_codes_by_Agency.png")
ggsave(chart_path, plot = zip_codesChart, width = 8.5, height = 6)

#########################################################################
# Duration is the time between created_date and closed_date
# Compute and store "duration" in a new additional column for the "d311" dataframe.

# Change the various date fields to date-time objects and reformat dates.There are four date fields in the 311 data.
d311$created_date <-
  strptime(d311$created_date, format = "%m/%d/%Y %I:%M:%S %p", tz = "EST")
d311$closed_date <-
  strptime(d311$closed_date, format = "%m/%d/%Y %I:%M:%S %p", tz = "EST")
d311$due_date <-
  strptime(d311$due_date, format = "%m/%d/%Y %I:%M:%S %p", tz = "EST")
d311$resolution_action_updated_date <-
  strptime(d311$resolution_action_updated_date, format = "%m/%d/%Y %I:%M:%S %p", tz = "EST")

d311$duration <-
  as.numeric(difftime(d311$closed_date, d311$created_date, units = "days"))

positiveDurations <- d311[d311$duration > 0 & !is.na(d311$duration), ]
zeroDurations < - d311[d311$duration = 0 & !is.na(d311$duration), ]
negativeDurations <- d311[d311$duration < 0 & !is.na(d311$duration), ]

negativeDurationSRs<- boxplot(negativeDurations$duration , 
            main="SR response times: Closed-Open dates",
            xlab="SRs",
            ylab="Days",
            col = "slategray3")

#########################################################################
# Identify SRs with negative duration (closed before they were created)
closedBeforeOpened <-
  d311[d311$duration < 0 &
    !is.na(d311$duration), c(
    "unique_key",
    "created_date",
    "closed_date",
    "duration",
    "agency"
  )]

numBlankClosedDate <-
  missingDataPerColumn[missingDataPerColumn$field == "closed_date", "blanks"]

if (nrow(closedBeforeOpened) > 0) {
  cat(
    "\n\nThere are",
    format(nrow(closedBeforeOpened), big.mark = ","),
    "SRs 'closed' before they were 'created' (i.e., negative duration) \nrepresenting",
    round( nrow(closedBeforeOpened) / (numRows - numBlankClosedDate) * 100, 2 ),
    "% of non-blank data.\n"
  )
  
  sortedClosed <-
    closedBeforeOpened[order(closedBeforeOpened$duration), ]
  sortedClosed$duration <- round(sortedClosed$duration, 6)

  cat("\nLargest errors (days):\n")
  print(head(sortedClosed, 5), row.names = FALSE, right = FALSE)

  cat("\nSmallest errors (days):\n")
  print(tail(sortedClosed, 5), row.names = FALSE, right = FALSE)

  # Calculate the count by agency
  count_by_agency <- table(sortedClosed$agency)

  # Create a dataframe from the count
  summary_df <-
    data.frame(
      agency = names(count_by_agency),
      count = as.numeric(count_by_agency)
    )

  # Calculate the percentage column
  summary_df$percentage <- round(prop.table(summary_df$count) * 100, 2)

  # Sort the dataframe by count in descending order
  summary_df <-
    summary_df[order(summary_df$count, decreasing = TRUE), ]

  # Reset row names
  row.names(summary_df) <- NULL
  
  cat("\nRanked by Agency\n")
  print(summary_df, row.names = FALSE, right = FALSE)
  
  closedBeforOpenedSRs<- boxplot(-closedBeforeOpened$duration , 
              main="# of days the SR was closed before created",
              xlab="SRs",
              ylab="Days",
              col = "indianred")
} else {
  cat("\n\nThere are no SRs 'closed' before they were 'created.\n")
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
  missingDataPerColumn[missingDataPerColumn$field == "closed_date", "blanks"]

if (nrow(zeroDurations) > 0) {
  cat(
    "\n\nThere are",
    format(nrow(zeroDurations), big.mark = ","),
    "SRs that were 'closed' and 'created' at the exact same time, \nto the second, representing",
    round(nrow(zeroDurations) / (numRows - numBlankClosedDate) * 100, 2),
    "% of non-blank data."
  )
  
  cat("\n\nSample of SRs 'closed' at the exact same time they are 'created':\n")
  random_sample <- zeroDurations %>% sample_n(min(nrow(zeroDurations), 5)) # random sample
  print(random_sample, row.names = FALSE, right = FALSE)

  zeroDurations$duration <- round(zeroDurations$duration, 4)
  sortedClosed <- zeroDurations[order(zeroDurations$duration), ]
  temp <- table(sortedClosed$agency)
  ordered_temp <- sort(temp, decreasing = TRUE)
  ordered_temp_df <-
    data.frame(agency = names(ordered_temp), no_of_SRs = ordered_temp)
  ordered_temp_df <- ordered_temp_df[, c(1, 3)]

  # Add the percentage column to the dataframe
  ordered_temp_df$percentage <-
    round(prop.table(ordered_temp_df$no_of_SRs.Freq) * 100, 2)
  
  cat("\nRanked by Agency\n")
  names(ordered_temp_df)[2] <- "count"
  print(ordered_temp_df, row.names = FALSE, right = FALSE)
} else {
  cat("\n\nThere are no SRs with a 'created_date' == 'closed_date'.\n")
}

#Create combo chart for bad incident_zip by Agency
ordered_temp_df$percentage <- ordered_temp_df$percentage/100
ordered_temp_df$cumulative_percentage <- cumsum(ordered_temp_df$percentage)

# Find the maximum value of cumulative_percentage and count
max_cumulative_percentage <- max(ordered_temp_df$cumulative_percentage)
max_count <- max(ordered_temp_df$count)

result <- calculate_values(max_count)
starting_value <- result$starting_value
increment <- result$increment
scaling_factor <- result$scaling_factor
scaling_factor_str <- format(scaling_factor, scientific = FALSE, big.mark = ",")

# Create a combination chart
zerodurationChart <- ggplot(ordered_temp_df) +
  geom_bar(aes(x = reorder(agency, cumulative_percentage), y = count), stat = "identity", fill = "sienna3", width = 0.5) +
  
  geom_line(aes(x = reorder(agency, cumulative_percentage), y = cumulative_percentage*max_count, 
                group = 1), color = "black", linewidth = 1, linetype="dotted") +
  
  geom_point(aes(x = reorder(agency, cumulative_percentage), y = cumulative_percentage*max_count), 
             color = "black") +
  
  geom_text(aes(label = round((count/scaling_factor),2), x = reorder(agency, cumulative_percentage), 
                y = count), colour = "sienna3", hjust = 0.5, vjust = -0.5) +
  
  geom_text(aes(label=round(cumulative_percentage,2), x= reorder(agency, cumulative_percentage), 
                y=max_count*cumulative_percentage), colour="black", hjust=0.5, vjust=1.75) +
  
  labs(x = "Agency", y = paste("zero duration SRs scaled by:", scaling_factor_str)) +
  theme(    axis.title.x = element_text(vjust = 0, size = 11),
            axis.title.y.right = element_text(vjust = 1, size = 11, color = "black", face = "bold"),
            axis.title.y.left = element_text(vjust = 1, size = 11, color = "sienna3", face = "bold"),
            axis.text.y.left = element_text(color = "sienna3", face = "bold"),
            axis.text.y.right = element_text(color = "black", face = "bold"),
            axis.text.x = element_text(angle = 90, vjust = -0.5, hjust = 1, face = "bold"),
            plot.title = element_text(hjust = 0.5, size = 13)) +
  ggtitle("zero duration SRs by Agency & cumulative percentage") +
  geom_hline(yintercept = seq(starting_value, max_count, by = increment), 
             linetype = "dotted", color = "black") +
  # Add a secondary Y-axix
  scale_y_continuous(breaks = seq(starting_value, max_count, by = increment),
                     labels = scales::comma,
                     sec.axis = sec_axis(~. / max_count, name = "Cumulative Percentage")) 

print(zerodurationChart)
chart_path <- file.path(chart_directory_path, "zero_duration_SRs_by_Agency.png")
ggsave(chart_path, plot = zerodurationChart, width = 8.5, height = 6)

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
closedinFuture$future_days <- round(as.numeric(difftime(closedinFuture$closed_date, max_closed_date, units = "days")), 4)

numBlankClosedDate <-
  missingDataPerColumn[missingDataPerColumn$field == "closed_date", "blanks"]

if (nrow(closedinFuture) > 0) {
  cat(
    "\n\nThere are",
    format(nrow(closedinFuture), big.mark = ","),
    "SRs with 'closed_date' in the future, \nrepresenting",
    round(nrow(closedinFuture) / (numRows - numBlankClosedDate) * 100, 2),
    "% of non-blank data.\n"
  )

  cat("\nSample of SRs with a 'closed_date' in the future:\n")
  closedinFuture$future_days <- round(closedinFuture$future_days, 4)
  closedinFuture$duration <- round(closedinFuture$duration, 4)
  random_sample <- closedinFuture %>% sample_n(min(nrow(closedinFuture), 5)) # random sample
  print(random_sample, row.names = FALSE, right = FALSE)

  sortedClosed <- closedinFuture[order(closedinFuture$future_days), ]
  temp <- table(sortedClosed$agency)
  ordered_temp <- sort(temp, decreasing = TRUE)
  ordered_temp_df <-
    data.frame(agency = names(ordered_temp), no_of_SRs = ordered_temp)
  ordered_temp_df <- ordered_temp_df[, c(1, 3)]

  # Add the percentage column to the dataframe
  ordered_temp_df$percentage <-
    round(prop.table(ordered_temp_df$no_of_SRs.Freq) * 100, 2)
  
  cat("\nRanked by Agency\n")
  names(ordered_temp_df)[2] <- "count"
  print(ordered_temp_df, row.names = FALSE, right = FALSE)
  
  closeinFutureSRs<- boxplot(closedinFuture$future_days , 
              main="SRs with a 'closed_date' in the future",
              xlab="SRs",
              ylab="Days",
              col = "khaki")
  
} else {
  cat("\n\nThere are no SRs with a 'closed_date' in the future.\n")
}

#########################################################################
# Identify SRs with a 'due_date' that is before the 'created_date'
dueinPast <-
  d311[ !is.na(d311$due_date), c("unique_key", "created_date", "due_date", "agency") ]

# Compute the # of days in the past the SR is due before created.
dueinPast$due_duration <- round(as.numeric(difftime(dueinPast$created_date, dueinPast$due_date, units = "days")), 4)

bad_due_date <- dueinPast[dueinPast$due_duration > 0, ]
numBlankDueDate <-
  missingDataPerColumn[missingDataPerColumn$field == "due_date", "blanks"]

if (nrow(bad_due_date) > 0) {
  cat(
    "\n\nThere are",
    format(nrow(bad_due_date), big.mark = ","),
    "SRs with a 'due_date' before 'created_date', \nrepresenting",
    round(nrow(bad_due_date) / (numRows - numBlankDueDate) * 100, 2),
    "% of non-blank data.\n"
  )

  cat("\nSample of SRs with a 'due_date' in the past:\n")
  random_sample_due <- bad_due_date %>% sample_n(min(nrow(bad_due_date), 5)) # random sample
  print(random_sample_due, row.names = FALSE, right = FALSE)

  sortedBadDue <- bad_due_date[order(bad_due_date$due_duration), ]
  temp <- table(sortedBadDue$agency)
  ordered_temp <- sort(temp, decreasing = TRUE)
  ordered_temp_df <-
    data.frame(agency = names(ordered_temp), no_of_SRs = ordered_temp)

  # Add the percentage column to the dataframe
  ordered_temp_df$percentage <-
    round(prop.table(ordered_temp_df$no_of_SRs) * 100, 2)

  cat("\nRanked by Agency\n")
  names(ordered_temp_df)[2] <- "count"
  print(ordered_temp_df, row.names = FALSE, right = FALSE)
} else {
  cat("\n\nThere are no SRs with a 'due_date' before 'created_date'.\n")
}

#########################################################################
# Identify SRs with a 'resolution_action_updated_date' that is > 45 days after 'closed_date'
# Add the "postClosedUpdateDuration" column
d311 <- d311 %>%
  mutate(
    postClosedUpdateDuration = ifelse(
      !is.na(resolution_action_updated_date) & !is.na(closed_date),
      as.numeric(difftime(resolution_action_updated_date, closed_date, units = "days")),
      NA
    )
  )

d311$postClosedUpdateDuration <- round(d311$postClosedUpdateDuration, 2)

resoultion_action_threshold = 30
updatedLate <- d311[d311$postClosedUpdateDuration  >= resoultion_action_threshold & !is.na(d311$postClosedUpdateDuration), 
                    c("unique_key", "closed_date", "resolution_action_updated_date", "postClosedUpdateDuration", "agency")]

updatedLate <- updatedLate %>% arrange(desc(postClosedUpdateDuration))

numBlankResolutionDate <-
  missingDataPerColumn[missingDataPerColumn$field == "resolution_action_updated_date", "blanks"]

if (nrow(updatedLate) > 0) {
  cat(
    "\n\nThere are",
    format(nrow(updatedLate), big.mark = ","),
    "SRs with a 'resolution_action_update_date' > 30 day after the 'closed_date', \nrepresenting",
    round(nrow(updatedLate) / (numRows - numBlankResolutionDate) * 100, 2),
    "% of non-blank data.\n"
  )
  
  cat("\nSample of SRs with a 'resolution_action_update_date' >= 30 days after 'closed_date':\n")
  random_sample_due <- updatedLate %>% sample_n(min(nrow(updatedLate), 5)) # random sample
  print(random_sample_due, row.names = FALSE, right = FALSE)
 
  temp <- table(updatedLate$agency)
  temp <- sort(temp, decreasing = TRUE)
  ordered_temp_df <-
    data.frame(agency = names(temp), no_of_SRs = temp)
  
  # Add the percentage column to the dataframe
  ordered_temp_df <- ordered_temp_df[, c(1, 3)]
  ordered_temp_df$percentage <-
    round(prop.table(ordered_temp_df$no_of_SRs.Freq) * 100, 2)
 
  
  cat("\nRanked by Agency\n")
  names(ordered_temp_df)[2] <- "count"
  print(ordered_temp_df, row.names = FALSE, right = FALSE)
} else {
  cat("\n\nThere are no SRs with a 'resolution_action_updated_date' > 30 days after 'closed_date'.\n")
}

postiveValues <- d311[ d311$postClosedUpdateDuration >0, ]

updatedSRs<- boxplot(postiveValues$postClosedUpdateDuration, 
          main="Days after 'closed' that SR was updated (> 0)",
          xlab="SRs",
          ylab="Days an update occurred after closing",
          col = "darkseagreen")

#########################################################################
cat("\n\n**********CHECKING FOR DUPLICATE VALUES**********\n")

#########################################################################
# Check if "location" is a concatenation of "latitude" and "longitude"

cat("\n***Comparing Lat/Long and Location. Elapsed time:", round(Sys.time()-programStart,2 ))
# Extract latitude and longitude using a regex pattern
matches <- regmatches(d311$location, gregexpr("-?\\d+\\.\\d+", d311$location))
lat <- as.numeric(sapply(matches, `[`, 1))
long <- as.numeric(sapply(matches, `[`, 2))

# Check if "location" is a concatenation of "latitude" and "longitude"
latitude_match <- (is.na(d311$latitude) | d311$latitude == "" | round(d311$latitude, 5) == round(lat, 5))
longitude_match <- (is.na(d311$longitude) | d311$longitude == "" | round(d311$longitude, 5) == round(long, 5))

# Get the rows where latitude or longitude does not match
mismatched_rows <- d311[!latitude_match | !longitude_match, ]
mismatched_rows <- mismatched_rows[complete.cases(mismatched_rows[, c("latitude", "longitude", "location")]), ]

# Print the results
if (nrow(mismatched_rows) > 0) {
  cat("\n\nThere are", nrow(mismatched_rows), "non-matches between 'latitude' & 'longitude' and 'location'.\n")
  print(head(mismatched_rows, 5), row.names = FALSE, right = FALSE)
} else {
  cat("\nAll values of 'latitude' & 'longitude' match the 'location' field.")
}

#########################################################################
# check the encoded borough_boundaries field for redundancy with borough
d311$translatedborough_boundaries <-
  ifelse(
    d311$borough_boundaries == "2",
    "BROOKLYN",
    ifelse(
      d311$borough_boundaries == "3",
      "QUEENS",
      ifelse(
        d311$borough_boundaries == "4",
        "MANHATTAN",
        ifelse(
          d311$borough_boundaries == "5",
          "BRONX",
          ifelse(d311$borough_boundaries == "1", "STATEN ISLAND", NA)
        )
      )
    )
  )

# making the assumption that borough == "Unspecified" and is.na(translatedborough_boundaries) that is a match, ie. Unspecified and NA are the same
Matchingborough_boundaries <-
  subset(
    d311,
    subset = borough == translatedborough_boundaries |
      (borough == "Unspecified" &
        is.na(translatedborough_boundaries)),
    select = c(
      "unique_key",
      "borough",
      "translatedborough_boundaries",
      "agency"
    )
  )

nonMatchingborough_boundaries <-
  subset(
    d311,
    subset = !d311$unique_key %in% Matchingborough_boundaries$unique_key,
    select = c(
      "unique_key",
      "borough",
      "translatedborough_boundaries",
      "agency"
    )
  )

cat(
  "\n\nThere are",
  format(nrow(nonMatchingborough_boundaries), big.mark = ","),
  "non-matches between 'borough' and 'borough_boundaries' \nrepresenting",
  round(nrow(nonMatchingborough_boundaries) / (numRows) * 100, 2),
  "% of non-blank data.\n"
)

if (nrow(nonMatchingborough_boundaries) > 0) {
  nonMatchingborough_boundaries_blank <-
    subset(
      nonMatchingborough_boundaries,
      is.na(
        nonMatchingborough_boundaries$translatedborough_boundaries
      )
    )
  cat(
    "\nSample of the",
    nrow(nonMatchingborough_boundaries_blank),
    "non-matching 'borough_boundaries' (where 'borough_boundaries' is blank):\n"
  )
  random_sample <- nonMatchingborough_boundaries_blank %>% sample_n(min(nrow(nonMatchingborough_boundaries_blank), 10)) # random sample
  print(random_sample, row.names = FALSE, right = FALSE)

  nonMatchingborough_boundaries_non_blank <-
    subset(
      nonMatchingborough_boundaries,
      !is.na(
        nonMatchingborough_boundaries$translatedborough_boundaries
      )
    )
  cat(
    "\n\nSample of the",
    nrow(nonMatchingborough_boundaries_non_blank),
    "non-matching 'borough_boundaries' (where 'borough_boundaries' is NOT blank):\n"
  )
  random_sample <- nonMatchingborough_boundaries_non_blank %>% sample_n(min(nrow(nonMatchingborough_boundaries_non_blank), 10)) # random sample
  print(random_sample, row.names = FALSE, right = FALSE)

  sortedDataborough_boundaries <-
    as.data.frame(table(nonMatchingborough_boundaries$agency))
  sortedDataborough_boundaries$Percentage <-
    round(prop.table(sortedDataborough_boundaries$Freq) * 100, 2)
  sortedDataborough_boundaries <-
    sortedDataborough_boundaries[order(-sortedDataborough_boundaries$Freq), ]

  cat("\nRanked by Agency\n")
  names(sortedDataborough_boundaries)[1:3] <- c("agency", "count", "percentage")
  print(sortedDataborough_boundaries, row.names = FALSE, right = FALSE)
}


#Create combo chart for bad incident_zip by Agency
sortedDataborough_boundaries$percentage <- sortedDataborough_boundaries$percentage/100
sortedDataborough_boundaries$cumulative_percentage <- cumsum(sortedDataborough_boundaries$percentage)

# Find the maximum value of cumulative_percentage and count
max_cumulative_percentage <- max(sortedDataborough_boundaries$cumulative_percentage)
max_count <- max(sortedDataborough_boundaries$count)

result <- calculate_values(max_count)
starting_value <- result$starting_value
increment <- result$increment
scaling_factor <- result$scaling_factor
scaling_factor_str <- format(scaling_factor, scientific = FALSE, big.mark = ",")

# Create a combination chart
nonmatchingboroughChart <- ggplot(sortedDataborough_boundaries) +
  geom_bar(aes(x = reorder(agency, cumulative_percentage), y = count), stat = "identity", fill = "sienna3", width = 0.5) +
  
  geom_line(aes(x = reorder(agency, cumulative_percentage), y = cumulative_percentage*max_count, 
                group = 1), color = "black", linewidth = 1, linetype="dotted") +
  
  geom_point(aes(x = reorder(agency, cumulative_percentage), y = cumulative_percentage*max_count), 
             color = "black") +
  
  geom_text(aes(label = round((count/scaling_factor),2), x = reorder(agency, cumulative_percentage), 
                y = count), colour = "sienna3", hjust = 0.5, vjust = -0.5) +
  
  geom_text(aes(label=round(cumulative_percentage,2), x= reorder(agency, cumulative_percentage), 
                y=max_count*cumulative_percentage), colour="black", hjust=0.5, vjust=1.75) +
  
  labs(x = "Agency", y = paste("zero duration SRs scaled by:", scaling_factor_str)) +
  theme(    axis.title.x = element_text(vjust = 0, size = 11),
            axis.title.y.right = element_text(vjust = 1, size = 11, color = "black", face = "bold"),
            axis.title.y.left = element_text(vjust = 1, size = 11, color = "sienna3", face = "bold"),
            axis.text.y.left = element_text(color = "sienna3", face = "bold"),
            axis.text.y.right = element_text(color = "black", face = "bold"),
            axis.text.x = element_text(angle = 90, vjust = -0.5, hjust = 1, face = "bold"),
            plot.title = element_text(hjust = 0.5, size = 13)) +
  ggtitle("zero duration SRs by Agency & cumulative percentage") +
  geom_hline(yintercept = seq(starting_value, max_count, by = increment), 
             linetype = "dotted", color = "black") +
  # Add a secondary Y-axix
  scale_y_continuous(breaks = seq(starting_value, max_count, by = increment),
                     labels = scales::comma,
                     sec.axis = sec_axis(~. / max_count, name = "Cumulative Percentage"))

print(nonmatchingboroughChart)
chart_path <- file.path(chart_directory_path, "non_matching_borough_boundaries_by_Agency.png")
ggsave(chart_path, plot = nonmatchingboroughChart, width = 8.5, height = 6)

#########################################################################
# check to see if there are any non-matches between 'borough' and 'park_borough'
nonMatchingpark_borough <- subset(
  d311,
  borough != park_borough,
  select = c("unique_key", "borough", "park_borough", "agency")
)

# retrieve # of blanks for calculations
numBlankpark_borough <-
  missingDataPerColumn[missingDataPerColumn$field == "park_borough", "blanks"]

if (nrow(nonMatchingpark_borough) > 0) {
  cat(
    "\n\nThere are",
    format(nrow(nonMatchingpark_borough), big.mark = ","),
    "Non-matches between the 'borough' and 'park_borough' fields number \nrepresenting",
    round(
      nrow(nonMatchingpark_borough) / (numRows - numBlankpark_borough) * 100,
      2
    ),
    "% of non-blank data.\n"
  )
} else {
  cat("\nAll entries match for columns 'borough' and 'park_borough'.")
}

if (nrow(nonMatchingpark_borough) > 0) {
  cat("\n\nSample of non-matching park_boroughs\n")
  print(head(nonMatchingpark_borough, 5), row.names = FALSE, right = FALSE)

  cat("\nRanked by Agency\n")
  sortedDatapark_borough <-
    as.data.frame(table(nonMatchingpark_borough$agency))
  sortedDatapark_borough$Percentage <-
    round(prop.table(sortedDatapark_borough$Freq) * 100, 2) # Calculate the percentage column

  sortedDatapark_borough <-
    sortedDatapark_borough[order(-sortedDatapark_borough$Freq), ]
  sortedDatapark_borough <-
    sortedDatapark_borough[order(-sortedDatapark_borough$Freq), ]
  
  names(sortedDatapark_borough)[1] <- "count"
  print(sortedDatapark_borough, row.names = FALSE, right = FALSE)
}

#########################################################################
# check to see if there are any mis-matches between 'borough' and 'taxi_company_borough'
nonMatchingtaxi_company_borough <-
  subset(
    d311,
    borough != taxi_company_borough & taxi_company_borough != "",
    select = c("unique_key", "borough", "taxi_company_borough", "agency")
  )

# retrieve # of blanks for calculations
numBlanktaxi_company_borough <-
  missingDataPerColumn[missingDataPerColumn$field == "taxi_company_borough", "blanks"]

cat(
  "\n\nThere are",
  format(nrow(nonMatchingtaxi_company_borough), big.mark = ","),
  "non-matches between 'borough' and 'taxi_company_borough' (excluding blanks) \nrepresenting",
  round(nrow(nonMatchingtaxi_company_borough) / (numRows - numBlanktaxi_company_borough) * 100, 2),
  "% of non-blank data\n"
)

if (nrow(nonMatchingtaxi_company_borough) > 0) {
  cat("\nSample of non-matching 'taxi_company_borough' (exluding blanks):\n")
  random_sample <- nonMatchingtaxi_company_borough %>% sample_n(min(nrow(nonMatchingtaxi_company_borough), 5)) # random sample
  print(random_sample, row.names = FALSE, right = FALSE)

  #  print(head(nonMatchingtaxi_company_borough, 5))
  sortedDatataxi_company_borough <-
    as.data.frame(table(nonMatchingtaxi_company_borough$agency))
  sortedDatataxi_company_borough$Percentage <-
    round(prop.table(sortedDatataxi_company_borough$Freq) * 100, 2) # Calculate the percentage column

  sortedDatataxi_company_borough <-
    sortedDatataxi_company_borough[order(-sortedDatataxi_company_borough$Freq), ]
  sortedDatataxi_company_borough <-
    sortedDatataxi_company_borough[order(-sortedDatataxi_company_borough$Freq), ]
  cat("\nRanked by Agency\n")

  names(sortedDatataxi_company_borough)[1:3] <- c("agency", "count", "percentage")
  print(head(sortedDatataxi_company_borough, 5), row.names = FALSE, right = FALSE)
}

#########################################################################
# check to see if cross_street1 and intersection_street_1 are the same value

####################
# conduct replacements on a copy of original data
xcloned311 <- d311

# startup <- Sys.time()
####################
# Replace "EAST" or "WEST" with "E" or "W" Same for "NORTH" and "SOUTH"
# Columns to replace
address_fields <- c(
  "incident_address",
  "street_name",
  "cross_street_1",
  "intersection_street_1",
  "cross_street_2",
  "intersection_street_2"
)
# Pattern to replace
pattern <- "\\b(EAST|WEST|NORTH|SOUTH)\\b"
# Replacement string
replacement <- "\\U\\1"

# Use lapply and gsub for replacements across all columns
xcloned311[address_fields] <- lapply(xcloned311[address_fields], function(col) gsub(pattern, replacement, col, perl = TRUE))

####################
# Clean up street types and replace with standard USPS abbreviations
# Loop over columns and apply normalization function
# startup <- Sys.time()

cat("\n***Normalizing street names. Elapsed time:", round(Sys.time()-programStart,2 ))
for (col in address_fields) {
  xcloned311[[col]] <- normal_address(
    xcloned311[[col]],
    abbs = USPSabbreviations,
    na = NULL,
    punct = "",
    abb_end = TRUE
  )
}

# Apply the function to each column in the `xcloned311` dataframe
xcloned311$cross_street_1 <- sapply(xcloned311$cross_street_1, replace_suffix, USPSabbreviations)
xcloned311$cross_street_2 <- sapply(xcloned311$cross_street_2, replace_suffix, USPSabbreviations)

xcloned311$intersection_street_1 <- sapply(xcloned311$intersection_street_1, replace_suffix, USPSabbreviations)
xcloned311$intersection_street_2 <- sapply(xcloned311$intersection_street_2, replace_suffix, USPSabbreviations)

# print(Sys.time()-startup)

####################
# startup <- Sys.time()

cat("\n***Normalizing address fields. Elapsed time:", round(Sys.time()-programStart,2 ))

## Identify affected rows with logical vectors
#affected_rows1 <- rowSums(sapply(xcloned311[address_fields], function(col) grepl("UNNAMED|NONAME", col))) > 0

# Replace "UNNAMED" or "NONAME" with empty strings in affected rows
#xcloned311[affected_rows1, address_fields] <- lapply(xcloned311[affected_rows1, address_fields], function(col) gsub("UNNAMED|NONAME", "", col))

# Replace consecutive question marks with spaces in affected rows
#xcloned311[affected_rows1, address_fields] <- lapply(xcloned311[affected_rows1, address_fields], function(col) gsub("\\?+", " ", col))

# Identify the modified dataframe
#affected_rows_after1 <- xcloned311[affected_rows1, ]

####################
# fix street addresses that end in "E" or "W", and replace that to the front of the street address
# Identify rows with affected fields using regular expressions
# startup <- Sys.time()
exceptions <-
  c(
    "AVE W",
    "AVENUE W",
    "AVENUE N",
    "AVENUE S",
    "CENTRAL PARK W",
    "CENTRAL PARK DR W",
    "CPW",
    "PROSPECT PARK W",
    "SHEA STADIUM GATE E"
  )

pattern <- paste(exceptions, collapse = "|")
affected_rows1 <-
  apply(xcloned311[address_fields], 1, function(row) {
    any(grepl(" E$", row) & !grepl(pattern, row))
  })
affected_rows2 <-
  apply(xcloned311[address_fields], 1, function(row) {
    any(grepl(" W$", row) & !grepl(pattern, row))
  })
affected_rows3 <-
  apply(xcloned311[address_fields], 1, function(row) {
    any(grepl(" N$", row) & !grepl(pattern, row))
  })
affected_rows4 <-
  apply(xcloned311[address_fields], 1, function(row) {
    any(grepl(" S$", row) & !grepl(pattern, row))
  })

# Identify the affected rows before replacement
affected_rows_before2E <- xcloned311[affected_rows1, ]
affected_rows_before2W <- xcloned311[affected_rows2, ]
affected_rows_before2N <- xcloned311[affected_rows3, ]
affected_rows_before2S <- xcloned311[affected_rows4, ]

# Define a function to replace trailing values in a single column
replace_trailing_value <- function(col, pattern, replacement) {
  col <- ifelse(grepl(pattern, col), paste0(replacement, gsub(pattern, "", col)), col)
  return(col)
}

# Loop through address fields and apply the function
for (field in address_fields) {
  xcloned311[affected_rows1, field] <- replace_trailing_value(xcloned311[affected_rows1, field], " E$", "E ")
  xcloned311[affected_rows2, field] <- replace_trailing_value(xcloned311[affected_rows2, field], " W$", "W ")
  xcloned311[affected_rows3, field] <- replace_trailing_value(xcloned311[affected_rows3, field], " N$", "N ")
  xcloned311[affected_rows4, field] <- replace_trailing_value(xcloned311[affected_rows4, field], " S$", "S ")
}

####################
# Replace "EAST" with "E" for the selected columns and rows. Similarly for WEST, NORTH, and SOUTH
# startup <- Sys.time()

patterns <- c("^EAST ", "^WEST ", "^NORTH ", "^SOUTH ")
replacements <- c("E ", "W ", "N ", "S ")

cat("\n***Additional corrections for directions in street names. Elapsed time:", round(Sys.time()-programStart,2 ))
logical_vectors <- lapply(patterns, function(pattern) {
  lapply(d311[address_fields], grepl, pattern = pattern)
})

# Loop over patterns and replacements
for (i in seq_along(patterns)) {
  pattern <- patterns[i]
  replacement <- replacements[i]

  # Apply the replacement to each column separately
  for (col in address_fields) {
    xcloned311[logical_vectors[[i]][[col]], col] <-
      gsub(pattern, replacement, xcloned311[logical_vectors[[i]][[col]], col])
  }
}
# print(Sys.time() - startup)

####################
# Precompute logical vectors for each pattern
ave_logical <- lapply(d311[address_fields], grepl, pattern = "^AVE ")
e_end_logical <- lapply(d311[address_fields], grepl, pattern = "^E END AVE$")
w_end_logical <- lapply(d311[address_fields], grepl, pattern = "^W END AVE$")

# Loop over columns and apply replacements using precomputed logical vectors
for (j in seq_along(address_fields)) {
  xcloned311[ave_logical[[j]], address_fields[j]] <-
    gsub("^AVE ", "AVENUE ", xcloned311[ave_logical[[j]], address_fields[j]])

  xcloned311[e_end_logical[[j]], address_fields[j]] <-
    gsub("^E END AVE$", "EAST END AVE", xcloned311[e_end_logical[[j]], address_fields[j]])

  xcloned311[w_end_logical[[j]], address_fields[j]] <-
    gsub("^W END AVE$", "WEST END AVE", xcloned311[w_end_logical[[j]], address_fields[j]])

  xcloned311[, address_fields[j]] <-
    gsub("^(\\d+) (\\w+) (\\w+)$", "\\3 \\1 \\2", xcloned311[, address_fields[j]])
}
# print(Sys.time() - startup)

####################
# find rows where cross_street_1 is blank, but intersection_street_1 is not blank. Use later.
crossStreet1Blank1 <-
  subset(
    xcloned311,
    xcloned311$cross_street_1 == "" &
      xcloned311$intersection_street_1 != "",
    select = c("cross_street_1", "intersection_street_1", "agency")
  )

# find rows where intersection_street is blank, but cross_street is not blank
intersectionStreet1Blank1 <-
  subset(
    xcloned311,
    xcloned311$cross_street_1 != "" &
      xcloned311$intersection_street_1 == "",
    select = c("cross_street_1", "intersection_street_1", "agency")
  )

# find rows where both cross_street_1 and intersection_street_1 are blank
bothBlank1 <-
  subset(
    xcloned311,
    xcloned311$cross_street_1 == "" &
      xcloned311$intersection_street_1 == "",
    select = c("cross_street_1", "intersection_street_1", "agency")
  )

####################
# Check for matching fields
dupXStreets1 <- subset(
  xcloned311,
  cross_street_1 == intersection_street_1,
  select = c("cross_street_1", "intersection_street_1", "agency")
)

####################
cat(
  "\n\nThere are",
  format(nrow(dupXStreets1), big.mark = ","),
  "matching 'cross_street_1' & 'intersection_street_1' \nrepresenting",
  round(nrow(dupXStreets1) / nrow(d311) * 100, 2),
  "% of non-blank rows."
)
cat("\n\nSample of matching 'cross_street_1' & 'intersection_street_1':\n")
non_blank_rows_match <-
  dupXStreets1[dupXStreets1$cross_street_1 != "" &
    dupXStreets1$intersection_street_1 != "", ]

random_sample <- non_blank_rows_match %>% sample_n(min(nrow(non_blank_rows_match), 10)) # random sample
print(random_sample, row.names = FALSE, right = FALSE)

agency_match_counts1 <- table(dupXStreets1$agency)
agency_match1 <-
  data.frame(
    agency = names(agency_match_counts1),
    count = as.numeric(agency_match_counts1)
  )
agency_match1$percentage <-
  round(prop.table(agency_match1$count) * 100, 2)
agency_match1 <-
  agency_match1[order(agency_match1$count, decreasing = TRUE), ]
cat("\nRanked by Agency\n")

names(agency_match1)[1:3] <- c("agency", "count", "percentage")
print(agency_match1, row.names = FALSE, right = FALSE)

####################
# non-matching cross_street1 and intersection_1
nondupXStreets1 <- subset(
  xcloned311,
  cross_street_1 != intersection_street_1,
  select = c("cross_street_1", "intersection_street_1", "agency")
)
cat(
  "\n\nThere are",
  format(nrow(nondupXStreets1), big.mark = ","),
  "non-matching 'cross_street_1' & 'intersection_1' \nrepresenting",
  round(nrow(nondupXStreets1) / nrow(d311) * 100, 2),
  "% of total rows."
)
cat("\n\nSample of non-matching 'cross_street_1' and 'intersection_street_1':\n")
non_blank_rows_non_match1 <-
  nondupXStreets1[nondupXStreets1$cross_street_1 != "" &
    nondupXStreets1$intersection_street_1 != "", ]

random_sample <- non_blank_rows_non_match1 %>% sample_n(min(nrow(non_blank_rows_non_match1), 10)) # random sample
print(random_sample, row.names = FALSE, right = FALSE)

cat("\nAgencies with non-matches\n")
agency_nonmatch_counts1 <- table(nondupXStreets1$agency)
agency_nonmatch1 <-
  data.frame(
    agency = names(agency_nonmatch_counts1),
    count = as.numeric(agency_nonmatch_counts1)
  )
agency_nonmatch1$percentage <-
  round(prop.table(agency_nonmatch1$count) * 100, 2)
agency_nonmatch1 <-
  agency_nonmatch1[order(agency_nonmatch1$count, decreasing = TRUE), ]

names(agency_nonmatch1)[1:3] <- c("agency", "count", "percentage")
print(agency_nonmatch1, row.names = FALSE, right = FALSE)

####################
# cross_street_1 is blank, but intersection_street_1 is not blank
cat(
  "\n\nThere are",
  format(nrow(crossStreet1Blank1), big.mark = ","),
  "occurrences where 'cross_street_1' is blank, \nbut 'intersection_street_1' is not blank representing",
  round(nrow(crossStreet1Blank1) / nrow(d311) * 100, 2),
  "% of total rows."
)
cat(
  "\n\nSample where 'cross_street_1' is blank but 'intersection_street_1' is not blank:\n"
)
random_sample <- crossStreet1Blank1 %>% sample_n(min(nrow(crossStreet1Blank1), 10)) # random sample
print(random_sample, row.names = FALSE, right = FALSE)

# Count by agency
cat("\nRanked by Agency\n")
agency_countsXStreet1 <- table(crossStreet1Blank1$agency)

# Calculate the agency percentage column
agency_XStreetBlank1 <-
  data.frame(
    agency = names(agency_countsXStreet1),
    count = as.numeric(agency_countsXStreet1)
  )
agency_XStreetBlank1$percentage <-
  round(prop.table(agency_XStreetBlank1$count) * 100, 2)
agency_XStreetBlank1 <-
  agency_XStreetBlank1[order(agency_XStreetBlank1$count, decreasing = TRUE), ]

names(agency_XStreetBlank1)[1:3] <- c("agency", "count", "percentage")
print(agency_XStreetBlank1, row.names = FALSE, right = FALSE)

####################
# Intersection_street_1 is blank, but cross_street_1 is not blank
cat(
  "\n\nThere are",
  format(nrow(intersectionStreet1Blank1), big.mark = ","),
  "occurrences where 'intersection_street_1' is blank, \nbut 'cross_street_1' is not blank representing",
  round(nrow(intersectionStreet1Blank1) / nrow(d311) * 100, 2),
  "% of total rows."
)
cat(
  "\n\nSample where 'cross_street_1' is not blank but 'intersection_street_1' is blank:\n"
)
random_sample <- intersectionStreet1Blank1 %>% sample_n(min(nrow(intersectionStreet1Blank1), 10)) # random sample
print(random_sample, row.names = FALSE, right = FALSE)

# Count by agency
cat("\nRanked by Agency\n")
agency_inter1_counts1 <- table(intersectionStreet1Blank1$agency)

# Calculate the agency percentage column
agency_inter_StreetBlank1 <-
  data.frame(
    agency = names(agency_inter1_counts1),
    count = as.numeric(agency_inter1_counts1)
  )
agency_inter_StreetBlank1$percentage <-
  round(prop.table(agency_inter_StreetBlank1$count) * 100, 2)
agency_inter_StreetBlank1 <-
  agency_inter_StreetBlank1[order(agency_inter_StreetBlank1$count, decreasing = TRUE), ]

names(agency_inter_StreetBlank1)[1:3] <- c("agency", "count", "percentage")
print(agency_inter_StreetBlank1, row.names = FALSE, right = FALSE)

####################
# summary of cross_street_1 and intersection_street_1 columns
summaryXstreet1 <-
  data.frame(
    category = c(
      "matching",
      "non-matching",
      "X-street1Blank",
      "inter-street1Blank"
    ),
    count = c(
      nrow(dupXStreets1),
      nrow(nondupXStreets1),
      nrow(crossStreet1Blank1),
      nrow(intersectionStreet1Blank1)
    )
  )

summaryXstreet1$percentage[1] <-
  round(nrow(dupXStreets1) / numRows * 100, 2)
summaryXstreet1$percentage[2] <-
  round(nrow(nondupXStreets1) / numRows * 100, 2)
summaryXstreet1$percentage[3] <- "N/A"
summaryXstreet1$percentage[4] <- "N/A"
summaryXstreet1$count <-
  format(summaryXstreet1$count, big.mark = ",")

cat("\nSummary of 'cross_street_1' and 'intersection_street_1':\n")
names(summaryXstreet1)[1:3] <- c("category", "count", "percentage")
print(summaryXstreet1, row.names = FALSE, right = FALSE)

#########################################################################
# check to see if cross_street2 and intersection_2 are the same value

####################
# find rows where cross_street_2 is blank, but intersection_street_2 is not blank. Use later.
crossStreet2Blank2 <-
  subset(
    xcloned311,
    d311$cross_street_2 == "" &
      d311$intersection_street_2 != "",
    select = c("cross_street_2", "intersection_street_2", "agency")
  )

# find rows where intersection_street_2 is blank, but cross_street_2 is not blank
intersectionStreet2Blank2 <-
  subset(
    xcloned311,
    d311$cross_street_2 != "" &
      d311$intersection_street_2 == "",
    select = c("cross_street_2", "intersection_street_2", "agency")
  )

# find rows where both cross_street_2 and intersection_street_2 are blank
bothBlank2 <-
  subset(
    xcloned311,
    xcloned311$cross_street_2 == "" &
      xcloned311$intersection_street_2 == "",
    select = c("cross_street_2", "intersection_street_2", "agency")
  )

####################
# Matching cross_street_2 and intersection_street_2
# remove extra spaces and blanks to perform checks
dupXStreets2 <- subset(
  xcloned311,
  cross_street_2 == intersection_street_2,
  select = c("cross_street_2", "intersection_street_2", "agency")
)

cat(
  "\n\nThere are",
  format(nrow(dupXStreets2), big.mark = ","),
  "matching 'cross_street_2' & 'intersection_2' \nrepresenting",
  round(nrow(dupXStreets2) / nrow(d311) * 100, 2),
  "% of total rows."
)

non_blank_dupXStreets2 <- dupXStreets2[dupXStreets2$cross_street_2 != "" &
  dupXStreets2$intersection_street_2 != "", ]
random_sample <- non_blank_dupXStreets2 %>% sample_n(min(nrow(non_blank_dupXStreets2), 10)) # random sample
cat("\n\nSample of matching 'cross_street_2' & 'intersection_street_2':\n")
print(random_sample, row.names = FALSE, right = FALSE)

cat("\nRanked by Agency (matching)\n")
agency_match_counts2 <- table(dupXStreets2$agency)
agency_match2 <-
  data.frame(
    agency = names(agency_match_counts2),
    count = as.numeric(agency_match_counts2)
  )
agency_match2$percentage <-
  round(prop.table(agency_match2$count) * 100, 2)
agency_match2 <-
  agency_match2[order(agency_match2$count, decreasing = TRUE), ]
names(agency_match2)[1:3] <- c("agency", "count", "percentage")
print(agency_match2, row.names = FALSE, right = FALSE)

####################
# non-matching cross_street1 and intersection_1
nondupXStreets2 <- subset(
  xcloned311,
  cross_street_2 != intersection_street_2,
  #                           gsub("\\s+", " ", cross_street_2) != gsub("\\s+", " ", intersection_street_2),
  select = c("cross_street_2", "intersection_street_2", "agency")
)

cat(
  "\n\nThere are",
  format(nrow(nondupXStreets2), big.mark = ","),
  "non-matching 'cross_street_2' & 'intersection_street_2' \nrepresenting",
  round(nrow(nondupXStreets2) / nrow(d311) * 100, 2),
  "% of total rows."
)
cat("\n\nSample of non-matching 'cross_street_2' and 'intersection_street_2':\n")
non_blank_rows_non_match2 <-
  nondupXStreets2[nondupXStreets2$cross_street_2 != "" &
    nondupXStreets2$intersection_street_2 != "", ]

print_sample <- nondupXStreets2[nondupXStreets2$cross_street_2 != "" &
  nondupXStreets2$intersection_street_2 != "", ]

random_sample <- print_sample %>% sample_n(min(nrow(print_sample), 10)) # random sample
print(random_sample, row.names = FALSE, right = FALSE)

cat("\nRanked by Agency (non-matches)\n")
agency_nonmatch_counts2 <- table(nondupXStreets2$agency)
agency_nonmatch2 <-
  data.frame(
    agency = names(agency_nonmatch_counts2),
    count = as.numeric(agency_nonmatch_counts2)
  )
agency_nonmatch2$percentage <-
  round(prop.table(agency_nonmatch2$count) * 100, 2)
agency_nonmatch2 <-
  agency_nonmatch2[order(agency_nonmatch2$count, decreasing = TRUE), ]
names(agency_nonmatch2)[1:3] <- c("agency", "count", "percentage")
print(agency_nonmatch2, row.names = FALSE, right = FALSE)

####################
# cross_street_2 is blank, but intersection_street_2 is not blank
cat(
  "\n\nThere are",
  format(nrow(crossStreet2Blank2), big.mark = ","),
  "occurrences where 'cross_street_2' is blank, \nbut 'intersection_street_2' is not blank representing",
  round(nrow(crossStreet2Blank2) / nrow(d311) * 100, 2),
  "% of total rows"
)
cat(
  "\n\nSample where 'cross_street_2' is blank but 'intersection_street_2' is not blank:\n"
)
random_sample <- crossStreet2Blank2 %>% sample_n(min(nrow(crossStreet2Blank2), 10)) # random sample
print(random_sample, row.names = FALSE, right = FALSE)

# Count by agency
cat("\nRanked by Agency\n")
agency_countsXStreet2 <- table(crossStreet2Blank2$agency)

# Calculate the agency percentage column
agency_XStreetBlank2 <-
  data.frame(
    agency = names(agency_countsXStreet2),
    count = as.numeric(agency_countsXStreet2)
  )
agency_XStreetBlank2$percentage <-
  round(prop.table(agency_XStreetBlank2$count) * 100, 2)
agency_XStreetBlank2 <-
  agency_XStreetBlank2[order(agency_XStreetBlank2$count, decreasing = TRUE), ]
names(agency_XStreetBlank2)[1:3] <- c("agency", "count", "percentage")
print(agency_XStreetBlank2, row.names = FALSE, right = FALSE)

####################
# Intersection_1 is blank, but cross_street_1 is not blank
cat(
  "\n\nThere are",
  format(nrow(intersectionStreet2Blank2), big.mark = ","),
  "occurrences where 'cross_street_2' is not blank, \nbut 'intersection_street_2' is blank representing",
  round(nrow(intersectionStreet2Blank2) / nrow(d311) * 100, 2),
  "% of total rows"
)
cat(
  "\n\nSample where 'cross_street_2' is not blank but 'intersection_street_2' is blank:\n"
)
random_sample <- intersectionStreet2Blank2 %>% sample_n(min(nrow(intersectionStreet2Blank2), 10)) # random sample
print(random_sample, row.names = FALSE, right = FALSE)

# Count by agency
cat("\nRanked by Agency\n")
agency_inter2_counts2 <- table(intersectionStreet2Blank2$agency)

# Calculate the agency percentage column
agency_inter_StreetBlank2 <-
  data.frame(
    agency = names(agency_inter2_counts2),
    count = as.numeric(agency_inter2_counts2)
  )
agency_inter_StreetBlank2$percentage <-
  round(prop.table(agency_inter_StreetBlank2$count) * 100, 2)
agency_inter_StreetBlank2 <-
  agency_inter_StreetBlank2[order(agency_inter_StreetBlank2$count, decreasing = TRUE), ]
names(agency_inter_StreetBlank2)[1:3] <- c("agency", "count", "percentage")
print(agency_inter_StreetBlank2, row.names = FALSE, right = FALSE)

####################
# summary of cross_street_2 and intersection_street_2 columns
summaryXstreet2 <-
  data.frame(
    category = c(
      "matching",
      "non-matching",
      "X-streetBlank",
      "inter-streetBlank"
    ),
    count = c(
      nrow(dupXStreets2),
      nrow(nondupXStreets2),
      nrow(crossStreet2Blank2),
      nrow(intersectionStreet2Blank2)
    )
  )

summaryXstreet2$percentage[1] <-
  round(nrow(dupXStreets2) / numRows * 100, 2)
summaryXstreet2$percentage[2] <-
  round(nrow(nondupXStreets2) / numRows * 100, 2)
summaryXstreet2$percentage[3] <- "N/A"
summaryXstreet2$percentage[4] <- "N/A"
summaryXstreet2$count <-
  format(summaryXstreet2$count, big.mark = ",")

cat("\nSummary of 'cross_street_2' and 'intersection_street_2':\n")
names(summaryXstreet2)[1:3] <- c("category", "count", "percentage")
print(summaryXstreet2, row.names = FALSE, right = FALSE)

#########################################################################
# similarity_threshold <- 0.8

almost_match_1 <-
  subset(
    nondupXStreets1,
    nondupXStreets1$cross_street_1 != "" &
      nondupXStreets1$intersection_street_1 != ""
  )

nzDelta1 <- 3
cat(
  "\n\n*****A near-match is when the two addresses have no more than",
  nzDelta1,
  "characters different.*****"
)

match_1 <-
  data.frame(
    cross_street_1 = character(),
    intersection_street_1 = character(),
    agency = character()
  )

# Loop through the columns
for (i in 1:nrow(almost_match_1)) {
  cross_street <- almost_match_1$cross_street_1[i]
  intersection_street <- almost_match_1$intersection_street_1[i]
  Xagency1 <- almost_match_1$agency[i]

  # Check if it's a close match
  if (is_close_match(cross_street, intersection_street, nzDelta1)) {
    # Create a new row with the matching values
    new_row <-
      data.frame(
        cross_street_1 = cross_street,
        intersection_street_1 = intersection_street,
        agency = Xagency1
      )

    # Append the row to the match_df dataframe
    match_1 <- rbind(match_1, new_row)
  }
}

# Print the resulting dataframe
if (nrow(match_1 > 0)) {
  cat(
    "\n\nThere are",
    nrow(match_1),
    "near-matches between 'cross_street_1' & 'intersection_street_1'. \nHere is a sample:\n"
  )
  random_sample <- match_1 %>% sample_n(min(nrow(match_1), 10)) # random sample
  print(random_sample, row.names = FALSE, right = FALSE)

  # Count by agency
  cat("\nRanked by Agency\n")
  counts_near_match_1 <- table(match_1$agency)

  # Calculate the agency percentage column
  near_match_1_counts <-
    data.frame(
      agency = names(counts_near_match_1),
      count = as.numeric(counts_near_match_1)
    )
  near_match_1_counts$percentage <-
    round(prop.table(near_match_1_counts$count) * 100, 2)
  near_match_1_counts <-
    near_match_1_counts[order(near_match_1_counts$count, decreasing = TRUE), ]
  names(near_match_1_counts)[1:3] <- c("agency", "count", "percentage")
  print(near_match_1_counts, row.names = FALSE, right = FALSE)
} else {
  cat(
    "\n\nThere are no near-matches between 'cross_street_1' & 'intersection_street_1'.\n"
  )
}

####################
almost_match_2 <-
  subset(
    nondupXStreets2,
    nondupXStreets2$cross_street_2 != "" &
      nondupXStreets2$intersection_street_2 != ""
  )
match_2 <-
  data.frame(
    cross_street_2 = character(),
    intersection_street_2 = character(),
    agency = character()
  )
nzDelta2 <- 3

# Loop through the columns
for (i in 1:nrow(almost_match_2)) {
  cross_street <- almost_match_2$cross_street_2[i]
  intersection_street <- almost_match_2$intersection_street_2[i]
  Xagency2 <- almost_match_2$agency[i]

  # Check if it's a close match
  if (is_close_match(cross_street, intersection_street, nzDelta2)) {
    # Create a new row with the matching values
    new_row <-
      data.frame(
        cross_street_2 = cross_street,
        intersection_street_2 = intersection_street,
        agency = Xagency2
      )

    # Append the row to the match_df dataframe
    match_2 <- rbind(match_2, new_row)
  }
}

# Print the resulting dataframe
if (nrow(match_2 > 0)) {
  cat(
    "\n\nThere are",
    nrow(match_2),
    "near-matches between 'cross_street_2' & 'intersection_street_2'. \nHere is a sample:\n"
  )
  random_sample <- match_2 %>% sample_n(min(nrow(match_2), 10)) # random sample
  print(random_sample, row.names = FALSE, right = FALSE)

  # Count by agency
  cat("\nRanked by Agency\n")
  counts_near_match_2 <- table(match_2$agency)

  # Calculate the agency percentage column
  near_match_2_counts <-
    data.frame(
      agency = names(counts_near_match_2),
      count = as.numeric(counts_near_match_2)
    )
  near_match_2_counts$percentage <-
    round(prop.table(near_match_2_counts$count) * 100, 2)
  near_match_2_counts <-
    near_match_2_counts[order(near_match_2_counts$count, decreasing = TRUE), ]
  names(near_match_2_counts)[1:3] <- c("agency", "count", "percentage")
  print(near_match_2_counts, row.names = FALSE, right = FALSE)
} else {
  cat("\n\nThere are no near-matches between 'cross_street_2' & 'intersection_street_2.'\n")
}

#########################################################################

# cat("\n**********REDUCE FILE SIZE. REMOVE DUPLICATE VALUES**********\n")

#########################################################################
# remove redundant columns to reduce file size

# cat("\nShrinking file size by deleting redundant fields:")
# cat(
#  "\n 'park_borough', 'borough_boundaries', 'location', 'intersection_street_1', 'interesetion_street_2', & 'agency_name'."
# )
# oldsize <- round(file.size(data1File) / 1024, 0)
# cat(
#  "\nCurrent file size: '",
#  basename(data1File),
#  "' is size",
#  format(oldsize, big.mark = ","),
#  "KB"
# )

# remove duplicate columns and write out the smaller file
# cloned311 <- d311
# write.csv(
#  subset(
#    cloned311,
#    select = -c(
#      park_borough,
#      borough_boundaries,
#      location,
#      intersection_street_1,
#      intersection_street_2,
#      agency_name
#    )
#  ),
#  quote = FALSE,
#  row.names = FALSE,
#  writeFilePath
# )

# convert size to KB
# newsize <- round(file.size(writeFilePath) / 1024, 0)
# cat(
#  "\nRevised file size: '",
#  basename(writeFilePath),
#  "' is size",
#  format(newsize, big.mark = ","),
#  "KB"
# )

# determine magnitude of file size reduction
# cat(
#  "\nReduction in file size:",
#  format((newsize - oldsize), big.mark = ","),
#  "KB or",
#  round(((newsize - oldsize) / oldsize) * 100, 2),
#  "%"
# )

#########################################################################
programStop <- as.POSIXct(Sys.time())
duration <- difftime(programStop, programStart, units = "secs")

if (duration > 3600) {
  duration <- duration / 3600 # Convert to hours
  cat("\n\nProgram run-time: ", sprintf("%.2f", duration), "hours\n")
} else if (duration > 60) {
  duration <- duration / 60 # Convert to minutes
  cat("\n\nProgram run-time: ", sprintf("%.2f", duration), "minutes\n")
} else {
  cat("\n\nProgram run-time: ", sprintf("%.2f", duration), "seconds\n")
}

#########################################################################
cat("\n *****END OF PROGRAM*****")
#########################################################################
