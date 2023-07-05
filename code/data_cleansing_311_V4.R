#########################################################################
install.packages("campfin")
install.packages("stringr")

library(campfin)
library(stringr)

#########################################################################
##  This function standardizes column names even if there are multiple "."s and trailing "."s
##  This leaves the column names with spaces replaced by an underscore "_", i.e. nicer names.
makeColNamesUserFriendly <- function( dataset) {
  
  ## Convert any number of consecutive "."s to an underscore.
  names( dataset ) <- gsub( x = names( dataset ),
                            pattern = "(\\.)+",
                            replacement = "_" )
  
  ## Drop the trailing "."s
  names( dataset ) <- gsub( x = names( dataset ),
                            pattern = "(_)+$",
                            replacement = "" )
  
  ## Convert to lower case. 
  names( dataset ) <- tolower( names( dataset ) )
  
  ## Return the revised column names
  return( dataset )
}

#########################################################################
#Validate that date fields are all dates
areAllDates <- function ( dateField ) {
  
  # remove blank and NAs
  allDates <- suppressWarnings( !is.na( as.Date( dateField[dateField != ""],format = "%m/%d/%Y %I:%M:%S %p"  ) ) )
  
  if( !all( allDates ) ) {
    
    # find indices of values that are not dates
    not_date_indices <- which(is.na(as.Date(dateField[dateField], format="%m/%d/%Y %I:%M:%S %p")))
    
    cat("Values that are not dates:")
    print( dateField[not_date_indices] )
  }
  return ( all( allDates ) )
}  

#########################################################################
#Validate that number fields are all numeric
areAllNumbers <- function ( numberField ){
  
  # Identify the non-blank values using nzchar()
  non_blank_indices <- which(nzchar(numberField))
  
  # Subset the vector to keep only the non-blank values
  numberField <- numberField[non_blank_indices]
  
  # remove blank and NAs
  allNumbers <- suppressWarnings( !is.na( as.numeric( numberField[numberField != ""] ) ) )
  
  if( !all( allNumbers ) ) {
    
    # find indices of values that are not numeric
    non_numeric_values <- numberField[grepl("[^[:digit:]]", numberField)]
    
    cat("Non-numeric values in the vector:", non_numeric_values, "\n")
  }
  return ( all( allNumbers ) )
}

#########################################################################
areFiveDigits2 <- function(zipcodes) {
  
  # Remove blank values
  non_blank_zipcodes <- zipcodes[nzchar(zipcodes)]
  
  # Identify non-numeric and non-5-digit zipcodes using regular expression
  zipcode_pattern <- "^\\d{5}$"
  
  # identify non-compliant zipcodes
  not_valid_zipcodes <- non_blank_zipcodes[!grepl(zipcode_pattern, non_blank_zipcodes)]
  
  if (length(not_valid_zipcodes) > 0) {
    cat("\n\n*****Non 5-digit and/or non-numeric zipcodes found:", not_valid_zipcodes)
    return(FALSE)
  }
  return(TRUE)
}

#########################################################################
#Validate that all fields are in the list of allowable values
areInList <- function ( dataset, listValidValues ){
  
  # Identify the non-blank values using nzchar()
  # Subset the vector to keep only the non-blank values
  non_blank_indices <- which(nzchar(dataset))
  dataset <- dataset[non_blank_indices]
  dataset <- na.omit(dataset)
  
  # determine valid zipcodes
  inList <- ( dataset %in% listValidValues[, 1] )
  notInList <- dataset[!inList]
  
  
  # return non-allowable values & a Boolean variable indicating result of non-allowable test
  results <- list( checkIt = all( inList ), non_allowable = notInList )
  return ( results )
}

#########################################################################
## Function to count the number of blanks in each column of the 311 dataframe
countColumnsMissingData <- function( dataset ) {
  
  # create a dataframe to store the column name, the number of blank rows, and the percentage of rows that are blank.
  results <- data.frame( columnName =    character(),
                         blankCount =    integer(),
                         fractionBlank = numeric(),
                         unspecifiedCount = integer(),
                         fractionUnspecified = numeric(),
                         unknownCount = integer(),
                         fractionUnknown = numeric() )
  numberOfUnspecifieds <- 0
  numberOfUnknowns <- 0
  ## Count the number of rows to step through. Used to compute % blank.
  rowCount <- nrow( dataset )
  
  for ( col in names( dataset ) ) {
    
    # count the number of blanks and NAs in each column
    numberOfBlanks <- sum(dataset[col] == "" | is.na(dataset[col]))
    
    # count the number of "Unspecified" in each column
    numberOfUnspecifieds <- sum( dataset[col] == "UNSPECIFIED" ) 
    
    # count the number of "UNKNOWN" in each column
    numberOfUnknowns <- sum( dataset[col] == "UNKNOWN"  )
    
    # build new dataframe with the results. Compute the %
    newRow <- data.frame( columnName =    col, 
                          blankCount =    numberOfBlanks, 
                          fractionBlank = round(numberOfBlanks / rowCount, 4),
                          unspecifiedCount =    numberOfUnspecifieds, 
                          fractionUnspecified = round(numberOfUnspecifieds / rowCount, 4),
                          unknownCount =    numberOfUnknowns, 
                          fractionUnknown = round(numberOfUnknowns / rowCount, 4))
    
    # build the result matrix
    results <- rbind( results, newRow )
  }
  
  # clean up column names
  if (nrow(results) > 0) {names (results) <- c( "field", "blanks","pctBlank", "Unspecified", "pctUnspecified", "UNKNOWN", "pctUnknown") }
  
  return( results )              
}

#########################################################################
# evaluate street addresses for close match (<=2 characters different)
is_close_match <- function(value1, value2, threshold) {
  if (nchar(value1) != nchar(value2)) {
    return(FALSE)  # Values have different lengths, not a close match
  }
  num_diff <- sum(as.numeric(charToRaw(value1)) != as.numeric(charToRaw(value2)))
  return(num_diff<= threshold)
}

#########################################################################
# Function to perform the replacement using word pairs
replace_misspelled <- function(x, word_pairs) {
  for (i in seq_len(nrow(word_pairs))) {
    misspelled_word <- word_pairs$misspelled[i]
    replacement_word <- word_pairs$correct[i]
    x <- str_replace_all(x, misspelled_word, replacement_word)
  }
  x
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
programStart <- as.POSIXct( Sys.time() )
setwd("C:/Users/david/OneDrive/Documents/nyc311clean/code")
options(digits = 14)  # Set the number of decimal places to 14

#########################################################################
# Load the USPS zipcode file
data2File <- file.path( "..", "data", "USPS_zipcodes.csv" ) 
USPSzipcodes <- read.csv( data2File, header = TRUE, colClasses = rep( "character", ncol( read.csv( data2File ) ) ) )
USPSzipcodes <- makeColNamesUserFriendly( USPSzipcodes )

# extract the 'delivery_zipcode' field
USPSzipcodesOnly <- USPSzipcodes[, "delivery_zipcode", drop = FALSE]
zipRows <- nrow( USPSzipcodesOnly )

#########################################################################
# Load USPS official street abbreviations
data5File <- file.path( "..", "data", "USPSabb.csv" ) 
USPSabbreviations <- read.csv(data5File, stringsAsFactors=FALSE, header=TRUE, colClasses=rep("character", ncol(read.csv(data5File))))
USPSabbreviations <- makeColNamesUserFriendly( USPSabbreviations )
names(USPSabbreviations) <- c("full", "abb")
numAbbreviations <- nrow(USPSabbreviations)

#########################################################################
# Load the Police Precinct reference file
data3File <- file.path( "..", "data", "NYPDPrecincts2023.csv" ) 
precinctsNYPD <- read.csv( data3File, header = TRUE, colClasses = rep( "character", ncol( read.csv( data3File ))))
precinctsNYPD <- makeColNamesUserFriendly( precinctsNYPD )
numPrecincts <- nrow( precinctsNYPD )

#########################################################################
# Load the NYC City Council file
data4File <- file.path( "..", "data", "NYCCityCouncil2023.csv" )
cityCouncilNYC <- read.csv( data4File, header = TRUE, colClasses = rep( "character", ncol( read.csv( data4File ))))
cityCouncilNYC <- makeColNamesUserFriendly( cityCouncilNYC )
numCityCouncil <- nrow( cityCouncilNYC )

#########################################################################
# Load the main 311 SR data file. Set the read & write paths.
data1File <- file.path( "..","data", "311_APR_2023.csv")
writeFilePath <- file.path( "..","data", "311_APR_2023_smaller.csv")
d311 <- read.csv( data1File, header = TRUE, colClasses = rep( "character", ncol( read.csv( data1File ))))
#d311 <- read_csv_arrow( data1File, col_names = TRUE, as_data_frame = TRUE, skip_empty_rows = TRUE)
#d311 <- as.data.frame(d311)

# make columns names user friendly
d311 <- makeColNamesUserFriendly( d311 )
# Delete rows with all missing values
d311 <- d311[rowSums(!is.na(d311)) > 0, ]

#Remove rows with value 01/01/1900 (observed in the 2022 dataset)
d311$closed_date <- ifelse(d311$closed_date == "01/01/1900 12:00:00 AM", "", d311$closed_date)
numRows <- nrow( d311 )

#########################################################################
# Convert character fields to upper case to facilitate comparisons
columns_to_upper <- c("agency", 
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
                      "bridge_highway_segment")

# Convert selected columns to uppercase
d311[, columns_to_upper] <- lapply(d311[, columns_to_upper], toupper)

cat("\n# of rows in the 311 SR data set:", format( numRows, big.mark = ","))

#########################################################################
# build table of agency and count of SRs by agency
sortedAllData <- as.data.frame(table(d311$agency))
sortedAllData <- sortedAllData[order(-sortedAllData$Freq),]
sortedAllData$percentage <- round(prop.table(sortedAllData$Freq)*100, 2)

# print frequency and 
cat("\n\nSRs by Agency:\n")
print(sortedAllData)

#########################################################################
# Dictionary with misspelled word pairs and their correct replacements
word_pairs_city <- data.frame(
  misspelled = c("TENESSE", "MELLVILLE", "FT. WASHIGTON", "FLUSHING AVE", "NEW YORK CITY", "FAMINGDALE", "LOUIEVILLE", "BAYOUN",
                 "STATEN ISLAND NEW YORK", "FLUSHING AVE", "DEDHAM, MA"),
  correct = c("TENNESSEE", "MELVILLE",  "FT. WASHINGTON", "FLUSHING", "NEW YORK", "FARMINGDALE", "LOUISVILLE", "BAYONNE",
              "STATEN ISLAND", "FLUSHING", "DEDHAM" ),
  stringAsFactors = FALSE
)

# Apply the replacement function to the "city" column
d311$city <- replace_misspelled(d311$city, word_pairs_city)

# Perform the replacement using regular expressions with word boundaries
d311$city <- gsub("\\bQUEEN\\b", "QUEENS", d311$city)
d311$city <- gsub("\\bNASSAU$", "NASSAU COUNTY", d311$city)
d311$city <- gsub("\\bNA$", "", d311$city)

word_pairs_zipcode <- data.frame(
  misspelled = c("na", "N/A"),
  correct = c("", "" ),
  stringAsFactors = FALSE
)

# Apply the replacement function to the "incident_zip" column to remove invalid values
d311$incident_zip <- replace_misspelled(d311$incident_zip, word_pairs_zipcode)

#########################################################################
complaintData <- as.data.frame(table(d311$complaint_type))
complaintData <- complaintData[order(-complaintData$Freq), ]
complaintData$percentage <- round(prop.table(complaintData$Freq)*100,4)


cat("\nSample of Complaint type (>1% occurrence)\n")
samplecomplaintData <- complaintData[complaintData$percentage > 1,]
print(head(samplecomplaintData, 10))

#########################################################################
# calculate the number of blank, Unspecified, and Unknown data entires
missingDataPerColumn <- countColumnsMissingData(d311)
## Function to count the number of blanks in each column of the 311 dataframe
# Multiply the percentage columns by 100
percentage_columns <- c("pctBlank", "pctUnspecified", "pctUnknown")
missingDataPerColumn[, percentage_columns] <- missingDataPerColumn[, percentage_columns] * 100

# Sort the data frame by the second column in descending order
missingDataPerColumn <- missingDataPerColumn[order(-missingDataPerColumn[, 2]), ]

cat("\n\nNumber and % of blanks (incl 'NA'), Unspecified, and Unknown values per column\n")
print(missingDataPerColumn)

#########################################################################
# determine if field values correspond to their data 'type', i.e. numeric, date, etc.

# check to see if date fields are all dates.
created_dateDates <- areAllDates( d311$created_date )
closed_dateDates <- areAllDates( d311$closed_date )
due_dateDates <- areAllDates( d311$due_date )
resolution_action_updated_dateDates <-  areAllDates( d311$resolution_action_updated_date )

# print out results to the console
cat( "\nAre all the values in the 'closed_date' field dates?", closed_dateDates )
cat( "\nAre all the values in the 'due_date' field dates?", due_dateDates )
cat( "\nAre all the values in the 'created_date' field dates?", created_dateDates )
cat( "\nAre all the values in the 'resolution_action_updated_date' field dates?", resolution_action_updated_dateDates )

#########################################################################
# determine if the unique_key is in fact unique
uniqueKeys <- length( unique(d311$unique_key)) == nrow(d311 )
cat("\nAre all the 'unique_key' fields truely unique?", uniqueKeys)

#########################################################################
# determine if the incident_zip and zip_codes fields contain 5 numeric digits
incident_zip5Digits <- areFiveDigits2( d311$incident_zip )
cat( "\nAre all the zipcodes in the 'incident_zip' field 5 numeric digits?", incident_zip5Digits  )

zip_codes5Digits <- areFiveDigits2( d311$zip_codes )
cat( "\nAre all the zipcodes in the 'zip_codes' field 5 numeric digits?", zip_codes5Digits )

#########################################################################
# determine if various fields are numeric values

x_coordinateNum <- areAllNumbers( d311$x_coordinate_state_plane )
cat( "\nAre all the values in the 'x_coordinate_state_plane' field numbers?", x_coordinateNum )

y_coordinateNum <- areAllNumbers( d311$y_coordinate_state_plane )
cat( "\nAre all the values in the 'y_coordinate_state_plane' field numbers?", y_coordinateNum )

latitudeNum <- areAllNumbers( d311$latitude )
cat( "\nAre all the values in the 'latitude' field numbers?", latitudeNum  )

longitudeNum <- areAllNumbers( d311$longitude )
cat( "\nAre all the values in the 'longitude' field numbers?", longitudeNum )

community_districtsNum <- areAllNumbers( d311$community_districts )
cat( "\nAre all the values in the 'community_districts' field numbers?", areAllNumbers( community_districtsNum ))

borough_boundariesNum <- areAllNumbers( d311$borough_boundaries )
cat( "\nAre all the values in the 'borough_boundaries' field numbers?", borough_boundariesNum )

city_council_districtsNum <- areAllNumbers( d311$city_council_districts )
cat( "\nAre all the values in the 'city_council_district' field numbers?", city_council_districtsNum )

police_precinctsNum <- areAllNumbers( d311$police_precincts )
cat( "\nAre all the values in the 'police_precincts' field numbers?", police_precinctsNum )

#########################################################################
## Change the lat/long and state_plane fields into type "numeric" to enable comparison.
d311$x_coordinate_state_plane <- as.numeric( d311$x_coordinate_state_plane )
d311$y_coordinate_state_plane <- as.numeric (d311$y_coordinate_state_plane )

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
badLatitudes <- d311[(is.na(d311$latitude) | 
                        d311$latitude < southernMostLatitude |
                        d311$latitude > northernMostLatitude) &
                       !is.na(d311$latitude), ]
badLongitudes <- d311[(is.na(d311$longitude) |
                         d311$longitude > easternMostLongitude |
                         d311$longitude < westernMostLongitude) &
                        !is.na(d311$longitude), ]

cat("\nThe number of latitudes outside the boundaries of NYC is:", nrow(badLatitudes))
if (nrow(badLatitudes) > 0) { print(head(badLatitudes$latitude, 5)) }

cat("\nThe number of longitudes outside the boundaries of NYC is:", nrow(badLongitudes))
if (nrow(badLongitudes) > 0) { print(head(badLongitudes$longitude, 5)) }

#########################################################################
# Check if "location" is a concatenation of "latitude" and "longitude"
# Create empty vectors to store latitude and longitude values
lat <- numeric(length(d311$location))
long <- numeric(length(d311$location))

# Parse latitude and longitude from each location value
parsed_values <- lapply(d311$location, function(loc) {
  matches <- regmatches(loc, gregexpr("-?\\d+\\.\\d+", loc))[[1]]
  c(as.numeric(matches[1]), as.numeric(matches[2]))
})

# Assign parsed latitude and longitude values to separate vectors
for (i in seq_along(parsed_values)) {
  lat[i] <- parsed_values[[i]][1]
  long[i] <- parsed_values[[i]][2]
}

# Check if "location" is a concatenation of "latitude" and "longitude"
# Compare d311$latitude to lat and d311$longitude to long
latitude_match <- (!is.na(d311$latitude) | d311$latitude != "" |  !is.na(lat)) & round(d311$latitude, 5) == round(lat, 5)
longitude_match <- (!is.na(d311$longitude) | d311$latitude != "" | !is.na(long)) & round(d311$longitude, 5) == round(long, 5)

# Get the rows where latitude or longitude does not match
mismatched_rows <- d311[!latitude_match | !longitude_match, ]
mismatched_rows <- mismatched_rows[complete.cases(mismatched_rows[, c("latitude", "longitude", "location")]), ]

# Check if there are any mismatched rows
if (nrow(mismatched_rows) > 0) {
  # Print the mismatched rows
  cat("\nThere are", nrow(mismatched_rows), "mis-matches between latitude & longitude and the location field.\n")
  print(head(mismatched_rows, 5))
} else {
  cat("\nThere are no mis-matches between latitude & longitude and the location field.")
}

#########################################################################
# check to see if cross_street1 and intersection_street_1 are the same value

####################
# Matching cross_street_1 and intersection_street_1
# conduct replacements on a copy of original data
xcloned311 <- d311

####################
# Clean up street types and replace with standard USPS abbreviations
# Replace multiple blank spaces with a single space in address fields
# Apply the function to each address column in xcloned311
xcloned311$cross_street_1 <- normal_address(xcloned311$cross_street_1, abbs = USPSabbreviations, na=NULL, punct = "", abb_end = TRUE)
xcloned311$cross_street_2 <- normal_address(xcloned311$cross_street_2, abbs = USPSabbreviations, na=NULL, punct = "", abb_end = TRUE)
xcloned311$intersection_street_1 <- normal_address(xcloned311$intersection_street_1, abbs = USPSabbreviations, na=NULL, punct = "", abb_end = TRUE)
xcloned311$intersection_street_2 <- normal_address(xcloned311$intersection_street_2, abbs = USPSabbreviations, na=NULL, punct = "", abb_end = TRUE)

####################
# convert address fields that contain the values "NONAME" and "UNNAMED" to blanks. 
# Define the address fields
address_fields <- c("cross_street_1", "cross_street_2", "intersection_street_1", "intersection_street_2")

# Identify rows with affected fields using regular expressions
affected_rows1 <- apply(xcloned311[address_fields], 1, function(row) any(grepl("UNNAMED|NONAME", row)))

# Identify the affected rows before replacement
affected_rows_before1 <- xcloned311[affected_rows1, ]

# Loop through each address field and replace the matching values with blanks
for (field in address_fields) {
  xcloned311[affected_rows1, field] <- ifelse(grepl("UNNAMED|NONAME", xcloned311[affected_rows1, field]), "", xcloned311[affected_rows1, field])
  xcloned311[affected_rows1, field] <- gsub("\\?+", " ",  xcloned311[affected_rows1, field])
}

# Identify the modified dataframe
affected_rows_after1 <- xcloned311[affected_rows1, ]

####################
# fix street addresses that end in "E" or "W", and replace that to the front of the street address
address_fields <- c("cross_street_1", "cross_street_2", "intersection_street_1", "intersection_street_2")

# Identify rows with affected fields using regular expressions
# Loop through each address field and replace the matching values with blanks
exceptions <- c("AVE W", "AVENUE W", "CENTRAL PARK W", "CENTRAL PARK DR W", "CPW", "PROSPECT PARK W", "SHEA STADIUM GATE E")
pattern <- paste(exceptions, collapse = "|")
affected_rows1 <- apply(xcloned311[address_fields], 1, function(row) any(grepl(" E$", row) & !grepl(pattern, row)))
affected_rows3 <- apply(xcloned311[address_fields], 1, function(row) any(grepl(" W$", row) & !grepl(pattern, row)))

# Identify the affected rows before replacement
affected_rows_before2E <- xcloned311[affected_rows1, ]
affected_rows_before2W <- xcloned311[affected_rows3, ]

# Loop through each address field and replace the matching values with blanks
for (field in address_fields) {
  xcloned311[affected_rows1, field] <- ifelse(grepl(" E$", xcloned311[affected_rows1, field]), 
                                              paste0("E ", gsub(" E$", "", xcloned311[affected_rows1, field])),
                                              xcloned311[affected_rows1, field])

  xcloned311[affected_rows3, field] <- ifelse(grepl(" W$", xcloned311[affected_rows3, field]), 
                                              paste0("W ", gsub(" W$", "", xcloned311[affected_rows3, field])),
                                              xcloned311[affected_rows3, field])  
}
# Identify the modified dataframe
affected_rows_after2E <- xcloned311[affected_rows1, ]
affected_rows_after2W <- xcloned311[affected_rows3, ]

####################
# Replace "EAST" with "E" for the selected columns and rows
logical_vector <- grepl("^EAST ", d311$cross_street_1)
columns_to_replace <- c("cross_street_1")
xcloned311[, columns_to_replace] <- ifelse(logical_vector, gsub("^EAST ", "E ", xcloned311[, columns_to_replace]), xcloned311[, columns_to_replace])

logical_vector <- grepl("^EAST ", d311$cross_street_2)
columns_to_replace <- c("cross_street_2")
xcloned311[, columns_to_replace] <- ifelse(logical_vector, gsub("^EAST ", "E ", xcloned311[, columns_to_replace]), xcloned311[, columns_to_replace])

logical_vector <- grepl("^EAST ", d311$intersection_street_2)
columns_to_replace <- c("intersection_street_2")
xcloned311[, columns_to_replace] <- ifelse(logical_vector, gsub("^EAST ", "E ", xcloned311[, columns_to_replace]), xcloned311[, columns_to_replace])

logical_vector <- grepl("^EAST ", d311$intersection_street_1)
columns_to_replace <- c("intersection_street_1")
xcloned311[, columns_to_replace] <- ifelse(logical_vector, gsub("^EAST ", "E ", xcloned311[, columns_to_replace]), xcloned311[, columns_to_replace])

####################
# Replace "WEST" with "W" for the selected columns and rows
logical_vector <- grepl("^WEST ", d311$cross_street_1)
columns_to_replace <- c("cross_street_1")
xcloned311[, columns_to_replace] <- ifelse(logical_vector, gsub("^WEST ", "W ", xcloned311[, columns_to_replace]), xcloned311[, columns_to_replace])

logical_vector <- grepl("^WEST ", d311$cross_street_2)
columns_to_replace <- c("cross_street_2")
xcloned311[, columns_to_replace] <- ifelse(logical_vector, gsub("^WEST ", "W ", xcloned311[, columns_to_replace]), xcloned311[, columns_to_replace])

logical_vector <- grepl("^WEST ", d311$intersection_street_2)
columns_to_replace <- c("intersection_street_2")
xcloned311[, columns_to_replace] <- ifelse(logical_vector, gsub("^WEST ", "W ", xcloned311[, columns_to_replace]), xcloned311[, columns_to_replace])

logical_vector <- grepl("^WEST ", d311$intersection_street_1)
columns_to_replace <- c("intersection_street_1")
xcloned311[, columns_to_replace] <- ifelse(logical_vector, gsub("^WEST ", "W ", xcloned311[, columns_to_replace]), xcloned311[, columns_to_replace])

####################
# Replace "AVE " with "AVENUE" for the selected columns and rows
logical_vector <- grepl("^AVE ", d311$cross_street_1)
columns_to_replace <- c("cross_street_1")
xcloned311[, columns_to_replace] <- ifelse(logical_vector, gsub("^AVE ", "AVENUE ", xcloned311[, columns_to_replace]), xcloned311[, columns_to_replace])

logical_vector <- grepl("^AVE ", d311$cross_street_2)
columns_to_replace <- c("cross_street_2")
xcloned311[, columns_to_replace] <- ifelse(logical_vector, gsub("^AVE ", "AVENUE ", xcloned311[, columns_to_replace]), xcloned311[, columns_to_replace])

logical_vector <- grepl("^AVE ", d311$intersection_street_2)
columns_to_replace <- c("intersection_street_2")
xcloned311[, columns_to_replace] <- ifelse(logical_vector, gsub("^AVE ", "AVENUE ", xcloned311[, columns_to_replace]), xcloned311[, columns_to_replace])

logical_vector <- grepl("^AVE ", d311$intersection_street_1)
columns_to_replace <- c("intersection_street_1")
xcloned311[, columns_to_replace] <- ifelse(logical_vector, gsub("^AVE ", "AVENUE ", xcloned311[, columns_to_replace]), xcloned311[, columns_to_replace])

####################
# find rows where cross_street_1 is blank, but intersection_street_1 is not blank. Use later.
crossStreet1Blank1 <- subset(xcloned311, xcloned311$cross_street_1 == "" & xcloned311$intersection_street_1 != "", select = c("cross_street_1", "intersection_street_1", "agency"))

# find rows where intersection_street is blank, but cross_street is not blank
intersectionStreet1Blank1 <- subset(xcloned311, xcloned311$cross_street_1 != "" & xcloned311$intersection_street_1 == "", select = c("cross_street_1", "intersection_street_1", "agency"))

# find rows where both cross_street_1 and intersection_street_1 are blank
bothBlank1 <- subset(xcloned311, xcloned311$cross_street_1 == "" & xcloned311$intersection_street_1 == "", select = c("cross_street_1", "intersection_street_1", "agency"))

####################
# Check for matching fields
dupXStreets1 <- subset(xcloned311,
                      cross_street_1 == intersection_street_1,
                      select = c("cross_street_1", "intersection_street_1", "agency"))

####################
cat("\n\n# of matching cross_street_1 & intersection_street_1 is", format(nrow(dupXStreets1), big.mark = ",") ,
    "representing", round(nrow(dupXStreets1)/nrow(d311)*100,2),"% of total rows.")
cat("\n\nSample of matching cross_street_1 & intersection_street_1\n")
non_blank_rows_match <- dupXStreets1[dupXStreets1$cross_street_1 != "" & dupXStreets1$intersection_street_1 != "", ]
print(head(dupXStreets1, 20))

cat("\nAgencies with matches\n")
agency_match_counts1 <- table(dupXStreets1$agency)
agency_match1 <- data.frame(agency = names(agency_match_counts1), count = as.numeric(agency_match_counts1))
agency_match1$percentage <- round(prop.table(agency_match1$count)*100,2)
agency_match1 <- agency_match1[order(agency_match1$count, decreasing = TRUE) ,]
print(agency_match1)

####################
# non-matching cross_street1 and intersection_1
nondupXStreets1 <- subset( xcloned311,
                          cross_street_1 != intersection_street_1,
                          select = c("cross_street_1", "intersection_street_1", "agency"))
cat("\n# of non-matching cross_street_1 & intersection_1 is", format(nrow(nondupXStreets1), big.mark = ","),"representing", round(nrow(nondupXStreets1)/nrow(d311)*100,2),"% of total rows")
cat("\n\nSample of non-matching cross_street_1 and intersection_street_1\n")
non_blank_rows_non_match1 <- nondupXStreets1[nondupXStreets1$cross_street_1 != "" & nondupXStreets1$intersection_street_1 != "", ]
print(head(nondupXStreets1[nondupXStreets1$cross_street_1 !="" & nondupXStreets1$intersection_street_1 != "", ], 15))

cat("\nAgencies with non-matches\n")
agency_nonmatch_counts1 <- table(nondupXStreets1$agency)
agency_nonmatch1 <- data.frame(agency = names(agency_nonmatch_counts1), count = as.numeric(agency_nonmatch_counts1))
agency_nonmatch1$percentage <- round(prop.table(agency_nonmatch1$count)*100, 2)
agency_nonmatch1 <- agency_nonmatch1[order(agency_nonmatch1$count, decreasing = TRUE) ,]
print(agency_nonmatch1)

####################
# cross_street_1 is blank, but intersection_street_1 is not blank
cat("\n\nNumber of ocurrences where cross_street_1 is blank, but intersection_street_1 is not blank is", format(nrow(crossStreet1Blank1),  big.mark = ","),
      "representing", round(nrow(crossStreet1Blank1)/nrow(d311)*100,2),"% of total rows") 
cat("\n\nSample of cross_street_1 is blank and intersection_street_1 is not blank.\n")
print(head(crossStreet1Blank1, 10))

# Count by agency
cat("\nAgencies\n")
agency_countsXStreet1 <- table(crossStreet1Blank1$agency)

# Calculate the agency percentage column
agency_XStreetBlank1 <- data.frame(agency = names(agency_countsXStreet1), count = as.numeric(agency_countsXStreet1))
agency_XStreetBlank1$percentage <- round(prop.table(agency_XStreetBlank1$count)*100, 2)
agency_XStreetBlank1 <- agency_XStreetBlank1[order(agency_XStreetBlank1$count, decreasing = TRUE) ,]
print(agency_XStreetBlank1)

####################
# Intersection_street_1 is blank, but cross_street_1 is not blank
cat("\nNumber of ocurrences where intersection_street_1 is blank, but cross_street_1 is not blank", format(nrow(intersectionStreet1Blank1), big.mark = ",") ,
        "representing", round(nrow(intersectionStreet1Blank1)/nrow(d311)*100,2),"% of total rows") 
cat("\n\nSample of cross_street_1 is not blank and intersection_street_1 is blank.\n")
print(head(intersectionStreet1Blank1, 10))

# Count by agency
cat("\nAgencies\n")
agency_inter1_counts1 <- table( intersectionStreet1Blank1$agency)

# Calculate the agency percentage column
agency_inter_StreetBlank1 <- data.frame(agency = names(agency_inter1_counts1), count = as.numeric(agency_inter1_counts1))
agency_inter_StreetBlank1$percentage <- round(prop.table(agency_inter_StreetBlank1$count)*100, 2)
agency_inter_StreetBlank1 <- agency_inter_StreetBlank1[order(agency_inter_StreetBlank1$count, decreasing = TRUE) ,]
print(agency_inter_StreetBlank1)

####################
# summary of cross_street_1 and intersection_street_1 columns
summaryXstreet1 <- data.frame(category = c("matching", "non-matching", "Xstreet1Blank", "interstreet1Blank" ), 
                              count = c(nrow(dupXStreets1), nrow(nondupXStreets1), nrow(crossStreet1Blank1), nrow(intersectionStreet1Blank1)))

summaryXstreet1$percentage[1] <- round(nrow(dupXStreets1)/numRows*100, 2)
summaryXstreet1$percentage[2] <- round(nrow(nondupXStreets1)/numRows*100, 2)
summaryXstreet1$percentage[3] <- "N/A"
summaryXstreet1$percentage[4] <- "N/A"
summaryXstreet1$count <- format(summaryXstreet1$count, big.mark = ",")

cat("\nSummary of cross_street_1 and intersection_street_1\n")
print(summaryXstreet1)

#########################################################################
# check to see if cross_street2 and intersection_2 are the same value

####################
# find rows where cross_street_2 is blank, but intersection_street_2 is not blank. Use later.
crossStreet2Blank2 <- subset(xcloned311, d311$cross_street_2 == "" & d311$intersection_street_2 != "", select = c("cross_street_2", "intersection_street_2", "agency"))

# find rows where intersection_street_2 is blank, but cross_street_2 is not blank
intersectionStreet2Blank2 <- subset(xcloned311, d311$cross_street_2 != "" & d311$intersection_street_2 == "", select = c("cross_street_2", "intersection_street_2", "agency"))

# find rows where both cross_street_2 and intersection_street_2 are blank
bothBlank2 <- subset(xcloned311, xcloned311$cross_street_2 == "" & xcloned311$intersection_street_2 == "", select = c("cross_street_2", "intersection_street_2", "agency"))

####################
# Matching cross_street_2 and intersection_street_2
# remove extra spaces and blanks to perform checks
dupXStreets2 <- subset(xcloned311,
#                       gsub("\\s+", " ", cross_street_2) == gsub("\\s+", " ", intersection_street_2), 
                      cross_street_2 == intersection_street_2,                       
                      select = c("cross_street_2", "intersection_street_2", "agency"))
                        
cat("\n\n# of matching cross_street_2 & intersection_2 is", format(nrow(dupXStreets2), big.mark = ","),
        "representing", round(nrow(dupXStreets2)/nrow(d311)*100,2),"% of total rows.")
cat("\n\nSample of matching cross_street_2 & intersection_street_2\n")
print(head(dupXStreets2, 15))

cat("\nAgencies with matches\n")
agency_match_counts2 <- table(dupXStreets2$agency)
agency_match2 <- data.frame(agency = names(agency_match_counts2), count = as.numeric(agency_match_counts2))
agency_match2$percentage <- round(prop.table(agency_match2$count)*100,2)
agency_match2 <- agency_match2[order(agency_match2$count, decreasing = TRUE) ,]
print(agency_match2)

####################
# non-matching cross_street1 and intersection_1
nondupXStreets2 <- subset( xcloned311,
                           cross_street_2 != intersection_street_2,
#                           gsub("\\s+", " ", cross_street_2) != gsub("\\s+", " ", intersection_street_2), 
                           select = c("cross_street_2", "intersection_street_2", "agency"))

cat("\n# of non-matching cross_street_2 & intersection_street_2 is", format(nrow(nondupXStreets2), big.mark = ","),
        "representing", round(nrow(nondupXStreets2)/nrow(d311)*100,2),"% of total rows")
cat("\n\nSample of non-matching cross_street_2 and intersection_street_2\n")
non_blank_rows_non_match2 <- nondupXStreets2[nondupXStreets2$cross_street_2 !="" & nondupXStreets2$intersection_street_2 != "", ]
print(head(nondupXStreets2[nondupXStreets2$cross_street_2 !="" & nondupXStreets2$intersection_street_2 != "", ], 15))

cat("\nAgencies with non-matches\n")
agency_nonmatch_counts2 <- table(nondupXStreets2$agency)
agency_nonmatch2 <- data.frame(agency = names(agency_nonmatch_counts2), count = as.numeric(agency_nonmatch_counts2))
agency_nonmatch2$percentage <- round(prop.table(agency_nonmatch2$count)*100, 2)
agency_nonmatch2 <- agency_nonmatch2[order(agency_nonmatch2$count, decreasing = TRUE) ,]
print(agency_nonmatch2)

####################
# cross_street_2 is blank, but intersection_street_2 is not blank
cat("\n\nNumber of ocurrences where cross_street_2 is blank, but intersection_street_2 is not blank is", format(nrow(crossStreet2Blank2), big.mark = ","),
      "representing", round(nrow(crossStreet2Blank2)/nrow(d311)*100,2),"% of total rows") 
cat("\n\nSample of cross_street_2 is blank intersection_street_2 is not blank.\n")
print(head(crossStreet2Blank2, 10))

# Count by agency
cat("\nAgencies\n")
agency_countsXStreet2 <- table(crossStreet2Blank2$agency)

# Calculate the agency percentage column
agency_XStreetBlank2 <- data.frame(agency = names(agency_countsXStreet2), count = as.numeric(agency_countsXStreet2))
agency_XStreetBlank2$percentage <- round(prop.table(agency_XStreetBlank2$count)*100, 2)
agency_XStreetBlank2 <- agency_XStreetBlank2[order(agency_XStreetBlank2$count, decreasing = TRUE) ,]
print(agency_XStreetBlank2)

####################
# Intersection_1 is blank, but cross_street_1 is not blank
cat("\nNumber of ocurrences where intersection_street_2 is blank, but cross_street_2 is not blank", format(nrow(intersectionStreet2Blank2), big.mark = ","),
        "representing", round(nrow(intersectionStreet2Blank2)/nrow(d311)*100,2),"% of total rows") 
cat("\n\nSample of cross_street_2 is not blank and intersection_street_2 is blank.\n")
print(head(intersectionStreet2Blank2, 10))

# Count by agency
cat("\nAgencies\n")
agency_inter2_counts2 <- table( intersectionStreet2Blank2$agency)

# Calculate the agency percentage column
agency_inter_StreetBlank2 <- data.frame(agency = names(agency_inter2_counts2), count = as.numeric(agency_inter2_counts2))
agency_inter_StreetBlank2$percentage <- round(prop.table(agency_inter_StreetBlank2$count)*100, 2)
agency_inter_StreetBlank2 <- agency_inter_StreetBlank2[order(agency_inter_StreetBlank2$count, decreasing = TRUE) ,]
print(agency_inter_StreetBlank2)

####################
# summary of cross_street_2 and intersection_street_2 columns
summaryXstreet2 <- data.frame(category = c("matching", "non-matching", "XstreetBlank", "interstreetBlank" ), 
                          count = c(nrow(dupXStreets2), nrow(nondupXStreets2), nrow(crossStreet2Blank2), nrow(intersectionStreet2Blank2)))

summaryXstreet2$percentage[1] <- round(nrow(dupXStreets2)/numRows*100, 2)
summaryXstreet2$percentage[2] <- round(nrow(nondupXStreets2)/numRows*100, 2)
summaryXstreet2$percentage[3] <- "N/A"
summaryXstreet2$percentage[4] <- "N/A"
summaryXstreet2$count <- format(summaryXstreet2$count, big.mark = ",") 

cat("\nSummary of cross_street_2 and intersection_street_2")
print(summaryXstreet2)

#########################################################################
almost_match_1 <- subset(nondupXStreets1, nondupXStreets1$cross_street_1 !="" & nondupXStreets1$intersection_street_1 != "")
# Loop through the columns
# Initialize an empty dataframe to store the matches
match_1 <- data.frame(cross_street_1 = character(), intersection_street_1 = character(), agency = character())

# Loop through the columns
for (i in 1:nrow(almost_match_1)) {
  cross_street <- almost_match_1$cross_street_1[i]
  intersection_street <- almost_match_1$intersection_street_1[i]
  Xagency1 <- almost_match_1$agency[i]
  
  # Check if it's a close match
  if (is_close_match(cross_street, intersection_street, 2)) {
    # Create a new row with the matching values
    new_row <- data.frame(cross_street_1 = cross_street, intersection_street_1 = intersection_street, agency = Xagency1)
    
    # Append the row to the match_df dataframe
    match_1 <- rbind(match_1, new_row)
  }
}

# Print the resulting dataframe
if(nrow(match_1 >0)) {
  cat("\n\nThere are", nrow(match_1), "near-matches between 'cross_street_1' & 'intersection_street_1'. Here is a sample\n")
  print(head(match_1,10))
}else{
  cat("\n\nThere are no near-matches between 'cross_street_1' & 'intersection_street_1'\n")
}

####################
almost_match_2 <- subset(nondupXStreets2, nondupXStreets2$cross_street_2  !="" & nondupXStreets2$intersection_street_2 != "")
match_2 <- data.frame(cross_street_2 = character(), intersection_street_2 = character(), agency = character())

# Loop through the columns
for (i in 1:nrow(almost_match_2)) {
  cross_street <- almost_match_2$cross_street_2[i]
  intersection_street <- almost_match_2$intersection_street_2[i]
  Xagency2 <- almost_match_2$agency[i]
  
  # Check if it's a close match
  if (is_close_match(cross_street, intersection_street, 2)) {
    # Create a new row with the matching values
    new_row <- data.frame(cross_street_2 = cross_street, intersection_street_2 = intersection_street, agency = Xagency2)
    
    # Append the row to the match_df dataframe
    match_2 <- rbind(match_2, new_row)
  }
}

# Print the resulting dataframe
if(nrow(match_2 >0)) {
  cat("\nThere are", nrow(match_2), "near-matches between 'cross_street_2' & 'intersection_street_2'. Here is a sample\n")
  print(head(match_2,10))
}else{
  cat("\nThere are no near-matches between 'cross_street_2' & 'intersection_street_2'\n")
}

#########################################################################
cat("\n\n########## Check for allowable values ##########")

addresss_typeResults <- areInList( d311$address_type,  data.frame(values = c("ADDRESS", "BLOCKFACE", "INTERSECTION", "PLACENAME", "UNRECOGNIZED") ) )
cat( "\nAre all the values in the 'address_type' field valid?", addresss_typeResults$checkIt ) 
if (!addresss_typeResults$checkIt) { 
  cat("\nNumber of non-allowable value", length(addresss_typeResults$non_allowable), "\nNon-allowable values:\n", head(addresss_typeResults$non_allowable, 10) ) }

statusResults <- areInList( d311$status,  data.frame(values = c("ASSIGNED", "CLOSED", "IN PROGRESS", "OPEN", "PENDING", "STARTED", "UNSPECIFIED") ) )
cat( "\nAre all the values in the 'status' field valid?", statusResults$checkIt ) 
if (!statusResults$checkIt) { 
  cat("\nNumber of non-allowable value", length(statusResults$non_allowable), "\nNon-allowable values:\n", head(statusResults$non_allowable, 10) ) }

# check if borough, borough_boundaries, taxi_company_borough, and park_borough contain only allowable values
boroughResults <- areInList( d311$borough,  data.frame(values = c("BRONX", "BROOKLYN", "MANHATTAN", "QUEENS", "STATEN ISLAND", "UNSPECIFIED") ) )
cat( "\nAre all the values in the 'borough' field valid?", boroughResults$checkIt ) 
if (!boroughResults$checkIt) { 
  cat("\nNumber of non-allowable value", length(boroughResults$non_allowable), "\nNon-allowable values:\n", head(boroughResults$non_allowable, 10) ) }

borough_boundariesResults <- areInList( d311$borough_boundaries,  data.frame(values = c("1", "2", "3", "4", "5") ))
cat( "\nAre all the values in the 'borough_boundaries' field valid?", borough_boundariesResults$checkIt )
if (!borough_boundariesResults$checkIt) { 
  cat("\nNumber of non-allowable value", length(borough_boundariesResults$non_allowable), "\nNon-allowable values:\n", head(borough_boundariesResults$non_allowable, 10) ) }

park_boroughResults <- areInList( d311$park_borough,  data.frame( values = c("BRONX", "BROOKLYN", "MANHATTAN", "QUEENS", "STATEN ISLAND", "UNSPECIFIED") ) )
cat( "\nAre all the values in the 'park_borough' field valid?", park_boroughResults$checkIt )
if (!park_boroughResults$checkIt) { 
  cat("\nNumber of non-allowable value", length(park_boroughResults$non_allowable), "\nNon-allowable values:\n", head(park_boroughResults$non_allowable, 10) ) }

taxi_company_boroughResults <- areInList( d311$taxi_company_borough, data.frame( values = c("BRONX", "BROOKLYN", "MANHATTAN", "QUEENS", "STATEN ISLAND") ) )
cat( "\nAre all the values in the 'taxi_company_borough' field valid?", taxi_company_boroughResults$checkIt )
if (!taxi_company_boroughResults$checkIt) { 
  cat("\nNumber of non-allowable value", length(taxi_company_boroughResults$non_allowable), "\nNon-allowable values:\n", head(taxi_company_boroughResults$non_allowable, 10) ) }

open_data_channelResults <- areInList( d311$open_data_channel_type, data.frame( values = c( "UNKNOWN", "MOBILE", "ONLINE", "PHONE", "OTHER" ) ) )
cat( "\nAre all the values in the 'open_data_channel_type'valid?", open_data_channelResults$checkIt )
if (!open_data_channelResults$checkIt) { 
  cat("\nNumber of non-allowable value", length(open_data_channelResults$non_allowable), "\nNon-allowable values:\n", head(open_data_channelResults$non_allowable, 10) ) }

vehcile_typeResults <- areInList( d311$vehcile_typeResults, data.frame( values = c( "AMBULETTE/PARATRANSIT", "CAR SERVICE", "COMMUTER VAN", "GREEN TAXI") ) )
cat( "\nAre all the values in the 'vehcile_type' valid?", vehcile_typeResults$checkIt )
if (!vehcile_typeResults$checkIt) { 
  cat("\nNumber of non-allowable value", length(vehcile_typeResults$non_allowable), "\nNon-allowable values:\n", head(vehcile_typeResults$non_allowable, 10) ) }

city_councilResults <- areInList( d311$city_council_districts, cityCouncilNYC )
cat( "\nAre all the values in the 'city_council_district valid?", city_councilResults$checkIt )
if (!city_councilResults$checkIt) { 
  cat("\nNumber of non-allowable value", length(city_councilResults$non_allowable), "\nNon-allowable values:\n", head(city_councilResults$non_allowable, 10) ) }

police_precinctResults <- areInList( d311$police_precincts, precinctsNYPD )
cat( "\nAre all the values in the 'police_precinct valid?", police_precinctResults$checkIt )
if (!police_precinctResults$checkIt) {
  
  # retrieve the number of blank fields for calculation purposes
  numBlankpolice_precinct <- missingDataPerColumn[missingDataPerColumn$field == "police_precincts", "blanks"]
  
  cat("\n\nInvalid values in the 'police_precincts' field number", format(length(police_precinctResults$non_allowable), big.mark = "," ), 
      "representing", round((length(police_precinctResults$non_allowable)/(numRows - numBlankpolice_precinct)) * 100, 2), "% of non-blank data.\n")
  cat("Made up by", length(unique(police_precinctResults$non_allowable)), "'unique' police_precincts.\n")
  
  # Sort the table in descending order
  sorted_pp_table <- sort(table(police_precinctResults$non_allowable), decreasing = TRUE)
  
  # Convert the table to a data frame and calculate the percentage column
  pp_df <- data.frame(precinct = names(sorted_pp_table), count = as.numeric(sorted_pp_table))
  pp_df$percentage <- round(prop.table(pp_df$count)*100, 2)
  
  # Print the top 10 values vertically
  cat("\nTop Ten Invalid Precincts:\n")
  print(pp_df[1:10, ])
}

#########################################################################
# check for allowable values in the 'community_board' field
cbValues <- c("01 BRONX",	"01 BROOKLYN",	"01 MANHATTAN",	"01 QUEENS",	"01 STATEN ISLAND",	
            "02 BRONX",	"02 BROOKLYN", "02 MANHATTAN",	"02 QUEENS",	"02 STATEN ISLAND",	
            "03 BRONX",	"03 BROOKLYN",	"03 MANHATTAN",	"03 QUEENS",	"03 STATEN ISLAND",	
            "04 BRONX",	"04 BROOKLYN",	"04 MANHATTAN",	"04 QUEENS",	
            "05 BRONX",	"05 BROOKLYN",	"05 MANHATTAN",	"05 QUEENS",	
            "06 BRONX",	"06 BROOKLYN",	"06 MANHATTAN",	"06 QUEENS",	
            "07 BRONX",	"07 BROOKLYN",	"07 MANHATTAN",	"07 QUEENS",	
            "08 BRONX",	"08 BROOKLYN",	"08 MANHATTAN",	"08 QUEENS",	
            "09 BRONX",	"09 BROOKLYN",	"09 MANHATTAN",	"09 QUEENS",	
            "10 BRONX",	"10 BROOKLYN",	"10 MANHATTAN",	"10 QUEENS",	
            "11 BRONX",	"11 BROOKLYN",	"11 MANHATTAN",	"11 QUEENS",	
            "12 BRONX",	"12 BROOKLYN",	"12 MANHATTAN",	"12 QUEENS",	
            "13 BROOKLYN",	"13 QUEENS",	
            "14 BROOKLYN",	"14 QUEENS",	
            "15 BROOKLYN",	
            "16 BROOKLYN",	
            "17 BROOKLYN",	
            "18 BROOKLYN",
            "UNSPECIFIED BRONX", "UNSPECIFIED BROOKLYN", "UNSPECIFIED MANHATTAN", "UNSPECIFIED QUEENS", "UNSPECIFIED STATEN ISLAND",
            "0 UNSPECIFIED")

invalid_cb <- subset(d311, ! community_board %in% cbValues)
invalid_cb_count <- table(invalid_cb$community_board)
total_invalid_cb_count <- sum(invalid_cb_count)

invalid_cb_df <- data.frame(community_board = names(invalid_cb_count), count = as.numeric(invalid_cb_count))
invalid_cb_df$percentage <- round(prop.table(invalid_cb_df$count)*100, 2)

agency_counts_cb <- table(invalid_cb$agency)
agency_cb_df <- data.frame(agency = names(agency_counts_cb), count = as.numeric(agency_counts_cb))
agency_cb_df$percentage <- round(prop.table(agency_cb_df$count)*100, 2)

invalid_cb_df <- invalid_cb_df[order(invalid_cb_df$count, decreasing = TRUE),]
agency_cb_df <- agency_cb_df[order(agency_cb_df$count, decreasing =  TRUE),]

numBlank_cb <- missingDataPerColumn[missingDataPerColumn$field == "community_board", "blanks"]
sumInvalid_cb <- sum(as.vector(agency_counts_cb))

cat("\n\nThere are", format( sum(as.vector(agency_counts_cb)), big.mark = ","), "invalid 'community_board' entries representing", 
    round(sumInvalid_cb/(nrow(d311) - numBlank_cb)*100,2), "% of non-blank data.\n")
cat("Made up by",length(invalid_cb_count) ,"unique 'community_board' entires.\n")

# Print the results
cat("\nInvalid community_board counts (sample):\n")
print(head(invalid_cb_df,10))

cat("\nAgency Counts and Percentages:\n")
print(agency_cb_df)

#########################################################################
# Check for invalid zip codes in d311$incident_zip using USPSzipcodesOnly
invalid_zips1 <- d311[!(d311$incident_zip %in% USPSzipcodesOnly$delivery_zip) & !is.na(d311$incident_zip ) & d311$incident_zip != "na" & d311$incident_zip != "", ]

# Count invalid zip codes
invalid_zip_counts1 <- table(invalid_zips1$incident_zip)

# Calculate the total count
total_invalid_count1 <- sum(invalid_zip_counts1)

# Calculate the overall percentage column
invalid_zip_df1 <- data.frame(zip_code = names(invalid_zip_counts1), count = as.numeric(invalid_zip_counts1))
invalid_zip_df1$percentage <- round(prop.table(invalid_zip_df1$count)*100, 2)

# Count by agency
agency_counts1 <- table(invalid_zips1$agency)

# Calculate the agency percentage column
agency_df1 <- data.frame(agency = names(agency_counts1), count = as.numeric(agency_counts1))
agency_df1$percentage <- round(prop.table(agency_counts1)*100, 2)

# Sort the dataframes
invalid_zip_df1 <- invalid_zip_df1[order(invalid_zip_df1$count, decreasing = TRUE), ]
agency_df1 <- agency_df1[order(agency_df1$count, agency_df1$agency, decreasing = TRUE), ]

# Output the number of invalid zip codes
numBlankzip_codes1 <- missingDataPerColumn[missingDataPerColumn$field == "incident_zip", "blanks"]
sumInvalid1 <- sum(as.vector(agency_counts1))

cat("\n\nThere are", format( sum(as.vector(agency_counts1)), big.mark = ","), "invalid d311$incident_zip entries representing", 
    round(sumInvalid1/(nrow(d311) - numBlankzip_codes1)*100,2), "% of non-blank data.\n")
cat("Made up by",length(invalid_zip_counts1) ,"unique 'incident_zip's.\n")

# Print the results
cat("\nInvalid Zip Code Counts (sample):\n")
print(head(invalid_zip_df1,10))

cat("\nAgency Counts and Percentages:\n")
print(agency_df1)

#########################################################################
# Check for invalid zip codes in d311$zip_codes using USPSzipcodesOnly
invalid_zips2 <- d311[!(d311$zip_codes %in% USPSzipcodesOnly$delivery_zip) & !is.na(d311$zip_codes ) & d311$zip_codes != "", ]

# Count invalid zip codes
invalid_zip_counts2 <- table(invalid_zips2$zip_codes )

# Calculate the total count
total_invalid_count2 <- sum(invalid_zip_counts2)

# Calculate the overall percentage column
invalid_zip_df2 <- data.frame(zip_code = names(invalid_zip_counts2), count = as.numeric(invalid_zip_counts2))
invalid_zip_df2$percentage <- round(prop.table(invalid_zip_df2$count)*100, 2)

# Count by agency
agency_counts2 <- table(invalid_zips2$agency)

# Calculate the agency percentage column
agency_df2 <- data.frame(agency = names(agency_counts2), count = as.numeric(agency_counts2))
agency_df2$percentage <- round(prop.table(agency_counts2)*100, 2)

# Sort the dataframes
invalid_zip_df2 <- invalid_zip_df2[order(invalid_zip_df2$count, decreasing = TRUE), ]
agency_df2 <- agency_df2[order(agency_df2$count, agency_df2$agency, decreasing = TRUE), ]

# Output the number of invalid zip codes
numBlankzip_codes2 <- missingDataPerColumn[missingDataPerColumn$field == "zip_codes", "blanks"]
sumInvalid2 <- sum(as.vector(agency_counts2))

cat("\nThere are", format( sum(as.vector(agency_counts2)), big.mark = ","), "invalid d311$zip_codes representing", 
    round(sumInvalid2/(nrow(d311) - numBlankzip_codes2)*100,2), "% of non-blank data.\n")
cat("Made up by",length(invalid_zip_counts2) ,"unique 'zip_codes's.\n")

# Print the results
cat("\nInvalid Zip Code Counts (sample):\n")
print(head(invalid_zip_df2,10))

cat("\nAgency Counts and Percentages:\n")
print(agency_df2)

#########################################################################
# check the encoded borough_boundaries field for redundancy with borough
d311$translatedborough_boundaries <- ifelse(d311$borough_boundaries == "2", "BROOKLYN",
                                            ifelse(d311$borough_boundaries == "3", "QUEENS",
                                                   ifelse(d311$borough_boundaries == "4", "MANHATTAN",
                                                          ifelse(d311$borough_boundaries == "5", "BRONX",
                                                                 ifelse(d311$borough_boundaries == "1", "STATEN ISLAND", NA)))))

# making the assumption that borough == "Unspecified" and is.na(translatedborough_boundaries) that is a match, ie. Unspecified and NA are the same
Matchingborough_boundaries <- subset(d311, subset = borough == translatedborough_boundaries | 
                                       (borough == "Unspecified" & is.na(translatedborough_boundaries)),
                                       select = c("unique_key", "borough", "translatedborough_boundaries", "agency"))
                                     
nonMatchingborough_boundaries <- subset(d311, subset =! d311$unique_key %in% Matchingborough_boundaries$unique_key,
                                        select = c("unique_key", "borough", "translatedborough_boundaries", "agency"))

cat("\n\nTotal Non-matches between the 'borough' and 'borough_boundaries' fields are", format(nrow(nonMatchingborough_boundaries), big.mark = ","),
    "representing", round( nrow(nonMatchingborough_boundaries) / (numRows) * 100, 2), "% of data.\n")

if (nrow(nonMatchingborough_boundaries) > 0) {
  
  nonMatchingborough_boundaries_blank <- subset(nonMatchingborough_boundaries, is.na(nonMatchingborough_boundaries$translatedborough_boundaries))
  cat("\n\nSample of the", nrow(nonMatchingborough_boundaries_blank),"non-matching borough_boundaries (where borough_boundaries is blank)\n")
  print(head(nonMatchingborough_boundaries_blank, 10))
  
  nonMatchingborough_boundaries_non_blank <- subset(nonMatchingborough_boundaries, !is.na(nonMatchingborough_boundaries$translatedborough_boundaries))
  cat("\n\nSample of the", nrow(nonMatchingborough_boundaries_non_blank),"non-matching borough_boundaries (where borough_boundaries is NOT blank)\n")
  print(head(nonMatchingborough_boundaries_non_blank, 10))
  
  cat("\nSorted by Agency:\n")
  sortedDataborough_boundaries <- as.data.frame(table(nonMatchingborough_boundaries$agency))
  sortedDataborough_boundaries$Percentage <- round(prop.table(sortedDataborough_boundaries$Freq)*100, 2)
  sortedDataborough_boundaries <- sortedDataborough_boundaries[order(-sortedDataborough_boundaries$Freq), ]

  cat("\nAgencies associted with non-matching 'borough' $ 'borough_boundaries'\n")
    print(sortedDataborough_boundaries)
}

#########################################################################
# check to see if there are any non-matches between 'borough' and 'park_borough'
nonMatchingpark_borough <- subset(d311, borough != park_borough , 
                            select = c("unique_key", "borough", "park_borough", "agency"))

# retrieve # of blanks for calculations
numBlankpark_borough <- missingDataPerColumn[missingDataPerColumn$field == "park_borough", "blanks"]

cat("\n\nNon-matches between the 'borough' and 'park_borough' fields number", format(nrow(nonMatchingpark_borough), big.mark = ","),
    "representing", round(nrow(nonMatchingpark_borough) / (numRows - numBlankpark_borough) *100, 2), "% of non-blank data\n")

if (nrow(nonMatchingpark_borough) > 0) {
  cat("\n\nSample of non-matching park_boroughs\n")
  print(head(nonMatchingpark_borough, 5))
  
  cat("\nSorted by Agency:\n")
  sortedDatapark_borough <- as.data.frame(table(nonMatchingpark_borough$agency))
  sortedDatapark_borough$Percentage <- round( prop.table(sortedDatapark_borough$Freq)*100, 2)  # Calculate the percentage column
  
  sortedDatapark_borough <- sortedDatapark_borough[order(-sortedDatapark_borough$Freq), ]
  sortedDatapark_borough <- sortedDatapark_borough[order(-sortedDatapark_borough$Freq), ]
  print(sortedDatapark_borough)
}

#########################################################################
# check to see if there are any mis-matches between 'borough' and 'taxi_company_borough'
nonMatchingtaxi_company_borough <- subset(d311, borough != taxi_company_borough & taxi_company_borough != "", 
                                  select = c("unique_key", "borough", "taxi_company_borough", "agency"))

# retrieve # of blanks for calculations
numBlanktaxi_company_borough <- missingDataPerColumn[missingDataPerColumn$field == "taxi_company_borough", "blanks"]

cat("\n\nNon-matches between the 'borough' and 'taxi_company_borough' fields (excluding blanks) number", format(nrow(nonMatchingtaxi_company_borough), big.mark = ","),
    "representing", round(nrow(nonMatchingtaxi_company_borough) / (numRows - numBlanktaxi_company_borough) *100, 2), "% of non-blank data\n")

if (nrow(nonMatchingtaxi_company_borough) > 0) {
  cat("\nSample of non-matching taxi_company_borough (exluding blanks in taxi_company_borough).\n")
  print(head(nonMatchingtaxi_company_borough, 5))
  
  cat("\nSorted by Agency:\n")
  sortedDatataxi_company_borough <- as.data.frame(table(nonMatchingtaxi_company_borough$agency))
  sortedDatataxi_company_borough$Percentage <- round( prop.table(sortedDatataxi_company_borough$Freq)*100, 2)  # Calculate the percentage column
  
  sortedDatataxi_company_borough <- sortedDatataxi_company_borough[order(-sortedDatataxi_company_borough$Freq), ]
  sortedDatataxi_company_borough <- sortedDatataxi_company_borough[order(-sortedDatataxi_company_borough$Freq), ]
  print(sortedDatataxi_company_borough)
}

#########################################################################
# Change the various date fields to date-time objects and reformat dates.There are four date fields in the 311 data.
d311$created_date <- strptime(d311$created_date, format = "%m/%d/%Y %I:%M:%S %p", tz = "EST")
d311$closed_date <- strptime(d311$closed_date, format = "%m/%d/%Y %I:%M:%S %p", tz = "EST")
d311$due_date <- strptime(d311$due_date, format = "%m/%d/%Y %I:%M:%S %p", tz = "EST")
d311$resolution_action_updated_date <- strptime(d311$resolution_action_updated_date, format = "%m/%d/%Y %I:%M:%S %p", tz = "EST")

# Duration is the time between created_date and closed_date
# Compute and store "duration" in a new additional column for the dataframe "d311".
d311$duration <- as.numeric( difftime( d311$closed_date, d311$created_date, units = "days"))

#########################################################################
# Identify the SRs with negative duration (closed before they were created)
closedBeforeOpened <- d311[ d311$duration < 0 & !is.na(d311$duration), c("unique_key", "created_date", "closed_date", "duration", "agency")]

numBlankClosedDate <- missingDataPerColumn[missingDataPerColumn$field == "closed_date", "blanks"]

cat( "\n\n#SRs 'closed' before they were 'opened' (negative duration):", format( nrow( closedBeforeOpened ), big.mark = ","), "representing",
     round( nrow( closedBeforeOpened )/( numRows - numBlankClosedDate)*100,2),"% of non-blank data.\n" )

if ( nrow( closedBeforeOpened ) > 0 ) {
  sortedClosed <- closedBeforeOpened[ order(closedBeforeOpened$duration),]
  sortedClosed$duration <- round(sortedClosed$duration, 4)
  cat("\nLargest errors (days):\n")
  print( head( sortedClosed, 5 ) )
  cat("\nSmallest errors (days):\n")
  print( tail( sortedClosed, 5))
  cat("\nBy Agency\n")

  # Calculate the count by agency
  count_by_agency <- table(sortedClosed$agency)
  
  # Create a dataframe from the count
  summary_df <- data.frame(agency = names(count_by_agency), count = as.numeric(count_by_agency))
  
  # Calculate the percentage column
  summary_df$percentage <- round(prop.table(summary_df$count)*100,2)
  
  # Sort the dataframe by count in descending order
  summary_df <- summary_df[order(summary_df$count, decreasing = TRUE), ]
  
  # Reset row names
  row.names(summary_df) <- NULL
  print(summary_df)
}

#########################################################################
# count SRs that have a zero duration, i.e. closed and opened at the exact same time
zeroDurations <- d311[!is.na(d311$duration) & d311$duration == 0, c("unique_key", "created_date", "closed_date", "duration", "agency")]
numBlankClosedDate <- missingDataPerColumn[missingDataPerColumn$field == "closed_date", "blanks"]

cat( "\n\n#SRs that were 'closed' and 'opened' at the exact same time, to the second:", format( nrow( zeroDurations ), big.mark = ","), "representing",
     round(nrow( zeroDurations )/( numRows - numBlankClosedDate) *100, 2), "% of non-blank data.\n" )

if (nrow(zeroDurations) > 0) {
  cat("\n\nSample of SRs closed the exact same time they are opened\n")
  print(head(zeroDurations, 6))
  zeroDurations$duration <- round(zeroDurations$duration, 4)
  sortedClosed <- zeroDurations[order(zeroDurations$duration), ]
  cat("\nBy Agency\n")
  temp <- table(sortedClosed$agency)
  ordered_temp <- sort(temp, decreasing = TRUE)
  ordered_temp_df <- data.frame(Agency = names(ordered_temp), no_of_SRs = ordered_temp)
  ordered_temp_df <- ordered_temp_df[, c(1, 3)]

  # Add the percentage column to the dataframe
  ordered_temp_df$percentage <- round(prop.table(ordered_temp_df$no_of_SRs.Freq)*100,2)
  print(ordered_temp_df)
}

#########################################################################
# explore removing redundant columns to reduce file size

cat("\n\nShrinking file size by eliminating redundant fields:")
cat(" park_borough, borough_boundaries, location, intersection_street_1, interesetion_street_2, & agency_name")
oldsize <- round(file.size(data1File)/1024,0)
cat("\nThe current file '", basename(data1File), "' is size", format(oldsize, big.mark = ","), "KB")

# remove duplicate columns and write out the smaller file
cloned311 <- d311
write.csv(subset( cloned311, select = -c(park_borough, borough_boundaries, location,
                    intersection_street_1, intersection_street_2, agency_name)),
                    quote=FALSE, row.names = FALSE,
                    writeFilePath)

# convert size to KB
newsize <- round(file.size(writeFilePath)/1024,0)
cat("\nThe revised file '", basename(writeFilePath), "' is size", format(newsize, big.mark = ","), "KB")

# determine magnitude of file size reduction
cat("\nReduction in file size is", format((newsize- oldsize),big.mark = ","), "KB or", round(((newsize- oldsize) / oldsize )*100, 2), "%")

#########################################################################
programStop <- as.POSIXct( Sys.time() )
duration <- sprintf( "%.2f", difftime( programStop, programStart, units = "secs"))
cat( "\n\n\nProgram run-time is: ", duration, "seconds.\n" )

#########################################################################
cat("\n *****END OF PROGRAM*****")
#########################################################################
#########################################################################
