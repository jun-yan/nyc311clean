#########################################################################
##  Install necessary packages

#########################################################################
##  This function makes standard column names even if there are multiple "."s and trailing "."s
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
  
  inList <- ( dataset %in% listValidValues[, 1] )
  notInList <- dataset[!inList]
  
  if( !all( inList ) ) {
  }
  results <- list( checkIt = all( inList ), non_allowable = notInList )
  return ( results )
}

#########################################################################
## Function to count the number of blanks in each column of the 311 dataframe
countColumnsMissingData <- function( dataset ) {
  ## Create a dataframe to store the column name, the number of blank rows, and the percentage of rows that are blank.
  
  results <- data.frame( columnName =    character(),
                         blankCount =    integer(),
                         fractionBlank = numeric(),
                         unspecifiedCount = integer(),
                         fractionUnspecified = numeric(),
                         unknownCount = integer(),
                         fractionUnknown = numeric() )
  
  rowCount <- nrow( dataset )    ## Count the number of rows to step through. Used to compute % blank.
  for ( col in names( dataset ) ) {
    numberOfBlanks <- sum(dataset[col] == "" | is.na(dataset[col]))
    numberOfUnspecifieds <- sum( dataset[col] == "Unspecified" ) 
    
    numberOfUnknowns <- sum( dataset[col] == "UNKNOWN"  ) 
    newRow <- data.frame( columnName =    col, 
                          blankCount =    numberOfBlanks, 
                          fractionBlank = round(numberOfBlanks / rowCount, 4),
                          unspecifiedCount =    numberOfUnspecifieds, 
                          fractionUnspecified = round(numberOfUnspecifieds / rowCount, 4),
                          unknownCount =    numberOfUnknowns, 
                          fractionUnknown = round(numberOfUnknowns / rowCount, 4))
    results <- rbind( results, newRow )
  }
  
  if (nrow(results) > 0) {names (results) <- c( "field", "blanks","pctBlank", "Unspecified", "pctUnspecified", "unknowns", "pctUnknown") }
  return( results )              
}

#########################################################################
##  The file contains column names in the "header" line.
##  The R "read.csv" function uses a "." to replace the spaces in column names. This makes the column names
##    into legal variables, but the "." can cause problems elsewhere. The function "makeColNamesUserFriendly"
##    replaces the "." with an underscore "_". thus simplifying the field names.
##  Additionally, the field names are converted to lower case with the "tolower" function. 
#########################################################################
programStart <- as.POSIXct( Sys.time() )
# Set the working directory to the "nyc311clean/code" directory to enable relative codes.
#Alter this line of code to match your particular machine.
setwd("C:/Users/david/OneDrive/Documents/nyc311clean/code")

# Load the USPS zipcode file
data2File <- file.path( "..", "data", "USPS_zipcodes.csv" ) 
USPSzipcodes <- read.csv( data2File, header = TRUE, colClasses = rep( "character", ncol( read.csv( data2File ) ) ) )
USPSzipcodes <- makeColNamesUserFriendly( USPSzipcodes )
USPSzipcodesOnly <- USPSzipcodes[, "delivery_zipcode", drop = FALSE]
zipRows <- nrow( USPSzipcodesOnly )

#########################################################################
data3File <- file.path( "..", "data", "NYPDPrecincts2023.csv" ) 
precinctsNYPD <- read.csv( data3File, header = TRUE, colClasses = rep( "character", ncol( read.csv( data3File ))))
precinctsNYPD <- makeColNamesUserFriendly( precinctsNYPD )
numPrecincts <- nrow( precinctsNYPD )

#########################################################################
data4File <- file.path( "..", "data", "NYCCityCouncil2023.csv" ) 
cityCouncilNYC <- read.csv( data4File, header = TRUE, colClasses = rep( "character", ncol( read.csv( data4File ))))
cityCouncilNYC <- makeColNamesUserFriendly( cityCouncilNYC )
numCityCouncil <- nrow( cityCouncilNYC )

#########################################################################
# Load the 311 SR data file
data1File <- file.path( "..","data", "311_Mar_2023.csv")
d311 <- read.csv( data1File, header = TRUE, colClasses = rep( "character", ncol( read.csv( data1File ))))

#Remove any totally empty rows
d311 <- d311[complete.cases(d311), ]

d311 <- makeColNamesUserFriendly( d311 )
numRows <- nrow( d311 )

#Remove rows with value 01/01/1900
d311$closed_date <- ifelse(d311$closed_date == "01/01/1900 12:00:00 AM", "", d311$closed_date)

cat("\nRows in the 311 SR data set:", format( numRows, big.mark = ","))

sortedAllData <- as.data.frame(table(d311$agency))
sortedAllData <- sortedAllData[order(-sortedAllData$Freq),]

sortedAllData$percentage <- round((sortedAllData$Freq / sum(sortedAllData$Freq)) * 100, 2)

sortedAllData$Freq <- format(sortedAllData$Freq, big.mark = ",")
SRsbyAgency <- sortedAllData

cat("\nSRs by Agency:\n")
print(SRsbyAgency)

#########################################################################
missingDataPerColumn <- countColumnsMissingData(d311)
missingDataPerColumn[, sapply(missingDataPerColumn, is.numeric)] <- missingDataPerColumn[, sapply(missingDataPerColumn, is.numeric)]

# Multiply the percentage columns by 100
percentage_columns <- c("pctBlank", "pctUnspecified", "pctUnknown")
missingDataPerColumn[, percentage_columns] <- missingDataPerColumn[, percentage_columns] * 100

# Sort the data frame by the second column in descending order
missingDataPerColumn <- missingDataPerColumn[order(-missingDataPerColumn[, 2]), ]

missingDataPerColumn$blanks <- format(missingDataPerColumn$blanks, big.mark = ",")
missingDataPerColumn$Unspecified <- format(missingDataPerColumn$Unspecified, big.mark = ",")
missingDataPerColumn$unknowns <- format(missingDataPerColumn$unknowns, big.mark = ",")

cat("\n\nNumber and % of blanks(incl 'NA'), Unspecified, and Unknown values in each data field\n")
print(missingDataPerColumn)

# Revert fields back from the format with the thousand's ","
missingDataPerColumn$blanks <- as.numeric(gsub(",", "", missingDataPerColumn$blanks))
missingDataPerColumn$Unspecified <- as.numeric(gsub(",", "", missingDataPerColumn$Unspecified))
missingDataPerColumn$unknowns <- as.numeric(gsub(",", "", missingDataPerColumn$unknowns))

#########################################################################
created_dateDates <- areAllDates( d311$created_date )
closed_dateDates <- areAllDates( d311$closed_date )
due_dateDates <- areAllDates( d311$due_date )
resolution_action_updated_dateDates <-  areAllDates( d311$resolution_action_updated_date )

cat( "\nAre all the values in the 'closed_date' field dates?", closed_dateDates )
cat( "\nAre all the values in the 'due_date' field dates?", due_dateDates )
cat( "\nAre all the values in the 'created_date' field dates?", created_dateDates )
cat( "\nAre all the values in the 'resolution_action_updated_date' field dates?", resolution_action_updated_dateDates )

#########################################################################
uniqueKeys <- length( unique(d311$unique_key)) == nrow(d311 )
cat("\nAre all the 'unique_key' fields truely unique?", uniqueKeys)

#########################################################################
incident_zip5Digits <- areFiveDigits2( d311$incident_zip )
cat( "\nAre all the zipcodes in the 'incident_zip' field 5 numeric digits?", incident_zip5Digits  )

zip_codes5Digits <- areFiveDigits2( d311$zip_codes )
cat( "\nAre all the zipcodes in the 'zip_codes' field 5 numeric digits?", zip_codes5Digits )

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
## Change the lat/long and state_plane fields into type "numeric".
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
badLatitudes <- d311[(is.na(d311$latitude) | d311$latitude < southernMostLatitude | d311$latitude > northernMostLatitude) & !is.na(d311$latitude), ]
badLongitudes <- d311[(is.na(d311$longitude) | d311$longitude > easternMostLongitude | d311$longitude < westernMostLongitude) & !is.na(d311$longitude), ]

cat("\nThe number of latitudes outside the boundaries of NYC is:", nrow(badLatitudes))
if (nrow(badLatitudes) > 0) {
  print(head(badLatitudes$latitude, 5))
}

cat("\nThe number of longitudes outside the boundaries of NYC is:", nrow(badLongitudes))
if (nrow(badLongitudes) > 0) {
  print(head(badLongitudes$longitude, 5))
}

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

#Check if "location" is a concatenation of "latitude" and "longitude"
# Get the rows where latitude or longitude does not match
#latitude_match <- round(d311$latitude, 5) == round(lat, 5)
#longitude_match <- round(d311$longitude, 5) == round(long, 5)

# Get the rows where latitude or longitude does not match
mismatched_rows <- d311[!latitude_match | !longitude_match, ]
mismatched_rows <- mismatched_rows[complete.cases(mismatched_rows[, c("latitude", "longitude", "location")]), ]

# Check if there are any mismatched rows
if (nrow(mismatched_rows) > 0) {
  # Print the mismatched rows
  cat("\nThere are", nrow(mismatched_rows), "mis-matches between latitude & longitude and the location field.\n")
  print(head(mismatched_rows, 5))
} else {
  cat("\nThere are no mis-matches between latitude & longitude and the location field..")
}

#########################################################################
cat("\n\n########## Check for allowable values ##########")
boroughResults <- areInList( d311$borough,  data.frame(values = c("BRONX", "BROOKLYN", "MANHATTAN", "QUEENS", "STATEN ISLAND", "Unspecified") ) )
cat( "\nAre all the values in the 'borough' field valid?", boroughResults$checkIt ) 
if (!boroughResults$checkIt) { 
  cat("\nNumber of non-allowable value", length(boroughResults$non_allowable), "\nNon-allowable values:\n", head(boroughResults$non_allowable, 10) ) }

borough_boundariesResults <- areInList( d311$borough_boundaries,  data.frame(values = c("1", "2", "3", "4", "5") ))
cat( "\nAre all the values in the 'borough_boundaries' field valid?", borough_boundariesResults$checkIt )
if (!borough_boundariesResults$checkIt) { 
  cat("\nNumber of non-allowable value", length(borough_boundariesResults$non_allowable), "\nNon-allowable values:\n", head(borough_boundariesResults$non_allowable, 10) ) }

park_boroughResults <- areInList( d311$park_borough,  data.frame( values = c("BRONX", "BROOKLYN", "MANHATTAN", "QUEENS", "STATEN ISLAND", "Unspecified") ) )
cat( "\nAre all the values in the 'park_borough' field valid?", park_boroughResults$checkIt )
if (!park_boroughResults$checkIt) { 
  cat("\nNumber of non-allowable value", length(park_boroughResults$non_allowable), "\nNon-allowable values:\n", head(park_boroughResults$non_allowable, 10) ) }

open_data_channelResults <- areInList( d311$open_data_channel_type, data.frame( values = c( "UNKNOWN", "MOBILE", "ONLINE", "PHONE", "OTHER" ) ) )
cat( "\nAre all the values in the 'open_data_channel_type'valid?", open_data_channelResults$checkIt )
if (!open_data_channelResults$checkIt) { 
  cat("\nNumber of non-allowable value", length(open_data_channelResults$non_allowable), "\nNon-allowable values:\n", head(open_data_channelResults$non_allowable, 10) ) }

results <- areInList( d311$city_council_districts, cityCouncilNYC )
cat( "\nAre all the values in the 'city_council_district valid?", results$checkIt )
if (!results$checkIt) { 
  cat("\nNumber of non-allowable value", length(results$non_allowable), "\nNon-allowable values:\n", head(results$non_allowable, 10) ) }

police_precinctResults <- areInList( d311$police_precincts, precinctsNYPD )
cat( "\n\nAre all the values in the 'police_precinct valid?", police_precinctResults$checkIt )
if (!police_precinctResults$checkIt) {
  numBlankpolice_precinct <- missingDataPerColumn[missingDataPerColumn$field == "police_precincts", "blanks"]
  cat("\nInvalid values in the 'police_precincts' field number", format(length(police_precinctResults$non_allowable), big.mark = "," ), 
      "representing", percent( length(police_precinctResults$non_allowable)/(numRows - numBlankpolice_precinct), accuracy = 0.01 ), 
      "of non-blank data.\n")
  cat("Made up by", length(unique(police_precinctResults$non_allowable)), "'unique' police_precincts.\n")
  
  # Sort the table in descending order
  sorted_table <- sort(table(police_precinctResults$non_allowable), decreasing = TRUE)
  
  # Convert the table to a data frame and calculate the percentage column
  df <- data.frame(Precinct = names(sorted_table), Count = as.numeric(sorted_table))
  total_count <- sum(df$Count)
  df$Percentage <- round((df$Count / total_count) * 100, 2)
  
  # Format the Count column with thousand separators
  df$Count <- format(df$Count, big.mark = ",")
  
  # Print the top 10 values vertically
  cat("\nTop Ten Invalid Precincts:\n")
  print(df[1:10, ])
}

#########################################################################
# Check for invalid zip codes in d311$incident_zip using USPSzipcodesOnly
invalid_zips1 <- d311[!(d311$incident_zip %in% USPSzipcodesOnly$delivery_zip) & !is.na(d311$incident_zip ) & d311$incident_zip != "na" & d311$incident_zip != "", ]

# Count invalid zip codes
invalid_zip_counts1 <- table(invalid_zips1$incident_zip)

# Calculate the total count
total_invalid_count1 <- sum(invalid_zip_counts1)

# Calculate the percentage column
invalid_zip_df1 <- data.frame(zip_code = names(invalid_zip_counts1), count = as.numeric(invalid_zip_counts1))
invalid_zip_df1$percentage <- round((invalid_zip_df1$count / total_invalid_count1) * 100, 2)

# Count by agency
agency_counts1 <- table(invalid_zips1$agency)

# Calculate the percentage column
agency_df1 <- data.frame(agency = names(agency_counts1), count = as.numeric(agency_counts1))
agency_df1$percentage <- round((agency_counts1 / sum(agency_counts1)) * 100, 2)

# Create a dataframe for agency counts and percentages

# Sort the dataframes
invalid_zip_df1 <- invalid_zip_df1[order(invalid_zip_df1$count, decreasing = TRUE), ]
agency_df1 <- agency_df1[order(agency_df1$count, agency_df1$agency, decreasing = TRUE), ]

# Reset row names
row.names(invalid_zip_df1) <- NULL
row.names(agency_df1) <- NULL

# Output the number of invalid zip codes
numBlankzip_codes1 <- missingDataPerColumn[missingDataPerColumn$field == "incident_zip", "blanks"]
sumInvalid1 <- sum(as.vector(agency_counts1))
cat("\nThere are", format( sum(as.vector(agency_counts1)), big.mark = ","), "invalid d311$incident_zip entries representing", 
    round(sumInvalid1/(nrow(d311) - numBlankzip_codes1)*100,2), "% of non-blank data.\n")
cat("Made up by",length(invalid_zip_counts1) ,"unique 'incident_zip's.\n")

# Print the results
cat("\nInvalid Zip Code Counts (sample):\n")
print(head(invalid_zip_df1,10))

cat("\nAgency Counts and Percentages:\n")
agency_df1$count <- format(agency_df1$count, big.mark = ",")
print(agency_df1)

#########################################################################
# Check for invalid zip codes in d311$zip_codes using USPSzipcodesOnly
invalid_zips2 <- d311[!(d311$zip_codes %in% USPSzipcodesOnly$delivery_zip) & !is.na(d311$zip_codes ) & d311$zip_codes != "", ]

# Count invalid zip codes
invalid_zip_counts2 <- table(invalid_zips2$zip_codes )

# Calculate the total count
total_invalid_count2 <- sum(invalid_zip_counts2)

# Calculate the percentage column
invalid_zip_df2 <- data.frame(zip_code = names(invalid_zip_counts2), count = as.numeric(invalid_zip_counts2))
invalid_zip_df2$percentage <- round((invalid_zip_df2$count / total_invalid_count2) * 100, 2)

# Count by agency
agency_counts2 <- table(invalid_zips2$agency)

# Create a dataframe for agency counts and percentages
agency_df2 <- data.frame(agency = names(agency_counts2), count = as.numeric(agency_counts2))

# Calculate the percentage column
agency_df2$percentage <- round((agency_counts2 / sum(agency_counts2)) * 100, 2)

# Sort the dataframes
invalid_zip_df2 <- invalid_zip_df2[order(invalid_zip_df2$count, decreasing = TRUE), ]
agency_df2 <- agency_df2[order(agency_df2$count, agency_df2$agency, decreasing = TRUE), ]

# Reset row names
row.names(invalid_zip_df2) <- NULL
row.names(agency_df2) <- NULL

# Output the number of invalid zip codes
numBlankzip_codes2 <- missingDataPerColumn[missingDataPerColumn$field == "zip_codes", "blanks"]
sumInvalid2 <- sum(as.vector(agency_counts2))
cat("\nThere are", format( sum(as.vector(agency_counts2)), big.mark = ","), "invalid d311$zip_codes representing", 
    round(sumInvalid2/(nrow(d311) - numBlankzip_codes2)*100,2), "% of non-blank data.\n")
cat("Made up by",length(invalid_zip_counts2) ,"unique 'zip_codes's.\n")

# Print the results
cat("\nInvalid Zip Code Counts (sample):\n")
invalid_zip_df2$count <- format(invalid_zip_df2$count, big.mark = ",")
print(head(invalid_zip_df2,10))

cat("\nAgency Counts and Percentages:\n")
agency_df2$count <- format(agency_df2$count, big.mark = ",")
print(agency_df2)

#########################################################################
d311$translatedborough_boundaries <- ifelse(d311$borough_boundaries == "2", "BROOKLYN",
                                            ifelse(d311$borough_boundaries == "3", "QUEENS",
                                                   ifelse(d311$borough_boundaries == "4", "MANHATTAN",
                                                          ifelse(d311$borough_boundaries == "5", "BRONX",
                                                                 ifelse(d311$borough_boundaries == "1", "STATEN ISLAND", NA)))))


nonMatchingborough_boundaries <- d311[!is.na(d311$translatedborough_boundaries) & d311$borough != d311$translatedborough_boundaries & d311$translatedborough_boundaries != "", 
                          c("unique_key", "borough", "translatedborough_boundaries", "agency")]
numBlankborough_boundaries <- missingDataPerColumn[missingDataPerColumn$field == "borough_boundaries", "blanks"]

cat("\nNon-matches between the 'borough' and 'borough_boundaries' fields number", format(nrow(nonMatchingborough_boundaries), big.mark = ","),
    "representing", percent(nrow(nonMatchingborough_boundaries) / (numRows - numBlankborough_boundaries), accuracy = 0.01),
    "of non-blank data.\n")

if (nrow(nonMatchingborough_boundaries) > 0) {
  cat("\n Sample of non-matching park_boroughs\n")#
#  colnames(nonMatchingFields) <- c("unique_key", "borough","borough_boundaries", "Agency") 
  print(head(nonMatchingborough_boundaries, 10))
  
  cat("\nSorted by Agency:\n")
  sortedDataborough_boundaries <- as.data.frame(table(nonMatchingborough_boundaries$agency))
  sortedDataborough_boundaries$Percentage <- round((sortedDataborough_boundaries$Freq / sum(sortedDataborough_boundaries$Freq)) * 100, 2)  # Calculate the percentage column
  sortedDataborough_boundaries <- sortedDataborough_boundaries[order(-sortedDataborough_boundaries$Freq), ]
#  colnames(sortedDataborough_boundaries) <- c("Agency", "# mismatches", "Pct")
  print(sortedDataborough_boundaries)
}

#########################################################################
nonMatchingpark_borough <- subset(d311, borough != park_borough & park_borough != "", 
                            select = c("unique_key", "borough", "park_borough", "agency"))
numBlankpark_borough <- missingDataPerColumn[missingDataPerColumn$field == "park_borough", "blanks"]

cat("\nNon-matches between the 'borough' and 'park_borough' fields number", format(nrow(nonMatchingpark_borough), big.mark = ","),
    "representing", percent(nrow(nonMatchingpark_borough) / (numRows - numBlankpark_borough), accuracy = 0.01),
    "of non-blank data\n")

if (nrow(nonMatchingpark_borough) > 0) {
  cat("\n Sample of non-matching park_boroughs\n")
  print(head(nonMatchingpark_borough, 5))
  
  cat("\nSorted by Agency:\n")
  sortedDatapark_borough <- as.data.frame(table(nonMatchingpark_borough$agency))
  sortedDatapark_borough$Percentage <- round((sortedDatapark_borough$Freq / sum(sortedDatapark_borough$Freq)) * 100, 2)  # Calculate the percentage column
  
  sortedDatapark_borough <- sortedDatapark_borough[order(-sortedDatapark_borough$Freq), ]
  sortedDatapark_borough <- sortedDatapark_borough[order(-sortedDatapark_borough$Freq), ]
  print(sortedDatapark_borough)
}

#########################################################################
##  Change the various date fields to date-time objects and reformat dates.There are four date fields in the 311 data.
d311$created_date <- strptime(d311$created_date, format = "%m/%d/%Y %I:%M:%S %p", tz = "EST")
d311$closed_date <- strptime(d311$closed_date, format = "%m/%d/%Y %I:%M:%S %p", tz = "EST")
d311$due_date <- strptime(d311$due_date, format = "%m/%d/%Y %I:%M:%S %p", tz = "EST")
d311$resolution_action_updated_date <- strptime(d311$resolution_action_updated_date, format = "%m/%d/%Y %I:%M:%S %p", tz = "EST")

##  Duration is the time between created_date and closed_date, although due to data errors the value may be negative.
##  Identify the Service Records that were closed before they were create, i.e. bad dates.
##  Compute and store "duration" in a new additional column for the dataframe "d311".
d311$duration <- as.numeric( difftime( d311$closed_date, d311$created_date, units = "days"))

#########################################################################
# Find SRs with a negative duration
closedBeforeOpened <- d311[ d311$duration < 0 & !is.na(d311$duration), c("unique_key", "created_date", "closed_date", "duration", "agency")]

numBlankClosedDate <- missingDataPerColumn[missingDataPerColumn$field == "closed_date", "blanks"]

cat( "\n#SRs 'closed' before they were 'opened' (negative duration):", format( nrow( closedBeforeOpened ), big.mark = ","), "representing",
     round( nrow( closedBeforeOpened )/( numRows - numBlankClosedDate)*100,2),"% of non-blank data.\n" )

if ( nrow( closedBeforeOpened ) > 0 ) {
  
  sortedClosed <- closedBeforeOpened[ order(closedBeforeOpened$duration),]
  sortedClosed$duration <- round(sortedClosed$duration, 3)
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
  summary_df$percentage <- (summary_df$count / nrow(sortedClosed)) * 100
  
  # Sort the dataframe by count in descending order
  summary_df <- summary_df[order(summary_df$count, decreasing = TRUE), ]
  
  # Reset row names
  row.names(summary_df) <- NULL
  colnames(summary_df) <- c("Agency", "Count", "Pct")
  summary_df$Pct <- round(summary_df$Pct, 2)
  # View the summary dataframe
  print(summary_df)
}

#########################################################################
zeroDurations <- d311[!is.na(d311$duration) & d311$duration == 0, c("unique_key", "created_date", "closed_date", "duration", "agency")]
numBlankClosedDate <- missingDataPerColumn[missingDataPerColumn$field == "closed_date", "blanks"]

cat( "\n#SRs that were 'closed' and 'opened' at the exact same time, to the second:", format( nrow( zeroDurations ), big.mark = ","), "representing",
     percent( nrow( zeroDurations )/( numRows - numBlankClosedDate), accuracy = 0.01 ), 
     "of non-blank data.\n" )

if (nrow(zeroDurations) > 0) {
  cat("\nSample of SRs closed the exact same time they are opened\n")
  print(head(zeroDurations, 6))
  zeroDurations$duration <- round(zeroDurations$duration, 4)
  sortedClosed <- zeroDurations[order(zeroDurations$duration), ]
  cat("\nBy Agency\n")
  temp <- table(sortedClosed$agency)
  ordered_temp <- sort(temp, decreasing = TRUE)
  ordered_temp_df <- data.frame(Agency = names(ordered_temp), no_of_SRs = ordered_temp)
  ordered_temp_df <- ordered_temp_df[, c(1, 3)]

  #Calculate the total countAgency
  totalSRs <- sum(as.numeric(temp))
  
  # Add the percentage column to the dataframe
  ordered_temp_df$percentage <- round((ordered_temp_df$no_of_SRs.Freq / totalSRs * 100),2)
  
  ordered_temp_df$no_of_SRs.Freq <- format( ordered_temp_df$no_of_SRs.Freq, big.mark = ",")
  colnames(ordered_temp_df) <- c("Agency", "Count", "Pct")
  print(ordered_temp_df)
}

#########################################################################
programStop <- as.POSIXct( Sys.time() )
duration <- sprintf( "%.2f", difftime( programStop, programStart, units = "secs"))
cat( "\n\nTime spent in entire R program is: ", duration, "seconds.\n" ) 

#########################################################################
cat("\n\n\n END OF PROGRAM")