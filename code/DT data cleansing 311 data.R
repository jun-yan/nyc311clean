#########################################################################
##  Install necessary packages
##
install.packages( "scales" )
install.packages( "stringr" )
library( scales )
library( stringr )

#########################################################################
##  Utility functions
#########################################################################
##  This function makes standard column names even if there are multiple "."s and trailing "."s
##  This leaves the column names with spaces replaced by an underscore "_", i.e. nicer names.
##
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
##
#########################################################################
##
##  This function converts a datetimestamp in character format to a date object in a slightly different format.
##  The 311 data set is read into the d311 dataframe as all character string.
##  It is necessary to convert these dates from string to a date object to compute duration.
##  The data is also reformatted to a different format using YYY-MM-DD and a 24-hour clock.
##
convertToDateObject <- function( dateString ) {

  ## Convert the date string ("02/01/2023 11:44:53 PM") to a date-time object
  dateTimeObject <- strptime( dateString, format = "%m/%d/%Y %I:%M:%S %p" )

  ## Convert the new date-time object to a new string in a new format ("2023-02-02 02:52:50") 
  newString <- format( dateTimeObject, format = "%Y-%m-%d %H:%M:%S" )

  ## Convert the new string to a date-time object using POSIXct. This will enable calculation of duration.
  newDateTimeObject <- as.POSIXct( newString, tz = "EST", format = "%Y-%m-%d %H:%M:%S" )

  ## Return the revised variable in date-time object now formatted as "2023-04-26 21:50:45"
  return( newDateTimeObject )
}
##
#########################################################################
##
## Function to count the number of blanks in each column of the 311 dataframe
##
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
  
  ##  Step through the 311 dataframe to build the new dataframe containing column blank counts
  for ( col in names( dataset ) ) {
    numberOfBlanks <- sum( is.na( dataset[col]) | dataset[col] == "" ) 
    
    numberOfUnspecifieds <- sum( is.na( dataset[col]) | dataset[col] == "Unspecified" ) 

    numberOfUnknowns <- sum( is.na( dataset[col] ) | dataset[col] == "UNKNOWN" | dataset[col] == "OTHER"  ) 
    newRow <- data.frame( columnName =    col, 
                          blankCount =    numberOfBlanks, 
                          fractionBlank = round(numberOfBlanks / rowCount, 6),
                          unspecifiedCount =    numberOfUnspecifieds, 
                          fractionUnspecified = round(numberOfUnspecifieds / rowCount, 6),
                          unknownCount =    numberOfUnknowns, 
                          fractionUnknown = round(numberOfUnknowns / rowCount, 6))
    results <- rbind( results, newRow )
  }
  
  
  ## return the newly created dataframe with the column blank counts.   
  if (nrow(results) > 0) {names (results) <- c( "field", "#blanks","%blank", "#unspecified", "%unspecified", "#unknowns", "%Unknown") }
  return( results )              
}
##

#########################################################################
##
##  Function to find bad dates in the 311 data, i.e. Service Records that are closed before they're opened.
##  Such Service Records will show a negative value for the duration. A dataframe is returned with these SR's unique_key.
##
findBadDates <- function( dataset, data1Position, data2Position, data3Position, data4Position, data5position ) {
    ##  Create a dataframe with elements unique_key, created_date, closed_date, duration, and organization for rows where duration <0.
    ##  data1Position = unique key for each row of the dataframe
    ##  data2Position = beginning date, e.g. created_date
    ##  data3Position = ending date, e.g. closed_date
    ##  data4Position = duration, previously calculated
    ##  data5Position = option value, typically organization, e.g. agency

      results <- data.frame( unique_key =   character(),
                            created_date = numeric(),
                            closed_date =  numeric(),
                            duration =     numeric(),
                            agency = character())

      rowCount <- nrow( dataset )         ##  Value used to control stepping through the data set
  
##  Step through the 311 dataframe to build a new dataframe containing created, closed, and duration values <0.

      for ( row in 1:rowCount )  {
        if ( !is.na(dataset[row, data4Position] ) ) {     ##  Exclude durations == "NA" which exist if there is no closed_date, i.e. SR is still open.
          if ( dataset[row, data4Position] < 0 ) {      ##  Look for durations that are <0, indicated closed before created.
              newRow <- data.frame(       unique_key =   dataset[row, data1Position], 
                                          created_date = dataset[row, data2Position], 
                                          closed_date =  dataset[row, data3Position], 
                                          duration =      dataset[row, data4Position],
                                          agency = dataset[row, data5position] )
              results <- rbind( results, newRow )
      }
    }
  }
  ## return the new dataframe (result) containing the unique_key (identifying the row), created_date,  closed_date, and duration.  
#  cat("\nResults")
#  print(tail(results))
  if ( nrow(results) >0 ) { results1 <- results[ order( results$duration ), ] }
#  print(tail(results1))
  return( results1 )              
}
##
##
#########################################################################
mismatchedBoroughs <- function( dataset ) {
  boroughData <- data.frame ( matrix(ncol = 4, nrow = nrow(dataset)) )
  colnames( boroughData ) <- c( "uniqueKey", "borough1", "borough2", "organization" )
  
  results <- data.frame()
  
  boroughData$borough1 <- dataset$borough
  boroughData$borough2 <- str_replace_all(dataset$borough_boundaries, c("1" = "STATEN ISLAND", "2"= "BROOKLYN", "3" = "QUEENS", "4"="MANHATTAN", "5"= "BRONX"))
  boroughData$uniqueKey <- dataset$unique_key
  boroughData$organization <- dataset$agency
  
  for ( row in 1:nrow( boroughData) ) {
    if( boroughData[row,3] == "" ){
        next                          
       } else {
        if (boroughData[row,2] == "" || boroughData[row,2] == "Unspecified" ) {
          next                        
        } else {
            if ( boroughData[row,2] != boroughData[row, 3] ) { 
              newRow <- data.frame( unique_key =   boroughData[row, 1],
                                  boroughData[row, 2],
                                  boroughData[row, 3],
                                  organization = boroughData[row, 4] )
              results <- rbind( results, newRow )            
              }                                                        
             } 
         }
        }
      
#  cat("\n class of results", class( results))
#  print( results )
  if ( nrow( results ) > 0 ) { names( results ) <- c( "key","borough", "borough_boundaries", "agency" ) }
  if ( nrow( results ) > 0 ) { results <- results[ order( results$agency, results$borough, results$key ), ] }
  return( results )      
}
#
#

#########################################################################
findMismatchedFields <- function( dataset, data1Position, data2Positon, data3Position, data4Position ) {
  
  results <- data.frame()
  
  comparisonData <- data.frame ( matrix(ncol = 4, nrow = nrow(dataset)) )
  colnames( comparisonData ) <- c( "uniqueKey", "field1", "field2", "organization" )
  
  comparisonData$uniqueKey <- dataset[, data1Position]
  comparisonData$field1 <- dataset[, data2Positon]
  comparisonData$field2 <- dataset[, data3Position]
  comparisonData$organization <- dataset[, data4Position ]
    str_replace_all(dataset$borough_boundaries, c("1" = "STATEN ISLAND", "2"= "BROOKLYN", "3" = "QUEENS", "4"="MANHATTAN", "5"= "BRONX"))
  boroughData$organization <- dataset$agency
  
  for ( row in 1:nrow( boroughData) ) {
    if( boroughData[row,3] == "" ){
      next                          
    } else {
      if (boroughData[row,2] == "" || boroughData[row,2] == "Unspecified" ) {
        next                        
      } else {
        if ( boroughData[row,2] != boroughData[row, 3] ) { 
          newRow <- data.frame( unique_key =   boroughData[row, 1],
                                boroughData[row, 2],
                                boroughData[row, 3],
                                organization = boroughData[row, 4] )
          results <- rbind( results, newRow )            
        }                                                        
      } 
    }
  }
  
  cat("\n class of results", class( results))
  print(results)
  if ( nrow( results ) > 0 ){ names( results ) <- c( "key","borough", "borough_boundaries", "agency" ) }
  if ( nrow( results ) > 0 ) { results <- results[ order( results$agency, results$borough, results$key ), ] }
  return( results )      
}
#

#########################################################################
findInvalidZipcodes <- function( referenceZipcodes, dataset, data1Position, data2Position, data3Position ) { 
  ##	data1Position is the column # in the dataset array containing the uniqueID, aka the "key"
  ##	data2Position is the column# in the dataset array containing the zipcode to be evaluated 
  ##	data3Position is the organization involved, e.g. the “agency” 

  ##  Create a dataframe with elements key, badZipcode,and department for rows where duration
  
  maxRows <- nrow( dataset ) 
  # Preallocate the size of the results2 dataframe to allow for row insertion in lieu of row creation with rbind
  results2 <- data.frame( badZipcode = character(maxRows),
                         department = character(maxRows), 
                         key = character(maxRows),
                         stringsAsFactors = FALSE)
  
#  results <- data.frame( badZipcode = character(),
#                          department = character(), 
#                          key = character() )
  invalidZipcodes = data.frame() 
  
  referenceZipcodes <- referenceZipcodes[ order( referenceZipcodes[, 1] ), ]
  referenceZipcodes <- data.frame( referenceZipcodes, stringsAsFactors = FALSE )
  uniqueZipcodes <- unique(  dataset[ , data2Position ], na.rm = TRUE )
  noBlanks <- uniqueZipcodes[uniqueZipcodes !=""]
  sortedZipcodes <- sort( noBlanks )
  uniqueZipcodes <- data.frame( sortedZipcodes, stringsAsFactors = FALSE )
#  cat("\n# of entries in the zip code field", nrow( dataset ) )
#  cat("\n# of non-blank zip codes in data", sum( !is.na( dataset[, data2Position] ) & dataset[, data2Position] != "" ) )
#  cat("\n# of uniqueZipCodes in the data", nrow( uniqueZipcodes ))
  
  startTime <- as.POSIXct( Sys.time() )
  startTimeFormatted <- format( startTime, "%H:%M:%S" )
#  cat( "\nBegin unique zipcode lookup in USPS:", startTimeFormatted )
  
  rowsUniqueZipcodes <- nrow( uniqueZipcodes )
  for ( row in 1:rowsUniqueZipcodes ) {
    targetCode <- uniqueZipcodes[row, 1]
    if ( targetCode == "" || targetCode %in% referenceZipcodes[, 1] ) {
      next
    } else {
      newRow <- uniqueZipcodes[ row, 1 ]
      invalidZipcodes <- rbind( invalidZipcodes, newRow ) 
    }
  }

  stopTime <- as.POSIXct( Sys.time() )
  stopTimeFormatted <- format( stopTime, "%H:%M:%S" )
#  cat( "\nEnd unique zipcoad lookup in USPS:", stopTimeFormatted )
#  cat( "\n\nTime spent in the lookup of unique zipcodes in USPS: ", sprintf( "%.2f", difftime( stopTime, startTime, units = "secs")), "seconds." ) 
  
  
  startTime <- as.POSIXct( Sys.time() )
  startTimeFormatted <- format( startTime, "%H:%M:%S" )
#  cat( "\nBegin lookup of all zipcodes in invalidZipcodes:", startTimeFormatted )
  
  rowDataset <- nrow( dataset )
  rowInvalidZipcodes <- nrow( invalidZipcodes )
  
  if ( rowInvalidZipcodes > 0 ) {
    rowIndex <- 1
    for ( row in 1:rowDataset ) {
        if ( dataset[row, data2Position] != "" && dataset[row, data2Position] %in% invalidZipcodes[, 1] ) { 
            tempInvalid <- data.frame ( dataset[ row, data3Position ], 
                                        dataset[ row, data2Position ], 
                                        dataset[ row, data1Position ] ) 
            results2[rowIndex, ] <- tempInvalid
            rowIndex <- rowIndex + 1
        } else{
          next
          }
        }
      }
  stopTime <- as.POSIXct( Sys.time() )
  results2 <- results2[ seq_len( rowIndex -1 ), ]# Trim empty rows from the 'results2' dataframe
  stopTimeFormatted <- format( stopTime, "%H:%M:%S" )
  duration <- sprintf( "%.2f", difftime( stopTime, startTime, units = "secs"))
  
  cat( "\n\nTime spent in lookup of ALL dataset zipcodes %in% invalidZipcodes: ", duration, "seconds." ) 

  if ( nrow( results2 ) > 0 ) { names( results2 ) <- c( "agency","invalid_zip", "unique_id") }
  if ( nrow( results2 ) > 0 ) { results <- results2[ order( results2$agency, results2$invalid_zip, results2$unique_id ), ] }
  return( results2 )
} 
#
#
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
  allNumbers <- suppressWarnings( !is.na( as.numeric( numberField[numberField != ""] ) ) )
  if( !all( allNumbers ) ) {
    # find indices of values that are not numeric
    not_numeric_indices <- which(!grepl("^\\d+\\.?\\d*$", numberField[numberField]) | is.na(as.numeric(numberField[numberField])))
    cat("\nValues that are not numeric:")
    print( numberField[not_numeric_indices] )
  }
  return ( all( allNumbers ) )
  }

#########################################################################

#Validate that zipcodes are 5 digits in length

areFiveDigits <- function ( zipcodes ){
  zipcode_pattern <- "^\\d{5}$"
  is_valid_zipcode <- all(grepl(zipcode_pattern, data$zipcode))

  if( !is_valid_zipcode){
    not_valid_zipcode_indices <- grep(zipcode_pattern, zipcodes[zipcodes], invert = TRUE)
    cat("\nZipcodes that are not 5 digits:")
    print(  zipcode[not_valid_zipcode_indices] )
  }
  return ( is_valid_zipcode )
}

#########################################################################

#Validate that all fields are in the list of allowable values

areInList <- function ( dataset, listValidValues ){
  dataset <- dataset[dataset != ""]
  inList <- ( dataset %in% listValidValues )
  if( !all( inList ) ) {
    # find indices of zipcodes that are not in the validUSPSzipcodes list
    not_valid_zipcode_indices <- which( !dataset[dataset] %in% listValidValues )
    cat("\nValues that are not in the list of allowable valuest:")
    print( dataset[not_valid_zipcode_indices] )..
  }
  return ( all( inList ) )
}
#########################################################################

##  Create the path to the file containing the 311 Service Request data.

data1File <- file.path("C:", "Users", "david", "OneDrive", "Documents", "DataCleansingProject", "311_Q1_2023.csv") 
data2File <- file.path("C:", "Users", "david", "OneDrive", "Documents", "DataCleansingProject", "USPS_zipcodes.csv") 
data3File <- file.path("C:", "Users", "david", "OneDrive", "Documents", "DataCleansingProject", "NYPDPrecincts2023.csv") 
data4File <- file.path("C:", "Users", "david", "OneDrive", "Documents", "DataCleansingProject", "NYCCityCouncil2023.csv") 

##  The file contains column names in the "header" line.
##  The R "read.csv" function uses a "." to replace the spaces in column names. This makes the column names
##    into legal variables, but the "." can cause problems elsewhere. The function "makeColNamesUserFriendly"
##    replaces the "." with an underscore "_". thus simplifying the field names. Example "Created.date"
##    becomes "Created_date". The function also removes any additional and trailing spaces between column names.
##
##  Additionally, the field names are converted to lower case with the "tolower" function. Thus "Create_date"
##    becomes "create_date".
##
##  These corrections are applied to the column names using the "names" data field created from the header of the file.
##  The new field names, with the "_" character and lower case replace the current field names.
##
#########################################################################

# Load the USPS zipcode file
USPSzipcodes <- read.csv( data2File, header = TRUE, colClasses = rep( "character", ncol( read.csv( data2File ) ) ) )
USPSzipcodes <- makeColNamesUserFriendly( USPSzipcodes )
#cat("\nNumber of zipcodes in the US", format( nrow( USPSzipcodes ), big.mark = ",", scientific = FALSE))

#########################################################################

# Tailor the zipcodes to just those in NY, CT, and NJ
desiredDistricts <- c( "CONNECTICUT", "NEW JERSEY", "NEW YORK 1", "NEW YORK 3", "NEW YORK 2" )
USPSzipcodes <- subset( USPSzipcodes, district_name %in% desiredDistricts)
USPSzipcodesOnly <- USPSzipcodes[, "delivery_zipcode", drop = FALSE]
zipRows <- nrow( USPSzipcodesOnly )

#########################################################################

precinctsNYPD <- read.csv( data3File, header = TRUE, colClasses = rep( "character", ncol( read.csv( data3File ))))
precinctsNYPD <- makeColNamesUserFriendly( precinctsNYPD )
numPrecincts <- nrow( precinctsNYPD )

#########################################################################

cityCouncilNYC <- read.csv( data4File, header = TRUE, colClasses = rep( "character", ncol( read.csv( data3File ))))
cityCouncilNYC <- makeColNamesUserFriendly( cityCouncilNYC )
numCityCouncil <- nrow( cityCouncilNYC )

#########################################################################

# Load the 311 SR data file
d311 <- read.csv( data1File, header = TRUE, colClasses = rep( "character", ncol( read.csv( data1File ))))
d311 <- makeColNamesUserFriendly( d311 )
numRows <- nrow( d311 )
cat("\nNumber of rows in the 311 SR data set:", format( numRows, big.mark = ",", scientific = FALSE), "\n" )

#########################################################################

cat( "Are all the values in the 'created_date' field dates?", areAllDates( d311$created_date ) )
cat( "Are all the values in the 'closed_date' field dates?", areAllDates( d311$closed_date ) )
cat( "\nAre all the values in the 'due_date' field dates?", areAllDates( d311$due_date ) )
cat( "Are all the values in the 'resolution_action_updated_date' field dates?", areAllDates( d311$resolution_action_updated_date ) )

#########################################################################

cat( "Are all the values in the 'incident_zip' field numbers?", areAllNumbers( d311$incident_zip ) )
cat( "Are all the zipcodes in the 'incident_zip' field 5 digitis?", areFiveDigits( d311$incident_zip ) )
cat( "\nAre all the values in the 'x_coordinate_state_plane' field numbers?", areAllNumbers( d311$x_coordinate_state_plane ) )
cat( "Are all the values in the 'y_coordinate_state_plane' field numbers?", areAllNumbers( d311$y_coordinate_state_plane ) )
cat( "\nAre all the values in the 'latitude' field numbers?", areAllNumbers( d311$latitude ) )
cat( "Are all the values in the 'longitude' field numbers?", areAllNumbers( d311$longitude ) )
cat( "\nAre all the values in the 'incident_zip' field numbers?", areAllNumbers( d311$incident_zip ) )
cat( "Are all the values in the 'zip_codes' field numbers?", areAllNumbers( d311$zip_codes ) )
cat( "Are all the zipcodes in the 'zip_codes' field 5 digitis?", areFiveDigits( d311$zip_codes ) )
cat( "n\Are all the values in the 'community_districts' field numbers?", areAllNumbers( d311$community_districts ) )
cat( "Are all the values in the 'borough_boundaries' field numbers?", areAllNumbers( d311$borough_boundaries ) )
cat( "Are all the values in the 'city_council_district' field numbers?", areAllNumbers( d311$city_council_districts ) )
cat( "Are all the values in the 'police_precincts' field numbers?", areAllNumbers( d311$police_precincts ) )

#########################################################################

cat( "Are all the values in the 'borough' field valid?", areInList( d311$borough,  c("BRONX", "BROOKLYN", "MANHATTAN", "QUEENS", "STATEN ISLAND", "Unspecified") ) )
cat( "Are all the values in the 'borough_boundaries' field valid?", areInList( d311$borough_boundaries,  c("1", "2", "3", "4", "5") ) )
cat( "Are all the values in the 'park_borough' field valid?", areInList( d311$park_borough,  c("BRONX", "BROOKLYN", "MANHATTAN", "QUEENS", "STATEN ISLAND", "Unspecified") ) )
cat( "\nAre all the values in the 'open_data_channel_type'valid?", areInList( d311$open_data_channel_type,  c("UNKNOWN", "MOBILE", "ONLINE", "PHONE", "OTHER") ) )
cat( "\nAre all the values in the 'police_precinct valid?", areInList( d311$police_precincts, precinctsNYPD$nypd_precinct ) )
cat( "\nAre all the values in the 'city_council_district valid?", areInList( d311$city_council_districts,  cityCouncilNYC$nyc_city_council ) )

#########################################################################

missingDataPerColumn <- countColumnsMissingData( d311 )
missingDataPerColumn[, sapply( missingDataPerColumn, is.numeric )] <- round( missingDataPerColumn[, sapply(missingDataPerColumn, is.numeric )], 4)
cat( "\n Number and fraction of blank values in each column, sorted descending" )
print( missingDataPerColumn[ order( -missingDataPerColumn[ , 2] ), ])

#########################################################################

# Look for invalid zipcodes in the'incident_zip'field, which is densely populated
badZipcodes1 <- findInvalidZipcodes( USPSzipcodesOnly, d311,
                                  which( colnames( d311 ) == "unique_key" ),
                                  which( colnames( d311 ) == "incident_zip" ),
                                  which( colnames( d311 )  == "agency") )

numBlankincident_zip <- subset(missingDataPerColumn, field == "incident_zip")$'#blanks'
cat("\nThe number of invalid zip codes in the 'incident_zip' field is", format( nrow( badZipcodes1 ), big.mark = ",", scientific = FALSE ), "representing", 
                            percent( nrow( badZipcodes1 )/(numRows - numBlankincident_zip), accuracy = 0.01 ), 
                            "of non-blank data.")
if ( nrow( badZipcodes1 ) > 0 ) {
  print( head( badZipcodes1,15 ) )
  sortedData <- as.data.frame(table(badZipcodes1$agency))
  sortedData <- sortedData[order(-sortedData$Freq),]
  cat("\nSorted by Agency:\n")
  print(sortedData)
}

# Look for invalid zipcodes in the 'zip_codes' field, Expect a large # 
badZipcodes2 <- findInvalidZipcodes( USPSzipcodesOnly, d311,
                                     which( colnames( d311 ) == "unique_key" ),
                                     which( colnames( d311 ) == "zip_codes" ),
                                     which( colnames( d311 ) == "agency" ) )
numBlankzip_codes <- subset(missingDataPerColumn, field == "zip_codes")$'#blanks'
cat( "The number of bad zip codes in 'zip_codes' field is", format( nrow( badZipcodes2 ), big.mark = ",", scientific = FALSE ), "representing", 
     percent( nrow( badZipcodes2 )/numRows, accuracy = 0.01 ), 
     "of non-blank data." )
if ( nrow( badZipcodes2 ) > 0 ) {
  print( head( badZipcodes2, 15 ) )
  cat("\nSorted by Agency:\n")
  sortedData <- as.data.frame(table(badZipcodes2$agency))
  sortedData <- sortedData[order(-sortedData$Freq),]
  print(sortedData)
}

#########################################################################

nonMatchingBouroughs <- mismatchedBoroughs( d311 )
numBlankborough_boundaries <- subset(missingDataPerColumn, field == "borough_boundaries")$'#blanks'
cat( "\nThe number of non-matching boroughs between 'borough' and 'borough_boundaries' is", format( nrow( nonMatchingBouroughs ), big.mark = ",", scientific = FALSE ), 
     "representing", percent( nrow( nonMatchingBouroughs )/( numRows - numBlankborough_boundaries ), accuracy = 0.01 ), 
     "of non-blank data." )
if ( nrow( nonMatchingBouroughs ) > 0 ) { 
  cat("\n Sample of non-mathcing boroughs\n")
  print(head( nonMatchingBouroughs,15 ) )
  cat("\nSorted by Agency:\n")
  sortedData <- as.data.frame(table( nonMatchingBouroughs$agency))
  sortedData <- sortedData[order(-sortedData$Freq),]
  print(sortedData)
}

#########################################################################

##  Change the various date fields to date-time objects and reformat dates.There are four date fields in the 311 data.
numBlankClosedDate <- sum(d311$closed_date =="")
d311$created_date <- convertToDateObject( d311$created_date )
d311$closed_date  <- convertToDateObject( d311$closed_date )
d311$due_date     <- convertToDateObject( d311$due_date )
d311$resolution_action_updated_date <- convertToDateObject( d311$resolution_action_updated_date )

##  Compute and store "duration" in a new additional column for the dataframe "d311".
##  Duration is the time between created_date and closed_date, although due to data errors the value may be negative.
##  Identify the Service Records that were closed before they were create, i.e. bad dates.
d311$duration <- round( as.numeric( difftime( d311$closed_date, d311$created_date, units = "days") ), 2 )

#########################################################################

closedBeforeOpened <- findBadDates( d311,
                                  which( colnames(d311) == "unique_key" ), 
                                  which( colnames(d311) == "created_date" ), 
                                  which( colnames(d311) == "closed_date" ), 
                                  which( colnames(d311) == "duration" ),
                                  which( colnames(d311) == "agency") )

#########################################################################

numBlankClosedDate <- sum(is.na(d311$closed_date))

cat( "The number of SRs 'closed' before 'opened'is ", format( nrow( closedBeforeOpened ), big.mark = ",", scientific = FALSE ), "representing",
     percent( nrow( closedBeforeOpened )/( numRows - numBlankClosedDate), accuracy = 0.01 ), 
     "of non-blank data." )

if ( nrow( closedBeforeOpened ) > 0 ) {
  sortedClosed <- closedBeforeOpened[ order(closedBeforeOpened$duration),]
  cat("\nLargest errors:\n")
  print( head( sortedClosed, 10 ) )
  cat("\nSmallest errors:\n")
  print( tail( sortedClosed, 10))
  print( as.data.frame( table( sortedClosed$agency ) ) )
}

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
d311$latitude <- as.numeric( d311$latitude )
d311$longitude <- as.numeric( d311$longitude )

# Check latitudes & longitudes in 311 data to determine any outliers
badLatitudes <- subset(d311, ( latitude < southernMostLatitude | latitude > northernMostLatitude ) & latitude != "" )
badLongitudes <- subset(d311, ( longitude > easternMostLongitude | longitude < westernMostLongitude ) & latitude != "")

cat("\nThe number of bad latitudes is:", nrow(badLatitudes))
if ( nrow( badLatitudes ) >0 ) { print(head ( badLatitudes$latitude,5 ))}

cat("\nThe number of bad longitudes is", nrow(badLongitudes))
if ( nrow( badLongitudes ) >0 ) { print(head ( badLongitudes$longitude,5 ))}

#########################################################################

uniqueKeys <- length(unique(d311$unique_key)) == nrow(d311)
cat(" Are all the 'unique_key' fields actually unique?", uniqueKeys)

#########################################################################

cat("\n\n\n END OF PROGRAM")
##
#########################################################################
#########################################################################

##
# use colSums() function to check which columns have no values
#blank_cols <- colSums(is.na(d311) | d311 == "") == nrow(d311)

# print the column names that have no values
#names(df)[blank_cols]

# display the unique values and their count for each column
#for (col in names(d311)) {
#  cat("Column:", col, "\n")
#  cat("Unique values and their count:\n")
#  print(table(d311[[col]]))
#  cat("\n")
#}

# display the unique values, their count, and the count of blank or missing fields for each column
#for (col in names(d311)) {
#  cat("Column:", col, "\n")
#  cat("Unique values and their count:\n")
#  print(table(d311[[col]]))
#  cat("Blank/missing fields:", sum(is.na(d311[[col]]) | !nzchar(d311[[col]])), "\n")
#  cat("\n")
#}

# count the unique values of col2 and sort them in descending order
#cat("Counts of unique values in borough:\n")
#print(sort(table(d311$borough, useNA = "ifany"), decreasing = TRUE))

# count the unique values of col2 and sort them in descending order
#cat("Counts of unique values in city:\n")
#print(sort(table(d311$city, useNA = "ifany"), decreasing = TRUE))




## get the duration from creation time to closing time
#t1 <- strptime(d311$closed_date, format = '%m/%d/%Y %I:%M:%S %p')
#t0 <- strptime(d311$created_date, format = '%m/%d/%Y %I:%M:%S %p')
#tt <- as.numeric(difftime(t1, t0, units =  "secs"))


###########################################################
## Exploratory cleaning
###########################################################

## any creation time later than closing time?
#table(tt < 0, useNA = "ifany")



## to be organized later
## mean(tt[d311$agency == "NYPD"]/ 3600 > 3, na.rm = TRUE)

## table(d311$intersection_street_1 == d311$cross_street_1)

## head(subset(d311, d311$intersection_street_1 != d311$cross_street_1, select = c("intersection_street_1", "cross_street_1")))

## d311$cross_street_1 <- ifelse(is.na(d311$cross_street_1), d311$intersection_street_1, d311$cross_street_1)


## library(lubridate)
## library(ggplot2)
## wkday <- ifelse(wday(t0, week_start = 1) > 5, "weekend", "weekday")
## str_df <- na.omit(subset(data.frame(time = tt / 3600, day = wkday, borough = d311$borough),
##                          d311$agency == "NYPD"))
## str_df <- subset(str_df, borough != "Unspecified")
## pdf("nypdtime.pdf", height = 10, width = 15)
## ggplot(str_df, aes(x = borough, y = time, fill = day)) + 
##     geom_violin() + 
##     coord_flip() +
##     ylim(0, 24) +
##     ylab("time to close requests to NYPD (hours)") + 
##     theme(legend.position = "top", 
##           strip.background = element_rect(fill = "grey77", color = "grey77"))
## dev.off()
