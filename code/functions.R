#########################################################################
##  Install necessary packages
## if (!require("scales")) install.packages("scales")
## if (!require("stringr")) install.packages("stringr")
## library(scales)
## library(stringr)

#########################################################################
##  Utility functions
#########################################################################
##  This function makes standard column names even if there are multiple "."s and trailing "."s
##  This leaves the column names with spaces replaced by an underscore "_", i.e. nicer names.
makeColNamesUserFriendly <- function(names) {
    ## Convert any number of consecutive "."s to an underscore.
    names <- gsub(x = names, pattern = "(\\.)+", replacement = "_")
    ## Drop the trailing "."s
    names <- gsub(x = names, pattern = "(_)+$", replacement = "")
    ## Convert to lower case. 
    tolower(names)
}

#########################################################################
##
##  This function converts a datetimestamp in character format to a date object
##  in a slightly different format.
##  The 311 data set is read into the d311 dataframe as all character string.
##  It is necessary to convert these dates from string to a date object to compute duration.
##  The data is also reformatted to a different format using YYY-MM-DD and a 24-hour clock.
convertToDateObject <- function(dateString) {

  ## Convert the date string ("02/01/2023 11:44:53 PM") to a date-time object
  dateTimeObject <- strptime(dateString, format = "%m/%d/%Y %I:%M:%S %p")

  ## Convert the new date-time object to a new string in a new format ("2023-02-02 02:52:50") 
  newString <- format(dateTimeObject, format = "%Y-%m-%d %H:%M:%S")

  ## Convert the new string to a date-time object using POSIXct. This will enable calculation of duration.
  newDateTimeObject <- as.POSIXct(newString, tz = "EST", format = "%Y-%m-%d %H:%M:%S")

  ## Return the revised variable in date-time object now formatted as "2023-04-26 21:50:45"
  return(newDateTimeObject)
}

#########################################################################
## Function to count the number of blanks in each column of the 311 dataframe
charColumnsMissingData <- function(dataset) {
    charData <- dataset[, sapply(dataset, data.class) == "character"]
    blanks <- sapply(charData, function(x) sum(x == ""))
    unspecifieds <- sapply(charData, function(x) sum(tolower(x) == "unspecified"))
    unknowns <- sapply(charData, function(x) sum(tolower(x) == "unknown"))
    data.frame(blanks, unspecifieds, unknowns)
}

numColumnsMissingData <- function(dataset) {
    numData <- dataset[, sapply(dataset, data.class) == "numeric"]
    nas <- sapply(numData, function(x) sum(is.na(x)))
    nas
}
