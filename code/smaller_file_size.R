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

#retain original data, use new dataframe for analysis
 smalld311<- d311

#########################################################################
# Drop redundant columns (park_borough, borough_boundaries,intersection_street_1, intersection_street_2, location)
 smalld311 <- subset(smalld311, select = -c(park_borough, borough_boundaries, intersection_street_1, intersection_street_2, location))

 #########################################################################
 # Drop redundant columns (park_borough, borough_boundaries,intersection_street_1, intersection_street_2, location)
 