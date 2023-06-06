#########################################################################
install.packages("readr")
library(readr)

#########################################################################
# Load the 311 SR data file
setwd("C:/Users/david/OneDrive/Documents/nyc311clean/code")

data1File <- file.path( "..","data", "311_test_2023.csv")
d311 <- read.csv( data1File, header = TRUE, colClasses = rep( "character", ncol( read.csv( data1File ))))

#output_file <- file.path("..", "data", "311_test_2023.csv")
#write.csv(d311, file = output_file, row.names = FALSE)


#data1File <- file.path( "..","data", "311_test_2023.csv")
#testd311 <- read.csv( data1File, header = TRUE, colClasses = rep( "character", ncol( read.csv( data1File ))))

#are_identical <- identical(d311, testd311)

#if (are_identical) {
#  print("The dataframes are identical.")
#} else {
#  print("The dataframes are not identical.")


#data1File <- file.path( "..","data", "311_test_2023.csv")
#file_path <- data1File
#encoding <- guess_encoding(file_path)$encoding

#print(encoding)

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
 # Replace Agency name with agency_code. Ensure data dictionary is updated
 
#cat("\nMemory size of orignial d311$agency column with names.")
#object.size(smalld311$agency)
#object_size_before <- object.size(smalld311$agency)
#formatted_size_before <- prettyNum(object_size_before, big.mark = ",", preserve.width = "individual")
#formatted_size_before <- paste(formatted_size_before, "bytes")
#print(formatted_size_before)

 
smalld311freq_agency <- table(smalld311$agency)
sort(smalld311freq_agency, decreasing = TRUE)
agency_code <- rep(seq(length(smalld311freq_agency)), smalld311freq_agency)
smalld311$agency <- agency_code
 
#print(smalld311freq_agency )
#print(table(agency_code))
agency_names <- names(smalld311freq_agency)
agency_numbers <- names(table(agency_code))
namesTOnumbers <- paste(agency_names , agency_numbers, sep = ": ")
for (i in namesTOnumbers) { cat(i, "\n") }
 
#cat("\nMemory size of revised d311$agency column with corresponding numbers.")
#object.size(smalld311$agency)
#object_sizex_after <- object.size(smalld311$agency)
#formatted_size_after <- prettyNum(object_size_after, big.mark = ",", preserve.width = "individual")
#formatted_size_after <- paste(formatted_size_after, "bytes")
#print(formatted_size_after)

#########################################################################
output_file <- file.path("..", "data", "smalld311.csv")
write.csv(smalld311, file = output_file, row.names = FALSE)
cat("\n\n***** END OF PROGRAM *****")
