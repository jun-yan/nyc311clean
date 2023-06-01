library(stringr)

source("../code/functions.R")
##  Create the path to the file containing the 311 Service Request data.
data1File <- file.path("../data", "test_sample4.csv") # 311_202303.csv")

#########################################################################
## Load the 311 SR data file
d311 <- read.csv(data1File, header = TRUE)
names(d311) <- makeColNamesUserFriendly(names(d311))

## number of rows
numRows <- nrow( d311 )

## counts by agency
sort(table(d311$agency), decreasing = TRUE)

#########################################################################
## check for missing info

cMissingDataPerColumn <- charColumnsMissingData(d311)
cMissingDataPerColumn[order(-rowSums(cMissingDataPerColumn)), ]


nMissingDataPerColumn <- numColumnsMissingData(d311)
sort(nMissingDataPerColumn, decreasing = TRUE)


#########################################################################
## any creation date later than closing date
t1 <- strptime(d311$closed_date, format = '%m/%d/%Y %I:%M:%S %p')
t0 <- strptime(d311$created_date, format = '%m/%d/%Y %I:%M:%S %p')
tt <- as.numeric(difftime(t1, t0, units =  "secs"))

badTime <- subset(d311, tt <= 0, select = c("created_date", "closed_date", "agency"))
nrow(badTime)
sum(tt == 0, na.rm = TRUE)
summary(tt[tt < 0 & (!is.na(tt))])
table(badTime$agency)

#########################################################################
## Check for allowable values
validBorough <- c("BRONX", "BROOKLYN", "MANHATTAN", "QUEENS", "STATEN ISLAND")
badBorough <- subset(d311, ! borough %in% validBorough,
                     select = c("borough", "agency"))
head(table(badBorough$agency))

validBB <- c("1", "2", "3", "4", "5")
badBB <- subset(d311, ! borough_boundaries %in% validBB,
                select = c("borough_boundaries", "agency"))
head(table(badBB$agency))

badPark_borough <- subset(d311, ! park_borough %in% validBorough,
                          select = c("park_borough", "agency"))
head(table(badPark_borough$agency))

validChannel <- c("MOBILE", "ONLINE", "PHONE")
badChannel_type <- subset(d311, ! open_data_channel_type %in% validChannel,
                          select = c("open_data_channel_type", "agency"))
head(table(badChannel_type$agency))

validCCD <- scan("../data/NYCCityCouncil2023.csv", skip = 1)
badCCD <- subset(d311, ! city_council_districts %in% validCCD,
                 select = c("city_council_districts", "agency"))
head(badCCD)
table(badCCD$agency)

validPolice_precincts <- scan("../data/NYPDPrecincts2023.csv", skip = 1)
badPolice_precincts <- subset(d311, ! police_precincts %in% validPolice_precincts,
                              select = c("police_precincts", "agency"))
sort(table(badPolice_precincts$agency), descreasing = TRUE)

#########################################################################
## Look for invalid zipcodes in the'incident_zip'field, which is densely populated
validZipcodes <- scan("../data/NYCzipcodes.csv")

badIncident_zip <- subset(d311, ! incident_zip %in% validZipcodes,
                          select = c("incident_zip", "agency"))
head(sort(table(badIncident_zip$incident_zip), decreasing = TRUE))

## Look for invalid zipcodes in the mysterious 'zip_code' field
badZipcode  <- subset(d311, ! zip_codes %in% validZipcodes,
                      select = c("zip_codes", "agency"))
nrow(badZipcode)  # mostly invalid!
head(sort(table(badZipcode$zip_codes), decreasing = TRUE))


#########################################################################
## Check for redundant columns
#########################################################################

## lat, long, and location
head(d311[, c("latitude", "longitude", "location")])

## 'borough' and 'borough_boundaries' are almost identical
nonMatchingBorough <- subset(
    d311, borough_boundaries !=
          as.integer(str_replace_all(borough,
                                     c("STATEN ISLAND" = "1",
                                       "BROOKLYN"      = "2",
                                       "QUEENS"        = "3",
                                       "MANHATTAN"     = "4",
                                       "BRONX"         = "5",
                                       "UNSPECIFIED"   = NA))),
    select = c("borough", "borough_boundaries", "agency"))
nrow(nonMatchingBorough)

## 'borough' and 'park_borough' are identical
any(d311$borough != d311$park_borough)

## 'cross_street_1' and 'intersection_street_1'
nrow(subset(d311, cross_street_1 == "" & intersection_street_1 != ""))
nrow(subset(d311, cross_street_1 != "" & intersection_street_1 == ""))

## if both not empty, they are almost the same
head(subset(d311, cross_street_1 != "" & intersection_street_1 != "" &
                  cross_street_1 != intersection_street_1,
            select = c("cross_street_1", "intersection_street_1", "agency")))

## after removing extra spaces, most of them are the same
dupXStreets <-subset(
    d311, gsub(" ", "", cross_street_1) ==
          gsub(" ", "", intersection_street_1),
    select = c("cross_street_1", "intersection_street_1", "agency"))
head(dupXStreets)
table(dupXStreets$agency)
head(subset(dupXStreets, cross_street_1 != intersection_street_1))


##########################################################################
## Inefficient Storage
##########################################################################

## code borough by integer
object.size(d311$borough_boundaries)
object.size(d311$borough)

## code agency by integer
object.size(d311$agency)
agency_code <- rep(1:14, table(d311$agency))
object.size(agency_code)

