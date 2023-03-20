## import the data and clean the column names

d311 <- read.csv("../data/nyc311_011523-012123_by022023.csv", header = TRUE)
names(d311) <- tolower(gsub("\\.", "_", names(d311)))


## get the duration from creation time to closing time
t1 <- strptime(d311$closed_date, format = '%m/%d/%Y %I:%M:%S %p')
t0 <- strptime(d311$created_date, format = '%m/%d/%Y %I:%M:%S %p')
tt <- as.numeric(difftime(t1, t0, units =  "secs"))

###########################################################
## Exploraty cleaning
###########################################################

## any creation time later than closing time?
table(tt < 0, useNA = "ifany")



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

