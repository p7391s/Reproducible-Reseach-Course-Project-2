library("data.table")

# path <- getwd()

# downloading data
url_data <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"

file_data <- "StormData.csv.bz2"
if (!file.exists(file_data)) {
        download.file(url_data, file_data, mode = "wb")
}

# reading data
storm_data <- read.csv(file = file_data, header=TRUE, sep=",")

# dimention
dim(storm_data)

# first 6 row
head(storm_data)

# summare of storm_data
summary(storm_data)

# examining column names
colnames(storm_data)

# create subsetting by date
main_data <- storm_data

main_data$BGN_DATE <- strptime(storm_data$BGN_DATE, "%m/%d/%Y %H:%M:%S")
main_data <- subset(main_data, BGN_DATE > "1995-12-31")

# select variables
main_data <- subset(main_data, select = c(EVTYPE, FATALITIES, INJURIES, PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP))

# cleaning event types names
main_data$EVTYPE <- toupper(main_data$EVTYPE)

# eliminating zero data
main_data <- main_data[main_data$FATALITIES != 0 |
	main_data$INJURIES != 0 |
	main_data$PROPDMG != 0|
	main_data$CROPDMG != 0, ]

# unique event types
unique(main_data$EVTYPE)

# total people loss
health_data <- aggregate(cbind(FATALITIES, INJURIES) ~ EVTYPE, data = main_data, FUN=sum)
health_data$PEOPLE_LOSS <- health_data$FATALITIES + health_data$INJURIES
health_data <- health_data[order(health_data$PEOPLE_LOSS, decreasing = TRUE), ]
Top10_events_people <- health_data[1:10,]
print(Top10_events_people)

# transform letters to numbers

main_data$PROPDMGEXP <- gsub("[Hh]", "2", main_data$PROPDMGEXP)
main_data$PROPDMGEXP <- gsub("[Kk]", "3", main_data$PROPDMGEXP)
main_data$PROPDMGEXP <- gsub("[Mm]", "6", main_data$PROPDMGEXP)
main_data$PROPDMGEXP <- gsub("[Bb]", "9", main_data$PROPDMGEXP)
main_data$PROPDMGEXP <- gsub("\\+", "1", main_data$PROPDMGEXP)
main_data$PROPDMGEXP <- gsub("\\?|\\-|\\ ", "0",  main_data$PROPDMGEXP)
main_data$PROPDMGEXP <- as.numeric(main_data$PROPDMGEXP)

main_data$CROPDMGEXP <- gsub("[Hh]", "2", main_data$CROPDMGEXP)
main_data$CROPDMGEXP <- gsub("[Kk]", "3", main_data$CROPDMGEXP)
main_data$CROPDMGEXP <- gsub("[Mm]", "6", main_data$CROPDMGEXP)
main_data$CROPDMGEXP <- gsub("[Bb]", "9", main_data$CROPDMGEXP)
main_data$CROPDMGEXP <- gsub("\\+", "1", main_data$CROPDMGEXP)
main_data$CROPDMGEXP <- gsub("\\-|\\?|\\ ", "0", main_data$CROPDMGEXP)
main_data$CROPDMGEXP <- as.numeric(main_data$CROPDMGEXP)

main_data$PROPDMGEXP[is.na(main_data$PROPDMGEXP)] <- 0
main_data$CROPDMGEXP[is.na(main_data$CROPDMGEXP)] <- 0

#creating total damage values

library(dplyr)

main_data <- mutate(main_data, 
                    PROPDMGTOTAL = PROPDMG * (10 ^ PROPDMGEXP), 
                    CROPDMGTOTAL = CROPDMG * (10 ^ CROPDMGEXP))

#analyzing
economic_data <- aggregate(cbind(PROPDMGTOTAL, CROPDMGTOTAL) ~ EVTYPE, data = main_data, FUN=sum)
economic_data$ECONOMIC_LOSS <- economic_data$PROPDMGTOTAL + economic_data$CROPDMGTOTAL
economic_data <- economic_data[order(economic_data$ECONOMIC_LOSS, decreasing = TRUE), ]
Top10_events_economy <- economic_data[1:10,]
print(Top10_events_economy)

#plotting health loss
library(ggplot2)

g <- ggplot(data = Top10_events_people, aes(x = reorder(EVTYPE, PEOPLE_LOSS), y = PEOPLE_LOSS))
g <- g + geom_bar(stat = "identity", colour = "green", fill = "darkgreen")
g <- g + labs(title = "Total people loss in USA by weather events in 1996-2011")
g <- g + theme(plot.title = element_text(hjust = 0.5))
g <- g + labs(y = "Number of fatalities and injuries", x = "Event Type")
g <- g + coord_flip()
print(g)

#plotting economic loss

g <- ggplot(data = Top10_events_economy, aes(x = reorder(EVTYPE, ECONOMIC_LOSS), y = ECONOMIC_LOSS))
g <- g + geom_bar(stat = "identity", colour = "red", fill = "darkred")
g <- g + labs(title = "Total economic loss in USA by weather events in 1996-2011")
g <- g + theme(plot.title = element_text(hjust = 0.5))
g <- g + labs(y = "Size of property and crop loss", x = "Event Type")
g <- g + coord_flip()
print(g)

