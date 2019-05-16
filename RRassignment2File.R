library(dplyr)
library(tidyverse)
library(lubridate)

setwd("C:/Users/pweissburg/OneDrive - World Business Lenders/Documents/")
# 1. Load data into R
# Download file from URL
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2",temp)
# Load data into R
dataInitial <- as.tbl(read.csv("repdata_data_StormData.csv.bz2",stringsAsFactors = FALSE))
data <- dataInitial
names(data)
data$BGN_DATE <- mdy_hms(data$BGN_DATE)
data <- mutate(data,year = year(BGN_DATE))

#### filtered out data previous to 1993 ####

data <- filter(data,year == 1993)
data4Plot <- data %>% group_by(EVTYPE) %>% summarise(events = count(year))
data4Plot <- arrange(data4Plot,desc(events))
head(data4Plot)
ggplot(head(data4Plot,10),aes(head(data4Plot$EVTYPE,10),head(data4Plot$events,10)))+geom_bar(stat = "identity")

qplot(head(data4Plot$EVTYPE,25),head(data4Plot$events,25))

