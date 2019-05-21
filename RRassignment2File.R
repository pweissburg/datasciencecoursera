
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
data$BGN_DATE <- mdy_hms(data$BGN_DATE)
data <- mutate(data,year = year(BGN_DATE))
names <- names(data)
### Run this section to test that the replacement worked ###
evTYPE <- tolower(sort(data$EVTYPE))
evTYPEu <- evTYPE %>%
            unique() %>%
            str_trim() %>%
            sort()
View(evTYPEu)
############################################################
# Astronomical Low Tide

# Avalanche
data$EVTYPE <- gsub("avalance","avalanche",data$EVTYPE)

# Blizzard
r <- grep(".*blizz.*",data$EVTYPE)
data$EVTYPE[r] <- "blizzard"

# Coastal Flood
r <- grep(".*coast.*",data$EVTYPE)
data$EVTYPE[r] <- "coastal flood"

# Cold/Wind Chill
c <- grep(".*cold.*",
          data$EVTYPE,value = T)
unique(c)
r <- grep(".*record.* cold.* | .*extreme.*  cold.*|.*excessive.* cold.*",
      data$EVTYPE,value = T)
unique(r)
data$EVTYPE[r] <- "coastal flood"


