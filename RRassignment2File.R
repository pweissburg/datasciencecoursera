
library(dplyr)
library(tidyverse)
library(lubridate)
library(stringr)

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
#r <- grep(".*coast.*",data$EVTYPE)
coastalFloods <- grep("(.*)beach(.*)flood(.*)|(.*)cstl(.*)|(.*)coastal(.*)|(.*)tidal(.*)|(.*)coast(.*)",data$EVTYPE,value=F)
#length(coastalFloods)
data$EVTYPE[coastalFloods] <- "coastal flood"

# Cold/Wind Chill

# Identify all EVTYPEs that contain 'cold'
cold <- grep("(.*)cold(.*)",data$EVTYPE,value = F)
# Identify those that are extreme cold
vCold <- grep("(.*)record(.*)cold(.*)|(.*)extreme(.*)cold(.*)|(.*)excessive(.*)cold(.*)",
      data$EVTYPE,value = F)
# Remove the extreme cold values from the cold values vector
coldWindChill <- setdiff(cold,vCold)
# Close the deal
data$EVTYPE[coldWindChill] <- "cold/wind chill"

# Debris Flow (landslides)
# Identify those that are landslides/mudslides
landSlides <- grep("(.*)slide(.*)",data$EVTYPE,value = F)
data$EVTYPE[landSlides] <- "debris flow"

# Dense Fog
# Identify those that are fog-related
fog <- grep("(.*)fog(.*)",data$EVTYPE,value = F)
# unique(fog) -  used to spot check work with value = T on the above line
iceFog <- grep("(.*)freezing fog(.*)|(.*)ice fog(.*)",data$EVTYPE,value = F)
#unique(iceFog)
denseFog <- setdiff(fog,iceFog)
data$EVTYPE[denseFog] <- "dense fog"

# Dense Smoke
# Identify those that are smoke-related
denseSmoke <- grep("(.*)smoke(.*)",data$EVTYPE,value = F)
#unique(denseSmoke)
data$EVTYPE[denseSmoke] <- "dense smoke"

# Drought
# Identify those that are drought-related
dry <- grep("(.*)drought(.*)|(.*)dry(.*)|(.*)driest(.*)",data$EVTYPE,value = F)
# Remove dry microbursts (this is wind, not a drought)
microBursts <- grep("(.*)microburst(.*)",data$EVTYPE,value = F)
drought <- setdiff(dry,microBursts)
data$EVTYPE[drought] <- "drought"

# Dust Devil
# Identify those that are dust devil-related
dustDevil <- grep("(.*)dev[i,e]l(.*)",data$EVTYPE,value = F)
#unique(dustDevil)
data$EVTYPE[dustDevil] <- "dust devil"

# Dust Storm
# Identify those that are dust storm-related
dust <- grep("(.*)dust(.*)",data$EVTYPE,value = F)
unique(dust)
# Remove dust devils (this is a different category)
dustDevils <- grep("(.*)devil(.*)",data$EVTYPE,value = F)
dustStorm <- setdiff(dust,dustDevils)
data$EVTYPE[dustStorm] <- "dust storm"

# Extreme Heat
# Identify all EVTYPEs that pertain to extreme heat
eHeat <- grep("(.*)record(.*)heat(.*)|(.*)record(.*)warm(.*)|(.*)extreme(.*)heat(.*)|(.*)excessive(.*)heat(.*)",
              data$EVTYPE,value = F)
data$EVTYPE[eHeat] <- "extreme heat"

# Extreme Cold/Wind Chill
# Identify those that are extreme cold
vCold <- grep("(.*)record(.*)cold(.*)|(.*)extreme(.*)cold(.*)|(.*)excessive(.*)cold(.*)",
              data$EVTYPE,value = F)
data$EVTYPE[vCold] <- "extreme cold/wind chill"

# Freezing Fog
# Identify those that are freezing fog-related
freezingFog <- grep("(.*)freezing fog(.*)|(.*)ice fog(.*)",data$EVTYPE,value = F)
data$EVTYPE[freezingFog] <- "freezing fog"

# Heat
# Identify all EVTYPEs that pertain to heat
allHeat <- grep("(.*)heat(.*)|(.*)hot(.*)|(.*)warm(.*)",data$EVTYPE,value = F)
length(allHeat)
# Identify those that are extreme heat
eHeat <- grep("(.*)record(.*)heat(.*)|(.*)record(.*)warm(.*)|(.*)extreme(.*)heat(.*)|(.*)excessive(.*)heat(.*)",
              data$EVTYPE,value = F)
length(eHeat)
# Remove the extreme cold values from the cold values vector
heat <- setdiff(allHeat,eHeat)
length(heat)
# Close the deal
data$EVTYPE[heat] <- "heat"

# Flash Flood
# Identify those that are flash flood-related
flashFlood <- grep("(.*)flash(.*)flood(.*)|(.*)floood(.*)|(.*)flash(.*)",data$EVTYPE,value = F)
#unique(flashFlood)
data$EVTYPE[flashFlood] <- "flash flood"

# Flood
# Identify those that are flood-related
allFlood <- grep("(.*)flood(.*)",data$EVTYPE,value = F)
#unique(allFlood)
waterFloods <- grep("flash flood|(.*)beach(.*)flood(.*)|(.*)cstl(.*)|(.*)coastal(.*)|(.*)lakeshore(.*)|(.*)tidal(.*)",data$EVTYPE,value=F)
#unique(waterFloods)
inlandFloods <- setdiff(allFlood,waterFloods)
length(inlandFloods)
data$EVTYPE[inlandFloods] <- "flood"

# Lakeshore Flood
# Identify those that are lakeshore flood-related
lakeshoreFlood <- grep("lakeshore flood",data$EVTYPE,value=F)
#unique(lakeshoreFlood)
data$EVTYPE[lakeshoreFlood] <- "lakeshore flood"
