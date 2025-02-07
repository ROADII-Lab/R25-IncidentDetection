# Tennesee data initial formatting. Prepares the TITAN crash data and special events data provided by THP.
# The specific version of the TITAN crash data is in the code here, update as necessary
# Read in raw data files, clean up variables, apply time zones, and save as .RData files for next steps. 

# Setup ----
# Check for package installations

codeloc <- "~/SDI_Waze"

source(file.path(codeloc, 'utility/get_packages.R')) # Can be time-consuming when run first time

library(tidyverse) 
library(lubridate)
library(geonames) # for time zone work
library(httr)
library(readxl) # read excel
library(foreach)
library(doParallel)

localdir <- normalizePath("~/TN/workingdata/TN") # full path for readOGR

setwd(localdir)

# Get Waze data from S3 bucket if on the Secure Data Commons ----
if(ON_SDC){
  
yearmonths = c(
  paste(2017, formatC(4:12, width = 2, flag = "0"), sep="-"),
  paste(2018, formatC(1:12, width = 2, flag = "0"), sep="-"))

tn.ls = paste0("TN_", yearmonths, ".RData")

for(i in tn.ls){
  if(length(grep(i, dir(file.path('~', 'TN', 'Waze'))))==0){
    
    system(paste("aws s3 cp",
                 file.path(teambucket, 'TN', i),
                 file.path('~', 'TN', 'Waze', i)))
    
  }
}

}
# <><><><><><><><><><><><><><><><><><>
# Crash ----

# Read from vwCollision.txt
# First time need to read from txt, afterwards can read from RData, ~ 2 seconds vs 5 minutes. Every column is read as character.
if(length(grep("TN_Crash.RData", dir('Crash')))==0){
  crash <- read.csv("Crash/TITAN_Crash_181108/vwCollision.txt", sep = '|', stringsAsFactors = F)
  # format(object.size(crash), 'Mb')
  save(crash, file = "Crash/TN_Crash.RData")
} else {
  load('Crash/TN_Crash.RData')
}

# select the necessary columns for analysis (optional: save it as subset in the RData again?)
var <- c("MstrRecNbrTxt" # Unique crash ID
         , "CollisionDte" # Date of Crash
         , "CollisionTimeTxt" # Time of Crash
         , "NbrUnitsNmb" # Number of vehicles involved
         , "NbrFatalitiesNmb" # Fatals
         , "NbrInjuredNmb" # Injuries
         , "NbrNonInjuredNmb" # Non-Inguries
         , "NbrMotoristsNmb" # Number of drivers
         , "AlcoholInd" # whether alchohol is involved
         , "BlockNbrTxt" # Does not look like Census block
         , "CityCde" # City ID
         , "CountyStateCde" # County ID
         , "CrashTypeCde"  # What crash type do they have?
         , "IntersectionInd" # Whether it is an intersection             
         , "IntersectLocalIDTxt" # Intersection Location ID
         , "IntersectRoadNameTxt" # Intersection road name
         , "IntersectRoadNameSuffixTxt" # Name suffix
         , "IntersectRoadNbrTxt" # Intersection road/route number
         , "LatDecimalNmb" # Lat
         , "LongDecimalNmb" # Lon
         , "LightConditionCde" # light condition
         ) 
crash <- crash[,var]

# Data completeness
# Further reduce the data
var1 <- c("MstrRecNbrTxt", "CollisionDte", "LatDecimalNmb", "LongDecimalNmb") # only take ID, time, and location
crash1 <- crash[, var1]
crash1[!complete.cases(crash1),] # display rows with missing data, looks like most of them are just missing the geo-coordinates.

colSums(is.na(crash1)) # Number of missing data.
colSums(is.na(crash1))*100/nrow(crash1) # percent of missing data, ~29% missing Lat/Lon

# Get columns in the right format
numcol <- grep("Nmb$", names(crash))
for(i in numcol){
  crash[,i] <- as.numeric(crash[,i])
  }

# Timezone ----
# Here applying a timezone shapefile. First, filter out bad lat/long values, and bad record numbers

crash <- crash %>% filter(LatDecimalNmb > 25 & LatDecimalNmb < 45 &
                               LongDecimalNmb < -75 & LongDecimalNmb > -99 &
                               nchar(MstrRecNbrTxt) == 9)

# Read tz file
tz <- readOGR(file.path(localdir, 'Shapefiles'), layer = 'combined-shapefile')

# Project to Albers equal area, ESRI 102008
proj <- showP4(showWKT("+init=epsg:102008"))
# USGS version of AEA. Use this for all projections for consistency; this is what the hexagon layer is in 
proj.USGS <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"

tz <- spTransform(tz, CRS(proj.USGS))

# Project crashes

crash.proj <- SpatialPointsDataFrame(coords = crash[c('LongDecimalNmb', 'LatDecimalNmb')],
                                     data = crash,
                                     proj4string = CRS("+proj=longlat +datum=WGS84"))

crash.proj <- spTransform(crash.proj, CRS(proj.USGS))

# Overlay timezone on crashes
crash_tz <- over(crash.proj, tz[,"tzid"]) # Match a tzid name to each row in crash.proj 


crash.proj@data <- data.frame(crash.proj@data, tz = crash_tz)

crash <- crash.proj@data %>% filter(tzid == "America/Chicago" | tzid == "America/New_York")

pdf(file = 'Figures/Crash_TZ.pdf', width = 10, height = 10)
  par(mar = rep(0, 4))
  plot(crash.proj, cex = 0) 
  plot(crash.proj[crash.proj$tzid == "America/Chicago" ,], add = T, 
       col = scales::alpha("midnightblue", 0.01))
  plot(crash.proj[crash.proj$tzid == "America/New_York" ,], add = T, 
       col = scales::alpha("firebrick", 0.01))
  plot(crash.proj[crash.proj$tzid == "Antarctica/McMurdo" ,], add = T, col = "red", 
       cex = 3)
  plot(tz, add = T)
dev.off()

# Now make date/time using correct time zone. Need to loop because strptime does not accept vectors of mixed time zones. Parallelized.
crash$tzid = as.character(crash$tzid)

cl <- makeCluster(parallel::detectCores())
registerDoParallel(cl)

StartTime <- Sys.time()

foreach(i = 1:nrow(crash)) %dopar% {
  crash$date[i] = as.POSIXct(strptime(crash$CollisionDte[i], "%Y-%m-%d %H:%M:%S", 
                                      tz = crash$tzid[i]))
  crash$year[i] = as.numeric(format(crash$date[i], "%Y"))
  }
EndTime <- Sys.time() - StartTime
cat(round(EndTime, 2), attr(EndTime, "units"), "\n")
stopCluster(cl)

crash %>% 
  group_by(tzid) %>%
  summarize(min = min(date),
            max = max(date),
            n = length(date))

save("crash", file = "Crash/TN_Crash_Simple_2008-2018.RData")

if(ON_SDC){
  # Transfer the file to team bucket
  system(paste("aws s3 cp",
               file.path(localdir, 'Crash',"TN_Crash_Simple_2008-2018.RData"),
               file.path(teambucket, "TN", "Crash", "TN_Crash_Simple_2008-2018.RData")))
}

# <><><><><><><><><><><><><><><><><><>
# 2019 Special Events ----
# Includes all events from 2010-2019
spev <- read_excel("SpecialEvents/2019_Special_Events.xlsx")

spev <- spev %>% 
  filter(!is.na(Event_Date) & Event_Date >= '2019-01-01' & Event_Date <= '2019-12-31' &
         !is.na(Lat) & !is.na(Lon)) %>%
  mutate(Lat = as.numeric(Lat),
         Lon = as.numeric(Lon))
# The special event are from Jan 2019 - Dec 2019.

# spev$StartTime <- as.POSIXct(strptime(spev$StartTime, format = "%Y-%m-%d %H:%M:%S"))
spev$StartTime <- format(ymd_hms(spev$StartTime), "%H:%M:%S")
spev$EndTime <- format(ymd_hms(spev$EndTime), "%H:%M:%S")

# Assign time zone to each special event.
# Get timezone of event - TN has 2 timezones (eastern and central) - use geonames package 
spev$TimeZone <- NA

# Project spev
spev.proj <- SpatialPointsDataFrame(coords = spev[c('Lon', 'Lat')],
                                     data = spev,
                                     proj4string = CRS("+proj=longlat +datum=WGS84"))

spev.proj <- spTransform(spev.proj, CRS(proj.USGS))

# Overlay timezone on crashes
spev_tz <- over(spev.proj, tz[,"tzid"]) # Match a tzid name to each row in spev.proj 

spev.proj@data <- data.frame(spev.proj@data, tz = spev_tz)

spev <- spev.proj@data %>% filter(tzid == "America/Chicago" | tzid == "America/New_York")

## save special event data in the output
save("spev", file = "SpecialEvents/TN_SpecialEvent_2019.RData")

