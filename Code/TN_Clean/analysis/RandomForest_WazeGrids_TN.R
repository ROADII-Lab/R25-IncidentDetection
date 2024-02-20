# Random forest models of crash estimation for TN
# 2019-02: Now focusing just on TN_crash as response, stil with both grid IDs. 
# Friday to THursday model week

# Setup ---- 
rm(list=ls()) # Start fresh
library(randomForest)
library(foreach) # for parallel implementation
library(doParallel) # includes iterators and parallel
library(tidyverse)

inputdir <- file.path(getwd(),"Input")
outputdir <-file.path(getwd(),"Output")

source('utility/get_packages.R') # installs necessary packages

grids = c("TN_01dd_fishnet",
          "TN_1sqmile_hexagons")

source("utility/wazefunctions_TN.R") 

# Make outputdir if not already there
if(!dir.exists(outputdir)) { dir.create(outputdir) }

# <><><><><>
g = grids[1] # start with square grids, now running hex also. Change between 1 and 2.
state = "TN"
# <><><><><>


# Manually setting months to run here
do.months = c(paste("2017", c("04","05","06","07","08","09", "10", "11", "12"), sep="-"),
              paste("2018", c("01","02","03"), sep="-"))

# do.months = paste("2018", c("01","02","03"), sep="-")

# read random forest function, do.rf()
source("analysis/RandomForest_WazeGrid_Fx.R")

Waze_Prepared_Data = paste0(state, '_', do.months[1], '_to_', do.months[length(do.months)], '_', g)

# <><><><><><><><><><><><><><><><><><><><><><><><>
# Start data prep if the data for this time period and grid size are not ready yet, otherwise this will read from prepared data

if(length(grep(Waze_Prepared_Data, dir(outputdir))) == 0){
  
# rename data files by month. For each month, prep time and response variables
# See prep.hex() in wazefunctions.R for details.
for(mo in do.months){
  prep.hex(paste0("WazeTimeEdtHexAll_", mo, "_", g,".RData"), state = state, month = mo)
}

# Plot to check grid IDs. Requires shapefiles to be present in ~/workingdata/Hex, download from S3 if not there.
# This was useful when testing different grid sizes, to make sure everything was matching correctly.
CHECKPLOT = F
if(CHECKPLOT){
  usemonth = do.months[1]
  usedata <- ls()[grep(sub("-", "_", usemonth), ls())]
  
  grid_shp <- rgdal::readOGR(file.path(inputdir, "Shapefiles"), layer = g)
  
  w.g <- match(get(usedata)$GRID_ID, grid_shp$GRID_ID)
  w.g <- w.g[!is.na(w.g)]
  gs <- grid_shp[w.g,]
  
  plot(gs, col = "red")
  rm(w.g, gs, grid_shp)
}

na.action = "fill0" # This is an argument in append.hex, below. Other options are 'omit' or 'keep'.
monthfiles = paste("w", do.months, sep=".")
monthfiles = sub("-", "_", monthfiles)

# Append supplemental data ----

# Both 2017 and 2018 special event data now
source("utility/Prep_SpecialEvents.R") # gives spev.grid.time and spev.grid.time.holiday. Prep of 1sqmile ~ 4 hours, 01dd ~ 5 min.

source("utility/Prep_HistoricalCrash.R") # gives crash

source("utility/Prep_HistoricalWeather.R") # gives wx.grd.day. Weather variables, by grid ID, by day. Takes ~ 2 hrs for 0.1 dd on 16 core instance.
# Run for 1sqmile hex, after optimizing now approx 20 hours.

# Add prepared special events and historical crash data, with grid ID
for(w in monthfiles){ # w = "w.2017_04"
   cat(w, ". ")
   append.hex(hexname = w, data.to.add = "TN_SpecialEvent", state = state, na.action = na.action)

   append.hex(hexname = w, data.to.add = "crash", state = state, na.action = na.action)
   
   append.hex(hexname = w, data.to.add = "wx.grd.day", state = state, na.action = na.action)
 
}

# Bind all months together
w.allmonths.named <- monthfiles

w.allmonths <- vector()
for(i in w.allmonths.named){
  w.allmonths <- rbind(w.allmonths, get(i))
}

# Save compiled object 
save(w.allmonths, 
     file = file.path(outputdir, paste0(Waze_Prepared_Data, ".RData")))

# Output for Tableau
 usevars = c("GRID_ID", names(w.allmonths)[c(8:44, 63:69, 83:ncol(w.allmonths))])

 write.csv(w.allmonths, #[usevars],
      file = file.path(outputdir, paste0(Waze_Prepared_Data, ".csv")),
                                        row.names = F)

# format(object.size(w.allmonths), "Gb")

} # End data prep 
# <><><><><><><><><><><><><><><><><><><><><><><><>


# Start from prepared data
w.allmonths <- read.csv(file.path(outputdir, paste0(Waze_Prepared_Data, ".csv")))

w.allmonths$MatchTN_buffer <- as.factor(w.allmonths$MatchTN_buffer)
w.allmonths$MatchTN_buffer_Acc <- as.factor(w.allmonths$MatchTN_buffer_Acc)
w.allmonths$TN_crash <- as.factor(w.allmonths$TN_crash)
w.allmonths$date <- as.Date(w.allmonths$date)
w.allmonths$weekday <- as.factor(w.allmonths$weekday)
w.allmonths$DayOfWeek <- as.factor(w.allmonths$DayOfWeek)
w.allmonths$GRID_ID <- as.character(w.allmonths$GRID_ID)
                
avail.cores = parallel::detectCores()

if(avail.cores > 8) avail.cores = 12 # Limit usage below max if on r4.4xlarge instance. Comment this out to run largest models.

# Use this to set number of decision trees to use, and key RF parameters. mtry is especially important, should consider tuning this with caret package
# For now use same parameters for all models for comparision; tune parameters after models are selected
rf.inputs = list(ntree.use = avail.cores * 50, avail.cores = avail.cores, mtry = 10, maxnodes = 1000, nodesize = 100)

keyoutputs = redo_outputs = list() # to store model diagnostics

# Omit as predictors in this vector:
alwaysomit = c(grep("GRID_ID", names(w.allmonths), value = T), "day", "hextime", "year", "weekday", 'wDate', 'wHour', 'tDate', 'tHour',
             # "uniqueWazeEvents", 
               "nWazeRowsInMatch", 
               "uniqueTNreports", "TN_crash", "date",
               "nMatchWaze_buffer", "nNoMatchWaze_buffer",
               grep("nTN", names(w.allmonths), value = T),
               grep("MatchTN", names(w.allmonths), value = T),
               grep("TN_UA", names(w.allmonths), value = T))

alert_types = c("nWazeAccident", "nWazeJam", "nWazeRoadClosed", "nWazeWeatherOrHazard")

alert_subtypes = c("nHazardOnRoad", "nHazardOnShoulder" ,"nHazardWeather", "nWazeAccidentMajor", "nWazeAccidentMinor", "nWazeHazardCarStoppedRoad", "nWazeHazardCarStoppedShoulder", "nWazeHazardConstruction", "nWazeHazardObjectOnRoad", "nWazeHazardPotholeOnRoad", "nWazeHazardRoadKillOnRoad", "nWazeJamModerate", "nWazeJamHeavy" ,"nWazeJamStandStill",  "nWazeWeatherFlood", "nWazeWeatherFog", "nWazeHazardIceRoad")

response.var = "MatchTN_buffer_Acc" # now could use nTN_total, for all TN crashes in this grid cell, this hour. Or TN_crash, binary indicator of if any TN crash occurred in this grid cell/hours 

starttime = Sys.time()

# Best Model: 'Model 05' with main waze alert types but not sub-types included

# 05, add base Waze features
modelno = paste("05", g, sep = "_")

omits = c(alwaysomit,
          "uniqueWazeEvents",
          grep("nWazeRT", names(w.allmonths), value = T), # All Waze road type
          # grep("nWaze", names(w.allmonths), value = T), # All Waze events
          alert_subtypes,
          grep("Waze_UA", names(w.allmonths), value = T), # Waze Urban Area
          grep("nHazard", names(w.allmonths), value = T), # Waze hazards
          grep("MagVar", names(w.allmonths), value = T), # direction of travel
          grep("medLast", names(w.allmonths), value = T), # report rating, reliability, confidence
          grep("nWazeAcc_", names(w.allmonths), value = T), # neighboring accidents
          grep("nWazeJam_", names(w.allmonths), value = T) # neighboring jams
)

# Check to see what we are passing as predictors
cat('Predictors to use in model', modelno, ': \n\n',
    paste(names(w.allmonths)[is.na(match(names(w.allmonths), omits))], collapse = '\n'))


fitvars = names(w.allmonths)[is.na(match(names(w.allmonths), omits))]
class_fit = n_lev_fit = levs_fit = vector()

for(f in fitvars){
  class_fit = c(class_fit, class(w.allmonths[,f]))
  n_lev_fit = c(n_lev_fit, ifelse(is.factor(w.allmonths[,f]),
                                  length(levels((w.allmonths[,f]))),
                                  NA))
  levs_fit = c(levs_fit, ifelse(is.factor(w.allmonths[,f]),
                                  paste(levels((w.allmonths[,f])), collapse = ", "),
                                  NA))
}


fitvar_df <- data.frame(fitvars, class_fit, n_lev_fit, levs_fit)
write.csv(fitvar_df, file = file.path(outputdir,paste0('Fitvars_', modelno, ".csv")))

# Run the Random Forest model using `do.rf()` function.
keyoutputs[[modelno]] = do.rf(train.dat = w.allmonths, 
                                omits, response.var = "TN_crash", 
                                model.no = modelno, rf.inputs = rf.inputs,
                                cutoff = c(0.9, 0.1))  
  
save("keyoutputs", file = file.path(outputdir,paste0("Output_to_", modelno)))


timediff <- Sys.time() - starttime
cat(round(timediff, 2), attr(timediff, "units"), "to complete script")
