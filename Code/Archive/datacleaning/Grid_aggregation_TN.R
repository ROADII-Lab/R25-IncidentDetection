# Aggregation of Waze and TN crash by grid cell
# Goal: create a gridded data set where grid cell contain the count of events.
# Start from Hex_UA_Overlay_TN.R
# This is intended to run on the Secure Data Commons only.

# <><><><><><><><><><><><><><><><><><><><>
# Setup ----
rm(list = ls())
codeloc <- "~/SDI_Waze"
source(file.path(codeloc, 'utility/get_packages.R'))

# Flag for SDC work
ON_SDC = F
if(ON_SDC){teambucket <- "<Path_to_AWS_S3_Bucket_For_Waze_Data>"}


library(tidyverse)
library(lubridate)
library(utils)
library(doParallel)
library(foreach)
library(circular)

# Set parameters for data to process
state = "TN"

grids = c("TN_01dd_fishnet",
          "TN_1sqmile_hexagons")


localdir <- normalizePath("~/TN") # full path for readOGR

setwd(localdir)

wazemonthdir <- file.path(localdir,"workingdata", "Overlay") # contains the merged.waze.tn.YYYY-mm_<state>.RData files
temp.outputdir = file.path(localdir, "agg_out") # Will contain the WazeHexTimeList_YYYY-mm_grids_<state>.RData files

source(file.path(codeloc, "utility/wazefunctions.R")) 

crashdir <- normalizePath(file.path(localdir,"workingdata", "Crash"))
wazedir <- normalizePath(file.path(localdir,"workingdata", "Waze"))

if(ON_SDC){

# start grid loop ----
for(g in grids){ # g = grids[2]
  
  # Loop through months of available merged data for this state
  mergefiles <- dir(wazemonthdir)[grep("^merged.waze.tn", dir(wazemonthdir))]
  gridmergefiles <- mergefiles[grep(g, mergefiles)]
  
  avail.months = substr(unlist(
    lapply(strsplit(gridmergefiles, "_"), function(x) x[[4]])), 1, 7)
  
  # Look for already completed months and skip those
  tlfiles <- dir(temp.outputdir)[grep("WazeTimeEdtHexAll_", dir(temp.outputdir))]
  grid.tlfiles <- tlfiles[grep(g, tlfiles)]
  done.months <- unlist(lapply(strsplit(grid.tlfiles, "_"), function(x) x[[2]])) 
  
  todo.months = sort(avail.months)#[!avail.months %in% done.months]
  
  cl <- makeCluster(parallel::detectCores()) # make a cluster of all available cores
  registerDoParallel(cl)
  
  writeLines(c(""), paste0("GridAgg", g, "_log.txt"))    
  
  foreach(j = todo.months, .packages = c("dplyr", "lubridate", "utils")) %dopar% {
    # j = "2018-08"  
    sink(paste0("GridAgg", g, "_log.txt"), append=TRUE)
    
    cat(paste(Sys.time()), j, "\n")                                                  
    load(file.path(wazemonthdir, paste0("merged.waze.tn.", g,"_", j, ".RData"))) # includes both waze (link.waze.tn) and TN crash (crash.df) data, with grid for central and neighboring cells
    
    # format(object.size(link.waze.tn), "Mb"); format(object.size(crash.df), "Mb")
    # TN crash time and Waze time are now in POSIXct, not POSIXlt, with correct time zone for each row. ct: seconds since beginning of 1970 in UTC. lt is a list of vectors representing seconds, min, hours, day, year. ct is better for analysis, while lt is more human-readable.

    # <><><><><><><><><><><><><><><><><><><><>
    # Read in the data frame of all Grid IDs by day of year and time of day in each month of data (subset to all grid IDs with Waze OR EDT data)
 
    load(file.path(temp.outputdir, paste0("WazeHexTimeList_", j, "_", g, "TN.RData")))
    # aggregate: new data frame will have one row per cell, per hour, per day.
    # Response variable column: count of unique TN Crash events matching Waze events in this cell, within this time window. 
    # Predictor variable columns: median values for the numeric characteristics: report rating, confidence..
    # Counts for the number of waze events of each alert_type and sub_type, inside this grid cell at this time.
    # The same, but for each of the neighboring grid cells (N, NE, SE, S, SW, NW). 
    # counts for road_type, 11 columns: length(unique(link.waze.edt$road_type[!is.na(link.waze.edt$road_type)]))
    
    #summarize counts of Waze events in each hexagon and EDT matches to the Waze events (could be in neighboring hexagon)
    # names(Waze.hex.time)
    
    StartTime <- Sys.time()
    waze.hex <- Waze.hex.time %>%
      mutate(Date = as.POSIXct(Date),
             MagVar.circ = circular(magvar, units = "degrees", template = "geographics")) %>%
      group_by(GRID_ID, GRID_ID_NW, GRID_ID_N, GRID_ID_NE, GRID_ID_SW, GRID_ID_S, GRID_ID_SE,
               year = format(Date, "%Y"), day = format(Date, "%j"), hour = formatC(Hour, width = 2, flag = '0'), weekday = format(Date, "%u")) %>%
      summarize(
        nWazeRowsInMatch = n(), #includes duplicates that match more than one TN crash report (don't use in model)
        uniqueWazeEvents= n_distinct(uuid.waze), # number of unique Waze events.
        
        UA_Cluster = 1*any(Waze_UA_Type == "C" & !is.na(Waze_UA_Type)),
        UA_Urban = 1*any(Waze_UA_Type == "U" & !is.na(Waze_UA_Type)),
        UA_Rural = 1*any(is.na(Waze_UA_Type)),
    
        nMatchTN_buffer = n_distinct(ID[match=="M"]), # what's the difference between this one and the next row?
        nTN_total = n_distinct((ID[match=="M" | match=="T"])), # did no observe any "T" value in the match column, this is the response for the Waze only model, # of unique TN crashes that find a match with Waze events
        
        nMatchTN_buffer_Acc = n_distinct(ID[match=="M" & alert_type=="ACCIDENT"]),
        nMatchTN_buffer_Jam = n_distinct(ID[match=="M" & alert_type=="JAM"]),
        nMatchTN_buffer_RoadClosed = n_distinct(ID[match=="M" & alert_type=="ROAD_CLOSED"]),
        nMatchTN_buffer_WorH = n_distinct(ID[match=="M"& alert_type=="WEATHERHAZARD"]),
        
        nMatchWaze_buffer = n_distinct(uuid.waze[match=="M"]),
        nNoMatchWaze_buffer = n_distinct(uuid.waze[match=="W"]),
        
        nWazeAccident = n_distinct(uuid.waze[alert_type=="ACCIDENT"]),
        nWazeJam = n_distinct(uuid.waze[alert_type=="JAM"]),
        nWazeRoadClosed = n_distinct(uuid.waze[alert_type=="ROAD_CLOSED"]),
        nWazeWeatherOrHazard = n_distinct(uuid.waze[alert_type=="WEATHERHAZARD"]),
        
        nHazardOnRoad = n_distinct(uuid.waze[sub_type=="HAZARD_ON_ROAD"|sub_type=="HAZARD_ON_ROAD_CAR_STOPPED"
                                             |sub_type=="HAZARD_ON_ROAD_CAR_STOPPED"|sub_type=="HAZARD_ON_ROAD_CONSTRUCTION"
                                             |sub_type=="HAZARD_ON_ROAD_ICE"|sub_type=="HAZARD_ON_ROAD_LANE_CLOSED"
                                             |sub_type=="HAZARD_ON_ROAD_OBJECT"|sub_type=="HAZARD_ON_ROAD_POT_HOLE"
                                             |sub_type=="HAZARD_ON_ROAD_ROAD_KILL"]),
        nHazardOnShoulder = n_distinct(uuid.waze[sub_type=="HAZARD_ON_SHOULDER"|sub_type=="HAZARD_ON_SHOULDER_ANIMALS"
                                                 |sub_type=="HAZARD_ON_ROAD_CAR_STOPPED"|sub_type=="HAZARD_ON_SHOULDER_CAR_STOPPED"
                                                 |sub_type=="HAZARD_ON_SHOULDER_MISSING_SIGN"]),
        nHazardWeather = n_distinct(uuid.waze[sub_type=="HAZARD_WEATHER"|sub_type=="HAZARD_WEATHER_FLOOD"
                                              |sub_type=="HAZARD_WEATHER_FOG"|sub_type=="HAZARD_WEATHER_HAIL"
                                              |sub_type=="HAZARD_WEATHER_MONSOON"|sub_type=="HAZARD_WEATHER_FREEZING_RAIN"
                                              |sub_type=="HAZARD_WEATHER_HEAVY_SNOW"|sub_type=="HAZARD_WEATHER_HEAVY_RAIN"]),
  
        nWazeAccidentMajor = n_distinct(uuid.waze[sub_type=="ACCIDENT_MAJOR"]),
        nWazeAccidentMinor = n_distinct(uuid.waze[sub_type=="ACCIDENT_MINOR"]),
  
        nWazeHazardCarStoppedRoad = n_distinct(uuid.waze[sub_type=="HAZARD_ON_ROAD_CAR_STOPPED"]),
        nWazeHazardCarStoppedShoulder = n_distinct(uuid.waze[sub_type=="HAZARD_ON_SHOULDER_CAR_STOPPED"]),
        nWazeHazardConstruction = n_distinct(uuid.waze[sub_type=="HAZARD_ON_ROAD_CONSTRUCTION"]),
        nWazeHazardObjectOnRoad = n_distinct(uuid.waze[sub_type=="HAZARD_ON_ROAD_OBJECT"]),
        nWazeHazardPotholeOnRoad = n_distinct(uuid.waze[sub_type=="HAZARD_ON_ROAD_POT_HOLE"]),
        nWazeHazardRoadKillOnRoad = n_distinct(uuid.waze[sub_type=="HAZARD_ON_ROAD_ROAD_KILL"]),
        nWazeJamModerate = n_distinct(uuid.waze[sub_type=="JAM_MODERATE_TRAFFIC"]),
        nWazeJamHeavy = n_distinct(uuid.waze[sub_type=="JAM_HEAVY_TRAFFIC"]),
        nWazeJamStandStill = n_distinct(uuid.waze[sub_type=="JAM_STAND_STILL_TRAFFIC"]),
        nWazeWeatherFlood = n_distinct(uuid.waze[sub_type=="HAZARD_WEATHER_FLOOD"]),
        nWazeWeatherFog = n_distinct(uuid.waze[sub_type=="HAZARD_WEATHER_FOG"]),
        nWazeHazardIceRoad = n_distinct(uuid.waze[sub_type=="HAZARD_ON_ROAD_ICE"]),
        
        #Omit road closures from counts (not reported by users)
        nWazeRT3 = n_distinct(uuid.waze[alert_type!="ROAD_CLOSED" & roadclass=="3"]),
        nWazeRT4 = n_distinct(uuid.waze[alert_type!="ROAD_CLOSED" & roadclass=="4"]),
        nWazeRT6 = n_distinct(uuid.waze[alert_type!="ROAD_CLOSED" & roadclass=="6"]),
        nWazeRT7 = n_distinct(uuid.waze[alert_type!="ROAD_CLOSED" & roadclass=="7"]),
        nWazeRT2 = n_distinct(uuid.waze[alert_type!="ROAD_CLOSED" & roadclass=="2"]),
        nWazeRT0 = n_distinct(uuid.waze[alert_type!="ROAD_CLOSED" & roadclass=="0"]),
        nWazeRT1 = n_distinct(uuid.waze[alert_type!="ROAD_CLOSED" & roadclass=="1"]),
        nWazeRT20 = n_distinct(uuid.waze[alert_type!="ROAD_CLOSED" & roadclass=="20"]),
        nWazeRT17 = n_distinct(uuid.waze[alert_type!="ROAD_CLOSED" & roadclass=="17"]),
        
        medLastRepRate = median(last.reportRating), # median is going to be affected if Waze.hex.time table has duplicates 
        medLastConf = median(last.confidence),
        medLastReliab = median(last.reliability),
        
        meanCirMagVar = mean.circular(MagVar.circ[alert_type!="ROAD_CLOSED"]),
        medCirMagVar = median.circular(MagVar.circ[alert_type!="ROAD_CLOSED"]),
  
        nMagVar330to30N = n_distinct(uuid.waze[alert_type!="ROAD_CLOSED" & magvar>= 330 | magvar<30]),
        nMagVar30to90NE = n_distinct(uuid.waze[alert_type!="ROAD_CLOSED" & magvar>= 30 & magvar<90]),
        nMagVar90to150SE = n_distinct(uuid.waze[alert_type!="ROAD_CLOSED" & magvar>= 90 & magvar<150]),
        nMagVar150to210S = n_distinct(uuid.waze[alert_type!="ROAD_CLOSED" & magvar>= 150 & magvar<210]),
        nMagVar210to270SW = n_distinct(uuid.waze[alert_type!="ROAD_CLOSED" & magvar>= 210 & magvar<270]),
        nMagVar270to330NW = n_distinct(uuid.waze[alert_type!="ROAD_CLOSED" & magvar>= 270 & magvar<330]))

    #Compute grid counts for TN data -- update all of these to the TN data variables
    # Add accident severity counts by grid cell 
    # names(crash.df)
    tn.hex <- 
      crash.df %>%
      group_by(GRID_ID.TN, year = format(date, "%Y"), day = format(date, "%j"), hour = format(date, "%H")) %>%
      summarize(
        uniqueTNreports= n_distinct(MstrRecNbrTxt),
        
        nTNInjuryFatal = n_distinct(MstrRecNbrTxt[NbrFatalitiesNmb > 0]),
        nTNInjury = n_distinct(MstrRecNbrTxt[NbrInjuredNmb > 0]),
        nTNAlcoholInd = n_distinct(MstrRecNbrTxt[AlcoholInd == "Y"]),
        nTNIntersectionInd = n_distinct(MstrRecNbrTxt[IntersectionInd == "Y"])
        ) 
    
    #Merge TN crash counts to waze counts by hexagons
    names(waze.hex)
    names(tn.hex)
    colnames(tn.hex)[1] <- "GRID_ID"
    
    wazeTime.tn.hex <- full_join(waze.hex, tn.hex, by = c("GRID_ID", "year", "day", "hour")) %>%
      mutate_all(funs(replace(., is.na(.), 0)))
    # Replace NA with zero (for the grid counts here, 0 means there were no reported Waze events or TN crashes in the grid cell at that hour)
    
    #Add columns containing data for neighboring grid cells 
    names(wazeTime.tn.hex)
  
    #Accident counts for neighboring cells
    nWazeAcc <- wazeTime.tn.hex %>%
      ungroup()%>%
      select(GRID_ID, day, hour, nWazeAccident)%>%
      rename(nWazeAcc_neighbor = nWazeAccident)
    
    wazeTime.tn.hex_NW <- left_join(wazeTime.tn.hex, nWazeAcc, by = c("GRID_ID_NW"="GRID_ID","day"="day", "hour"="hour")) %>%
      rename(nWazeAcc_NW=nWazeAcc_neighbor)
    wazeTime.tn.hex_NW_N <- left_join(wazeTime.tn.hex_NW, nWazeAcc, by = c("GRID_ID_N"="GRID_ID","day"="day", "hour"="hour")) %>%
      rename(nWazeAcc_N=nWazeAcc_neighbor)
    wazeTime.tn.hex_NW_N_NE <- left_join(wazeTime.tn.hex_NW_N, nWazeAcc, by = c("GRID_ID_NE"="GRID_ID","day"="day", "hour"="hour"))%>%
      rename(nWazeAcc_NE=nWazeAcc_neighbor)
    wazeTime.tn.hex_NW_N_NE_SW <- left_join(wazeTime.tn.hex_NW_N_NE, nWazeAcc, by = c("GRID_ID_SW"="GRID_ID","day"="day", "hour"="hour"))%>%
      rename(nWazeAcc_SW=nWazeAcc_neighbor)
    wazeTime.tn.hex_NW_N_NE_SW_S <- left_join(wazeTime.tn.hex_NW_N_NE_SW, nWazeAcc, by = c("GRID_ID_S"="GRID_ID","day"="day", "hour"="hour"))%>%
      rename(nWazeAcc_S=nWazeAcc_neighbor)
    wazeTime.tn.hex_NW_N_NE_SW_S_SE <- left_join(wazeTime.tn.hex_NW_N_NE_SW_S, nWazeAcc, by = c("GRID_ID_SE"="GRID_ID","day"="day", "hour"="hour"))%>%
      rename(nWazeAcc_SE=nWazeAcc_neighbor)
    
    #Jam counts for neighboring cells
    nWazeJam <- wazeTime.tn.hex %>%
      ungroup()%>%
      select(GRID_ID, day, hour, nWazeJam)%>%
      rename(nWazeJam_neighbor = nWazeJam)
    
    wazeTime.tn.hex <- wazeTime.tn.hex_NW_N_NE_SW_S_SE # edit names so we're not overwriting earlier df
    wazeTime.tn.hex_NW <- left_join(wazeTime.tn.hex, nWazeJam, by = c("GRID_ID_NW"="GRID_ID","day"="day", "hour"="hour")) %>%
      rename(nWazeJam_NW=nWazeJam_neighbor)
    wazeTime.tn.hex_NW_N <- left_join(wazeTime.tn.hex_NW, nWazeJam, by = c("GRID_ID_N"="GRID_ID","day"="day", "hour"="hour")) %>%
      rename(nWazeJam_N=nWazeJam_neighbor)
    wazeTime.tn.hex_NW_N_NE <- left_join(wazeTime.tn.hex_NW_N, nWazeJam, by = c("GRID_ID_NE"="GRID_ID","day"="day", "hour"="hour"))%>%
      rename(nWazeJam_NE=nWazeJam_neighbor)
    wazeTime.tn.hex_NW_N_NE_SW <- left_join(wazeTime.tn.hex_NW_N_NE, nWazeJam, by = c("GRID_ID_SW"="GRID_ID","day"="day", "hour"="hour"))%>%
      rename(nWazeJam_SW=nWazeJam_neighbor)
    wazeTime.tn.hex_NW_N_NE_SW_S <- left_join(wazeTime.tn.hex_NW_N_NE_SW, nWazeJam, by = c("GRID_ID_S"="GRID_ID","day"="day", "hour"="hour"))%>%
      rename(nWazeJam_S=nWazeJam_neighbor)
    wazeTime.tn.hex_NW_N_NE_SW_S_SE <- left_join(wazeTime.tn.hex_NW_N_NE_SW_S, nWazeJam, by = c("GRID_ID_SE"="GRID_ID","day"="day", "hour"="hour"))%>%
      rename(nWazeJam_SE=nWazeJam_neighbor)
    
    # test process - look at value for highest count in nWazeAcc_NW column (10)
    # t=filter(wazeTime.tn.hex_NW_N_NE_SW_S_SE, GRID_ID=="EG-53" & day=="141" & hour=="15")
    # t #10 - this matches, test more
    
    wazeTime.tn.hexAll <- wazeTime.tn.hex_NW_N_NE_SW_S_SE %>%
      mutate_all(funs(replace(., is.na(.), 0)))
    # Replace NA with zero (for the grid counts here, 0 means there were no reported Waze events or EDT crashes in the neighbor grid cell at that hour)
    
    # Update time variable 
    hextimeChar <- paste(paste(wazeTime.tn.hexAll$year, wazeTime.tn.hexAll$day, sep = "-"), wazeTime.tn.hexAll$hour, sep=" ")
    wazeTime.tn.hexAll$hextime <- hextimeChar #strptime(hextimeChar, "%Y:%j:%H", tz = use.tz)
  
    class(wazeTime.tn.hexAll) <- "data.frame" # POSIX date/time not supported for grouped tbl
    
    fn = paste("WazeTimeEdtHexAll_", j,"_", g, ".RData", sep="")
    
    save(list="wazeTime.tn.hexAll", file = file.path(temp.outputdir, fn))
    if(ON_SDC){
      # Copy to S3
      system(paste("aws s3 cp",
                   file.path(temp.outputdir, fn),
                   file.path(teambucket, state, fn)))
    }
    EndTime <- Sys.time()-StartTime
    cat(j, 'completed', round(EndTime, 2), attr(EndTime, 'units'), '\n')

  } # End month aggregation loop ----
  
  
  stopCluster(cl)

} # end grid loop

REUPLOAD = F

if(REUPLOAD){
# Move to S3 if it failed in the loop for some reason
for(g in grids){ # g = grids[1]
  # Loop through months of available merged data for this state
  mergefiles <- dir(wazemonthdir)[grep("^merged.waze.tn", dir(wazemonthdir))]
  gridmergefiles <- mergefiles[grep(g, mergefiles)]
  
  avail.months = substr(unlist(
    lapply(strsplit(gridmergefiles, "_"), function(x) x[[4]])), 1, 7)
  
  
  for(j in avail.months){
  fn = paste("WazeTimeEdtHexAll_", j,"_", g, ".RData", sep="")
  
  if(ON_SDC){
    # Copy to S3
    system(paste("aws s3 cp",
                 file.path(temp.outputdir, fn),
                 file.path(teambucket, state, fn)))
  }
  }
  }
}
# Check with plots -- 

CHECKPLOT = T

state = "TN"
library(rgdal)
library(maps) # for state.fips

if(CHECKPLOT){  
  
  co <- rgdal::readOGR(file.path(localdir,"workingdata", 'census'), layer = "cb_2017_us_county_500k")

  FIPS = state.fips[state.fips$abb %in% state,]
  FIPS = FIPS[!duplicated(FIPS$abb),]
  FIPS$fips = formatC(FIPS$fips, width = 2, flag = "0")  
  
  pdf(file.path("Figures", "Checking_Grid_agg_TN.pdf"), width = 11, height = 8)
  for(g in grids){ # g = grids[1]

  months = c("2017-04", "2017-09") # just a few for example

  state.co <- co[co$STATEFP == FIPS[FIPS$abb==state,"fips"],]
    
  # Read in hexagon shapefile. This is a rectangular surface of 1 sq mi area hexagons, 
  hex = readOGR(file.path(localdir,"workingdata", "TN", "Shapefiles"), layer = g)
    
  hex2 <- spTransform(hex, proj4string(state.co))
  
  for(j in months){
      plot(hex2, main = paste(g, j))
    
      plot(state.co, add = T, col = "red")
      
      fn = paste0("WazeTimeEdtHexAll_", j, "_", g, ".RData")
      
      load(file.path(temp.outputdir, fn))
      
      class(wazeTime.tn.hexAll) <- "data.frame"
      
      # Join back to hex2, just check unique grid IDs for plotting
      w.t <- wazeTime.tn.hexAll[!duplicated(wazeTime.tn.hexAll$GRID_ID),]
      
      h.w <- hex2[hex2$GRID_ID %in% w.t$GRID_ID,]
      plot(h.w, col = "blue", add = T)
    }
  }
  dev.off()
}

} # End if ON_SDC ----