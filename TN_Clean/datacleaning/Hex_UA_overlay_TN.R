# Assigns urban area classifications and hexagonal grid IDs to Waze and crash events
# Merges the two files into a matched data frame and saves
# Loops over all months of available data where both crash and Waze files exist, for each state

# <><><><><><><><><><><><><><><><><><><><>
# Setup ----
# Check for package installations
rm(list = ls())
codeloc <- "~/SDI_Waze"
source(file.path(codeloc, 'utility/get_packages.R'))

library(tidyverse)
library(sp)
library(maps) # for mapping base layers
library(rgdal) # for readOGR(), needed for reading in ArcM shapefiles
library(rgeos) # for gIntersects
library(foreach)
library(doParallel)

state = "TN"

output.loc <- "~/tempout"

localdir <- normalizePath("~/TN/workingdata") # full path for readOGR

setwd(localdir)

crashdir <- normalizePath(file.path(localdir, state, "Crash"))
wazedir <- normalizePath(file.path(localdir, state, "Waze")) # has State_Year-mo.RData files. Grab from S3 if necessary

# Flag for SDC work
ON_SDC = F
if(ON_SDC){teambucket <- "<Path_to_AWS_S3_Bucket_For_Waze_Data>"}


source(file.path(codeloc, "utility/wazefunctions.R")) 

# Only set up to run on SDC currently
if(ON_SDC){
  
# Time zones
# TN has 2 timezones (eastern and central) - Waze data have time zones applied by a shapefile, in the third step of ReduceWaze_SDC.R now.

# Project to Albers equal area conic 102008. Check comparision with USGS version, WKID: 102039
proj <- showP4(showWKT("+init=epsg:102008"))
# USGS version, used for producing hexagons: 
proj.USGS <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"

# Make TN/Overlay directory, if it doesn't already exist
system(paste('mkdir -p', file.path(localdir, "Overlay")))

TEST = F      # Change to F to run for all available months, T for only a subset of months
CHECKPLOT = T # Make plots for each stage, to make sure of spatial overlay

# Read in spatial data ----

# Read in urban/rural layer from Census
# From: https://www.census.gov/geo/maps-data/data/cbf/cbf_ua.html
# https://www2.census.gov/geo/pdfs/maps-data/data/tiger/tgrshprd13/TGRSHPrd13_TechDoc_A.pdf
# Get from S3 if necessary

ua <- readOGR(file.path(localdir, "census"), layer = "cb_2016_us_ua10_500k")
ua <- spTransform(ua, CRS(proj.USGS))

# Read in county shapefile, and apply coordinate reference system of hexagons (USGS version of Albers equal area) to Urban Areas and counties using uproj.USGS
co <- readOGR(file.path(localdir, "census"), layer = "cb_2017_us_county_500k")
co <- spTransform(co, CRS(proj.USGS))

# Read in grid shapefiles and overlay in a loop ----

grids = c("TN_01dd_fishnet",
          "TN_1sqmile_hexagons")

for(g in grids){
  
  grid = readOGR(file.path(localdir, state, "Shapefiles"), layer = g) # the grid layer
  grid <- spTransform(grid, CRS(proj.USGS))
  # grid column names
  gridcols <- names(grid)[grep("^GRID", names(grid))]
  
  FIPS_i = formatC(state.fips[state.fips$abb == state, "fips"][1], width = 2, flag = "0") # find the FIPS ID of TN
  co_i <- co[co$STATEFP == FIPS_i,] # find all counties fall in TN
  
  # Limit grid to only cells which touch a county in this state
  grid_o <- gIntersects(grid, co_i, byid = T)
  instate <- apply(grid_o, 2, function(x) any(x))
  grid <- grid[instate,]
  
  # Files -- 
  # about 29% are missing lat/long, and additional checks were done in TN_Data_Format.R for lat / long validity.
  load(file.path(localdir, state, "Crash", "TN_Crash_Simple_2008-2018.RData")) # 135.6 Mb, 26 columns, 447,224 rows
  tn_crash <- crash
  
  # project TN crashes
  if(class(tn_crash)=="data.frame"){
    tn_crash <- SpatialPointsDataFrame(tn_crash[c("LongDecimalNmb", "LatDecimalNmb")], tn_crash, 
                                  proj4string = CRS("+proj=longlat +datum=WGS84"))  
    
    tn_crash <-spTransform(tn_crash, CRS(proj.USGS))
  }
  
  tn_crash$YM = format(tn_crash$date, "%Y-%m")
  tn_crash_months <- unique(tn_crash$YM)

  # Names of files for this state, in a directory with all clipped, buffered, unique uuid events by month.
  wazemonthfiles <- dir(wazedir)[grep(state, dir(wazedir))]
  wazemonthfiles <- wazemonthfiles[grep("TN_\\d{4}-\\d{2}.RData$", wazemonthfiles)]
  wazemonths <- sort(unique(substr(wazemonthfiles, 4, 10)))
  
  months_shared <- sort(tn_crash_months[tn_crash_months %in% wazemonths])
  
  # Restrict TN crash data to just shared months
  tn_crash <- tn_crash[!is.na(match(tn_crash$YM, months_shared)),]
  dim(tn_crash)
  
  avail.cores <- parallel::detectCores()
  if(avail.cores > length(months_shared)) avail.cores = length(months_shared) # use only cores necessary
  cl <- makeCluster(avail.cores) 
  registerDoParallel(cl)
  
  starttime_state <- Sys.time()
  
  # Start parallel loop over yearmonths within state ----
  foreach(mo = months_shared, .packages = c("dplyr", "tidyr","sp","rgdal","scales")) %dopar% {
    
    # Waze: Comes from aggregated monthly Waze events, clipped to a 0.5 mile buffer around state, for each month.  All the waze files share the same object name, mb
    load(file.path(wazedir, wazemonthfiles[grep(mo, wazemonthfiles)]))
    if(class(mb)=="data.frame"){
      mb <- SpatialPointsDataFrame(mb[c("lon", "lat")], mb, 
                                    proj4string = CRS("+proj=longlat +datum=WGS84"))  #  make sure Waze data is a SPDF
      
      mb <- spTransform(mb, CRS(proj.USGS))
    }
    
    # Get time and last.pull.time as POSIXct
    if(!("POSIXct" %in% class(mb$time))){
      mbt <- as.character(mb$time)
      mbt.last <- as.character(mb$last.pull.time)
      mbtz <- substr(mbt, 21, 23) 
      
      new.mb <- vector()
      
      for(tz in unique(mbtz)){
        mbx.tz = mb[mbtz == tz,] # subset data frame mb to just rows with this time zone
        mbx.tz$time = as.POSIXct(mbt[mbtz == tz], tz = tz)
        mbx.tz$last.pull.time = as.POSIXct(mbt.last[mbtz == tz], tz = tz)
        new.mb = rbind(new.mb, mbx.tz@data)
      }
      mb = new.mb; rm(new.mb, mbt, mbt.last, mbtz)  
      mb <- SpatialPointsDataFrame(mb[c("lon", "lat")], mb, 
                                  proj4string = CRS("+proj=longlat +datum=WGS84")) # convert back to spatial points data frame
      
      mb <- spTransform(mb, CRS(proj.USGS))
    }
    
    link <- read.csv(file.path(localdir, "TN", "Link", paste0("TN_Waze_link_", mo,"_TN.csv")))
    
    # Add "M" code to the table to show matches if we merge in full datasets
    match <- rep("M", nrow(link))
    link <- mutate(link, match) 
    
    # <><><><><><><><><><><><><><><><><><><><>
    # Overlay points in polygons
    # Use over() from sp to join these to the census polygon. Points in Polygons, pip
    
    # First clip TN crash to state 
    tn.co <- over(tn_crash, co_i["COUNTYFP"])
    tn_crash <- tn_crash[!is.na(tn.co$COUNTYFP),] 
    
    # <><><><><><><><><><><><><><><><><><><><>
    # Urban area overlay ----
    
    waze_ua_pip <- over(mb, ua[,c("NAME10","UATYP10")]) # Match a urban area name and type to each row in mb (Waze events data). UATYPE10 is Census 2010 Urban Type, U = Urban Area, C = Urban Cluster.
    table(ua$UATYP10)
    
    edt_ua_pip <- over(tn_crash, ua[,c("NAME10","UATYP10")]) # Match a urban area name and type to each row in tn_crash. 
    mb@data <- data.frame(mb@data, waze_ua_pip)
    names(mb@data)[(length(mb@data)-1):length(mb@data)] <- c("Waze_UA_Name", "Waze_UA_Type") # Rename the two added columns
    
    tn_crash@data <- data.frame(tn_crash@data, edt_ua_pip)
    names(tn_crash@data)[(length(tn_crash@data)-1):length(tn_crash@data)] <- c("EDT_UA_Name", "EDT_UA_Type")
    
    # <><><><><><><><><><><><><><><><><><><><>
    # Grid overlay ----
    
    waze_hex_pip <- over(mb, grid[,gridcols]) # Match hexagon names to each row in mb. 
    edt_hex_pip <- over(tn_crash, grid[,gridcols]) # Match hexagon names to each row in tn_crash. 
    # add these columns to the data frame
    mb@data <- data.frame(mb@data, waze_hex_pip)
    
    tn_crash@data <- data.frame(tn_crash@data, edt_hex_pip)
    
    # Check:
    if(CHECKPLOT){ 
      jpeg(file = file.path(localdir, state, paste0("Figures/Checking_TN_Waze_UAoverlay_", g, "_", mo, ".jpg")), width = 500, height = 500) 
      plot(mb, main = paste("Pre-linking TN and Waze", mo))
      points(tn_crash, col = alpha("red", 0.2))
      plot(grid, add = T, col = alpha("grey80", 0.1))
      dev.off()
    }
    # <><><><><><><><><><><><><><><><><><><><>
    # Merge and save TN-Waze
    # <><><><><><><><><><><><><><><><><><><><>
    
    # Save Waze data as dataframe
    waze.df <- mb@data 
    
    # Save EDT data as dataframe, and subset to just the month of interest
    crash.df <- tn_crash@data  %>% filter(YM == mo)
    
    # before carrying out the join, rename the TN crash grid cell columns 
    names(crash.df)[grep("^GRID", names(crash.df))] <- paste(names(crash.df)[grep("^GRID", names(crash.df))], state, sep = ".")
    
    # Join Waze data to link table (full join)
    link$id.incidents = as.character(link$id.incidents)

    link.waze <- full_join(link, waze.df, by=c("id.incidents"="alert_uuid"))
    
    # Add W code to match column to indicate only Waze data
    link.waze$match <- ifelse(is.na(link.waze$match), 'W', link.waze$match)
    
    # now has M for rows with matching TN crahes, and W for all others in column "match"
    
    # Join TN crash data to Waze-link table (full join)
    crash.df$MstrRecNbrTxt <- as.character(crash.df$MstrRecNbrTxt)
    link.waze$id.accident <- as.character(link.waze$id.accident)
    
    link.waze.tn <- full_join(link.waze, crash.df, by = c("id.accident"="MstrRecNbrTxt")) 
    
    # Add T code to match column to indicate only TN crash data
    link.waze.tn$match <- ifelse(is.na(link.waze.tn$match), 'T', link.waze.tn$match)

    
    # rename ID variables for compatibility with existing code
    names(link.waze.tn)[grep("id.incident", names(link.waze.tn))] = "uuid.waze"
    names(link.waze.tn)[grep("id.accident", names(link.waze.tn))] = "ID"
    
    # save the merged file to local output directory, move to S3 after
    fn = paste0("merged.waze.tn.", g, "_", mo, ".RData")
    
    save(list=c("link.waze.tn", "crash.df"), file = file.path(localdir, "TN", "Overlay", fn))
    if(ON_SDC){
      system(paste("aws s3 cp",
                   file.path(localdir,"Overlay", fn),
                   file.path(teambucket, "Overlay", fn)))
      
    }

    if(CHECKPLOT) { 
      lwe.sp <- SpatialPoints(link.waze.tn[!is.na(link.waze.tn$lon) | !is.na(link.waze.tn$lat),c("lon", "lat")],
                                   proj4string = CRS("+proj=longlat +datum=WGS84")) 
      lwe.sp <-spTransform(lwe.sp, CRS(proj.USGS))
      jpeg(file = file.path(localdir, state, paste0("Figures/Checking2_TN_Waze_UAoverlay_", g, "_", mo, ".jpg")), width = 500, height = 500) 
      plot(co_i, main = paste("Linked TN-Waze", mo))
      plot(lwe.sp, col = alpha("blue", 0.3), add=T, pch = "+")
      dev.off()
    }  
  } # End month loop
  
  timediff <- Sys.time() - starttime_state
  cat(round(timediff, 2), attr(timediff, "units"), "elapsed to overlay TN", g, "\n\n")

  stopCluster(cl); gc()
  
} #End grid loop

} # End if ON_SDC ----