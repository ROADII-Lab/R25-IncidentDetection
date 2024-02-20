# Functions for working with Waze and EDT data

# Make a link table to match events
# making more generic: accident file (usually EDT) and inicident file (usually Waze)
# Location in EDT is in GPSLong_New and GPS_Lat, time is in CrashDate_Local.
# Location in Waze is in lon and lat, time is in time.

makelink <- function(accfile = edt.april, incfile = waze.april,
                     acctimevar = "CrashDate_Local",
                     inctimevar1 = "time",
                     inctimevar2 = "last.pull.time",
                     accidvar = "ID",
                     incidvar = "uuid"
                     ){
  library(foreach) 
  library(doParallel) # includes iterators and parallel
  
  cl <- makeCluster(parallel::detectCores()) # make a cluster of all available cores
  registerDoParallel(cl)
  
  linktable <- vector()
  
  starttime <- Sys.time()
  writeLines("", paste0("TN_Waze_log_", j, "_", Sys.Date(), ".txt")) # to store messages by year-month i, now it is "j" in months_shared
  
  # Start of %dopar% loop
  linktable <- foreach(i=1:nrow(accfile), .combine = rbind, .packages = "sp") %dopar% {
    
    ei = accfile[i,]
    
    dist.i <- spDists(ei, incfile)*0.0006213712 # spDists gives units in m for projected data, convert to miles
    dist.i.5 <- which(dist.i <= 0.5)
    
    # Spatially matching
    d.sp <- incfile[dist.i.5,]
    
    # Temporally matching
    # Match between the first reported time and last pull time of the Waze event. Last pull time is after the earliest time of EDT, and first reported time is earlier than the latest time of EDT
    if(class(ei)=="SpatialPointsDataFrame") { ei <- as.data.frame(ei) }
    if(class(d.sp)=="SpatialPointsDataFrame") { d.sp <- as.data.frame(d.sp) }
    
    # ei: was EDT events, now TN crashes. We want to look from the time of crash event -60 minutes to crash event +60 minutes, and find Waze events in this window
    # d.sp: Waze events. inctimevar2 is the *end* of the event and inctimevar1 is the *start* of the event. We look to see if the end of the event is greater than EDT event -60 minutes and see if the start of the Waze event is less than the EDT event +60 minutes.
    
    d.t <- d.sp[d.sp[,inctimevar2] >= ei[,acctimevar]-60*60 & d.sp[,inctimevar1] <= ei[,acctimevar]+60*60,] 
    
    id.accident <- rep(as.character(ei[,accidvar]), nrow(d.t)) # TN crash ID
    id.incidents <- as.character(d.t[,incidvar]) # Waze events ID
    
    if(i %% 50000 == 0) {
      timediff <- round(Sys.time()-starttime, 2)
      cat(i, "complete \n", timediff, attr(timediff, "units"), "elapsed \n",
      "approx", round(as.numeric(timediff)/i * (nrow(accfile)-i), 2), attr(timediff, "units"), "remaining \n",
        rep("<>",20), "\n\n",
       file = paste0("EDT_Waze_log_", i, "_", Sys.Date(), ".txt"), append = T) }

    data.frame(id.accident, id.incidents) # rbind this output
  } # end %dopar% loop
  
  stopCluster(cl) # stop the cluster.
  
  linktable # Give all Waze accident IDs with EDT incident IDs
  }
  
# Non-parallel
makelink.nonpar <- function(accfile = edt.april, incfile = waze.april,
                       acctimevar = "CrashDate_Local",
                       inctimevar1 = "time",
                       inctimevar2 = "last.pull.time",
                       accidvar = "ID",
                       incidvar = "uuid"
                       ){
    
    linktable <- vector()
  
    # keeping non-parallized version here for reference
    for(i in 1:nrow(accfile)){ # i=which(edt$ID == "2023680")
      ei = accfile[i,]
      
      dist.i <- spDists(ei, incfile, longlat = T)*0.6213712 # spDists gives units in km, convert to miles
      dist.i.5 <- which(dist.i <= 0.5)
      
      # Spatially matching
      d.sp <- incfile[dist.i.5,]
      
      # Temporally matching
      # Match between the first reported time and last pull time of the Waze event. Last pull time is after the earliest time of EDT, and first reported time is earlier than the latest time of EDT
      if(class(ei)=="SpatialPointsDataFrame") { ei <- as.data.frame(ei) }
      if(class(d.sp)=="SpatialPointsDataFrame") { d.sp <- as.data.frame(d.sp) }
      
      
      d.t <- d.sp[d.sp[,inctimevar2] >= ei[,acctimevar]-60*60 & d.sp[,inctimevar1] <= ei[,acctimevar]+60*60,] 
      
      id.accident <- rep(as.character(ei[,accidvar]), nrow(d.t))
      id.incidents <- as.character(d.t[,incidvar])
      
      linktable <- rbind(linktable, data.frame(id.accident, id.incidents))
      
      if(i %% 1000 == 0) {
        timediff <- round(Sys.time()-starttime, 2)
        cat(i, "complete \n", timediff, attr(timediff, "units"), "elapsed \n", rep("<>",20), "\n")
      }
    } # end loop
    
    
    linktable
  }


# moving files from a temporary directory on local machine to shared drive. 
# Files are removed from the local machine by this process.
movefiles <- function(filelist, temp = outdir, wazedir){
  
  # Check to make sure the from and to directories are accessible
  if(!dir.exists(wazedir)) stop("Destination output directory does not exist or shared drive is not connected")
  if(length(filelist) < 1) stop("No files selected to move, check filelist argument")
  if(length(dir(temp)[grep(filelist[1], dir(temp))]) < 1) stop("Selected files not found in the temporary output directory")
    
  if(.Platform$OS.type == "windows"){
    # Fix path separators for Windows / R 
    temp <- gsub("\\\\", "/", temp)
    temp <- gsub("C:/U", "C://U", temp)
  }
  
  for(i in filelist){
    # Encase the destination path in quotes, because of spaces in path name
    system(paste0("mv ", file.path(temp, i), ' \"', file.path(wazedir, i), '\"'))
    
  }
}

# Function to return the most frequent value for character vectors.
# Breaks ties by taking the first value
ModeString <- function(x) {
  ux <- unique(x)
  
  # One unique value: return that value
  if(length(ux)==1) { 
    return(ux)
  } else {
    
    # Multiple values, no duplicates: return first one
    if(!anyDuplicated(x)) {
      return(ux[1])
    } else {
      
      # Multiple values, one category more frequent: return first most frequent
      tbl <-   tabulate(match(x, ux))
      return(ux[tbl==max(tbl)][1])
    }
  }
}

# Function for extracting time from file names
gettime <- function(x, tz = "America/New_York"){
  d <- substr(x, 5, 14)
  t <- substr(x, 16, 23)
  dt <- strptime(paste(d, t), "%Y-%m-%d %H-%M-%S", tz = tz)
}

# Function to show objects in the memory, orderd by size. Useful for cleaning up the workspace to free up RAM
showobjsize <- function(units = "Mb", limit = 100) {
  sizes <- vector()
  for (thing in ls(envir = .GlobalEnv)) { 
    sz <- format(object.size(get(thing)), 
           units = units,
           digits = 2)
    sizes <- rbind(sizes, data.frame(obj = thing, 
                                     size = as.numeric(unlist(lapply(strsplit(sz, " "), function(x) x[[1]][1])))
                                     ))
  }
  printlim <- ifelse(nrow(sizes) >= limit, limit, nrow(sizes))
  message("Object size in ", units)
  print(sizes[order(sizes$size, decreasing = T)[1:printlim],])
}

# Print diagnotstics from a confusion matrix.
# RF function give observed as rows, predicted as columns. More common in observed as columns, predicted as rows. Here following caret::confusionMatrix
# given a 2x2 table where columns are observed postive, and rows are predicted negative and positive:
# |: ---------------------------- Observed ------:|
# |: ----------------------:|:Positive:|:Negative:|
# |: Predicted  :|:Positive:|   TP     |    FP    |
# |:            :|:Negative:|   FN     |    TN    |

bin.mod.diagnostics <- function(predtab){

  accuracy = (predtab[1,1] + predtab[2,2] )/ sum(predtab) # true positives and true negatives divided by all observations
  precision = (predtab[1,1] )/ sum(predtab[1,]) # true positives divided by all predicted positives
  recall = (predtab[1,1] )/ sum(predtab[,1]) # true positives divided by all observed positives
  false.positive.rate = (predtab[1,2] )/ sum(predtab[,2]) # false positives divided by all observed negatives

  round(t(data.frame(accuracy, precision, recall, false.positive.rate)), 4)  
}


# Read in hexagonally-gridded data of a specific name, prep fields, and save it as a short-named data frame
# Will not be used in TN Case Study
prep.hex <- function(hexname, state, month, bucket = teambucket){
  # Specify month as 6-digit character value, e.g. "2017-04" for April 2017
  # Specify full path in hexname, e.g. for MD weather-overlaid files: file.path(inputdir, paste0("WazeTimeEdtHexWx_", mo,".RData"))
  
  mo = month
  load(file.path("~/TN","Input", hexname))

  wte <- get(ls(envir = environment())[grep("WazeTime", ls(envir = environment()), ignore.case = T)])
  
  class(wte) <- "data.frame" 


  if(length(grep("weekday", names(wte)) > 0)){
    wte$DayOfWeek <- as.factor(wte$weekday)
  }
  
  wte$hextime <- as.character(wte$hextime)
  wte$hour <- as.numeric(wte$hour)
  
  # Set NA values in wx (reflectivity) to zero
  if(length(grep("wx", names(wte)) > 0)){
    wte$wx[is.na(wte$wx)] = 0
  }
  
  if(state == "TN"){
    # Going to binary for all Waze buffer match, for Tennessee
    
    wte$MatchTN_buffer <- wte$nMatchTN_buffer
    wte$MatchTN_buffer[wte$MatchTN_buffer > 0] = 1 
    wte$MatchTN_buffer <- as.factor(wte$MatchTN_buffer)
    wte$MatchTN_buffer_Acc <- wte$nMatchTN_buffer_Acc
    wte$MatchTN_buffer_Acc[wte$MatchTN_buffer_Acc > 0] = 1 
    wte$MatchTN_buffer_Acc <- as.factor(wte$MatchTN_buffer_Acc)
    
    # Going to binary for all TN crahes:
    wte$TN_crash <- wte$nTN_total
    wte$TN_crash[wte$TN_crash > 0] = 1 
    wte$TN_crash <- as.factor(wte$TN_crash)
    
  } else {
    wte$MatchEDT_buffer <- wte$nMatchEDT_buffer
    wte$MatchEDT_buffer[wte$MatchEDT_buffer > 0] = 1 
    wte$MatchEDT_buffer <- as.factor(wte$MatchEDT_buffer)
    wte$MatchEDT_buffer_Acc <- wte$nMatchEDT_buffer_Acc
    wte$MatchEDT_buffer_Acc[wte$MatchEDT_buffer_Acc > 0] = 1 
    wte$MatchEDT_buffer_Acc <- as.factor(wte$MatchEDT_buffer_Acc)
    
    # Going to binary for all Waze Accident buffer match:
    
    wte$MatchEDT_buffer_Acc <- wte$nMatchEDT_buffer_Acc
    wte$MatchEDT_buffer_Acc[wte$MatchEDT_buffer_Acc > 0] = 1 
    wte$MatchEDT_buffer_Acc <- as.factor(wte$MatchEDT_buffer_Acc)
  }  
  
  # Omit grid cell x day combinations which are outside of this particular month (Waze road closures)
  yr = substr(mo, 1, 4)
  month.2 = substr(mo, 6, 7)
  yrday = strptime(paste(yr, formatC(wte$day, width =3, flag = 0), sep="-"), "%Y-%j")
  month.1 = format(yrday, "%m")
  wte = wte[month.1 %in% month.2,]
  
  wte$Year = yr # Add year as a variable
  
  mo <- sub("-", "_", mo) # change e.g. from 2017-04 to 2017_04 for R object naming
  
  assign(paste("w", mo, sep="."), wte, envir = globalenv()) 
}

#Helper function for finding class of a column
notDate = function(col){
  return(sum(class(col) != "Date")>0 & sum(class(col) != "POSIXct")>0)
}

# Append historical crash, weather, special events, or other supplemental data such as jobs, road class, and other variables by grid ID, year and day.

# Assumptions: 
# 1. There are already files like "w.2017_04" representing gridded, hourly Waze-crash data fusions in the working environment. This script appends static data to the hourly gridded data, repeating the static values for each hour.
# 2. There are paths like the S3 bucket, inputdir, localdir, all in working environment
# 3. rgdal and tidyverse libraries are loaded
# 4. The data files to add are unzipped files in the localdir (i.e., they have already been copied over from the S3 bucket, and are sitting on the instance)

append.hex <- function(hexname, data.to.add, state, na.action = c("omit", "keep", "fill0")){
  # hexname: string of data frame names in working memory like "w.04"
  # data.to.add: string of unzipped file set in the localdir, including sub-directory path and state name, like "FARS/CT/FARS_CT_2015_2016_sum_fclass"
  # na.action: what to do if missing any values; applies this action across the whole data frame, not just the appended data
  if(missing(na.action)) { na.action = "omit" } # set default value
  na.action <- match.arg(na.action) # match partially entered values
  
  w <- get(hexname)
  
  # Prepare data ----
  
  # SpecialEvent prep
  if(length(grep("TN_SpecialEvent", data.to.add)) > 0){
    
    dd <- spev.grid.time
    
    dd$hour <- as.numeric(format(dd$GridDayHour, "%H"))
    
    dd <- dd %>%
      group_by(GRID_ID, Year, day, hour) %>%
      summarise(SpecialEvent_sum = n())
   
    dd$GRID_ID <- as.character(dd$GRID_ID)
    class(dd) <- "data.frame"
    
  }

  if(length(grep("crash", data.to.add)) > 0){
    dd <- crash
    
    dd$hour <- as.numeric(format(dd$date, "%H"))
    
    dd1 <- dd %>%
      filter(year >= 2011 & year <= 2016) %>%
      group_by(GRID_ID) %>%
      summarise(TotalHistCrashsum = n())
    
    dd2 <- dd %>%
      filter(year >= 2011 & year <= 2016 &
               NbrFatalitiesNmb > 0) %>%
      group_by(GRID_ID) %>%
      summarise(TotalFatalCrashsum = n())
    
    dd <- full_join(dd1, dd2, by = 'GRID_ID')
      
  }
  
  if(length(grep("wx.grd.day", data.to.add)) > 0){
    
    dd <- wx.grd.day
    
    dd$date <- dd$day
    dd$Year <- format(dd$date, "%Y")
    dd$day <- formatC(format(dd$date, "%j"), width = 3, flag = 0)
    names(dd)[which(names(dd)=="ID")] = "GRID_ID"
    dd$GRID_ID = as.character(dd$GRID_ID)
    
  } 
  
  # End data type if statements, now merge with w data frame of Waze-crash data
  # join ----
  
  dd$GRID_ID <- as.character(dd$GRID_ID)
  if(length(grep("TN_SpecialEvent", data.to.add)) > 0){
    
    class(dd) = "data.frame"
    w2 <- left_join(w, dd, by = c("GRID_ID", "Year", "day", "hour")) 
     
  } 
  
  if(length(grep("wx.grd.day", data.to.add)) > 0){
    
    w2 <- left_join(w, dd, by = c("GRID_ID", "Year", "day")) 
    
  } 
  
  if(length(grep("crash", data.to.add)) > 0){
    
    w2 <- left_join(w, dd, by = "GRID_ID")
  
  }
  
  # Consider assigning 0 to NA values after joining; e.g. no road info available, give 0 miles
  if(na.action == "fill0") { w2[,sapply(w2,notDate)][is.na(w2[,sapply(w2,notDate)])] = 0 }
  if(na.action == "omit") { w2 = w2[complete.cases(w2),] }
  
  assign(hexname, w2, envir = globalenv()) 
  
}
