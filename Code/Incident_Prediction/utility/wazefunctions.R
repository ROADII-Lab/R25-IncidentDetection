# Functions for working with Waze data

# Create a function that looks for potential matches in a second dataset (sf object) and adds info to a first dataset 
# (sf object) based on them.
find_matches <- function(data1_sf, # first sf object (e.g., CAD data file)
                         data2_sf, # second sf object for comparison (e.g., Waze data file)
                         time1, # name of column in first sf object with date/time in POSIXct
                         time2, # name of column in second sf object with date/time in POSIXct
                         ID2, # name of column in second sf object with a unique ID for each record
                         near_dist = 0.5, # threshold distance for defining a potential match, in miles (0.5 by default)
                         near_time = 60 # threshold time difference for defining a potential match, in minutes (plus or minus 60 mins, by default)
){
  data1_df <- data1_sf %>% st_drop_geometry()
  data2_df <- data2_sf %>% st_drop_geometry()
  
  for(r in 1:nrow(data1_sf)){
    # compute distances from this crash point to every point in the other dataset
    # use "as.numeric" to remove units and then convert from meters to miles by multiplying by 0.0006213712
    distances = as.numeric(st_distance(data1_sf[r,],data2_sf))*0.0006213712 
    # compute time differences
    time_diffs <- as.numeric(difftime(rep(data1_df[r,time1],nrow(data2_df)),data2_df[,time2], units = "mins"))
    # define possible matches as those that are within 0.5 miles and 1 hour (or other thresholds, based on inputs to the function)
    matches <- (distances <= near_dist) & (abs(time_diffs) <= near_time)
    # what is the total number of matches for this point in the first dataset?
    data1_sf[r,"matches"] <- sum(matches)
    # what are the IDs in the second dataset for the associated time differences and distances?
    t_d_w_ids <- data.frame(ID = data2_df[,ID2],time_diffs = time_diffs, distances = distances)
    
    # # if there is at least one match, insert the ID, time difference, and distance of the "best" match in 
    # # the data frame for the first dataset, where "best" is based on minimum time difference.
    # if(sum(matches)>0){
    #   min_time_diff_for_matches <- min(t_d_w_ids[matches,"time_diffs"])
    #   is_min <- t_d_w_ids$time_diffs == min_time_diff_for_matches
      
    # if there is at least one match, insert the ID, time difference, and distance of the "best" match in 
    # the data frame for the first dataset, where "best" is based on minimum time difference.
    if(sum(matches)>0){
      min_dist <- min(t_d_w_ids[matches,"distances"])
      is_min <- t_d_w_ids$distances == min_dist  
      
      # if there is a tie, just take the first one
      data1_sf[r,"best_m_ID"] <- t_d_w_ids[is_min,"ID"][1]
      data1_sf[r,"best_m_time"] <- t_d_w_ids[is_min,"time_diffs"][1]
      data1_sf[r,"best_m_dis"] <- t_d_w_ids[is_min,"distances"][1]
      
    } # end if loop to add details for "best match," if applicable
  } # end for loop to look for potential matches in the second dataset and append to the first based on them.
  return(data1_sf)
} # end find_matches function

data1_sf = temp_train # sf object
data2_sf = waze_temp # second sf object for comparison (e.g., Waze data file)
time1 = "time_local" # name of column in first sf object with date/time in POSIXct
time2 = "time_local" # name of column in second sf object with date/time in POSIXct
ID2 = "ID" # name of column in second sf object with a unique ID for each record
near_dist = 50 # threshold distance for defining a potential match, in miles (0.5 by default)
near_time = 120
r <- 1

# this function assumes the units are in meters for both sf objects, in 5070 projection
find_matches_buff <- function(data1_sf, # first sf object (e.g., CAD data file)
                         data2_sf, # second sf object for comparison (e.g., Waze data file)
                         time1, # name of column in first sf object with date/time in POSIXct
                         time2, # name of column in second sf object with date/time in POSIXct
                         ID2, # name of column in second sf object with a unique ID for each record
                         near_dist = 50, # threshold distance for defining a potential match, in meters (50 meters by default)
                         near_time = 120 # threshold time difference for defining a potential match, in minutes (plus or minus 120 mins, by default)
){
  data1_df <- data1_sf %>% st_drop_geometry()
  data2_df <- data2_sf %>% st_drop_geometry()
  
  data1_buff <- st_buffer(data1_sf, dist = near_dist)
  
  for(r in 1:nrow(data1_sf)){
    # determine which points in data2_sf are in the buffer zone around the rth row in data1_sf 
    # st_intersects returns a boolean vector of length nrow(data2_sf)
    geo_intersects_vec = st_intersects(data1_buff[r,], data2_sf)
    
    geo_intersects_vec = st_intersects(data1_buff, data2_sf)
    
    sum(geo_intersects_vec>0)
    
    geo_intersects_vec[[1]]
    
    # create temporary sf object
    match_frame_r <- data2_sf %>%
      # use st_intersects method to filter down to only the points that intersect
      mutate(geo_intersects = geo_intersects_vec) %>%
      filter(geo_intersects)
    
    summary(match_frame_r)
    
    # calculate distance in meters for each point to the rth row of data1_sf
    match_frame_r$distances <- as.numeric(st_distance(data1_sf[r,],match_frame_r))
    
    # remove geometry and convert to data frame
    match_frame_r <- match_frame_r %>% st_drop_geometry()
    
    # calculate time differences for each point to the rth row of data1_sf
    match_frame_r$time_diffs <- as.numeric(difftime(rep(data1_df[r,time1],nrow(match_frame_r)),match_frame_r[,time2], units = "mins"))
    
    # filter to those points within 120 minutes (or other threshold, based on input to the function), and also the shortest distance to the 
    # rth row of data1_sf
    min_dist <- min(match_frame_r$time_diffs)
    match_frame_r <- match_frame_r %>% 
      filter(abs(time_diffs) <= near_time) %>%
      filter(distances == min_dist)
    
    # if there is tie, just take the first one
    # assign the ID, time difference, and distance of the match to the data1_sf
    data1_sf[r,"best_m_ID"] <- match_frame_r[1,ID2]
    data1_sf[r,"best_m_time"] <- match_frame_r[1,"time_diffs"]
    data1_sf[r,"best_m_dis"] <- match_frame_r[1,"distances"]
      
  } # end for loop to look for potential matches in the second dataset and append to the first based on them.
  return(data1_sf)
} # end find_matches_buff function

find_matches_past <- function(data1_df, # first data frame
                              data2_df, # second data frame for comparison (e.g., Waze data file)
                              ID2=ID # name of column in second sf object with a unique ID for each record
){
  for(r in 1:nrow(data1_df)){
    # determine which points in data2_sf match to the same osm_id (or an adjacent osm_id), and are >=4hours in the past, and <=current hour
    # criteria = data2_sf$osm_id == data1_df[r,"osm_id"] & 
    #   data2_sf$time_local <= data1_df[r,"time_local"] + hours(4) & 
    #   data2_sf$time_local >= data1_df[r,"time_local"] + hours(-4)
    # 
    # matches = data2_
    
    matches = data2_df %>% 
      filter(osm_id == data1_df[r,"osm_id"] &
               time_local <= data1_df[r,"time_local"] + hours(4) &
                  time_local >= data1_df[r,"time_local"] + hours(-4)
             )
    data1_df[r,'waze_alerts'] <- nrow(matches)
    
  } # end for loop to look for potential matches in the second dataset and append to the first based on them.
  return(data1_df)
} # end find_matches_past function

# function for adding in geometry (based on osm_id) and timestamp (based on year, month, hour, day)
add_geo_time <- function(x, # x is a dataframe or tibble that must have columns for osm_id, month, hour, and day, e.g., total_crashes in osm_query
                         yr = year # the year 
){
  x <- x %>% 
    left_join(state_network %>% as.data.frame(), by = 'osm_id') %>% st_as_sf()
  
  if(!one_zone){x <- x %>% 
    st_join(timezone_adj, join = st_nearest_feature) %>%
    mutate(time_local = as.POSIXct(paste0(year,"-",
                                          formatC(month, width = 2, flag = "0"),"-",
                                          formatC(day, width = 2, flag = "0")," ",
                                          hour,":","30:00"
    ), 
    tz = tz_name
    )
    )
  } else {
    x <- x %>%
      mutate(time_local = as.POSIXct(paste0(year,"-",
                                            formatC(month, width = 2, flag = "0"),"-",
                                            formatC(day, width = 2, flag = "0")," ",
                                            hour,":","30:00"
      ), 
      tz = time_zone_name
      )
      )
  } # end if else statements
} # end add_geo_time function      

# This function doesn't yet work correctly
convert_toPOSIXct_w_local_tz <- function(x, # object (dataframe, tibble, sf)
                                         time_source_col, # name of column that has time (as character)
                                         new_POSIXct_col, # name of new POSIXct column to add to x
                                         fmt = '%Y-%m-%d %H:%M:%S', # format of the time_source_col
                                         unified_tz = time_zone_name, # name of object that has time zone string (if one zone for state)
                                         varied_tz = tz_name # name of column that has time zone string (if more than one zone in state)
) {
  if(!one_zone){
    x <- x %>% 
      st_join(timezone_adj, join = st_nearest_feature) %>% 
      mutate(new_POSIXct_col = as.POSIXct(time_source_col, format = fmt, tz = varied_tz))
  } else {
    x <- x %>% 
      mutate(new_POSIXct_col = as.POSIXct(time_source_col, format = '%Y-%m-%d %H%M', tz = unified_tz))
  } # end of if clause
} # end of function called convert_toPOSIXct_w_local_tz 

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
