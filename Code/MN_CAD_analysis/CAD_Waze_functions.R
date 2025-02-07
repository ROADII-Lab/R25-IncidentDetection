
#######################################################
# Create a function that looks for potential matches in a second dataset (sf object) and adds info to a first dataset
# (sf object) based on them.
find_matches <- function(data1_sf, 
                         data2_sf,
                         time1,
                         time2,
                         ID1,
                         ID2,
                         near_dist = 0.5,
                         near_time = 60
){
  data1_df <- data1_sf %>% st_drop_geometry()
  data2_df <- data2_sf %>% st_drop_geometry()
  
  for(r in 1:nrow(data1_sf)){
    # compute distances from this crash point to every other point in the other dataset
    # use 'as.numeric' to remove units and then conver from meters to miles by multiplying by 0.0006213712
    distances = as.numeric(st_distance(data1_sf[r,],data2_sf))*0.0006213712
    # compute time differences
    time_diffs <- as.numeric(difftime(rep(data1_df[r,time1],nrow(data2_df)),data2_df[,time2], units = "mins"))
    # define possible matches as those within the thresholds
    matches <- (distances <= near_dist) & (abs(time_diffs) <= near_time)
    # what is the total nmbe rof matches for htis point in the first dataset?
    data1_sf[r,"matches"] <- sum(matches)
    # what are the IDs in the second dataset for the asssociated time differences and distances?
    t_d_w_ids <- data.frame(ID = data2_df[,ID2], time_diffs = time_diffs, distances = distances)
    # if at least one match, inssert the ID, time difference ,and distance of the "bset" match in
    # the dataframe for hte first dataset, where "best" is based on minimum time difference.
    if(sum(matches)>0){
      min_time_diff_for_matches <- min(t_d_w_ids[matches, "time_diffs"])
      is_min <- t_d_w_ids$time_diffs == min_time_diff_for_matches
      # if tehre is a tie, take hte first on
      data1_sf[r,"best_m_ID"] <- t_d_w_ids[is_min,"ID"][1]
      data1_sf[r,"best_m_time"] <- t_d_w_ids[is_min,"time_diffs"][1]
      data1_sf[r,"best_m_dis"] <- t_d_w_ids[is_min,"distances"][1]
      
    } # end if loop to add details for "best match," if applicable
  } # end for loop to look for potential matches in the second dataset for each point and append to the first absed on them
  return(data1_sf)
} # end function
#################################

near_dist = 0.5
near_time = 60
CADtypes = c("STALL")
Wazetypes = c("HAZARD_ON_SHOULDER_CAR_STOPPED", "HAZARD_ON_ROAD_CAR_STOPPED")
sub_or_alert = 'sub_type'
m = 1
#######################################
# create a function to ID matches for a particular type of record (uses the find_matches function)
get_matches_by_type <- function(near_dist = 0.5,
                                near_time = 60,
                                CADtypes,
                                Wazetypes,
                                sub_or_alert
){
  starttime = Sys.time()
  for(m in 1:12){
    sub_or_alert = enquo(sub_or_alert)
    # read in data frames for that month
    waze.month <- read.csv(waze.files.year[m]) %>% 
      filter(!!sub_or_alert %in% Wazetypes) %>%
      st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
      st_transform(projection) %>%
      st_join(state_network, join = st_nearest_feature) %>% 
      # convert to date-time
      mutate(time_local = as.POSIXct(time_local, format = "%Y-%m-%d %H:%M:%S", tz = "America/Chicago"))
    waze.month$id <- paste0(formatC(m, width = 2, flag = "0"), "_", 1:nrow(waze.month))
    
    CAD.month <- CAD20 %>% filter(month == m) %>% filter(class %in% CADtypes)
    
    #call the find matches function on each and save the result, then clear from memory before doing the next month
    CAD.month <- find_matches(data1_sf = CAD.month,
                              data2_sf = waze.month,
                              time1 = "centraltime",
                              time2 = "time_local",
                              ID1 = "eid",
                              ID2 = "id",
                              near_dist = near_dist,
                              near_time = near_time)
    
    waze.month <- find_matches(data1_sf = waze.month,
                               data2_sf = CAD.month,
                               time1 = "time_local",
                               time2 = "centraltime",
                               ID1 = "id",
                               ID2 = "eid",
                               near_dist = near_dist,
                               near_time = near_time)
    save(list = c('CAD.month','waze.month'), file = file.path(intermediatedir, 'CAD_Waze', paste0("month_", m, ".RData")))
    
    rm(list = c('CAD.month','waze.month'))
    
    timediff = Sys.time() - starttime
    
    cat("Completed month ", m, "...   ")
    cat(round(timediff, 2), attr(timediff, "unit"), "elapsed", "\n")
  }
  
  # Finally, read in and combine all the files before summary analysis and visuals
  CADfull <- data.frame()
  wazefull <- data.frame()
  
  for(m in 1:12){
    load(file.path(intermediatedir, 'CAD_Waze', paste0("month_", m, ".RData")))
    CADfull <- rbind(CADfull, CAD.month)
    wazefull <- rbind(wazefull, waze.month)
    rm(list = c('CAD.month','waze.month'))
    cat("Loaded month ", m, "...  ")
  }
  # add a column for "matched"
  CADfull$matched = ifelse(CADfull$matches > 0, 1, 0)
  wazefull$matched = ifelse(wazefull$matches > 0, 1, 0)
  
  # wrap up
  timediff = Sys.time() - starttime
  cat("Completed processing ", m, "...   ")
  cat(round(timediff, 2), attr(timediff, "unit"), "elapsed in total.", "\n")
  match_files <- list(CADfull = CADfull, wazefull = wazefull)
  return(match_files)
}
##########################################
# create function to summarize matches by county
sum_by_county <- function(CAD_sf,
                          Waze_sf,
                          county_file){
  # summarize by county from CAD perspective looking for Waze matches
  CAD_sf = st_join(CAD_sf, county_file %>% select(CTY_FIPS), join = st_nearest_feature)
  
  CAD_sf$matched = ifelse(CAD_sf$matches > 0, 1, 0)
  
  by_county = CAD_sf %>% group_by(CTY_FIPS) %>% summarize(CAD_match_percentage = mean(matched)*100) %>% st_drop_geometry()
  
  cty_file = county_file %>% left_join(by_county, by = join_by(CTY_FIPS == CTY_FIPS))
  
  # same for Waze in the opposite direction
  Waze_sf = st_join(Waze_sf,  county_file %>% select(CTY_FIPS), join = st_nearest_feature)
  
  Waze_sf$matched = ifelse(Waze_sf$matches > 0, 1, 0)
  
  by_county_w = Waze_sf %>% group_by(CTY_FIPS) %>% summarize(waze_match_percentage = mean(matched)*100) %>% st_drop_geometry()
  
  cty_file = cty_file %>% left_join(by_county_w, by = join_by(CTY_FIPS == CTY_FIPS))
  
  # return counties
  return(cty_file)
}
########################################
# function to make and save county maps
make_county_maps <- function(county_file_with_matches,
                             CAD_class,
                             Waze_type,
                             year){
  
  CADmap = ggplot() +
    geom_sf(data = state_boundary, aes(), linetype = 'solid', linewidth = 2, fill = NA, alpha = 1) +
    geom_sf(data = county_file_with_matches, aes(fill = CAD_match_percentage), alpha = 0.5, size = 0.5) +
    labs(title = paste0("Percentage of CAD ", CAD_class , " Records with a \nWaze Match (by County)")) +
    theme(axis.line = element_blank(),
          axis.text = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  
  ggsave(file.path(outputdir, paste0("CADmap_", CAD_class, "_", year, ".png")), CADmap)
  
  wazemap = ggplot() +
    geom_sf(data = state_boundary, aes(), linetype = 'solid', linewidth = 2, fill = NA, alpha = 1) +
    geom_sf(data = county_file_with_matches, aes(fill = waze_match_percentage), alpha = 0.5, size = 0.5) +
    labs(title = paste0("Percentage of Waze", Waze_type , "Records with a \nCAD Match (by County)")) +
    theme(axis.line = element_blank(),
          axis.text = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  
  ggsave(file.path(outputdir, paste0("Wazemap_", Waze_type, "_", year, ".png")), wazemap)
}

###########################################

# function to view by road type
view_by_road_type <- function(CAD_sf,
                              Waze_sf,
                              CAD_class,
                              Waze_type,
                              year){
  
  by_roadtype_CAD <- CAD_sf %>% group_by(highway) %>% summarize(match_percentage = mean(matched)*100) %>% st_drop_geometry()
  
  by_roadtype_Waze <- Waze_sf %>% group_by(highway) %>% summarize(match_percentage = mean(matched)*100) %>% st_drop_geometry()
  
  CADchart = ggplot(by_roadtype_CAD) + 
    geom_col(aes(x = highway, y = match_percentage)) +
    labs(title = "Percentage of CAD Crash Records with a \nWaze Match (by Road Class)")  +
    theme_minimal()
  ggsave(file.path(outputdir, paste0("CAD_by_roadtype_", CAD_class, "_", year, ".png")), CADchart)
  
  Wazechart = ggplot(by_roadtype_Waze) + 
    geom_col(aes(x = highway, y = match_percentage)) +
    labs(title = "Percentage of Waze Crash Records with a \nCAD Match (by Road Class)")  +
    theme_minimal()
  ggsave(file.path(outputdir, paste0("Waze_by_roadtype_", Waze_type, "_", year, ".png")), Wazechart)
}

##########################################




# Below is scratch

#m <- 1
# gc()
# starttime = Sys.time()
# for(m in 1:12){
#   # read in data frames for that month
#   waze.month <- read.csv(waze.files.year[m]) %>% 
#     filter(alert_type =="ACCIDENT") %>%
#     st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
#     st_transform(projection) %>%
#     st_join(state_network, join = st_nearest_feature) %>% 
#     # convert to date-time
#     mutate(time_local = as.POSIXct(time_local, format = "%Y-%m-%d %H:%M:%S", tz = "America/Chicago"))
#   waze.month$id <- paste0(formatC(m, width = 2, flag = "0"), "_", 1:nrow(waze.month))
#   
#   CAD.month <- CAD20 %>% filter(month == m) %>% filter(class == "CRASH")
#   
#   #call the find matches function on each and save the result, then clear from memory before doing the next month
#   CAD.month <- find_matches(data1_sf = CAD.month,
#                             data2_sf = waze.month,
#                             time1 = "centraltime",
#                             time2 = "time_local",
#                             ID1 = "eid",
#                             ID2 = "id",
#                             near_dist = 0.5,
#                             near_time = 60)
#   
#   waze.month <- find_matches(data1_sf = waze.month,
#                              data2_sf = CAD.month,
#                              time1 = "time_local",
#                              time2 = "centraltime",
#                              ID1 = "id",
#                              ID2 = "eid",
#                              near_dist = 0.5,
#                              near_time = 60)
#   save(list = c('CAD.month','waze.month'), file = file.path(intermediatedir, 'CAD_Waze', paste0("month_", m, ".RData")))
#   
#   rm(list = c('CAD.month','waze.month'))
#   
#   timediff = Sys.time() - starttime
#   
#   cat("Completed month ", m, "...   ")
#   cat(round(timediff, 2), attr(timediff, "unit"), "elapsed", "\n")
# }
# 
# # Finally, read in and combine all the files before summary analysis and visuals
# CADfull <- data.frame()
# wazefull <- data.frame()
# 
# for(m in 1:12){
#   load(file.path(intermediatedir, 'CAD_Waze', paste0("month_", m, ".RData")))
#   CADfull <- rbind(CADfull, CAD.month)
#   wazefull <- rbind(wazefull, waze.month)
#   rm(list = c('CAD.month','waze.month'))
#   cat("Loaded month ", m, "...  ")
# }
# 
