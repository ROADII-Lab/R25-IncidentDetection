
lookup <- c(CAD_STALL = "STALL", CAD_CRASH = "CRASH", CAD_HAZARD = "HAZARD", CAD_ROADWORK = "ROADWORK", CAD_None = "None")

CAD <- read.csv(file.path(inputdir,"Crash",paste0(year, "_mndot_cadevents.csv"))) %>%
  filter(lon != "None" & lat != "None") %>%
  mutate(lat=as.numeric(lat),
         lon=as.numeric(lon),
         centraltime=as.POSIXct(open,format="%m/%d/%Y %H:%M",tz="America/Chicago"),
         close = as.POSIXct(close,format="%m/%d/%Y %H:%M",tz="America/Chicago"),
         time_open = difftime(close, centraltime, units = "mins")) %>%
  # some of the lat and lon values had the string "None," so above function resulted in NAs by coercion.
  # filter them out now
  filter(!is.na(lon)&!is.na(lat)) %>%
  # filter out also the rows that were duplicates, mistakes, or 'unable to locate' based on police or traffic reports
  filter(!p_disposition %in% c('DUPNCAN','UTL','CE','DUP','CANCELEV')) %>%
  filter(!t_disposition %in% c('DUPNCAN','UTL','CE','DUP','CANCELEV')) %>%
  # filter out also the rows where the time_open is only a few minutes. According to MnDOT,  
  # those are likely erroneous because they were closed so soon after being opened.
  filter(time_open > 5) %>%
  # select columns of interest
  dplyr::select(class, lat, lon, centraltime) %>%
  # convert to sf object
  st_as_sf(coords = c("lon","lat"), crs = 4326) %>%
  st_transform(projection) %>%
  # associated each record with the road segment it is nearest to
  st_join(state_network %>% select(osm_id), join = st_nearest_feature) %>%
  st_drop_geometry() %>%
  dplyr::select(osm_id, centraltime, class)

if(time_bins){
  CAD <- CAD %>% 
    as_tbl_time(index = centraltime) %>%
    collapse_by("6 hours", side = "start", clean = TRUE)
}

CAD <- CAD %>% 
  mutate(n = 1,
         month = lubridate::month(centraltime),
         day = lubridate::day(centraltime),
         hour = lubridate::hour(centraltime)) %>%
  group_by(osm_id, month, day, hour, class) %>%
  summarize(n = sum(n)) %>%
  pivot_wider(names_from = class, values_from = n)%>%
  replace_na(list(STALL = 0, CRASH = 0, HAZARD = 0, ROADWORK = 0, None = 0)) %>%
 rename(all_of(lookup))