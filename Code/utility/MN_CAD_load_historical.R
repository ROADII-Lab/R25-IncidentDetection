
lookup <- c(hist_CAD_STALL = "STALL", hist_CAD_CRASH = "CRASH", hist_CAD_HAZARD = "HAZARD", hist_CAD_ROADWORK = "ROADWORK", hist_CAD_None = "None")

is_weekday <- function(timestamps){lubridate::wday(timestamps, week_start = 1) < 6}

CAD_files <- list.files(file.path(inputdir,"Crash"), pattern = 'cadevents.csv', full.names = TRUE)

CAD_hist <- data.frame()
for(f in CAD_files){
  temp <- read.csv(f) %>%
    select(class, lat, lon, open, close, p_disposition, t_disposition)
  CAD_hist <- bind_rows(CAD_hist, temp)
}
rm(temp)

CAD_hist <- CAD_hist %>%
  filter(lon != "None" & lat != "None") %>%
  mutate(lat=as.numeric(lat),
         lon=as.numeric(lon),
         centraltime=as.POSIXct(open,format="%m/%d/%Y %H:%M",tz="America/Chicago"),
         close = as.POSIXct(close,format="%m/%d/%Y %H:%M",tz="America/Chicago"),
         time_open = difftime(close, centraltime, units = "mins"),
         YEAR = as.integer(lubridate::year(centraltime))) %>%
  filter(YEAR != year) %>%
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
  dplyr::select(osm_id, class)

CAD_hist <- CAD_hist %>% 
  mutate(n = 1) %>%
  group_by(osm_id, class) %>% 
  summarize(n = sum(n), .groups = "drop") %>%
  pivot_wider(names_from = class, values_from = n) %>%
  fill_na() %>%
  rename(all_of(lookup)) %>%
  #select(-hist_CAD_ROADWORK, -hist_CAD_None) %>%
  as.data.frame()