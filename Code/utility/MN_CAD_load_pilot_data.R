
lookup <- c(CAD_CRASH = "CRASH")

May20_26_CAD <- read.csv(file.path(inputdir,"Crash","250520 Metro CAD data.csv"))

colnames(May20_26_CAD) <- tolower(colnames(May20_26_CAD))

May20_26_CAD <- May20_26_CAD %>% 
  filter(lon != "None" & lat != "None") %>%
  mutate(lat=as.numeric(lat),
         lon=as.numeric(lon),
         centraltime=lubridate::ymd_hms(created, tz = "America/Chicago"),
         close=lubridate::mdy_hm(tmc.closed, tz = "America/Chicago"),
         time_open = difftime(close, centraltime, units = "mins")) %>%
  # some of the lat and lon values had the string "None," so above function resulted in NAs by coercion.
  # filter them out now
  filter(!is.na(lon)&!is.na(lat)) %>%
  # -- Note: could not filter based on p_disposition or t_disposition because columns not present
  # filter out also the rows where the time_open is only a few minutes. According to MnDOT,  
  # those are likely erroneous because they were closed so soon after being opened.
  filter( (time_open > 5) & ( !is.na(time_open) ) ) %>% 
  # select columns of interest
  dplyr::select(eventtype, lat, lon, centraltime) %>%
  dplyr::rename(class = eventtype) %>%
  # convert to sf object
  st_as_sf(coords = c("lon","lat"), crs = 4326) %>%
  st_transform(projection) %>%
  # associated each record with the road segment it is nearest to
  st_join(state_network %>% select(osm_id), join = st_nearest_feature) %>%
  st_drop_geometry() %>%
  dplyr::select(osm_id, centraltime, class)

May20_26_CAD <- May20_26_CAD %>% 
  mutate(n = 1) %>%
  group_by(osm_id, centraltime, class) %>% 
  summarize(n = sum(n), .groups = "drop") %>%
  pivot_wider(names_from = class, values_from = n)

May20_26_CAD <- May20_26_CAD %>% 
  as_tsibble(index = centraltime, key = osm_id) %>%
  arrange(osm_id, centraltime) %>%
  group_by(osm_id) %>%
  # time_interval is defined in RandomForest_Train.R script. 
  # if time bins are not being used then it just aggregates by hour.
  index_by(interval = floor_date(x = centraltime, unit = ifelse(time_bins, time_interval, "hours"))) %>%
  summarise(CRASH = sum(CRASH),
            .groups = "drop") %>%
  rename(centraltime = interval) %>%
  replace_na(list(CRASH = 0)) %>%
  rename(all_of(lookup)) %>%
  as.data.frame() %>% 
  mutate(Month = lubridate::month(centraltime),
         day = lubridate::day(centraltime),
         Hour = lubridate::hour(centraltime)) %>%
  select(!centraltime)