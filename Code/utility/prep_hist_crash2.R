# hist_crash_data processing
# purpose: processes crashes into hist_crashes variable

hist_crashes <- crashes %>% 
  mutate(YEAR = as.integer(lubridate::year(time_var)),
         month = as.integer(lubridate::month(time_var)),
         day = as.integer(lubridate::day(time_var)),
         hour = as.integer(lubridate::hour(time_var)),
         # storing coordinate information as numbers rather than sf geometry to reduce the size of the object
         # and speed subsequent operations.
         lon = sf::st_coordinates(.)[,1],
         lat = sf::st_coordinates(.)[,2],
         crash = 1,
         zone=tz(time_var)) %>%
  filter(YEAR != year) %>%
  st_join(state_network %>% select(osm_id), join = st_nearest_feature) %>%
  # removing geometry to speed group by operation and limit use of memory. Will add geometry back in later, but it will be the geometry for
  # the road segment, not the geometry for the crash point, because there are sometimes multiple crashes per observation (row)
  st_drop_geometry() %>%
  select(osm_id, crash) %>%
  group_by(osm_id) %>%
  summarise(crash = sum(crash)) %>%
  ungroup() %>%
  mutate(crash = ifelse(is.na(crash), 0, crash)) %>%
  rename(hist_crashes = crash)

hist_crashes <- state_network %>% 
  select(osm_id) %>% 
  st_drop_geometry() %>%
  left_join(hist_crashes, by = "osm_id") %>%
  replace_na(list(hist_crashes = 0))