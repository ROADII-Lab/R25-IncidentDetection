
##### function to load predictions
load_and_combine_csv <- function(runs) {
  # Initialize an empty list to store data frames
  df_list <- vector("list", length(runs))
  
  for (i in seq_along(runs)) {
    filename = file.path(predict_week_out_dir, paste0(modelno,'_', runs[i], '.csv'))
    df = read_csv(filename, col_types = cols(osm_id = col_character(),
                                             date = col_datetime(format = ""))) %>%
      mutate(date = lubridate::force_tz(date, tzone = "America/Chicago"))
    
    if(i == length(runs)){
      df_list[[i]] = df
    } else {
      df = df %>% dplyr::filter(date<runs[i+1])
      df_list[[i]] = df
    } # end of if - else
    
    combined_df = bind_rows(df_list)
  } # end of for loop
  return(combined_df)
} # end of load_and_combine_csv function

##### function to load CAD pilot results
lookup <- c(CAD_CRASH = "CRASH")

load_CAD_pilot_results <- function(fp) {
  
  df <- read.csv(fp)
  
  colnames(df) <- tolower(colnames(df))
  
  df <- df %>% 
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
  
  df <- df %>% 
    mutate(n = 1) %>%
    group_by(osm_id, centraltime, class) %>% 
    summarize(n = sum(n), .groups = "drop") %>%
    pivot_wider(names_from = class, values_from = n)
  
  df <- df %>% 
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
           Hour = lubridate::hour(centraltime)) 
  
  latest_record <- max(df$centraltime)
  
  df <- df %>% select(!centraltime)  
  
  return(list(df = df, latest_record = latest_record))
  
}

