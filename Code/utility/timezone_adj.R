# Title: Timezone Adjustment Script
# Purpose: Determines if the state has one timezone, if yes, generates the timezone varible, if no, generates the shapefile which
#          will be used to adjust data to be in the correct timezone. 
# Generated Variables: 
## one_zone: True or false variable determining if the state is entirely one timezone or two
## time_zone_name: Only produced if one_zone is true, contains the name of the state's one timezone
## timezone_adj: Used to adjust timezones later on


timezone_info <- data.frame(state = state.abb,
                            tz_name = c("US/Central", "US/Alaska", "US/Mountain", "US/Central", "US/Pacific", # AL, AK, AZ, AR, CA
                                        "US/Mountain", "US/Eastern", "US/Eastern", "US/Eastern", "US/Eastern", # CO, CT, DE, FL, GA 
                                        "US/Hawaii", "US/Mountain", "US/Central", "US/Eastern", "US/Central", # HI, ID, IL, IN, IA
                                        "US/Central", "US/Eastern", "US/Central", "US/Eastern", "US/Eastern", # KS, KY, LA, ME, MD
                                        "US/Eastern", "US/Eastern", "US/Central", "US/Central", "US/Central", # MA, MI, MN, MS, MO
                                        "US/Mountain", "US/Central", "US/Pacific", "US/Eastern", "US/Eastern", # MT, NE, NV, NH, NJ
                                        "US/Mountain", "US/Eastern", "US/Eastern", "US/Central", "US/Eastern", # NM, NY, NC, ND, OH
                                        "US/Central", "US/Mountain", "US/Eastern", "US/Eastern", "US/Eastern", # OK, OR, PA, RI, SC
                                        "US/Central", "US/Eastern", "US/Central", "US/Mountain", "US/Eastern", # SD, TN, TX, UT, VT
                                        "US/Eastern", "US/Pacific", "US/Eastern", "US/Central", "US/Mountain")) %>% # VA, WA, WV, WI, WY
  filter(!is.na(tz_name)) # filter for next step 

time_zone_name <- timezone_info$tz_name[which(timezone_info$state == state)]

if(state %in% c("AK", "FL", "ID", "IN", "KS", "KY", "MI", "NE", "ND", "OR", "SD", "TN", "TX")){
  
  one_zone <- F
  
  print(paste0(state, " has multiple timezones. The model will be ran in: ", time_zone_name))
  
} else{
  
  one_zone <- T
  
}

if(!dir.exists(file.path(getwd(), "Shapefiles"))){ dir.create(file.path(getwd(), "Shapefiles")) } # if shapefile path doesn't exist, create it

if(!dir.exists(file.path(getwd(), "Shapefiles", "Time_Zones"))){ dir.create(file.path(getwd(), "Shapefiles", "Time_Zones")) } # if Time_Zones folder doesn't exist, create it

if(file.exists(file.path("Shapefiles","Time_Zones","time_zones_ds_timezone_polygons.shp"))){ # check if timezone shapefile is already saved
  
  US_timezones <- st_read(file.path("Shapefiles","Time_Zones","time_zones_ds_timezone_polygons.shp")) # if exists, load it
  
} else{ # if it does not exist, pull it from the web and save it (link is subject to change, could be an issue)
  
  US_timezones <- st_read("https://services.arcgis.com/xOi1kZaI0eWDREZv/arcgis/rest/services/NTAD_Time_Zones/FeatureServer/0/query?returnGeometry=true&where=1=1&outFields=*&f=geojson")
  
  st_write(US_timezones, file.path("Shapefiles","Time_Zones","time_zones_ds_timezone_polygons.shp")) # save it
  
}

tz_adj_to_names <- data.frame(tz_adj = c(-5,-6,-7,-8,-9,-10,-11), tz_name = c("US/Eastern","US/Central","US/Mountain","US/Pacific", "US/Alaska", "US/Hawaii", "US/Samoa"))

timezone_adj <- US_timezones %>% st_transform(crs=projection) %>% 
  mutate(adjustment = as.numeric(paste0(str_sub(utc, 1, 1), str_sub(utc, 2, 3)))) %>% 
  select(adjustment) %>% 
  left_join(tz_adj_to_names,by = join_by(adjustment==tz_adj))

rm(tz_adj_to_names, US_timezones, timezone_info)