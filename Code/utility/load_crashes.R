# load crash files 
# purpose: this is a streamlined version to work with simple crash files (in various file formats ex. shp, xlsx, csv, .txt, etc.)
# notes: will not work for complex example like MN

  # Functions ----------------------------------------------------------------
  
  find_vars <- function(data,
                        str){
    
    vars <- colnames(data)[which(str_detect(string = colnames(data), pattern = regex(str, ignore_case = T)))]
    
    non_numeric <- c()
    
    if(length(vars) > 1){ # only look at numeric
      
      for(v in 1:length(vars)){
        
        if(!is.numeric(data[[which(colnames(data)==v)]])){ # if it's not numeric
          
          non_numeric <- v
          
        }
        
        vars[-non_numeric] # remove the non-numeric ones
        
      }
      
    }
    
    if(length(vars) > 1){ # if there are still multiple variables, get the user to define lat / long var
     
      stop(paste0("Warning: Multiple valid variables for ", str, " that are numeric: ", vars, ". Please define ", str, "_var."))
  
    }
    
    print(paste0(str, " variable defined as: ", vars))
    
    return(vars)
    
  }
  
  # Prep --------------------------------------------------------------------
    
  datalist <- list()
    
  n <- 0 # n keeps track of total files across all formats
  
  file_formats <- c(".shp$", ".gpkg", ".csv", ".xls", ".txt")
  
  
  # Load / Convert  to Shapefiles ---------------------------------------------------------------
    
  for(y in 1:length(file_formats)){
    
    crash_files <- list.files(crash_filepath, pattern = file_formats[y], full.names = TRUE)
      
    if(length(crash_files) != 0){
      
      print(paste0("Found ", length(crash_files), " ", file_formats[y], " files. Importing now."))
        
      for(x in 1:length(crash_files)){
          
        n <- n + 1 
          
        if(file_formats[y] == ".shp$" | file_formats[y] == ".gpkg" ){ # if shp file
            
          file <- st_read(crash_files[x])
          
          file <- file %>% mutate(across(1:length(file)-1, as.character))
            
        }
          
        if(file_formats[y] == ".xls"){ # if xls file 
            
          file <- read_excel(crash_files[x], col_types = "text")
            
        }
          
        if(file_formats[y] == ".csv" | file_formats[y] == ".txt"){
            
          file <- read_csv(crash_files[x], col_types = "c", show_col_types = FALSE)
            
        }
          
        if(file_formats[y] != ".shp$" & file_formats[y] != ".gpkg"){ # for every non-shape file
            
          if(is.na(lat_col)){ # if lat variable not defined
              
            lat_col <- find_vars(data = file, 
                                 str = "lat")
              
          }
            
          if(is.na(lon_col)){ # if long variable not defined
              
            lon_col <- find_vars(data = file,
                                  str = "lon")
              
          }
            
          file <- file %>% 
            filter(!is.na(!!as.name(lon_col)) & !is.na(!!as.name(lat_col))) %>%
            st_as_sf(coords = c(which(colnames(file) == lon_col), which(colnames(file) == lat_col)), crs = "WGS84")
            
        }
          
        file <- file %>% st_transform(projection)
          
        datalist[[n]] <- file
        
      }
        
    }
      
  }
  
  # Timezone Adjustment -----------------------------------------------------
  
  crashes <- do.call(bind_rows, datalist) 
  
  if(one_zone){
    
    crashes$adjustment <- 0 # no adjustment to any tz 
    
  } else{
    
    crashes <- crashes %>% 
      st_join(timezone_adj, join = st_nearest_feature) %>%
      mutate(adjustment = adjustment + abs(max(adjustment))) %>% 
      arrange(adjustment)
    
    time_zone_name <- crashes$tz_name[1] # need a time_zone_name / if we stick with this approach update it in tz script
    
  }
  
  if(is.na(date_time_col)){ # if data and time variable
  
  crashes <- crashes %>% 
    mutate(!!as.name(time_col) := case_when(str_length(!!as.name(time_col)) == 3 ~ paste0("0", !!as.name(time_col)),
                                            str_length(!!as.name(time_col)) == 2 ~ paste0("00", !!as.name(time_col)),
                                            str_length(!!as.name(time_col)) == 1 ~ paste0("000", !!as.name(time_col)),
                                            .default = !!as.name(time_col)),
           time_var = str_c(!!as.name(date_col), !!as.name(time_col), sep = " ")) # this is to address a likely issue where the leading zero gets cut

  
  } else{ # if just one variable name
    
    crashes <- crashes %>% 
      rename(time_var = !!as.name(date_time_col))
    
  }
  
  crashes <- crashes %>% 
    mutate(time_var = parse_date_time(time_var, orders = time_format, tz = time_zone_name),
           time_var = time_var + hour(adjustment)) # add adjustment 
