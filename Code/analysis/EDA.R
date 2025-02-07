
library(tidyverse)
library(ggplot2)

inputdir <- file.path(getwd(),"Input")
outputdir <-file.path(getwd(),"Output")
intermediatedir <- file.path(getwd(), "Intermediate")

state <- "WA"

year <- 2021

projection <- 5070 

imputed_values <- list.files(file.path(intermediatedir, "Month_Frames"), 
                             pattern = paste(state, year, "month_frame_imputed_waze", sep = "_"), 
                             full.names = TRUE)

imputed_waze_data <- list()

for(i in seq_along(imputed_values)){
  load(imputed_values[i])
  colnames(waze_averages)[2:ncol(waze_averages)] <- str_to_title(colnames(waze_averages)[2:ncol(waze_averages)])
  waze_averages$osm_id <- factor(waze_averages$osm_id)
  waze_averages$Month <- factor(waze_averages$Month)
  waze_averages$Weekday <- factor(waze_averages$Weekday)
  waze_averages$Hour <- factor(waze_averages$Hour)
  waze_averages <- waze_averages %>% rename(weekday = Weekday)
  imputed_waze_data[[i]] <- waze_averages
  gc()
}

imputed_waze_frame <- do.call(rbind, imputed_waze_data)

rm(imputed_waze_data)

source("utility/Prep_OSMNetwork.R")

state_network <- state_network %>% st_drop_geometry()

imputed_waze_frame <- imputed_waze_frame %>% left_join(state_network, by = "osm_id")

avg_exp_jam_level_by_hour <- imputed_waze_frame %>% 
  group_by(Hour) %>%
  summarize(mean_exp_jam_level = mean(Average_jam_level))

mean_by_hour = ggplot(data=avg_exp_jam_level_by_hour, mapping=aes(x=Hour, y=mean_exp_jam_level)) + 
  geom_bar(stat = "identity", fill = "red") + 
  labs(title = "Average Expected Jam Level by Hour",
       y = "Average Expected Jam Level",
       x = "Hour")

ggsave(plot = mean_by_hour, 
       filename = paste0("jam_by_hour_", state, "_", year, ".png"),
       path = file.path(outputdir, "Figures"),
       device = "png",
       create.dir = T,
       height = 5, width = 5, units = "in")

avg_exp_jam_level_by_highway <- imputed_waze_frame %>% 
  group_by(highway) %>%
  summarize(mean_exp_jam_level = mean(Average_jam_level))

mean_by_highway = ggplot(data=avg_exp_jam_level_by_highway, mapping=aes(x=highway, y=mean_exp_jam_level)) + 
  geom_bar(stat = "identity", fill = "blue") + 
  labs(title = "Average Expected Jam Level by Highway",
       y = "Average Expected Jam Level",
       x = "Hour")

ggsave(plot = mean_by_highway, 
       filename = paste0("jam_by_highway_", state, "_", year, ".png"),
       path = file.path(outputdir, "Figures"),
       device = "png",
       create.dir = T,
       height = 5, width = 5, units = "in")  

rm(imputed_waze_frame)

gc()
  
# a vector of all file paths for monthly data frames.
monthframe_fps <- file.path(intermediatedir, 'Month_Frames', paste(state, year, 1:12, 'month_frame_full.Rdata', sep = "_"))

m <- 1
list_of_hourly_charts = list()
for(m in 1:12){
  starttime <- Sys.time()
  
  load(monthframe_fps[m])
  
  avg_crash_by_hour <- temp_train %>% 
    group_by(Hour) %>%
    summarize(avg_crash = mean(crash))
  
  mean_by_hour = ggplot(data=avg_crash_by_hour, mapping=aes(x=Hour, y=avg_crash)) + 
    geom_bar(stat = "identity", fill = "red") + 
    labs(title = "Average Expected Crash by Hour",
         y = "Average Expected Crash",
         x = "Hour")
  
  ggsave(plot = mean_by_hour, 
         filename = paste0("waze_by_hour_", "for_month", m, "_", state, "_", year, ".png"),
         path = file.path(outputdir, "Figures"),
         device = "png",
         create.dir = T,
         height = 5, width = 5, units = "in")
  
  list_of_hourly_charts[[m]] <- mean_by_hour
  rm(temp_train)
  gc()
}


ggsave(plot = mean_by_hour, 
       filename = paste0("waze_by_hour_", "for_month", m, "_", state, "_", year, ".png"),
       path = file.path(outputdir, "Figures"),
       device = "png",
       create.dir = T,
       height = 5, width = 5, units = "in")

avg_exp_jam_level_by_highway <- imputed_waze_frame %>% 
  group_by(highway) %>%
  summarize(mean_exp_jam_level = mean(Average_jam_level))

mean_by_highway = ggplot(data=avg_exp_jam_level_by_highway, mapping=aes(x=highway, y=mean_exp_jam_level)) + 
  geom_bar(stat = "identity", fill = "blue") + 
  labs(title = "Average Expected Jam Level by Highway",
       y = "Average Expected Jam Level",
       x = "Hour")

ggsave(plot = mean_by_highway, 
       filename = paste0("jam_by_highway_", state, "_", year, ".png"),
       path = file.path(outputdir, "Figures"),
       device = "png",
       create.dir = T,
       height = 5, width = 5, units = "in") 