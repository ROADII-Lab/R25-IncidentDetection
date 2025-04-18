
# This script is called by PredictWeek.R
# next_week_out is a data frame that is already in memory because PredictWeek.R created it

next_week_out <- next_week_out %>% select(osm_id, date, Hourly_CrashRisk)

DateInfo <- next_week_out %>%
  select(date) %>%
  distinct() %>%
  arrange(date) %>%
  rename(Date = date)

CrashPrediction <- next_week_out %>%
  select(osm_id, date, Hourly_CrashRisk) %>%
  group_by(osm_id, date) %>%
  summarise(Hourly_CrashRisk = mean(Hourly_CrashRisk, na.rm = TRUE)) %>%
  pivot_wider(names_from = date, values_from = Hourly_CrashRisk)
names(CrashPrediction)[-1] <- seq(0, ncol(CrashPrediction) - 2)


