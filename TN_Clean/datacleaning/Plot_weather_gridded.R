# Plot results of historical weather prep
library(dplyr)

mins <- wx.grd.day %>%
  group_by(ID) %>%
  summarize(minP = min(PRCP, na.rm=T),
            minTmin = min(TMIN, na.rm=T),
            minTmax = min(TMAX, na.rm=T),
            minS = min(SNOW, na.rm=T))

maxs <- wx.grd.day %>%
  group_by(ID) %>%
  summarize(maxP = max(PRCP, na.rm=T),
            maxTmin = max(TMIN, na.rm=T),
            maxTmax = max(TMAX, na.rm=T),
            maxS = max(SNOW, na.rm=T))

sumPS <- wx.grd.day %>%
  group_by(ID) %>%
  summarize(sumP = sum(PRCP, na.rm=T),
            sumS = sum(SNOW, na.rm=T))

wx.grd.day$mo = format(wx.grd.day$day, "%m")

wx.avg.jan.T <- wx.grd.day %>%
  group_by(ID) %>%
  filter(mo == "01") %>%
  summarize(avg.01.tempmax = mean(TMAX, na.rm=T),
            avg.01.tempmin = mean(TMIN, na.rm=T))

wx.avg.jun.T <- wx.grd.day %>%
  group_by(ID) %>%
  filter(mo == "06") %>%
  summarize(avg.06.tempmax = mean(TMAX, na.rm=T),
            avg.06.tempmin = mean(TMIN, na.rm=T))

pdf(paste0("WX_Gridded_to_", g, ".pdf"), width = 8, height = 8)

  par(mfrow=c(2,2))
  hist(mins$minTmin, main = "Min TMIN by grid cell", col = "purple")
  hist(mins$minTmax, main = "Min TMAX by grid cell", col = "tomato")
  hist(mins$minP, main = "Min PRCP by grid cell", col = "cornflowerblue")
  hist(mins$minS, main = "Min SNOW by grid cell", col = "beige")
  
  hist(maxs$maxTmin, main = "Max TMIN by grid cell", col = "purple")
  hist(maxs$maxTmax, main = "Max TMAX by grid cell", col = "tomato")
  hist(maxs$maxP, main = "Max PRCP by grid cell", col = "cornflowerblue")
  hist(maxs$maxS, main = "Max SNOW by grid cell", col = "beige")

  par(mfrow=c(1,1))
  
  # Join to grid cells, make similar summaries as before
  wx.by.id <- left_join(mins, maxs)
  wx.by.id <- left_join(wx.by.id, sumPS)
  wx.by.id <- left_join(wx.by.id, wx.avg.jan.T)
  wx.by.id <- left_join(wx.by.id, wx.avg.jun.T)
  
  wx.by.id$ID <- as.character(wx.by.id$ID)
  
  plotgrid <- grid_shp
  plotgrid@data <- left_join(plotgrid@data, wx.by.id, by = c("GRID_ID"="ID"))
  
  plotgrid@data[plotgrid@data==-Inf] = NA
  
  # Make maxTmax, maxS, and sumP maps
  tempcol <- colorRampPalette(c("purple", "blue", "green", "yellow", "orange", "red"))
  cuts = cut(plotgrid@data$maxTmax, 10)
  plot(plotgrid, col = tempcol(10)[cuts], border =  tempcol(10)[cuts])
  legend("bottom", pch = 16, col = tempcol(10),
         legend = levels(cuts),
         cex = 0.8, ncol = 2, pt.cex = 2)
  title(main = "Max temperatures over study period")
  
  cuts = cut(plotgrid@data$avg.01.tempmin, 10)
  plot(plotgrid, col = tempcol(10)[cuts], border =  tempcol(10)[cuts])
  legend("bottom", pch = 16, col = tempcol(10),
         legend = levels(cuts),
         cex = 0.8, ncol = 2, pt.cex = 2)
  title(main = "Average January low temperatures")
  
  cuts = cut(plotgrid@data$avg.06.tempmax, 10)
  plot(plotgrid, col = tempcol(10)[cuts], border =  tempcol(10)[cuts])
  legend("bottom", pch = 16, col = tempcol(10),
         legend = levels(cuts),
         cex = 0.8, ncol = 2, pt.cex = 2)
  title(main = "Average June high temperatures")
  
  preccol <- colorRampPalette(c("white", "bisque", "green", "cornflowerblue", "blue", "purple"))
  cuts = cut(plotgrid@data$sumP, 10)
  plot(plotgrid, col = preccol(10)[cuts], border =  tempcol(10)[cuts])
  legend("bottom", pch = 16, col = preccol(10),
         legend = levels(cuts),
         cex = 0.8, ncol = 2, pt.cex = 2)
  title(main = "Sum of precipitation across the study period")
  
  
dev.off()


