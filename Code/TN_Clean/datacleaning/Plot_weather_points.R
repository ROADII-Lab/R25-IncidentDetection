# Plot weather data -- snipped called by Prep_HistoricalWeather.R


wx.avg.jan.T <- wx %>%
  group_by(STATION, lon, lat) %>%
  filter(mo == "01") %>%
  summarize(avgtempmax = mean(TMAX, na.rm=T),
            avgtempmin = mean(TMIN, na.rm=T)
  )
wx.jan.proj <- spTransform(SpatialPointsDataFrame(wx.avg.jan.T[c("lon", "lat")], 
                                                  wx.avg.jan.T,
                                                  proj4string = CRS("+proj=longlat +datum=WGS84")), CRS(proj.USGS))

wx.avg.jun.T <- wx %>%
  group_by(STATION, lon, lat) %>%
  filter(mo == "06") %>%
  summarize(avgtempmax = mean(TMAX, na.rm=T),
            avgtempmin = mean(TMIN, na.rm=T)
  )
wx.jun.proj <- spTransform(SpatialPointsDataFrame(wx.avg.jun.T[c("lon", "lat")], 
                                                  wx.avg.jun.T,
                                                  proj4string = CRS("+proj=longlat +datum=WGS84")), CRS(proj.USGS))

wx.avg.ann.P <- wx %>%
  group_by(STATION, lon, lat) %>%
  summarize(sumprecip = sum(PRCP, na.rm=T))
wx.prcp.proj <- spTransform(SpatialPointsDataFrame(wx.avg.ann.P[c("lon", "lat")], 
                                                   wx.avg.ann.P,
                                                   proj4string = CRS("+proj=longlat +datum=WGS84")), CRS(proj.USGS))

# Check with plot
pdf(file = paste0("Figures/WX_example_Maps_", g, ".pdf"), width = 10, height = 8)
plot(grid_shp, col = 'lightgrey')
tempcol <- colorRampPalette(c("purple", "blue", "green", "yellow", "orange", "red"))
cuts = cut(wx.jan.proj$avgtempmin, 10)
points(wx.jan.proj$lon, wx.jan.proj$lat,
       col = tempcol(10)[cuts], pch = 16, cex = 3)
legend("bottom", pch = 16, col = tempcol(10),
       legend = levels(cuts),
       cex = 0.8, ncol = 2, pt.cex = 2)
title(main = "Jan 2018 average low temperatures")


plot(grid_shp, col = 'lightgrey')
tempcol <- colorRampPalette(c("purple", "blue", "green", "yellow", "orange", "red"))
cuts = cut(wx.jun.proj$avgtempmax, 10)
points(wx.jun.proj$lon, wx.jun.proj$lat,
       col = tempcol(10)[cuts], pch = 16, cex = 3)
legend("bottom", pch = 16, col = tempcol(10),
       legend = levels(cuts),
       cex = 0.8, ncol = 2, pt.cex = 2)
title(main = "June 2018 average high temperatures")

plot(grid_shp, col = 'lightgrey')
preccol <- colorRampPalette(c("white", "bisque", "green", "cornflowerblue", "blue", "purple"))
cuts = cut(wx.prcp.proj$sumprecip, 10)
points(wx.prcp.proj$lon, wx.prcp.proj$lat,
       col = preccol(10)[cuts], pch = 16, cex = 3)
legend("bottom", pch = 16, col = preccol(10),
       legend = levels(cuts),
       cex = 0.8, ncol = 2, pt.cex = 2)
title(main = "Total precipitation")

dev.off()