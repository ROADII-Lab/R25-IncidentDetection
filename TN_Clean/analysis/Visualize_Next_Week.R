# Visualize next week predictions TN
# Relies on following objects:
# next_week_out: Created in PredictWeek_TN.R
# g: grid name, string
#
library(sf)

outputdir<- "~Output"
proj.USGS <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"

grid_shp <- read_sf(file.path(inputdir, "Shapefiles"), layer = g)
grid_shp <- st_transform(grid_shp, sp::CRS(proj.USGS))

# Read in buffered state shapefile
tn_buff <- read_sf(file.path(inputdir, "census"), layer = "TN_buffered")
tn_buff <- st_transform(tn_buff, sp::CRS(proj.USGS))

# Clip grid to county shapefile
grid_intersects <- st_intersects(tn_buff, grid_shp, sparse = F)

#grid_shp_TN <- grid_shp[] # this should be updated to filter grid_shp for all grid_intersects 

#grid_shp <- grid_shp[as.vector(grid_intersects),]

plotgrid <- grid_shp

# Plot by day, aggregated 
pred_dat <- next_week_out %>%
  group_by(GRID_ID, date) %>%
  summarize(Crash_pred = sum(as.numeric(Crash_pred)),
            Prob_NoCrash = max(Prob_NoCrash),
            Prob_Crash = max(Prob_Crash))

pred_dat$GRID_ID <- as.character(pred_dat$GRID_ID)

pdf(file.path("Output/Figures",paste0('Crash_prob_', g, "_", Sys.Date(),'.pdf')),
    width = 8, height = 5)

for(day in as.character(unique(pred_dat$date))){
  plotgrid <- grid_shp
  
  plotgrid <- left_join(plotgrid, 
                        pred_dat %>% filter(date == day), 
                        by = "GRID_ID")
  
  # plotgrid[plotgrid==-Inf] = NA 
  
  # Make crash probability maps
  n_colors = 5
  probcol <- rev(heat.colors(n_colors))
  
  cuts = cut(plotgrid$Prob_Crash, n_colors)
  
  plot(plotgrid['Prob_Crash'], col = probcol[cuts], 
       border =  probcol[cuts],
       bg = 'grey90', max.plot = 15)
  
  legend("bottom", 
         #bty = 'n',
         fill = probcol,
         legend = c('Very low',
                    'Low',
                    'Medium',
                    'High',
                    'Very high'),#levels(cuts),
         cex = 0.8, ncol = 3, pt.cex = 2,
         bg = 'grey70')
  title(main = paste("Crash probabilities on", as.character(day)))
}  
dev.off()
  