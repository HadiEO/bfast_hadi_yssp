# extr.ts <- read_csv(str_c(path, "/Amazon/extrL5n7RawBandsAmazonOneTile2000to2004Random1000.csv"))
# extr.ts <- read_csv(str_c(path, "/Amazon/extrL5n7RawBandsKalimantan2000to2004_DG1.csv"))
# extr.ts <- read_csv(str_c(path, "/Amazon/extrL5n7RawBandsKalimantan2000to2004_DG2.csv"))
# extr.ts <- read_csv(str_c(path, "/Amazon/extrL5n7RawBandsKalimantan2000to2004_SC1.csv"))
# extr.ts <- read_csv(str_c(path, "/Amazon/extrL5n7RawBandsKalimantan2000to2004_SQ9.csv"))
extr.ts <- read_csv(str_c(path, "/Amazon/extrL5n7RawBandsKalimantan2000to2004_SQ13.csv"))


# Apply scale factor
extr.ts <- extr.ts %>% mutate(blue = blue * 0.0001, green = green * 0.0001, red = red * 0.0001, 
                              nir = nir * 0.0001, swir1 = swir1 * 0.0001, swir2 = swir2 * 0.0001)

# Calculate spectral indices
extr.ts <- extr.ts %>% mutate(nbr = (nir - swir2)/(nir + swir2), 
                              ndmi = (nir - swir1)/(nir + swir1),
                              ndvi = (nir - red)/(nir + red))


# Rename scene id ('system:index') to 'sysIdx'
colnames(extr.ts)[1] <- "sysIdx"

## Parse the date and sensor from scene id
extr.ts <- extr.ts %>%  rowwise() %>%  mutate(sensor = str_split(sysIdx, "_")[[1]][2],
                                              date = ymd(str_split(sysIdx, "_")[[1]][4]))
# write_rds(extr.ts, str_c(path, "/Amazon/extrL5n7RawBandsAmazonOneTile2000to2004Random1000.rds"))
# write_rds(extr.ts, str_c(path, "/Amazon/extrL5n7RawBandsKalimantan2000to2004_DG1.rds"))
# write_rds(extr.ts, str_c(path, "/Amazon/extrL5n7RawBandsKalimantan2000to2004_DG2.rds"))
# write_rds(extr.ts, str_c(path, "/Amazon/extrL5n7RawBandsKalimantan2000to2004_SC1.rds"))
# write_rds(extr.ts, str_c(path, "/Amazon/extrL5n7RawBandsKalimantan2000to2004_SQ9.rds"))
write_rds(extr.ts, str_c(path, "/Amazon/extrL5n7RawBandsKalimantan2000to2004_SQ13.rds"))




# Tidy up 
extr.ts.Melt <- reshape2::melt(extr.ts, id.vars = c("ID", "date", "sensor"), 
                               measure.vars = c("blue", "green", "red", "nir", "swir1", "swir2", "nbr", "ndmi", "ndvi"))
# Make column point id for every unique c(lat, lon). Not needed if there is already plot Id. So later need to add Id in random points shapefile.
# extr.ts.Melt <- transform(extr.ts.Melt, pts.ID = as.numeric(interaction(lat, lon, drop=TRUE)))

x11()                                                                                             ## Plot interactive

v <- as.numeric(as.Date(paste0(seq(2000,2005,by=1),'-01-01')))
# v <- time2date(c(floor(min(allData$time)):ceiling(max(allData$time)))) 

my.ylim <- list(ndmi = c(-0.2,0.6), red = c(0, 0.1), nir = c(0,0.5), swir1 = c(0,0.3), ndvi = c(0.2, 1))

for(i in unique(extr.ts.Melt$ID)) {  
  temp.L5 <- extr.ts.Melt %>% dplyr::filter(variable == "ndvi", sensor == "LT05", ID == i)            # L5
  temp.L7 <- extr.ts.Melt %>% dplyr::filter(variable == "ndvi", sensor == "LE07", ID == i)            # L7

  plot(temp.L5$date, temp.L5$value, ylim = my.ylim$ndvi,                        #   set ylim depend on what spectral variable
       xlim = c(as.Date("2000-06-01"),             # Start date
                as.Date("2004-05-31")), pch = 19,                                      
       type = "n",              # Empty plot
       xlab = "Date", ylab = "VI")      
  
  points(temp.L5$date, temp.L5$value, pch = 19, col = "dark green")                       # Plot L5                           
  points(temp.L7$date, temp.L7$value, pch = 19, col = "magenta")                          # Plot L7

  legend("bottomleft", pch = c(19,19), col = c("dark green", "magenta"), legend = c("TM", "ETM+"))
  
  abline(v = v,lty = "dotted",col = "gray20",lwd = 1)
  
  locator(1)
}


