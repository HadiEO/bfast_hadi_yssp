require(plyr)
require(tidyverse)
require(reshape2)
require(stringr)
require(bfastSpatial)

setwd("H:/MyDocuments/RESULTS/extractedTimeSeries")
# setwd("D:/extractedTimeSeries")
list.files()

# ts <- read_csv("extrL5n7OilPalm.csv")                                          ## Change the time series data here !!!
ts <- read_csv("extrL5n7n8Fire.csv") 

# Apply scale factor
ts <- ts %>% mutate(blue = blue * 0.0001, green = green * 0.0001, red = red * 0.0001, 
                    nir = nir * 0.0001, swir1 = swir1 * 0.0001, swir2 = swir2 * 0.0001)

# Calculate spectral indices
ts <- ts %>% mutate(nbr = (nir - swir2)/(nir + swir2), 
                    ndmi = (nir - swir1)/(nir + swir1))

## Parse the date from scene id in column 'system:index'
# Rename 'system:index' to 'sysIdx'
colnames(ts)[1] <- "sysIdx"

# If there are other patterns, do the next code
# ts <- ts %>% rowwise() %>% dplyr::mutate(date = bfastSpatial::getSceneinfo(str_split(sysIdx, "_")[[1]][2])$date,
#                     sensor = bfastSpatial::getSceneinfo(str_split(sysIdx, "_")[[1]][2])$sensor)


temp1 <- ts[1:14671,] %>% rowwise() %>% dplyr::mutate(date = bfastSpatial::getSceneinfo(str_split(sysIdx, "_")[[1]][3])$date,
                                         sensor = bfastSpatial::getSceneinfo(str_split(sysIdx, "_")[[1]][3])$sensor)

temp2 <- ts[14672:nrow(ts),] %>% rowwise() %>% dplyr::mutate(date = bfastSpatial::getSceneinfo(str_split(sysIdx, "_")[[1]][2])$date,
                                                      sensor = bfastSpatial::getSceneinfo(str_split(sysIdx, "_")[[1]][2])$sensor)
ts <- dplyr::bind_rows(temp1, temp2)



# Rename ETM+ SLC-on and ETM+ SLC-off to just ETM+
ts <- ts %>% mutate(sensor = dplyr::if_else(sensor %in% c("ETM+ SLC-on", "ETM+ SLC-off"), "ETM+",
                                              if_else(sensor == "TM", "TM",
                                                      if_else(sensor == "OLI", "OLI", "unknown"))))
table(ts$sensor)
View(ts)


# Tidy up 
ts.Melt <- reshape2::melt(ts, id.vars = c("lat", "lon", "date", "sensor"), 
                          measure.vars = c("blue", "green", "red", "nir", "swir1", "swir2", "nbr", "ndmi"))
# Make column point id for every unique c(lat, lon)
ts.Melt <- transform(ts.Melt, pts.ID = as.numeric(interaction(lat, lon, drop=TRUE)))

x11()                                                                                             ## Plot interactive
for(i in 1:max(ts.Melt$pts.ID)) {  
  temp.L5 <- ts.Melt %>% dplyr::filter(variable == "ndmi", sensor == "TM", pts.ID == i)            # L5
  temp.L7 <- ts.Melt %>% dplyr::filter(variable == "ndmi", sensor == "ETM+", pts.ID == i)            # L7
  temp.L8 <- ts.Melt %>% dplyr::filter(variable == "ndmi", sensor == "OLI", pts.ID == i)            # L8

  plot(temp.L5$date, temp.L5$value, ylim = c(-0.4,0.8), xlim = c(as.Date("1987-01-01"), as.Date("2017-07-01")), pch = 19,                                      # Plot L5
       main = str_c(unique(temp.L5$lat), "  ", unique(temp.L5$lon)), type = "n",
       xlab = "Date", ylab = "NDMI")       # Show lat lon
  
  points(temp.L5$date, temp.L5$value, pch = 19, col = "dark green", main = str_c(unique(temp.L5$lat), "  ", unique(temp.L5$lon)))                            # Plot L7                           
  points(temp.L7$date, temp.L7$value, pch = 19, col = "magenta", main = str_c(unique(temp.L7$lat), "  ", unique(temp.L7$lon)))  
  points(temp.L8$date, temp.L8$value, pch = 19, col = "dark orange", main = str_c(unique(temp.L8$lat), "  ", unique(temp.L8$lon)))                      # Plot L8                          
  
  legend("bottomleft", pch = c(19,19,19), col = c("dark green", "magenta", "dark orange"), legend = c("TM", "ETM+", "OLI"))
  
  locator(1)
}


