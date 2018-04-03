library(dplyr)
library(tidyverse)
library(stringr)
library(lubridate)
library(reshape2)


extr.ts <- read_csv(str_c(path, "/extracted_time_series/extrMOD14A2_Ilam.csv"))
# See band values interpretation at https://explorer.earthengine.google.com/#detail/MODIS%2F006%2FMOD14A2 

## Parse the date from scene id in column 'system:index'
# Rename 'system:index' to 'sysIdx'
colnames(extr.ts)[1] <- "sysIdx"

# Don't forget rowwise() !
extr.ts <- extr.ts %>%  rowwise() %>% 
  mutate(date = as.Date(str_sub(sysIdx, 1, 10), format = "%Y_%m_%d"))

# Tidy up 
extr.ts.Melt <- reshape2::melt(extr.ts, id.vars = c("date", "lat", "lon"), 
                               measure.vars = c("FireMask", "QA"))
# Make column point id for every unique c(lat, lon)
extr.ts.Melt <- transform(extr.ts.Melt, pts.ID = as.numeric(interaction(lat, lon, drop=TRUE)))


# Plot interactively (click in the plot area to iterate the time series)
x11()                                                                                           

v <- as.numeric(as.Date(paste0(seq(2000,2018,by=1),'-01-01')))

my.ylim <- list(FireMask = c(-1,10))

for(i in unique(extr.ts.Melt$pts.ID)) {  
  temp <- extr.ts.Melt %>% dplyr::filter(variable == "FireMask", pts.ID == i)           

  plot(temp$date, temp$value, ylim = my.ylim$FireMask,                        
       xlim = c(as.Date("2000-01-01"),             # Start date
                as.Date("2017-10-01")), pch = 19,                                      
       main = str_c(i, " ", unique(temp$lat), " , ", unique(temp$lon)), type = "n", # Empty plot
       xlab = "Date", ylab = "FireMask")       # Show lat lon
  
  points(temp$date, temp$value, pch = 19, col = "magenta")                                             

  abline(v = v,lty = "dotted",col = "gray20",lwd = 1)
  
  locator(1)
}


