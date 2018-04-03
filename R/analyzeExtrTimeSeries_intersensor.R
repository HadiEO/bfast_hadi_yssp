# extr.ts <- read_csv(str_c(path, "/extracted_time_series/extrL578_Suonenjoki_fieldCCplots2015.csv"))
extr.ts <- read_csv(str_c(path, "/Amazon/extrL5n7RawBandsAmazonOneTileTest7DaysRandom10000.csv"))

# Apply scale factor
extr.ts <- extr.ts %>% mutate(blue = blue * 0.0001, green = green * 0.0001, red = red * 0.0001, 
                    nir = nir * 0.0001, swir1 = swir1 * 0.0001, swir2 = swir2 * 0.0001)

# Calculate spectral indices
extr.ts <- extr.ts %>% mutate(nbr = (nir - swir2)/(nir + swir2), 
                    ndmi = (nir - swir1)/(nir + swir1))

## Parse the date from scene id in column 'system:index'
# Rename 'system:index' to 'sysIdx'
colnames(extr.ts)[1] <- "sysIdx"

# substr(names(S1.VV_DG1), 18, 25)

# ******************************************************************************
# If there are other patterns (1_2_LE71890162016131_0000a6f7c8816952e47a_0 vs 2_LC81870162013132_0000b4b0135ed7661501_0), 
# temp1 <- extr.ts[1:4697,]
# temp2 <- extr.ts[4698:nrow(extr.ts),]
# # temp1$sceneId <- str_split(temp1$sysIdx, "_")[[1]][3]
# # temp2$sceneId <- str_split(temp2$sysIdx, "_")[[1]][2]

# Don't forget rowwise() !
# temp1 <- temp1 %>%  rowwise() %>%  mutate(sceneId = str_split(sysIdx, "_")[[1]][3])
# temp2 <- temp2 %>%  rowwise() %>%  mutate(sceneId = str_split(sysIdx, "_")[[1]][2])
# 
# extr.ts <- dplyr::bind_rows(temp1, temp2)
# ******************************************************************************
extr.ts <- extr.ts %>%  rowwise() %>%  mutate(sceneId = str_split(sysIdx, "_")[[1]][3])


# Use modified function to disable the line setting row.names, which somewhat return error "duplicate row names"
source("R/Rfunction/getSceneinfo_mod.R")

# extr.ts$date <- getSceneinfo_mod(extr.ts$sceneId)$date
# extr.ts$sensor <- getSceneinfo_mod(extr.ts$sceneId)$sensor

# Aha, if error of "invalid subscript type 'closure'", just subset by [["name"]]
# Don't forget rowwise() !
extr.ts <- extr.ts %>% rowwise() %>%  mutate(date = getSceneinfo_mod(sceneId)[["date"]], 
                              sensor = getSceneinfo_mod(sceneId)[["sensor"]])


# Rename ETM+ SLC-on and ETM+ SLC-off to just ETM+
extr.ts <- extr.ts %>% mutate(sensor = dplyr::if_else(sensor %in% c("ETM+ SLC-on", "ETM+ SLC-off"), "ETM+",
                                              if_else(sensor == "TM", "TM",
                                                      if_else(sensor == "OLI", "OLI", "unknown"))))
# table(extr.ts$sensor)
# View(extr.ts)


# Check if X,Y = lon,lat
x11()
plot(round(extr.ts$X, 5), round(extr.ts$lon, 5)); abline(0,1)
plot(round(extr.ts$Y, 5), round(extr.ts$lat, 5)); abline(0,1)
cor(round(extr.ts$X, 5), round(extr.ts$lon, 5))
cor(round(extr.ts$Y, 5), round(extr.ts$lat, 5))



# Tidy up 
extr.ts.Melt <- reshape2::melt(extr.ts, id.vars = c("Plot", "X", "Y", "date", "sensor", "CC"), 
                          measure.vars = c("blue", "green", "red", "nir", "swir1", "swir2", "nbr", "ndmi"))
# Make column point id for every unique c(lat, lon). Not needed if there is already plot Id
# extr.ts.Melt <- transform(extr.ts.Melt, pts.ID = as.numeric(interaction(lat, lon, drop=TRUE)))




x11()                                                                                             ## Plot interactive

v <- as.numeric(as.Date(paste0(seq(1984,2018,by=1),'-01-01')))

# my.ylim <- list(ndmi = c(-0.4,0.8), red = c(0, 0.1), nir = c(0,0.5), swir1 = c(0,0.3))
my.ylim <- list(ndmi = c(-0.2,0.6), red = c(0, 0.1), nir = c(0,0.5), swir1 = c(0,0.3))


for(i in unique(extr.ts.Melt$Plot)) {  
  temp.L5 <- extr.ts.Melt %>% dplyr::filter(variable == "ndmi", sensor == "TM", Plot == i)            # L5
  temp.L7 <- extr.ts.Melt %>% dplyr::filter(variable == "ndmi", sensor == "ETM+", Plot == i)            # L7
  temp.L8 <- extr.ts.Melt %>% dplyr::filter(variable == "ndmi", sensor == "OLI", Plot == i)            # L8

  plot(temp.L5$date, temp.L5$value, ylim = my.ylim$ndmi,                        #   set ylim depend on what spectral variable
       xlim = c(as.Date("1984-01-01"),             # Start date
              as.Date("2017-07-01")), pch = 19,                                      
       main = str_c(i, " CC = ", unique(temp.L5$CC)), type = "n",              # Empty plot
       xlab = "Date", ylab = "NDMI")      
  
  points(temp.L5$date, temp.L5$value, pch = 19, col = "dark green")                       # Plot L5                           
  points(temp.L7$date, temp.L7$value, pch = 19, col = "magenta")                          # Plot L7
  points(temp.L8$date, temp.L8$value, pch = 19, col = "dark orange")                      # Plot L8                          
  
  legend("bottomleft", pch = c(19,19,19), col = c("dark green", "magenta", "dark orange"), legend = c("TM", "ETM+", "OLI"))
  
  abline(v = v,lty = "dotted",col = "gray20",lwd = 1)
  
  locator(1)
}

# Plot 7 seems thinned after 2015

