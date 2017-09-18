# The Landsat NDMI time stack
list.files("sample_data/data3/rds")
NDMI.DG1 <- read_rds("sample_data/data3/rds/NDMITimeStack_L578_KalArea1_selectDG_1.rds")
NDMI.DG2 <- read_rds("sample_data/data3/rds/NDMITimeStack_L578_KalArea1_selectDG_2.rds")
NDMI.SC1 <- read_rds(paste(path, "/raster_time_stack/ndmi_rds/NDMITimeStack_L578_SC_1.rds", sep = ""))



# Go to arcmap, make mesh polygons and points from the Landsat pixels, select example polygons (= pixels) [DONE]
# Now import the selected mesh points (= pixels) to extract the time series
selectLandsatPixels.DG1 <- readOGR(dsn = "sample_data/data3/shp", layer = "meshSelect_prevDG1_label_ok")
selectLandsatPixels.DG2 <- readOGR(dsn = "sample_data/data3/shp", layer = "meshSelect_prevDG2_label_ok")
# Todo: use sf object instead of sp. 

#################################################################################################################
# Extract the NDMI at select Landsat pixels --------------------------------
##################################################################################################################

extrNDMI.DG1 <- raster::extract(x = NDMI.DG1, y = selectLandsatPixels.DG1,                                 # DG1
                                  method = "simple",            # no need buffer to account geometric error cause we are concerned with pixel-specific time series and relative changes
                                  cellnumbers = TRUE, df = TRUE)

extrNDMI.DG2 <- raster::extract(x = NDMI.DG2, y = selectLandsatPixels.DG2, method = "simple",              # DG2    
                                cellnumbers = TRUE, df = TRUE)


extrNDMI.DG1$pixId <- selectLandsatPixels.DG1$Id_1    # Add pixel ID
extrNDMI.DG2$pixId <- selectLandsatPixels.DG2$Id_1

extrNDMI.DG1$Visual <- selectLandsatPixels.DG1$Visual  # Add pixel visual interpretation
extrNDMI.DG2$Visual <- selectLandsatPixels.DG2$Visual


#################################################################################################################
# Plot and print the time series --------------------------------
##################################################################################################################

plot.extrTS <- function(which.extrTS, which.x, my.ylab, my.ylim) {
  
  temp.TS <- unlist(which.extrTS[which.x, -c(1,2,ncol(which.extrTS),ncol(which.extrTS)-1)])
  pixel.ID <- unlist(which.extrTS[which.x, "pixId"])
  temp.TS.df <- tibble(date = getSceneinfo(names(temp.TS))$date,
                       value = unname(temp.TS))
  
  # There can be several observations for same date, just take the mean
  temp.TS.df.unique <- temp.TS.df %>% group_by(date) %>% summarize(value = mean(value, na.rm = TRUE))
  
  # Create zoo time series object
  zoo.TS <- zoo(x = temp.TS.df.unique$value, order.by = temp.TS.df.unique$date)
  plot(zoo.TS, type = 'p', cex = 1, ylim = my.ylim, pch = 19, xlim = c(start(zoo.TS), end(zoo.TS)),
       xlab = "Date", ylab = my.ylab, main = paste("pixel ID ", pixel.ID, sep = ""))
  v <- as.numeric(as.Date(paste0(seq(1988,2018,by=1),'-01-01')))
  abline(v = v,lty = "dotted",col = "gray20",lwd = 1)

}


# Loop through time series and plot 4 time series in one page
# Here 12 time series. Do manually for the rest time series not in 1-4,5-8,...
for(k in seq(1, 12, by = 4)) {                                                                   
  filename <- paste(k, "_", k+3, ".pdf", sep = "")
  pdf(paste("figures/", filename, sep = ""),    # Change output dir here
      width = 7, height = 9, pointsize = 10)  
  par(mfrow = c(4,1))
  for(x in k:(k+3)) plot.extrTS(extrNDMI.DG1, x, "NDMI", c(-0.4,0.8))                            # Function called here, change arguments
  dev.off()
}  



#################################################################################################################
# Apply BFAST to individual time series  --------------------------------
##################################################################################################################
which.extrTS <- extrNDMI.DG1                   # time series in Which DG scene?
which.pixId <- 677                             # which pixel ID?  

temp.TS <- unlist(which.extrTS[which.extrTS$pixId == which.pixId, -c(1,2,ncol(which.extrTS),ncol(which.extrTS)-1)])
temp.TS.df <- tibble(date = getSceneinfo(names(temp.TS))$date,
                     value = unname(temp.TS))

# There can be several observations for same date, just take the mean
temp.TS.df.unique <- temp.TS.df %>% group_by(date) %>% summarize(value = mean(value, na.rm = TRUE))

# Create zoo time series object
zoo.TS <- zoo(x = temp.TS.df.unique$value, order.by = temp.TS.df.unique$date)

# Cut time series until 31 Dec 2015
zoo.TS <- window(zoo.TS, start = start(zoo.TS), end = as.Date("2015-12-31"))

# Interpolate time steps.
bts <- bfastts(zoo.TS, dates = time(zoo.TS), type = "irregular")

# Run bfastmonitor with different model formulations
bfm.H <- bfastmonitor(bts, start = c(2005,1), formula = response~harmon, order = 1, plot = TRUE, h = 0.25, history = "all")  
bfm.T <- bfastmonitor(bts, start = c(2005,1), formula = response~trend, order = 1, plot = TRUE, h = 0.25, history = "all")
bfm.TH <- bfastmonitor(bts, start = c(2005,1), formula = response~harmon+trend, order = 1, plot = TRUE, h = 0.25, history = "all")   

# Check bfm result
bfm.H
bfm.H$magnitude
summary(bfm.H$model)
plot(bfm.H$mefp, functional = NULL)
plot(bfm.H, ylim = c(-0.4,0.8), cex = 1, xlab = "Date", ylab = "NDMI")


## NOTE: STEF::ybfastmonitorNorm is wrapped within rasterEngine
## It has parameter to set minimum number of valid observations
## in the pixel time series for such pixel to be analyzed.
## Also there is parameter magThreshold.


#################################################################################################################
# Apply REGROWTH --------------------------------
##################################################################################################################
which.bfm <- bfm.H                   
reg <- tsreg(zoo.TS, change = which.bfm$breakpoint, h = 0.5, plot = TRUE)            # input is raw time series, not interpolated one (bts)
print(reg)

#################################################################################################################
# Apply sequential-BFAST  --------------------------------
##################################################################################################################
p <- 2; years <- seq(2005, 2015, by = p)              

bfmSeq.H <- lapply(years, 
                    FUN = function(z) bfastmonitor(window(bts, end = c(z + p, 1)), start = c(z, 1), history = "all", 
                                                   formula = response ~ harmon, order = 1, h = 0.25))
# history = "ROC" gives too few observations to fit a new history

# Plot the result
plot.bfmSeq <- bfmPlot(bfmSeq.H, plotlabs = years, displayTrend = TRUE, displayMagn = TRUE, displayResiduals = "monperiod") + 
  theme_bw() + scale_y_continuous(limits = c(-0.4,0.8))
plot.bfmSeq

#################################################################################################################
# Apply BFAST segmentation  --------------------------------
##################################################################################################################
# Interpolate NA (bfast() does not allow NA)
tseq <- seq(start(zoo.L7Logged), end(zoo.L7Logged), by = 16)                    # Create the regular time steps, by 16 days
zoo.L7Logged.interpolNA <- na.approx(zoo.L7Logged, xout = tseq)                 # Linearly interpolate NA values

plot(zoo.L7Logged, type = 'p', cex = 1, ylim = c(0,1), pch = 19, xlim = c(as.Date("1999-01-01"), as.Date("2017-12-31")),
     xlab = "Date", ylab = "NBR")
lines(zoo.L7Logged.interpolNA, ylim = c(0,1))

# Convert zoo to ts
# See https://disc.gsfc.nasa.gov/julian_calendar.html
ts.L7Logged.interpolNA <- ts(zoo.L7Logged.interpolNA, start = c(1999, 16), end = c(2017, 3), frequency = 23)      # 23 for 16-days

# Run bfast()
t.bfast <- system.time(
  bf <- bfast(ts.L7Logged.interpolNA, h = 0.1, season = "none", max.iter = 1, breaks = NULL)
)

plot(bf)
bf


#################################################################################################################
# Run BFAST Spatial to all pixels --------------------------------
##################################################################################################################
# Need to make each raster layer is unique data, so average (na.rm = T) the raster when date is same

temp <- table(getZ(NDMI.SC1)); temp <- as_tibble(temp)           # There are dates with multiple layers
# Insert check if table(temp$n) shows only unique value 1, then no multiple dates

temp2 <- names(NDMI.SC1)                # the layer names are unique
temp3 <- temp[temp$n > 1,]              # Z attribute = dates with multiple layers
temp4 <- which(as.character(getZ(NDMI.SC1)) %in% temp3$Var1)    # Which layer number (ordered) belongs to the dates with multiple layers?
temp5 <- subset(NDMI.SC1, temp4)
temp6 <- which(!as.character(getZ(NDMI.SC1)) %in% temp3$Var1)   # Which layer number (ordered) NOT belongs to the dates with multiple layers?
temp7 <- subset(NDMI.SC1, temp6)

# Take the mean of duplicated dates
k12.init <- temp5[[1]]; k12.init <- setZ(k12.init, z =  getZ(temp5)[1])      # Initialize storage variable
k12.init[] <- NA
names(k12.init) <- "init"

for(k in seq(1, nlayers(temp5), by = 2)) {     
  
  k1 <- temp5[[k]]; k1 <- setZ(k1, z = getZ(temp5)[k])
  k2 <- temp5[[k+1]]; k2 <- setZ(k2, z =  getZ(temp5)[k+1])                           # This works because each dates have exactly two scenes
  k12 <- stack(k1, k2)
  k12.mean <- mean(k12, na.rm = TRUE)
  names(k12.mean) <- names(k1); k12.mean <- setZ(k12.mean, z =  getZ(temp5)[k]) 
  k12.init <- stack(k12.init, k12.mean)
  
}


k12.init <- subset(k12.init, 2:nlayers(k12.init))   # Remove the first layer i.e. init


NDMI.SC1.uniqueDates <- stack(temp7, k12.init)      # Merge back with images with one date (temp7)

# SetZ and Re-order layers by dates
NDMI.SC1.uniqueDates <- setZ(NDMI.SC1.uniqueDates, getSceneinfo(names(NDMI.SC1.uniqueDates))$date, name = 'time')   
View(table(getZ(NDMI.SC1.uniqueDates)))

NDMI.SC1.uniqueDates <- subset(NDMI.SC1.uniqueDates, order(getZ(NDMI.SC1.uniqueDates)))
getZ(NDMI.SC1.uniqueDates)

# If no duplicated dates
NDMI.SC1.uniqueDates <- NDMI.SC1

# Run BFAST Spatial 
time.SC1 <- system.time(
  bfmArea.SC1 <- bfmSpatial(NDMI.SC1.uniqueDates, start = c(2005, 1), order = 1, h = 0.25, 
                            formula = response ~ harmon, history = "all",                    
                            monend = c(2014,35)) # c(2015,221)                                           # Set end monitoring period to 8 Aug 2015
)


# Change date
change <- raster(bfmArea.SC1, 1)
# x11()
raster::plot(change)

# Write to disk
writeRaster(change, filename = paste(path, "/change_map/bfmSpatial_SC1_date.tif", sep = ""), format = "GTiff")


# Change magnitude
magn <- raster(bfmArea.SC1, 2)                                    
magn.bkp <- magn                    # make a version showing only breakpoint pixels
magn.bkp[is.na(change)] <- NA

x11()
op <- par(mfrow=c(1, 2))
plot(magn.bkp, main="Magnitude: breakpoints")
plot(magn, main="Magnitude: all pixels")


#################################################################################################################
# Apply REGROWTH to all pixels  --------------------------------
##################################################################################################################
# Cut NDMI.DG1.uniqueDates raster time stack to 8 Aug 2015
NDMI.DG1.uniqueDates.trim <- subsetRasterTS(NDMI.DG1.uniqueDates, 
                                            maxDate = c(2015,221))

time.DG1.reg <- system.time(
  regrowArea.DG1 <- regSpatial(NDMI.DG1.uniqueDates.trim, change = bfmArea.DG1$breakpoint, h = 0.5, type = "16-day") 
)

plot(regrowArea.DG1)



