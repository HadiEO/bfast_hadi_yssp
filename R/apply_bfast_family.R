# The Landsat NDMI time stack
NDMI.DG1 <- read_rds(str_c(path, "/raster_time_stack/ndmi_rds/NDMITimeStack_L578_KalArea1_selectDG_1.rds"))
NDMI.DG2 <- read_rds(str_c(path, "/raster_time_stack/ndmi_rds/NDMITimeStack_L578_KalArea1_selectDG_2.rds"))
NDMI.SC1 <- read_rds(str_c(path, "/raster_time_stack/ndmi_rds/NDMITimeStack_L578_SC_1.rds"))
NDMI.SQ9 <- read_rds(str_c(path, "/raster_time_stack/ndmi_rds/NDMITimeStack_L578_sq_9.rds"))
NDMI.SQ10 <- read_rds(str_c(path, "/raster_time_stack/ndmi_rds/NDMITimeStack_L578_sq_10.rds"))


# Go to arcmap, make mesh polygons and points from the Landsat pixels, select example polygons (= pixels) [DONE]
# Now import the selected mesh points (= pixels) to extract the time series
selectLandsatPixels.DG1 <- readOGR(dsn = "sample_data/data3/shp", layer = "meshSelect_prevDG1_label_ok")
selectLandsatPixels.DG2 <- readOGR(dsn = "sample_data/data3/shp", layer = "meshSelect_prevDG2_label_ok")
selectLandsatPixels.SQ9 <- readOGR(dsn = str_c(path, "/vector_data"),
                                   layer = "meshSelect_sq_9_label_ok")

# Todo: use sf object instead of sp. 

#################################################################################################################
# Specify which NDMI stack and selectLandsatPixels --------------------------------
##################################################################################################################
NDMI <- NDMI.SQ10
# refPixels <- selectLandsatPixels.SQ9


#################################################################################################################
# Extract the NDMI at select Landsat pixels --------------------------------
##################################################################################################################

extrNDMI <- raster::extract(x = NDMI, y = refPixels,                                 # DG1
                                  method = "simple",            # no need buffer to account geometric error cause we are concerned with pixel-specific time series and relative changes
                                  cellnumbers = TRUE, df = TRUE)


extrNDMI$pixId <- refPixels$Id_1    # Add pixel ID

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
which.pixId <- 677                             # which pixel ID?  

temp.TS <- unlist(extrNDMI[extrNDMI$pixId == which.pixId, -c(1,2,ncol(extrNDMI),ncol(extrNDMI)-1)])
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
NDMI.uniqueDates <- makeUniqueDates(x = NDMI, sensor = "Landsat", collection = "Not Tier 1")
# If this returns error, it can be that the dates are already unique
write_rds(NDMI.uniqueDates, str_c(path, "/raster_time_stack/ndmi_rds/ndmi_sq_10_unique.rds"))

# If no duplicated dates, NDMI.uniqueDates <- NDMI

# Run BFAST Spatial 
# Todo: the years in the jday.monitStart need 
DG.firstDate <- as.Date("2010-11-13")
DG.lastDate <- as.Date("2015-08-15")
jday.monitStart <- c(year(DG.firstDate), yday(DG.firstDate))          # Date of earliest VHSR showing still forested area
jday.monitEnd <- c(year(DG.lastDate),yday(DG.lastDate))             # Date of latest VHSR
time <- system.time(  # Check formula!
  bfmArea <- bfmSpatial(NDMI.uniqueDates, start = jday.monitStart, order = 1, h = 0.25, 
                            formula = response ~ harmon + trend, history = "all",                    
                            monend = jday.monitEnd)
)
# Takes 8 mins!
write_rds(bfmArea, str_c(path, "/change_map/bfmSpatial_DG1_start2010.rds"))   # Output filename!

# Change date
change <- raster(bfmArea, 1)
# x11()
raster::plot(change)
change.year <- as.integer(change)

# Raster of change and no-change
# binary.change <- change
# binary.change[is.na(binary.change)] <- 0
# binary.change[!is.na(binary.change)] <- 1

# Write to disk
writeRaster(change.year, filename = paste(path, "/change_map/bfmSpatial_DG1_date.tif", sep = ""), format = "GTiff", overwrite = TRUE)
# writeRaster(binary.change, filename = paste(path, "/change_map/bfmSpatial_DG1_binary.tif", sep = ""), format = "GTiff")


# Change magnitude
magn <- raster(bfmArea, 2)                                    
magn.bkp <- magn                    # make a version showing only breakpoint pixels
magn.bkp[is.na(change)] <- NA

# x11()
op <- par(mfrow=c(1, 2))
raster::plot(magn.bkp, main="Magnitude: breakpoints")
raster::plot(magn, main="Magnitude: all pixels")

# Write to disk
writeRaster(magn.bkp, filename = paste(path, "/change_map/bfmSpatial_DG1_magn.tif", sep = ""), format = "GTiff")


#################################################################################################################
# Apply REGROWTH to all pixels  --------------------------------
##################################################################################################################
# Cut NDMI.DG1.uniqueDates raster time stack to 8 Aug 2015
NDMI.uniqueDates.trim <- subsetRasterTS(NDMI.uniqueDates, 
                                            maxDate = jday.monitEnd)

time.reg <- system.time(
  regrowArea <- regSpatial(NDMI.uniqueDates.trim, change = bfmArea$breakpoint, h = 0.5, type = "irregular") 
)

raster::plot(regrowArea)

# Write to disk
write_rds(regrowArea, str_c(path, "/change_map/regSpatial_DG1_end2015.rds"))

# Date of regrowth
regYear <- as.integer(regrowArea$regrowth_onset)
regYear[regYear == -9999] <- NA
writeRaster(regYear, filename = paste(path, "/change_map/regSpatial_DG1_date.tif", sep = ""), format = "GTiff", overwrite = TRUE)



