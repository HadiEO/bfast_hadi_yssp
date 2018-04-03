require(raster)
require(plyr)
require(tidyverse)
require(bfastSpatial)   # See http://www.loicdutrieux.net/bfastSpatial/
require(sp)
require(rgrowth)
require(bfastPlot)


setwd("H:/MyDocuments/RESULTS/complete_analysis_2") 


# The Landsat NDMI time stack
list.files("rds")
NDMI.DG1 <- read_rds("rds/NDMITimeStack_L578_KalArea1_selectDG_1.rds")
NDMI.DG2 <- read_rds("rds/NDMITimeStack_L578_KalArea1_selectDG_2.rds")
NDMI.sq9 <- read_rds("rds/NDMITimeStack_L578_sq_9.rds")
NDMI.sq10 <- read_rds("rds/NDMITimeStack_L578_sq_10.rds")
NDMI.sq11 <- read_rds("rds/NDMITimeStack_L578_sq_11.rds")
NDMI.sq13 <- read_rds("rds/NDMITimeStack_L578_sq_13.rds")


# Go to arcmap, make mesh polygons (and points) from the Landsat image, select example polygons (= pixels) [DONE]
# Now import the selected mesh points (= pixels) to extract the spectral time series
list.files("shp")
selectLandsatPixels.DG1 <- readOGR(dsn = "H:/MyDocuments/RESULTS/complete_analysis_2/shp", layer = "meshSelect_prevDG1_label_ok")
selectLandsatPixels.DG2 <- readOGR(dsn = "H:/MyDocuments/RESULTS/complete_analysis_2/shp", layer = "meshSelect_prevDG2_label_ok")
selectLandsatPixels.sq9 <- readOGR(dsn = "H:/MyDocuments/RESULTS/complete_analysis_2/shp", layer = "meshSelect_sq_9_label_ok")
selectLandsatPixels.sq10 <- readOGR(dsn = "H:/MyDocuments/RESULTS/complete_analysis_2/shp", layer = "meshSelect_sq_10_label_ok")
selectLandsatPixels.sq11 <- readOGR(dsn = "H:/MyDocuments/RESULTS/complete_analysis_2/shp", layer = "meshSelect_sq_11_label_ok")
selectLandsatPixels.sq13 <- readOGR(dsn = "H:/MyDocuments/RESULTS/complete_analysis_2/shp", layer = "meshSelect_sq_13_label_ok")



#################################################################################################################
# Extract the NDMI at select Landsat pixels --------------------------------
##################################################################################################################

extrNDMI.DG1 <- raster::extract(x = NDMI.DG1, y = selectLandsatPixels.DG1,                                 # DG1
                                  method = "simple",            # no need buffer to account geometric error cause we are concerned with pixel-specific time series and relative changes
                                  cellnumbers = TRUE, df = TRUE)

extrNDMI.DG2 <- raster::extract(x = NDMI.DG2, y = selectLandsatPixels.DG2, method = "simple",              # DG2    
                                cellnumbers = TRUE, df = TRUE)

extrNDMI.sq9 <- raster::extract(x = NDMI.sq9, y = selectLandsatPixels.sq9, method = "simple",              # sq9   
                                cellnumbers = TRUE, df = TRUE)

extrNDMI.sq10 <- raster::extract(x = NDMI.sq10, y = selectLandsatPixels.sq10, method = "simple",            # sq10   
                                cellnumbers = TRUE, df = TRUE)

extrNDMI.sq11 <- raster::extract(x = NDMI.sq11, y = selectLandsatPixels.sq11, method = "simple",            # sq11   
                                cellnumbers = TRUE, df = TRUE)

extrNDMI.sq13 <- raster::extract(x = NDMI.sq13, y = selectLandsatPixels.sq13, method = "simple",            # sq13   
                                cellnumbers = TRUE, df = TRUE)


extrNDMI.DG1$pixId <- selectLandsatPixels.DG1$Id_1
extrNDMI.DG2$pixId <- selectLandsatPixels.DG2$Id_1
extrNDMI.sq9$pixId <- selectLandsatPixels.sq9$Id_1
extrNDMI.sq10$pixId <- selectLandsatPixels.sq10$Id_1
extrNDMI.sq11$pixId <- selectLandsatPixels.sq11$Id_1
extrNDMI.sq13$pixId <- selectLandsatPixels.sq13$Id_1


extrNDMI.DG1$Visual <- selectLandsatPixels.DG1$Visual
extrNDMI.DG2$Visual <- selectLandsatPixels.DG2$Visual
extrNDMI.sq9$Visual <- selectLandsatPixels.sq9$Visual
extrNDMI.sq10$Visual <- selectLandsatPixels.sq10$Visual
extrNDMI.sq11$Visual <- selectLandsatPixels.sq11$Visual
extrNDMI.sq13$Visual <- selectLandsatPixels.sq13$Visual



write_rds(extrNDMI.DG1, "rds/extrNDMI_DG1.rds")
write_rds(extrNDMI.DG2, "rds/extrNDMI_DG2.rds")
write_rds(extrNDMI.sq9, "rds/extrNDMI_sq9.rds")
write_rds(extrNDMI.sq10, "rds/extrNDMI_sq10.rds")
write_rds(extrNDMI.sq11, "rds/extrNDMI_sq11.rds")
write_rds(extrNDMI.sq13, "rds/extrNDMI_sq13.rds")


extrNDMI.DG1 <- read_rds("rds/extrNDMI_DG1.rds")
extrNDMI.DG2 <- read_rds("rds/extrNDMI_DG2.rds")
extrNDMI.sq9 <- read_rds("rds/extrNDMI_sq9.rds")
extrNDMI.sq10 <- read_rds("rds/extrNDMI_sq10.rds")
extrNDMI.sq11 <- read_rds("rds/extrNDMI_sq11.rds")
extrNDMI.sq13 <- read_rds("rds/extrNDMI_sq13.rds")


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
  # x11()  # Plot to check
  plot(zoo.TS, type = 'p', cex = 1, ylim = my.ylim, pch = 19, xlim = c(start(zoo.TS), end(zoo.TS)),
       xlab = "Date", ylab = my.ylab, main = paste("pixel ID ", pixel.ID, sep = ""))
  v <- as.numeric(as.Date(paste0(seq(1988,2018,by=1),'-01-01')))
  abline(v = v,lty = "dotted",col = "gray20",lwd = 1)
  # points(zoo.TS.1mo, ylim = c(0,1), col = "blue")   # this is from running next line
}




for(k in seq(1, 12, by = 4)) {                                                                                 # just do manually for now
  filename <- paste(k, "_", k+3, ".pdf", sep = "")
  pdf(paste("H:/MyDocuments/RESULTS/complete_analysis_2/figures/sq13/", filename, sep = ""),    # Change output dir here
      width = 7, height = 9, pointsize = 10)  
  par(mfrow = c(4,1))
  
  for(x in k:(k+3)) plot.extrTS(extrNDMI.sq13, x, "NDMI", c(-0.4,0.8))                            # Change arguments here
  dev.off()
}  


# Manually do the rest
# filename <- paste(33, "_", 34, ".pdf", sep = "")
filename <- paste(1, "_", 4, ".pdf", sep = "")
pdf(paste("H:/MyDocuments/RESULTS/complete_analysis_2/figures/DG1/", filename, sep = ""),    # Change output dir here
    width = 7, height = 9, pointsize = 10)  
par(mfrow = c(4,1))
for(x in 1:4) plot.extrTS(extrNDMI.DG1, x, "NDMI", c(-0.4,0.8))                            # Change arguments here
dev.off()


#################################################################################################################
# Apply BFAST  --------------------------------
##################################################################################################################
which.extrTS <- extrNDMI.DG1
which.pixId <- 677

# which.extrTS <- extrNDMI.DG2
# which.pixId <- 249

temp.TS <- unlist(which.extrTS[which.extrTS$pixId == which.pixId, -c(1,2,ncol(which.extrTS),ncol(which.extrTS)-1)])
temp.TS.df <- tibble(date = getSceneinfo(names(temp.TS))$date,
                     value = unname(temp.TS))

# There can be several observations for same date, just take the mean
temp.TS.df.unique <- temp.TS.df %>% group_by(date) %>% summarize(value = mean(value, na.rm = TRUE))

# Create zoo time series object
zoo.TS <- zoo(x = temp.TS.df.unique$value, order.by = temp.TS.df.unique$date)


# Cut time series until 31 Dec 2015 *************************************************************************
zoo.TS <- window(zoo.TS, start = start(zoo.TS), end = as.Date("2015-12-31"))

# Remove observation likely outliers  *************************************************************************
# zoo.TS[zoo.TS < -0.3] <- NA



# Interpolate  time steps
bts <- bfastts(zoo.TS, dates = time(zoo.TS), type = "irregular")

# Run bfastmonitor
bfm.H <- bfastmonitor(bts, start = c(2005,1), formula = response~harmon, order = 1, plot = TRUE, h = 0.25, history = "all")   # No break detected
# bfm.T <- bfastmonitor(bts, start = c(2000,1), formula = response~trend, order = 1, plot = TRUE, h = 0.25)
bfm.TH <- bfastmonitor(bts, start = c(2005,1), formula = response~harmon+trend, order = 1, plot = TRUE, h = 0.25, history = "all")   # changing h doesn't impact much


# Check bfm result
bfm.TH
bfm.TH$magnitude
summary(bfm.TH$model)
plot(bfm.TH$mefp, functional = NULL)

# Plot bfm result
# x11()
plot(bfm.TH, ylim = c(-0.4,0.8), cex = 1, xlab = "Date", ylab = "NDMI")


# ggplot bfm result     *************************************************************************
bts.f2op <- bts
bst.natReg <- bts

y <- list(bts.f2op, bst.natReg)
bfm <- lapply(y, FUN=function(z) bfastmonitor(z, start = c(2005, 1), formula = response~harmon+trend, order = 1, history = "all", h = 0.25))
p <- bfmPlot(bfm, displayResiduals = "monperiod", plotlabs = c("(a)", "(b)"), displayTrend = FALSE) + theme_bw()
p <- p + labs(y = "NDMI") + scale_y_continuous(limits = c(-0.4,0.8))
p


# Plot raw time series
# x11()
plot(zoo.TS, type = 'p', cex = 1, ylim = c(-0.4,0.8), pch = 19, xlim = c(start(zoo.TS), end(zoo.TS)),
     xlab = "Date", ylab = "NDMI", main = paste("pixel ID ", which.pixId, sep = ""))
v <- as.numeric(as.Date(paste0(seq(1988,2016,by=1),'-01-01')))
abline(v = v,lty = "dotted",col = "gray20",lwd = 1)







#################################################################################################################
# A second BFM --------------------------------
##################################################################################################################


#################################################################################################################
# Apply REGROWTH --------------------------------
##################################################################################################################
# h = 0.5 in DeVries et al. (2015) for regrowth monitoring
# zoo.TS <- window(zoo.TS, start = start(zoo.TS), end = as.Date("2013-12-31"))        # Further cut if a second disturbance takes place


which.bfm <- bfm.TH                   # which bfm ? **************************
reg <- tsreg(zoo.TS, change = which.bfm$breakpoint, h = 0.5, plot = TRUE,            # input is raw time series, not interpolated one (bts)
             formula = response ~ harmon,
             ylabs = c("NDMI", "MOSUM"))  
# x11()
print(reg)

#################################################################################################################
# Apply sequential-BFAST  --------------------------------
##################################################################################################################

years <- c(2005:2015); p <- 1
years.by2 <- seq(2000, 2015, by = 2); p <- 2              # Monitoring sequentially by p years
# history can be "all" or "ROC"
bfmSeq.TH <- lapply(years, 
                    FUN = function(z) bfastmonitor(window(bts, end = c(z + p, 1)), start = c(z, 1), history = "all", formula = response ~ harmon + trend, order = 1, h = 0.25))


# Check the result
plot.bfmSeq <- bfmPlot(bfmSeq.TH, plotlabs = years, displayTrend = TRUE, displayMagn = TRUE, displayResiduals = "monperiod") + theme_bw() +
  scale_y_continuous(limits = c(-0.4,0.8))
plot.bfmSeq



#################################################################################################################
# Accuracy assessment  --------------------------------
##################################################################################################################

table(extrNDMI.DG1$Visual) 
table(extrNDMI.DG2$Visual) 
table(extrNDMI.sq9$Visual) 
table(extrNDMI.sq10$Visual) 
table(extrNDMI.sq11$Visual)
table(extrNDMI.sq13$Visual) 

# intact = 23 + 26 = 49
# disturbed = 49 (OP) + 







#################################################################################################################
# Run BFAST spatial: DG1  --------------------------------
##################################################################################################################

# Need to make each raster layer is unique data, so average (na.rm = T) the raster when date is same

length(getZ(NDMI.DG1))
length(unique(getZ(NDMI.DG1)))
temp <- table(getZ(NDMI.DG1))           # There are dates with multiple layers
temp <- as_tibble(temp)
temp2 <- names(NDMI.DG1)                # the layer names are unique

temp3 <- temp[temp$n > 1,]              # Z attribute = dates with multiple layers

temp4 <- which(as.character(getZ(NDMI.DG1)) %in% temp3$Var1)    # Which layer number (ordered) belongs to the dates with multiple layers?
temp5 <- subset(NDMI.DG1, temp4)

temp6 <- which(!as.character(getZ(NDMI.DG1)) %in% temp3$Var1)   # Which layer number (ordered) NOT belongs to the dates with multiple layers?
temp7 <- subset(NDMI.DG1, temp6)


View(table(getZ(temp5)))
table(table(getZ(temp5)))               # all have 2 layers


# Take the mean of duplicated dates
k12.init <- temp5[[1]]; k12.init <- setZ(k12.init, z =  getZ(temp5)[1]) 
k12.init[] <- NA
names(k12.init) <- "init"

for(k in seq(1, nlayers(temp5), by = 2)) {      # run k12.init first!
  
  k1 <- temp5[[k]]; k1 <- setZ(k1, z = getZ(temp5)[k])
  k2 <- temp5[[k+1]]; k2 <- setZ(k2, z =  getZ(temp5)[k+1])                           # This works because each dates have exactly two scenes
  # k12 <- overlay(k1, k2, fun = function(r1, r2) mean(c(r1, r2), na.rm = TRUE))
  k12 <- stack(k1, k2)
  k12.mean <- mean(k12, na.rm = TRUE)
  names(k12.mean) <- names(k1); k12.mean <- setZ(k12.mean, z =  getZ(temp5)[k]) 
  k12.init <- stack(k12.init, k12.mean)
  
}


# Remove the first layer i.e. init
k12.init <- subset(k12.init, 2:nlayers(k12.init))


# Merge back with 
NDMI.DG1.uniqueDates <- stack(temp7, k12.init)

# SetZ and Re-order layers by dates
NDMI.DG1.uniqueDates <- setZ(NDMI.DG1.uniqueDates, getSceneinfo(names(NDMI.DG1.uniqueDates))$date, name = 'time')   
View(table(getZ(NDMI.DG1.uniqueDates)))

NDMI.DG1.uniqueDates <- subset(NDMI.DG1.uniqueDates, order(getZ(NDMI.DG1.uniqueDates)))
getZ(NDMI.DG1.uniqueDates)

# Save RDS
write_rds(NDMI.DG1.uniqueDates, 
          "rds/NDMI_DG1_uniqueDates.rds")

# Cut date to end of 2015? Alternatively, set "monend" in bfmSpatial


# Run BFAST Spatial ****************************************************************************************************
time.DG1 <- system.time(
  bfmArea.DG1 <- bfmSpatial(NDMI.DG1.uniqueDates, start = c(2005, 1), order = 1, h = 0.25, 
                            formula = response ~ harmon, history = "all",      # Formula without trend
                            monend = c(2015,228))                              # Set end monitoring period to 15 Aug 2015
)
# Above takes ? hours

readr::write_rds(bfmArea.DG1, "H:/MyDocuments/RESULTS/bfastSpatial/bfmArea_DG1.rds", compress = "none")

# Extract date of breaks
change <- raster(bfmArea.DG1, 1)
x11()
plot(change)

# Plot discrete legend
require(RColorBrewer)
require(rasterVis)
change.year <- round(change, 0)

cbPallete <- c(brewer.pal(11, "RdBu"))
brks <- c(2005:2015)
change.year_breaks <- cut(change.year, breaks=brks)
plot(change.year_breaks, col=cbPallete, legend = FALSE)
# plot(change.year_breaks, legend = TRUE)

map(add=TRUE)

par(fig=c(0,1,0,1), oma=c(0, 0, 0, 0), mar=c(0,0,0,0), pty="s", new=TRUE)
plot(0,0, type="n", bty="n", xaxt="n", yaxt="n")
legend("bottom", fill=cbPallete, legend = c('0-20', '20-50', '50-100', 
                                            '100-250', '250-450', '450-600', '600-7000'), ncol=4)


# Radost code
library(RColorBrewer)
x <- change.year
brks <- c(2006:2015); brks.char <- as.character(brks)
x11()
col <- "RdYlBu"   # "RdBu"
plot(x, col=rev(brewer.pal(10, col)), legend=F, main="")
plot(x, col=rev(brewer.pal(10, col)), legend.only=T, legend.width=1, legend.shrink=1, side=4, cex=1.25,
     axis.args=list(at=brks, labels=brks.char, cex.axis=1.25))



# Or use levelplot
levelplot(change.year, col.regions = cbPallete)



# Extract magnitude raster
magn <- raster(bfmArea.DG1, 2)                                    # Rescale
# make a version showing only breakpoint pixels
magn.bkp <- magn
magn.bkp[is.na(change)] <- NA

x11()
op <- par(mfrow=c(1, 2))
plot(magn.bkp, main="Magnitude: breakpoints")
plot(magn, main="Magnitude: all pixels")



#################################################################################################################
# Run BFAST spatial: DG2  --------------------------------
##################################################################################################################

# Need to make each raster layer is unique data, so average (na.rm = T) the raster when date is same

length(getZ(NDMI.DG2))
length(unique(getZ(NDMI.DG2)))
temp <- table(getZ(NDMI.DG2))           # There are dates with multiple layers
temp <- as_tibble(temp)
temp2 <- names(NDMI.DG2)                # the layer names are unique

temp3 <- temp[temp$n > 1,]              # Z attribute = dates with multiple layers

temp4 <- which(as.character(getZ(NDMI.DG2)) %in% temp3$Var1)    # Which layer number (ordered) belongs to the dates with multiple layers?
temp5 <- subset(NDMI.DG2, temp4)

temp6 <- which(!as.character(getZ(NDMI.DG2)) %in% temp3$Var1)   # Which layer number (ordered) NOT belongs to the dates with multiple layers?
temp7 <- subset(NDMI.DG2, temp6)


View(table(getZ(temp5)))
table(table(getZ(temp5)))               # all have 2 layers


# Take the mean of duplicated dates
k12.init <- temp5[[1]]; k12.init <- setZ(k12.init, z =  getZ(temp5)[1]) 
k12.init[] <- NA
names(k12.init) <- "init"

for(k in seq(1, nlayers(temp5), by = 2)) {      # run k12.init first!
  
  k1 <- temp5[[k]]; k1 <- setZ(k1, z = getZ(temp5)[k])
  k2 <- temp5[[k+1]]; k2 <- setZ(k2, z =  getZ(temp5)[k+1])                           # This works because each dates have exactly two scenes
  # k12 <- overlay(k1, k2, fun = function(r1, r2) mean(c(r1, r2), na.rm = TRUE))
  k12 <- stack(k1, k2)
  k12.mean <- mean(k12, na.rm = TRUE)
  names(k12.mean) <- names(k1); k12.mean <- setZ(k12.mean, z =  getZ(temp5)[k]) 
  k12.init <- stack(k12.init, k12.mean)
  
}


# Remove the first layer i.e. init
k12.init <- subset(k12.init, 2:nlayers(k12.init))


# Merge back with 
NDMI.DG2.uniqueDates <- stack(temp7, k12.init)

# SetZ and Re-order layers by dates
NDMI.DG2.uniqueDates <- setZ(NDMI.DG2.uniqueDates, getSceneinfo(names(NDMI.DG2.uniqueDates))$date, name = 'time')   
View(table(getZ(NDMI.DG2.uniqueDates)))

NDMI.DG2.uniqueDates <- subset(NDMI.DG2.uniqueDates, order(getZ(NDMI.DG2.uniqueDates)))
getZ(NDMI.DG2.uniqueDates)

# Save RDS
write_rds(NDMI.DG2.uniqueDates, 
          "rds/NDMI_DG2_uniqueDates.rds")

# Cut date to end of 2015? Alternatively, set "monend" in bfmSpatial


# Run BFAST Spatial ****************************************************************************************************
time.DG2 <- system.time(
  bfmArea.DG2 <- bfmSpatial(NDMI.DG2.uniqueDates, start = c(2005, 1), order = 1, h = 0.25, 
                            formula = response ~ harmon, history = "all",      # Formula without trend
                            monend = c(2015,221))                              # Set end monitoring period to 8 Aug 2015
)
# Above takes ? hours

readr::write_rds(bfmArea.DG2, "H:/MyDocuments/RESULTS/bfastSpatial/bfmArea_DG2.rds", compress = "none")

# Extract date of breaks
change <- raster(bfmArea.DG2, 1)
x11()
plot(change)

# Extract magnitude raster
magn <- raster(bfmArea.DG2, 2)                                    # Rescale
# make a version showing only breakpoint pixels
magn.bkp <- magn
magn.bkp[is.na(change)] <- NA

x11()
op <- par(mfrow=c(1, 2))
plot(magn.bkp, main="Magnitude: breakpoints")
plot(magn, main="Magnitude: all pixels")



#################################################################################################################
# Regrowth to DG2  --------------------------------
##################################################################################################################

time.DG2.reg <- system.time(
  regrowArea.DG2 <- regSpatial(NDMI.DG2.uniqueDates, change = bfmArea.DG2$breakpoint, h = 0.5, type = "16-day")  # Set end monitoring period to 8 Aug 2015
)
# Above takes ? hours

readr::write_rds(regrowArea.DG2, "H:/MyDocuments/RESULTS/regrowth/regrowArea_DG2.rds", compress = "none")

x11()
plot(regrowArea.DG2)


#################################################################################################################
# Regrowth to DG1  --------------------------------
##################################################################################################################

time.DG1.reg <- system.time(
  regrowArea.DG1 <- regSpatial(NDMI.DG1.uniqueDates, change = bfmArea.DG1$breakpoint, h = 0.5, type = "16-day")  # Set end monitoring period to 8 Aug 2015
)
# Above takes ? hours

readr::write_rds(regrowArea.DG1, "H:/MyDocuments/RESULTS/regrowth/regrowArea_DG1.rds", compress = "none")

x11()
plot(regrowArea.DG1)


