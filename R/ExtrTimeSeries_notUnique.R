# The Landsat NDMI time stack --------------------------------------------- 
NDMI.DG1.notUnique <- read_rds(str_c(path, "/raster_time_stack/ndmi_rds/NDMITimeStack_L578_KalArea1_selectDG_1.rds"))
NDMI.DG2.notUnique <- read_rds(str_c(path, "/raster_time_stack/ndmi_rds/NDMITimeStack_L578_KalArea1_selectDG_2.rds"))
NDMI.sq9.notUnique <- read_rds(str_c(path, "/raster_time_stack/ndmi_rds/NDMITimeStack_L578_sq_9.rds"))
NDMI.sq10.notUnique <- read_rds(str_c(path, "/raster_time_stack/ndmi_rds/NDMITimeStack_L578_sq_10.rds"))
NDMI.sq11.notUnique <- read_rds(str_c(path, "/raster_time_stack/ndmi_rds/NDMITimeStack_L578_sq_11.rds"))
NDMI.sq13.notUnique <- read_rds(str_c(path, "/raster_time_stack/ndmi_rds/NDMITimeStack_L578_sq_13.rds"))
NDMI.SC1.notUnique <- read_rds(str_c(path, "/raster_time_stack/ndmi_rds/NDMITimeStack_L578_SC_1.rds"))


# The sample Landsat pixels (polygons) ------------------------------------
shp.folder <- paste(path, "/vector_data/FINALLY_USED", sep = "")
pixels.DG1 <- readOGR(dsn = shp.folder, layer = "meshSelect_prevDG1")
pixels.DG2 <- readOGR(dsn = shp.folder, layer = "meshSelect_prevDG2")
pixels.SC1 <- readOGR(dsn = shp.folder, layer = "meshSelect_SC_1")
pixels.SQ9 <- readOGR(dsn = shp.folder, layer = "meshSelect_sq_9")
pixels.SQ10 <- readOGR(dsn = shp.folder, layer = "meshSelect_sq_10")
pixels.SQ11 <- readOGR(dsn = shp.folder, layer = "meshSelect_sq_11")
pixels.SQ13 <- readOGR(dsn = shp.folder, layer = "meshSelect_sq_13")


# First and last date of VHSR ---------------------------------------------
ref.lastDate <- list(DG1 = as.Date("2015-08-15"), DG2 = as.Date("2015-08-08"),
                     sq9 = as.Date("2014-05-13"), sq10 = as.Date("2015-08-17"),
                     sq11 = as.Date("2014-02-04"), sq13 = as.Date("2014-05-13"),
                     SC1 = as.Date("2014-02-04"))

# Need these dates in c(year, jday)
source("R/Rfunction/dateToYearJday.R")
ref.lastDate <- lapply(ref.lastDate, dateToYearJday)

# Extract time series -----------------------------------------------------
myFun <- function(rasterTS, lastDate, samples, outName) {
  NDMI.sub <- subsetRasterTS(rasterTS, maxDate = lastDate)
  samples.centerCoord <- coordinates(samples)
  extrNDMI <- zooExtract(NDMI.sub, samples.centerCoord, method = "simple")
  colnames(extrNDMI) <- as.character(samples$Id)

  # Write to disk
  write_rds(extrNDMI, paste(path, "/extracted_time_series/FINALLY_USED/", outName, ".rds", sep = ""))
}




# Debug
# rasterTS = NDMI.DG1.notUnique
# lastDate = ref.lastDate$DG1
# samples = pixels.DG1
# outName = "extrNDMIsub_DG1_notUnique"

# Not yet run
# myFun(NDMI.DG1.notUnique, ref.lastDate$DG1, pixels.DG1, "extrNDMIsub_DG1_notUnique")







