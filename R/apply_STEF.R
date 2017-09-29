# NDMI raster time series
NDMI <- read_rds(str_c(path, "/raster_time_stack/ndmi_rds/ndmi_DG_1_unique.rds"))

# Subset raster date
DG.firstDate <- as.Date("2002-09-29")
DG.lastDate <- as.Date("2015-08-15")
jday.monitEnd <- c(year(DG.lastDate),yday(DG.lastDate))             # Date of latest VHSR
NDMI.sub <- subsetRasterTS(NDMI, maxDate = jday.monitEnd)

# Make it brick
NDMI.sub <- brick(NDMI.sub)

# Get the date
NDMI.sub.dates <- getZ(NDMI.sub)
NDMI.sub.dates.dec <- decimal_date(NDMI.sub.dates)

# Run stef_monitor
sfQuickInit(cpus = 4)                        ## register the cores

t.mySTEF <- system.time(
  res.STEF <- rasterEngine(inraster = NDMI.sub, 
                      fun = stef_monitor, window_dims = c(windowwidth=9, windowwidth=9),
                            args=list(mYear = 2002, density = F, 
                                      my_dates = NDMI.sub.dates.dec, threshold = 0.05,
                                      spatiaNormPercentile = 95, windowwidth = 9, 
                                      tryCatchError = T, sPatioNormalixse = F))
)
# This took 86 s with 4 cpus, 311 s without parallelization
sfQuickStop()                               ## unregister the cores
x11()
raster::plot(res.STEF$layer.1)
summary(res.STEF)
