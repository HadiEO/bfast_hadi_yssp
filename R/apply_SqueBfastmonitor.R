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
NDMI.sub.dates <- getSceneinfo(names(NDMI.sub))$date
NDMI.sub.dates.dec <- decimal_date(NDMI.sub.dates)

# Run SqueBfastmonitor
source("R/Rfunction/SqueBfastmonitor_mod.R")

SqueBfastmonitor_out <- ybfastmonitor_mod(inraster = NDMI.sub,
                                          myear = 2000,
                                          plot = F,
                                          history = "all",                         # only "all" implemented
                                          my_dates = NDMI.sub.dates.dec,
                                          minumum_observations = 3,                # try 5
                                          magThreshold = -0.1,                     # try -0.1, 0.01
                                          type = "irregular")                      # only "irregular" implemented
          

# Doesn't work
