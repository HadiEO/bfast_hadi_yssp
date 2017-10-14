

# Start cluster -----------------------------------------------------------

detectCores()
cl <- makeCluster(3)
registerDoParallel(cl)
getDoParWorkers()
clusterEvalQ(cl, .libPaths("C:/Program Files/R/R-3.3.1/library"))
clusterEvalQ(cl, library(doParallel))


# DG1 ---------------------------------------------------------------------

# Load the saved data
extrNDMI <- read_rds(str_c(path, "/extracted_time_series/extrNDMIsub_DG1.rds"))

# Create gap-less time series
extrNDMI.ls <- as.list(extrNDMI)
bts.ls <- lapply(extrNDMI.ls, FUN = function(z) bfastts(z, dates = time(z), type = "irregular"))

# Interpolate time series
bts.ls.int <- lapply(bts.ls, FUN = function(z) na.approx(z))

# Make monthly time series
# Function
source("R/Rfunction/makeMonthlyTs.R")

mts.ls <- lapply(bts.ls.int, makeMonthlyTs)


t.segment <- system.time(
  bf.ls <- lapply(mts.ls, function(z) bfast(z, h = 0.15, season = "harmonic", max.iter = 1, hpc = "foreach"))
)
write_rds(bf.ls, str_c(path, "/extracted_time_series/bf_DG1_subTS_h015.rds"))


# DG2 ---------------------------------------------------------------------

# Load the saved data
extrNDMI <- read_rds(str_c(path, "/extracted_time_series/extrNDMIsub_DG2.rds"))

# Create gap-less time series
extrNDMI.ls <- as.list(extrNDMI)
bts.ls <- lapply(extrNDMI.ls, FUN = function(z) bfastts(z, dates = time(z), type = "irregular"))

# Interpolate time series
bts.ls.int <- lapply(bts.ls, FUN = function(z) na.approx(z))

# Make monthly time series
# Function
source("R/Rfunction/makeMonthlyTs.R")

mts.ls <- lapply(bts.ls.int, makeMonthlyTs)

# Run segmentation
t.segment.2 <- system.time(
  bf.ls <- lapply(mts.ls, function(z) bfast(z, h = 0.15, season = "harmonic", max.iter = 1, hpc = "foreach"))
)
write_rds(bf.ls, str_c(path, "/extracted_time_series/bf_DG2_subTS_h015.rds"))

# Just natural revegetation samples (from DG2) ---------------------------------------------------------------------

# Load the saved data
extrNDMI <- read_rds(str_c(path, "/extracted_time_series/extrNDMIsub_DG2_justNatReveg.rds"))

# Create gap-less time series
extrNDMI.ls <- as.list(extrNDMI)
bts.ls <- lapply(extrNDMI.ls, FUN = function(z) bfastts(z, dates = time(z), type = "irregular"))

# Interpolate time series
bts.ls.int <- lapply(bts.ls, FUN = function(z) na.approx(z))

# Make monthly time series
# Function
source("R/Rfunction/makeMonthlyTs.R")

mts.ls <- lapply(bts.ls.int, makeMonthlyTs)

# Run segmentation
t.segment.3 <- system.time(
  bf.ls <- lapply(mts.ls, function(z) bfast(z, h = 0.15, season = "harmonic", max.iter = 1, hpc = "foreach"))
)
write_rds(bf.ls, str_c(path, "/extracted_time_series/bf_justNatReveg_subTS_h015.rds"))




# Stop cluster ------------------------------------------------------------

stopCluster(cl)


# Select oil palm (DG1) and natural reveg samples -------------------------
bf.DG1.ls <- read_rds(str_c(path, "/extracted_time_series/bf_DG1_subTS_h015.rds"))
bf.natReveg.ls <- read_rds(str_c(path, "/extracted_time_series/bf_justNatReveg_subTS_h015.rds"))

# The shapefile
shp.folder <- paste(path, "/vector_data", sep = "")
refPixels.DG1 <- readOGR(dsn = shp.folder, layer = "meshSelect_prevDG1_label_ok")
refPixels.justNatReveg <- readOGR(dsn = shp.folder, layer = "meshSelect_DG2_label_justNatReveg")  # 41 samples

# bf.natReveg.ls is already for only natural revegetation samples. Now get bf results for oil palm samples.
refPixels.DG1.oilPalm <- refPixels.DG1[refPixels.DG1$Visual %in% 
                                         c("Oil palm (visible 20150815) older",
                                           "Oil palm (visible 20150815) others",
                                           "Oil palm (visible 20150815) younger"), ]  # 49 samples


oilPalm.idx <- which(names(bf.DG1.ls) %in% refPixels.DG1.oilPalm$Id_1)
bf.DG1OilPalm.ls <- bf.DG1.ls[oilPalm.idx]

# Check bf results
for(i in 1:length(bf.DG1OilPalm.ls)) {
  plot(bf.DG1OilPalm.ls[[i]], type="trend", largest=TRUE, main = names(bf.DG1OilPalm.ls)[i])
  locator(1)
}

for(i in 1:length(bf.natReveg.ls)) {
  plot(bf.natReveg.ls[[i]], type="trend", largest=TRUE, main = names(bf.natReveg.ls)[i])
  locator(1)
}

# For natural revegetation, seems we need to take the latest break
# So, for now, just take latest break to ensure disturbance at the right date

# Test
fit <- bf.DG1OilPalm.ls$`322`
names(fit)
niter <- length(fit$output)
(slopes <- coef(fit$output[[niter]]$bp.Vt)[,2])
plot(fit, type="trend", largest=TRUE)
# OK

# Function to get slope of trend components
getLatestSlope <- function(fit) {
  niter <- length(fit$output)
  slopes <- coef(fit$output[[niter]]$bp.Vt)[,2]
  slope.latest <- slopes[length(slopes)]
  return(slope.latest)
}

latestSlope.DG1OilPalm.ls <- lapply(bf.DG1OilPalm.ls, getLatestSlope)
latestSlope.natReveg.ls <- lapply(bf.natReveg.ls, getLatestSlope)

# Compare slope distribution between NatReveg and OilPalm
x11()
par(mfrow = c(2,1))
hist(unlist(latestSlope.DG1OilPalm.ls), col = "blue", xlim = c(-0.1, 0.2), ylim = c(0,12),
     main = "Slope of recovery segment\nconversion to oil palm (N = 49)",
     xlab = "Slope of NMDI trend", ylab = "#samples")
hist(unlist(latestSlope.natReveg.ls), col = "red", xlim = c(-0.1, 0.2), ylim = c(0,12),
     # breaks = c(-0.1, 0, 0.05, 0.1, 0.11, 0.12, 0.13, 0.14, 0.15, 0.18), freq = TRUE,
     breaks = seq(-0.1, 0.2, by = 0.01), freq = TRUE,
     main = "Slope of recovery segment\nnatural revegetation (N = 41)",
     xlab = "Slope of NMDI trend", ylab = "#samples")



