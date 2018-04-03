# Check the effective temporal resolution of the time series with different situations
# of Landsat(s) in orbit. 
# In GEE, time series were extracted from all scenes, but 
# there can also be missing orbit or missing acquisition during overpass.

# Read rds made in analyzeExtrTimeSeries_newLandsatCollection.R
extr.ts <- read_rds(str_c(path, "/Amazon/extrL5n7RawBandsAmazonOneTile2000to2004Random1000.rds"))

NROW(extr.ts)
length(complete.cases(extr.ts)) # Huh, no NA?
# Thus it's not possible to analyze data availability with respect to scene overpass

# Group by pixel (ID)



# Make function to get date difference
# onlyValidObs = FALSE gives all acquisition (scenes)
# onlyValidObs = TRUE gives only valid (cloud-free) observations
# x can be (a) ts with index in %Y-%m-%d format
# (b) date in %Y-%m-%d format
getDateDiff <- function(x, onlyValidObs = FALSE) {
  if(onlyValidObs) {
    x <- x[!is.na(x)] 
  }
  if(class(x) == "Date") {
    xDate <- x
  } else {
    xDate <- time(x) 
  }
  
  dts <- rep(NA, length(x)-1)
  for(t in 2:length(x)) {
    dt <- xDate[t] - xDate[t-1]
    dt <- as.numeric(dt)
    dts[t-1] <- dt
  }
  return(dts)
}


# Test function
x <- extrNDMIsub_DG1[, "68"]
dateDiff_DG1 <- getDateDiff(x = x)
breaks <- seq(min(dateDiff_DG1), max(dateDiff_DG1), 8)
hist(dateDiff_DG1, breaks = breaks, right = FALSE)
# OK!

# Run the function through time series id, and through DG scenes
extrNDMIsub_DG1_ls <- as.list(extrNDMIsub_DG1)


# all acquisitions
dateDiff_DG1 <- lapply(extrNDMIsub_DG1_ls, getDateDiff)
dateDiff_DG1_vec <- unlist(dateDiff_DG1)


# Only valid observations
dateDiff_DG1_valid <- lapply(extrNDMIsub_DG1_ls, function(z) getDateDiff(z, onlyValidObs = TRUE))
dateDiff_DG1_vec_valid <- unlist(dateDiff_DG1_valid)


# Test plotting histogram
x <- dateDiff_DG1_vec
breaks <- seq(min(x), max(x), 8)
histOut <- hist(x = x, breaks = breaks, right = FALSE, freq = FALSE)

# ********************************************
# Plot histogram of frequency as percentage
# ********************************************

# Make function to plot a customized histogram
# (1) All acquisitions
myHist <- function(x, myMain) {
  myBreaks <- seq(0, 304, 8)                    # Common breaks
  histOut <- hist(x, 
                  breaks = myBreaks, 
                  right = FALSE, plot = FALSE)
  histOut$density <- histOut$counts/sum(histOut$counts) * 100   # Replace density slot with frequency percentage
  plot(histOut, freq = FALSE, ylim = c(0,50),
       ylab = "Frequency (%)", 
       xlab = "Time between acquisitions or scenes (days)",
       main = myMain)
}

# (2) Cloud-free observations
myHistValid <- function(x, myMain) {
  myBreaks <- seq(0, 1120, 8)                    # Common breaks. All but one DG scene has max=728. DG1 has 47 obs (0.7%) with dateDiff >728 (up to 1120)
  histOut <- hist(x, 
                  breaks = myBreaks, 
                  right = FALSE, plot = FALSE)
  histOut$density <- histOut$counts/sum(histOut$counts) * 100   # Replace density slot with frequency percentage
  plot(histOut, freq = FALSE, ylim = c(0,30), xlim = c(0,365),
       ylab = "Frequency (%)", 
       xlab = "Time between valid observations (days)",
       main = myMain)
}



# Use the function to plot a customized histogram
# (1) All acquisitions
myHist(dateDiff_DG1_vec, "DG1")               # DG1


# (2) Only valid observations
myHistValid(dateDiff_DG1_vec_valid, "DG1")               # DG1





