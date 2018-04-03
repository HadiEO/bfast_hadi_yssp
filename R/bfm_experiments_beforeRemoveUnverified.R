# ****************************************************************************
# Configure bfastmonitor, execute one run by one, 
# changing the bfmOutName and outSamplesName arguments accordingly
# ****************************************************************************
# Run 1: Original bfastmonitor *without* noise removal
historyNoiseRemoved <- FALSE 
cons <- 1 
maxTimeSpan <- NULL

# Run 2: Original bfastmonitor *with* noise removal
historyNoiseRemoved <- TRUE 
cons <- 1 
maxTimeSpan <- NULL
updateMOSUM <- FALSE

# Run 3: Modified bfastmonitor with noise removal, cons = 4, maxTimeSpan = 2
historyNoiseRemoved <- TRUE
cons <- 4
maxTimeSpan <- 2
updateMOSUM <- FALSE

# Run 3: Modified bfastmonitor with noise removal, cons = 4, maxTimeSpan = 2
historyNoiseRemoved <- TRUE
cons <- 4
maxTimeSpan <- 2
updateMOSUM <- FALSE

# Run 4: Modified bfastmonitor with noise removal, cons = 6, maxTimeSpan = 2
historyNoiseRemoved <- TRUE
cons <- 6
maxTimeSpan <- 2
updateMOSUM <- FALSE

# Run 5: Modified bfastmonitor with noise removal, cons = 4, maxTimeSpan = 9999 (no limit to time span)
historyNoiseRemoved <- TRUE
cons <- 4
maxTimeSpan <- 9999
updateMOSUM <- FALSE

# Run 6 : Run 3, but no noise removal
historyNoiseRemoved <- FALSE
cons <- 4
maxTimeSpan <- 2
updateMOSUM <- FALSE

# Run 7: Run 5, but no noise removal
historyNoiseRemoved <- FALSE
cons <- 4
maxTimeSpan <- 9999
updateMOSUM <- FALSE

# Run X: Run 6, but with season-only model
# change formula in attachBfmFlagToSp() to response~harmon
historyNoiseRemoved <- FALSE
cons <- 4
maxTimeSpan <- 2
updateMOSUM <- FALSE
# Function doesn't work yet

# Run 8: Run 6, but noise removed for the whole time series in priori
historyNoiseRemoved <- FALSE
allNoiseRemoved <- TRUE
cons <- 4
maxTimeSpan <- 2
updateMOSUM <- FALSE
# Not 100% sure if the code works fine if allNoiseRemoved == TRUE

# Run 9: without noise removal, cons = 3, maxTimSpan = 1
historyNoiseRemoved <- FALSE
allNoiseRemoved <- FALSE
cons <- 4
maxTimeSpan <- 1
updateMOSUM <- FALSE

# Run 10: history noise removal, cons = 4, maxTimSpan = 2, updateMOSUM = TRUE
historyNoiseRemoved <- TRUE
allNoiseRemoved <- FALSE
cons <- 4
maxTimeSpan <- 2
updateMOSUM <- TRUE

# Run 11: no noise removal, cons = 3, maxTimSpan = 2, updateMOSUM = TRUE
historyNoiseRemoved <- FALSE
allNoiseRemoved <- FALSE
cons <- 3
maxTimeSpan <- 2
updateMOSUM <- TRUE

# Run 12: run 11, but with history noise removal (to run)
historyNoiseRemoved <- TRUE
allNoiseRemoved <- FALSE
cons <- 3
maxTimeSpan <- 2
updateMOSUM <- TRUE

# Run 13: run 11, but updateMOSUM = FALSE
historyNoiseRemoved <- FALSE
allNoiseRemoved <- FALSE
cons <- 3
maxTimeSpan <- 2
updateMOSUM <- FALSE

# Run 14: run 11, but denoise whole time series
historyNoiseRemoved <- FALSE
allNoiseRemoved <- TRUE
cons <- 3
maxTimeSpan <- 2
updateMOSUM <- TRUE


# ***********************************************
# RMSE as boundary from here on
###############################################
# Run 15: 3 * histRMSE, history denoised, cons=3, span=2
historyNoiseRemoved <- TRUE
allNoiseRemoved <- FALSE
cons <- 3
maxTimeSpan <- 2
updateMOSUM <- FALSE    # updateMOSUM not necessary as new obs is assessed individually against histRMSE
boundaryRMSE <- TRUE
factorRMSE <- 3

# Run 16: 2 * histRMSE, history denoised, cons=3, span=2
historyNoiseRemoved <- TRUE
allNoiseRemoved <- FALSE
cons <- 3
maxTimeSpan <- 2
updateMOSUM <- FALSE   
boundaryRMSE <- TRUE
factorRMSE <- 2


# Run 17: Run 15, but history not denoised
historyNoiseRemoved <- FALSE
allNoiseRemoved <- FALSE
cons <- 3
maxTimeSpan <- 2
updateMOSUM <- FALSE    
boundaryRMSE <- TRUE
factorRMSE <- 3


# Run 18: Run 17, but cons = 6, and no limit to span
historyNoiseRemoved <- FALSE
allNoiseRemoved <- FALSE
cons <- 6
maxTimeSpan <- 99
updateMOSUM <- FALSE    
boundaryRMSE <- TRUE
factorRMSE <- 3

# Run 19; Run 17, but history = "ROC"
historyNoiseRemoved <- FALSE
allNoiseRemoved <- FALSE
cons <- 3
maxTimeSpan <- 2
updateMOSUM <- FALSE    
boundaryRMSE <- TRUE
factorRMSE <- 3
# Add temporarily history = "ROC" in the attachBfmFlagToSp() call below
# 36 failed in DG1 
# 47 failed in DG2
# 1 failed in SC1
# cause too few historical obs with ROC
# Oh, so we expect to need to remove 84 reference pixels? Leaving 525 - 84 = 441 pixels
