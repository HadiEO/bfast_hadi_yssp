# Final experiment runs is in accuracyAssessment.R
# The run numbers are not the same as here

# Run 1: Original bfastmonitor *without* history noise removal
historyNoiseRemoved <- FALSE
boundaryRMSE <- FALSE
factorRMSE <- NA
cons <- 1
maxTimeSpan <- NA
updateMOSUM <- FALSE    
bfmOutName <- str_c(path, "/bfm_results/bfm_run1")
outSamplesName <- str_c(path, "/accuracy_results/accuracy_run1")
oldFlagOutName <- str_c(path, "/bfm_results/oldFlag_bfm_run1")
factorRMSE_immediate <- NULL
allowImmediateConfirm <- FALSE


# Run 2: Original bfastmonitor *with* history noise removal

# Run 3: Modified bfastmonitor *with* history noise removal, cons = 3, maxTimeSpan = 2, updateMOSUM = TRUE
historyNoiseRemoved <- TRUE
boundaryRMSE <- FALSE
factorRMSE <- NA
cons <- 3
maxTimeSpan <- 2
updateMOSUM <- TRUE    
bfmOutName <- str_c(path, "/bfm_results/bfm_run3")
outSamplesName <- str_c(path, "/accuracy_results/accuracy_run3")
oldFlagOutName <- str_c(path, "/bfm_results/oldFlag_bfm_run3")
factorRMSE_immediate <- NULL
allowImmediateConfirm <- FALSE

# Run 4: Modified bfastmonitor *with* history noise removal, cons = 3, maxTimeSpan = 1, updateMOSUM = TRUE

# Run 5 : Run 3/4, but *without* history noise removal

# Run 6: Run 3/4, but *without* history noise removal

# Run 7: Run 5/6, but updateMOSUM = FALSE


# ***********************************************
# RMSE as boundary from here on
# Note updateMOSUM is not applicable here as error is assessed at individual point, unlike MOSUM
# Note that bfastmonitor_mode() has been changed to have
# histRMSE calculated with all historical obs (incl. noise)

###############################################
# Run 8: boundary 3 * histRMSE, *without* history noise removal, cons=3, span=2
historyNoiseRemoved <- FALSE
boundaryRMSE <- TRUE
factorRMSE <- 3
cons <- 3
maxTimeSpan <- 2
updateMOSUM <- FALSE    
bfmOutName <- str_c(path, "/bfm_results/bfm_run8")
outSamplesName <- str_c(path, "/accuracy_results/accuracy_run8")
oldFlagOutName <- str_c(path, "/bfm_results/oldFlag_bfm_run8")
factorRMSE_immediate <- NULL
allowImmediateConfirm <- FALSE


# Run 9: Run 8, but *with* history noise removal
historyNoiseRemoved <- TRUE
boundaryRMSE <- TRUE
factorRMSE <- 3
cons <- 3
maxTimeSpan <- 2
updateMOSUM <- FALSE    
bfmOutName <- str_c(path, "/bfm_results/bfm_run9")
outSamplesName <- str_c(path, "/accuracy_results/accuracy_run9")
oldFlagOutName <- str_c(path, "/bfm_results/oldFlag_bfm_run9")
factorRMSE_immediate <- NULL
allowImmediateConfirm <- FALSE

# Run 10: Run 9
historyNoiseRemoved <- TRUE
boundaryRMSE <- TRUE
factorRMSE <- 3
cons <- 3
maxTimeSpan <- 2
updateMOSUM <- FALSE    
bfmOutName <- str_c(path, "/bfm_results/bfm_run10")
outSamplesName <- str_c(path, "/accuracy_results/accuracy_run10")
oldFlagOutName <- str_c(path, "/bfm_results/oldFlag_bfm_run10")
factorRMSE_immediate <- NULL
allowImmediateConfirm <- FALSE

# Run 10 but start 2000-01-01
# Change in Run bfastmonitor code
historyNoiseRemoved <- TRUE
boundaryRMSE <- TRUE
factorRMSE <- 3
cons <- 3
maxTimeSpan <- 2
updateMOSUM <- FALSE    
bfmOutName <- str_c(path, "/bfm_results/bfm_run10_start2000")
outSamplesName <- str_c(path, "/accuracy_results/accuracy_run10_start2000")
oldFlagOutName <- str_c(path, "/bfm_results/oldFlag_bfm_run10_start2000")
factorRMSE_immediate <- NULL
allowImmediateConfirm <- FALSE


# Run 11: Run 10, but cons = 2, maxTimeSpan = 1
historyNoiseRemoved <- TRUE
boundaryRMSE <- TRUE
factorRMSE <- 3
cons <- 2
maxTimeSpan <- 1
updateMOSUM <- FALSE    
bfmOutName <- str_c(path, "/bfm_results/bfm_run11")
outSamplesName <- str_c(path, "/accuracy_results/accuracy_run11")
oldFlagOutName <- str_c(path, "/bfm_results/oldFlag_bfm_run11")
factorRMSE_immediate <- NULL
allowImmediateConfirm <- FALSE

# Run 11 but start 2000-01-01
# Change in Run bfastmonitor code
historyNoiseRemoved <- TRUE
boundaryRMSE <- TRUE
factorRMSE <- 3
cons <- 2
maxTimeSpan <- 1
updateMOSUM <- FALSE    
bfmOutName <- str_c(path, "/bfm_results/bfm_run11_start2000")
outSamplesName <- str_c(path, "/accuracy_results/accuracy_run11_start2000")
oldFlagOutName <- str_c(path, "/bfm_results/oldFlag_bfm_run11_start2000")
factorRMSE_immediate <- NULL
allowImmediateConfirm <- FALSE


# Run 12, allow immediate change confirmation if resid > 5 * histRMSE, when boundaryRMSE=TRUE 
# start 2000-01-01 -> change in Run bfastmonitor code
historyNoiseRemoved <- TRUE
boundaryRMSE <- TRUE
factorRMSE <- 3
cons <- 3
maxTimeSpan <- 2
updateMOSUM <- FALSE
factorRMSE_immediate <- 5          #
allowImmediateConfirm <- TRUE
bfmOutName <- str_c(path, "/bfm_results/bfm_run12_start2000")
outSamplesName <- str_c(path, "/accuracy_results/accuracy_run12_start2000")
oldFlagOutName <- str_c(path, "/bfm_results/oldFlag_bfm_run12_start2000")

# Run 13, allow immediate change confirmation if resid > 8 * histRMSE, when boundaryRMSE=TRUE 
# start 2000-01-01 -> change in Run bfastmonitor code
historyNoiseRemoved <- TRUE
boundaryRMSE <- TRUE
factorRMSE <- 3
cons <- 3
maxTimeSpan <- 2
updateMOSUM <- FALSE
factorRMSE_immediate <- 8          #
allowImmediateConfirm <- TRUE
bfmOutName <- str_c(path, "/bfm_results/bfm_run13_start2000")
outSamplesName <- str_c(path, "/accuracy_results/accuracy_run13_start2000")
oldFlagOutName <- str_c(path, "/bfm_results/oldFlag_bfm_run13_start2000")

# Try factorRMSE_immediate <- 10, 12

# Run 14, allow immediate change confirmation if resid > 12 * histRMSE, when boundaryRMSE=TRUE 
# start 2000-01-01 -> change in Run bfastmonitor code
historyNoiseRemoved <- TRUE
boundaryRMSE <- TRUE
factorRMSE <- 3
cons <- 3
maxTimeSpan <- 2
updateMOSUM <- FALSE
factorRMSE_immediate <- 12          #
allowImmediateConfirm <- TRUE
bfmOutName <- str_c(path, "/bfm_results/bfm_run14_start2000")
outSamplesName <- str_c(path, "/accuracy_results/accuracy_run14_start2000")
oldFlagOutName <- str_c(path, "/bfm_results/oldFlag_bfm_run14_start2000")