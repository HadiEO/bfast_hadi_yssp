# The Landsat NDMI time stack --------------------------------------------- 
NDMI.DG1.unique <- read_rds(str_c(path, "/raster_time_stack/ndmi_rds/ndmi_DG_1_unique.rds"))
NDMI.DG2.unique <- read_rds(str_c(path, "/raster_time_stack/ndmi_rds/ndmi_DG_2_unique.rds"))
NDMI.sq9.unique <- read_rds(str_c(path, "/raster_time_stack/ndmi_rds/ndmi_sq_9_unique.rds"))
NDMI.sq10.unique <- read_rds(str_c(path, "/raster_time_stack/ndmi_rds/ndmi_sq_10_unique.rds"))
NDMI.sq11.unique <- read_rds(str_c(path, "/raster_time_stack/ndmi_rds/ndmi_sq_11_unique.rds"))
NDMI.sq13.unique <- read_rds(str_c(path, "/raster_time_stack/ndmi_rds/ndmi_sq_13_unique.rds"))
NDMI.SC1.unique <- read_rds(str_c(path, "/raster_time_stack/ndmi_rds/ndmi_SC_1_unique.rds"))


# Now import the selected mesh points (= pixels) --------
# to extract the spectral time series
shp.folder <- paste(path, "/vector_data", sep = "")
selectLandsatPixels.DG1 <- readOGR(dsn = shp.folder, layer = "meshSelect_prevDG1_label_ok")
selectLandsatPixels.DG2 <- readOGR(dsn = shp.folder, layer = "meshSelect_prevDG2_label_ok")
selectLandsatPixels.sq9 <- readOGR(dsn = shp.folder, layer = "meshSelect_sq_9_label_ok")
selectLandsatPixels.sq10 <- readOGR(dsn = shp.folder, layer = "meshSelect_sq_10_label_ok")
selectLandsatPixels.sq11 <- readOGR(dsn = shp.folder, layer = "meshSelect_sq_11_label_ok")
selectLandsatPixels.sq13 <- readOGR(dsn = shp.folder, layer = "meshSelect_sq_13_label_ok")
selectLandsatPixels.SC1 <- readOGR(dsn = shp.folder, layer = "meshSelect_SC_1_label_ok")
selectLandsatPixels.addIntactForest.sq13 <- readOGR(dsn = shp.folder, layer = "meshSelect_sq_13_label_ok_addIntactForest")
# TODO: combine sq13.addIntactForest to sq13


# Recode reference change into DISTURBANCE (1) and NON-DISTURBANCE (0)
selectLandsatPixels.DG1$Disturbance <- 0
selectLandsatPixels.DG1[(selectLandsatPixels.DG1$Visual != "Intact (20020929 - 20150815)"), "Disturbance"] <- 1

selectLandsatPixels.DG2$Disturbance <- 0
selectLandsatPixels.DG2[(selectLandsatPixels.DG2$Visual != "Intact forest (20020929 - 20150808)"), "Disturbance"] <- 1

selectLandsatPixels.sq9$Disturbance <- 0
selectLandsatPixels.sq9[(selectLandsatPixels.sq9$Visual != "Intact forest 20020818-20140513"), "Disturbance"] <- 1

selectLandsatPixels.sq10$Disturbance <- 1    # All acacia

selectLandsatPixels.sq11$Disturbance <- 1    # All disturbed

selectLandsatPixels.sq13$Disturbance <- 1   # All disturbed

selectLandsatPixels.SC1$Disturbance <- 0
selectLandsatPixels.SC1[(selectLandsatPixels.SC1$Visual != "Intact forest 20050726 - 20140204 "), "Disturbance"] <- 1

selectLandsatPixels.addIntactForest.sq13$Disturbance <- 0  # All intact


# Summary of the reference samples ----------------------------------------
table.DG1 <- table(selectLandsatPixels.DG1$Visual); write.csv2(table.DG1, paste(path, "/table/reference_DG1.csv", sep = ""))
table.DG2 <- table(selectLandsatPixels.DG2$Visual); write.csv2(table.DG2, paste(path, "/table/reference_DG2.csv", sep = ""))
table.sq9 <- table(selectLandsatPixels.sq9$Visual); write.csv2(table.sq9, paste(path, "/table/reference_sq9.csv", sep = ""))
table.sq10 <- table(selectLandsatPixels.sq10$Visual); write.csv2(table.sq10, paste(path, "/table/reference_sq10.csv", sep = ""))
table.sq11 <- table(selectLandsatPixels.sq11$Visual); write.csv2(table.sq11, paste(path, "/table/reference_sq11.csv", sep = ""))
table.sq13 <- table(selectLandsatPixels.sq13$Visual); write.csv2(table.sq13, paste(path, "/table/reference_sq13.csv", sep = ""))
table.SC1 <- table(selectLandsatPixels.SC1$Visual); write.csv2(table.SC1, paste(path, "/table/reference_SC1.csv", sep = ""))
table.addIntactForest.sq13 <- table(selectLandsatPixels.addIntactForest.sq13$Visual); write.csv2(table.addIntactForest.sq13, paste(path, "/table/reference_addIntactForest_sq13.csv", sep = ""))


# First and last date of VHSR ---------------------------------------------
ref.firstDate <- list(DG1 = as.Date("2002-09-29"), DG2 = as.Date("2002-09-29"),
                      sq9 = as.Date("2002-08-18"), sq10 = as.Date("2005-07-26"),
                      sq11 = as.Date("2005-07-26"), sq13 = as.Date("2002-08-18"),
                      SC1 = as.Date("2005-07-26"))

ref.lastDate <- list(DG1 = as.Date("2015-08-15"), DG2 = as.Date("2015-08-08"),
                     sq9 = as.Date("2014-05-13"), sq10 = as.Date("2015-08-17"),
                     sq11 = as.Date("2014-02-04"), sq13 = as.Date("2014-05-13"),
                     SC1 = as.Date("2014-02-04"))


# Need these dates in c(year, jday) !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
source("R/Rfunction/dateToYearJday.R")
ref.firstDate <- lapply(ref.firstDate, dateToYearJday)
ref.lastDate <- lapply(ref.lastDate, dateToYearJday)



# Extract the NDMI at select Landsat pixels and run BFAST Monitor --------------------------------
# Debug
# NDMI.unique = NDMI.DG1.unique
# samples = selectLandsatPixels.DG1
# firstDate = ref.firstDate$DG1
# lastDate = ref.lastDate$DG1
# extrOutName = str_c(path, "/extracted_time_series/extrNDMIsub_DG1.rds")
# bfmOutName = str_c(path, "/extracted_time_series/bfm_extrNDMIsub_DG1.rds")


# Check formula!
attachBfmFlagToSp <- function(NDMI.unique, samples, firstDate, lastDate, extrOutName, bfmOutName, outSamplesName) {
  NDMI.unique.sub <- subsetRasterTS(NDMI.unique, maxDate = lastDate)
  extrNDMI <- zooExtract(NDMI.unique.sub, samples, method = "simple")
  colnames(extrNDMI) <- as.character(samples$Id_1)
  write_rds(extrNDMI, extrOutName)
  # Create gap-less time series
  extrNDMI.ls <- as.list(extrNDMI)
  bts.ls <- lapply(extrNDMI.ls, FUN = function(z) bfastts(z, dates = getZ(NDMI.unique.sub), type = "irregular"))
  # Run BFAST Monitor
  jday.monitStart <- firstDate
  bfm.TH.ls <- lapply(bts.ls,
                      FUN = function(z) bfastmonitor(z, start = jday.monitStart,
                                                    formula = response~harmon, order = 1, plot = FALSE, h = 0.25, history = "all"))
  write_rds(bfm.TH.ls, bfmOutName)
  # bfm.TH.ls <- read_rds(bfmOutName)
  # Tell sample if BFAST detects DISTURBANCE (1) or NON-DISTURBANCE (0)
  bfm.TH.magn.ls <- lapply(bfm.TH.ls, FUN = function(z) z$magnitude)
  bfm.TH.dist.ls <- lapply(bfm.TH.ls, FUN = function(z) ifelse(!is.na(z$breakpoint), 1, 0))
  bfm.TH.dist.ls.rev <- lapply(bfm.TH.ls, FUN = function(z) ifelse((!is.na(z$breakpoint)) & (z$magnitude < 0), 1, 0))
  for(i in 1:length(names(bfm.TH.dist.ls))) {
    samples[which(samples$Id_1 == names(bfm.TH.dist.ls)[i]), "bfm.flag"] <- bfm.TH.dist.ls[[i]]
    samples[which(samples$Id_1 == names(bfm.TH.dist.ls.rev)[i]), "bfm.flag.rev"] <- bfm.TH.dist.ls.rev[[i]]
    samples[which(samples$Id_1 == names(bfm.TH.dist.ls)[i]), "bfm.magn"] <- bfm.TH.magn.ls[[i]]
  } 
  # Record TP (true positive/disturbance) or FP
  samples$TP <- 0
  samples$FP <- 0
  samples[which((samples$Disturbance == 1) & (samples$bfm.flag == 1)), "TP"] <- 1
  samples[which((samples$Disturbance == 0) & (samples$bfm.flag == 1)), "FP"] <- 1
  samples$TP.rev <- 0
  samples$FP.rev <- 0
  samples[which((samples$Disturbance == 1) & (samples$bfm.flag.rev == 1)), "TP.rev"] <- 1
  samples[which((samples$Disturbance == 0) & (samples$bfm.flag.rev == 1)), "FP.rev"] <- 1
  
  write_rds(samples, outSamplesName)
  
  # Return
  return(samples)
}


# I ran firstly with harmon+trend model, a second time with just harmon
DG1.bfmFlag <- attachBfmFlagToSp(NDMI.DG1.unique, selectLandsatPixels.DG1, ref.firstDate$DG1, ref.lastDate$DG1, 
                                 extrOutName = str_c(path, "/extracted_time_series/extrNDMIsub_DG1.rds"), 
                                 bfmOutName = str_c(path, "/extracted_time_series/bfm_justHarmon_extrNDMIsub_DG1.rds"),
                                 outSamplesName = str_c(path, "/extracted_time_series/accuracy_justHarmon_DG1.rds"))

DG2.bfmFlag <- attachBfmFlagToSp(NDMI.DG2.unique, selectLandsatPixels.DG2, ref.firstDate$DG2, ref.lastDate$DG2, 
                                 extrOutName = str_c(path, "/extracted_time_series/extrNDMIsub_DG2.rds"), 
                                 bfmOutName = str_c(path, "/extracted_time_series/bfm_justHarmon_extrNDMIsub_DG2.rds"),
                                 outSamplesName = str_c(path, "/extracted_time_series/accuracy_justHarmon_DG2.rds"))

sq9.bfmFlag <- attachBfmFlagToSp(NDMI.sq9.unique, selectLandsatPixels.sq9, ref.firstDate$sq9, ref.lastDate$sq9, 
                                 extrOutName = str_c(path, "/extracted_time_series/extrNDMIsub_sq9.rds"), 
                                 bfmOutName = str_c(path, "/extracted_time_series/bfm_justHarmon_extrNDMIsub_sq9.rds"),
                                 outSamplesName = str_c(path, "/extracted_time_series/accuracy_justHarmon_sq9.rds"))

sq10.bfmFlag <- attachBfmFlagToSp(NDMI.sq10.unique, selectLandsatPixels.sq10, ref.firstDate$sq10, ref.lastDate$sq10, 
                                 extrOutName = str_c(path, "/extracted_time_series/extrNDMIsub_sq10.rds"), 
                                 bfmOutName = str_c(path, "/extracted_time_series/bfm_justHarmon_extrNDMIsub_sq10.rds"),
                                 outSamplesName = str_c(path, "/extracted_time_series/accuracy_justHarmon_sq10.rds"))

sq11.bfmFlag <- attachBfmFlagToSp(NDMI.sq11.unique, selectLandsatPixels.sq11, ref.firstDate$sq11, ref.lastDate$sq11, 
                                 extrOutName = str_c(path, "/extracted_time_series/extrNDMIsub_sq11.rds"), 
                                 bfmOutName = str_c(path, "/extracted_time_series/bfm_justHarmon_extrNDMIsub_sq11.rds"),
                                 outSamplesName = str_c(path, "/extracted_time_series/accuracy_justHarmon_sq11.rds"))

sq13.bfmFlag <- attachBfmFlagToSp(NDMI.sq13.unique, selectLandsatPixels.sq13, ref.firstDate$sq13, ref.lastDate$sq13, 
                                 extrOutName = str_c(path, "/extracted_time_series/extrNDMIsub_sq13.rds"), 
                                 bfmOutName = str_c(path, "/extracted_time_series/bfm_justHarmon_extrNDMIsub_sq13.rds"),
                                 outSamplesName = str_c(path, "/extracted_time_series/accuracy_justHarmon_sq13.rds"))

SC1.bfmFlag <- attachBfmFlagToSp(NDMI.SC1.unique, selectLandsatPixels.SC1, ref.firstDate$SC1, ref.lastDate$SC1, 
                                 extrOutName = str_c(path, "/extracted_time_series/extrNDMIsub_SC1.rds"), 
                                 bfmOutName = str_c(path, "/extracted_time_series/bfm_justHarmon_extrNDMIsub_SC1.rds"),
                                 outSamplesName = str_c(path, "/extracted_time_series/accuracy_justHarmon_SC1.rds"))

sq13.addIntact.bfmFlag <- attachBfmFlagToSp(NDMI.sq13.unique, selectLandsatPixels.addIntactForest.sq13, ref.firstDate$sq13, ref.lastDate$sq13, 
                                 extrOutName = str_c(path, "/extracted_time_series/extrNDMIsub_sq13_addIntact.rds"), 
                                 bfmOutName = str_c(path, "/extracted_time_series/bfm_justHarmon_extrNDMIsub_sq13_addIntact.rds"),
                                 outSamplesName = str_c(path, "/extracted_time_series/accuracy_justHarmon_sq13addIntact.rds"))





# Sample-based accuracy ---------------------------------------------------
ref <- matrix(c(DG1.bfmFlag$Disturbance, DG2.bfmFlag$Disturbance, sq9.bfmFlag$Disturbance,
                sq10.bfmFlag$Disturbance, sq11.bfmFlag$Disturbance, sq13.bfmFlag$Disturbance,
                SC1.bfmFlag$Disturbance, sq13.addIntact.bfmFlag$Disturbance), ncol=1)
pred <- matrix(c(DG1.bfmFlag$bfm.flag.rev, DG2.bfmFlag$bfm.flag.rev, sq9.bfmFlag$bfm.flag.rev,
                 sq10.bfmFlag$bfm.flag.rev, sq11.bfmFlag$bfm.flag.rev, sq13.bfmFlag$bfm.flag.rev,
                 SC1.bfmFlag$bfm.flag.rev, sq13.addIntact.bfmFlag$bfm.flag.rev), ncol=1)
cm <- table(pred, ref)


UA <- diag(cm) / rowSums(cm)
PA <- diag(cm) / colSums(cm)

sums <- vector()
for(i in 1:dim(cm)[1]){
  sums[i] <- cm[i,i]
}
OA <- sum(sums)/sum(cm)

# How well small clearing is detected?
DG1.bfmFlag.small <- DG1.bfmFlag[DG1.bfmFlag$Visual %in% c("Dirt road"), ]
DG2.bfmFlag.small <- DG2.bfmFlag[DG2.bfmFlag$Visual %in% c("Forest partially cleared 20150808"), ]
sq10.bfmFlag.small <- sq10.bfmFlag[sq10.bfmFlag$Visual %in% c("Acacia road (sub-pixel) from 20050726"), ]
SC1.bfmFlag.small <- SC1.bfmFlag[SC1.bfmFlag$Visual %in% c("Very small clearing 20140204", "Small clearing 20140204"), ]

ref.small <- matrix(c(DG1.bfmFlag.small$Disturbance, DG2.bfmFlag.small$Disturbance, sq10.bfmFlag.small$Disturbance,
                SC1.bfmFlag.small$Disturbance), ncol=1)
pred.small <- matrix(c(DG1.bfmFlag.small$bfm.flag, DG2.bfmFlag.small$bfm.flag, sq10.bfmFlag.small$bfm.flag,
                       SC1.bfmFlag.small$bfm.flag), ncol=1)
(cm.small <- table(pred.small, ref.small))

# Magnitude of false positive vs true positive
falseDist <- rbind(DG1.bfmFlag[which(DG1.bfmFlag$FP == 1), c("Visual", "bfm.magn")]@data,
                  DG2.bfmFlag[which(DG2.bfmFlag$FP == 1), c("Visual", "bfm.magn")]@data,
                  sq9.bfmFlag[which(sq9.bfmFlag$FP == 1), c("Visual", "bfm.magn")]@data,
                  sq10.bfmFlag[which(sq10.bfmFlag$FP == 1), c("Visual", "bfm.magn")]@data,
                  sq11.bfmFlag[which(sq11.bfmFlag$FP == 1), c("Visual", "bfm.magn")]@data,
                  sq13.bfmFlag[which(sq13.bfmFlag$FP == 1), c("Visual", "bfm.magn")]@data,
                  SC1.bfmFlag[which(SC1.bfmFlag$FP == 1), c("Visual", "bfm.magn")]@data,
                  sq13.addIntact.bfmFlag[which(sq13.addIntact.bfmFlag$FP == 1), c("Visual", "bfm.magn")]@data)

trueDist <- rbind(DG1.bfmFlag[which(DG1.bfmFlag$TP == 1), c("Visual", "bfm.magn")]@data,
                  DG2.bfmFlag[which(DG2.bfmFlag$TP == 1), c("Visual", "bfm.magn")]@data,
                  sq9.bfmFlag[which(sq9.bfmFlag$TP == 1), c("Visual", "bfm.magn")]@data,
                  sq10.bfmFlag[which(sq10.bfmFlag$TP == 1), c("Visual", "bfm.magn")]@data,
                  sq11.bfmFlag[which(sq11.bfmFlag$TP == 1), c("Visual", "bfm.magn")]@data,
                  sq13.bfmFlag[which(sq13.bfmFlag$TP == 1), c("Visual", "bfm.magn")]@data,
                  SC1.bfmFlag[which(SC1.bfmFlag$TP == 1), c("Visual", "bfm.magn")]@data,
                  sq13.addIntact.bfmFlag[which(sq13.addIntact.bfmFlag$TP == 1), c("Visual", "bfm.magn")]@data)

x11()
hist(trueDist$bfm.magn, col = "grey70", xlab = "Change magnitude", ylab = "No. of reference samples")
hist(falseDist$bfm.magn, add = TRUE, col = "grey20")
legend()


x11()
par(mfrow = c(2,1))
hist(trueDist$bfm.magn, main = "TP")
hist(falseDist$bfm.magn, main = "FP")

# Todo: -------------------------------------------------------------------

# Use STEF::spatialAccurayAssessment to validate date of change. Date must be decimal year
# Use STEF::accuracy.random or accuracy.stratified for area-weighted / error-adjusted accuracy,
# which requires area proportions (0-1) for each classes within the mapped area

# Check how many intact forest samples are incorrectly classified


