# The extrNDMI
(path)
path2 <- "/extracted_time_series/FINALLY_USED/"
extrNDMIsub.DG1 <- read_rds(str_c(path, path2, "extrNDMIsub_DG1.rds"))      
extrNDMIsub.DG2 <- read_rds(str_c(path, path2, "extrNDMIsub_DG2.rds"))
extrNDMIsub.SC1 <- read_rds(str_c(path, path2, "extrNDMIsub_SC1.rds"))
extrNDMIsub.sq9 <- read_rds(str_c(path, path2, "extrNDMIsub_sq9.rds"))
# extrNDMIsub.sq10 <- read_rds(str_c(path, path2, "extrNDMIsub_sq10.rds"))    # acacia
extrNDMIsub.sq11 <- read_rds(str_c(path, path2, "extrNDMIsub_sq11.rds"))
extrNDMIsub.sq13 <- read_rds(str_c(path, path2, "extrNDMIsub_sq13.rds"))


# The accuracy results
acc <- read_rds(str_c(path, "/accuracy_results/accuracy_run10_start2000_all_df.rds"))
acc <- read_rds(str_c(path, "/accuracy_results/before_snap_date/accuracy_run10_start2000_all_df.rds"))


# Check if acc$bfm.date.confirmed is in extrNDMI of corresponding scene and Id
for(i in 1:NROW(acc)) {
  
  id <- acc[[i,"Id"]]
  
  # NDMI for which scene?
  if(acc[[i,"Scene"]] == "DG1") {
    ndmi <- extrNDMIsub.DG1[, id]
  } else if(acc[[i,"Scene"]] == "DG2") {
    ndmi <- extrNDMIsub.DG2[, id]
  } else if(acc[[i,"Scene"]] == "SC1") {
    ndmi <- extrNDMIsub.SC1[, id]
  } else if(acc[[i,"Scene"]] == "SQ9") {
    ndmi <- extrNDMIsub.sq9[, id]
  } else if(acc[[i,"Scene"]] == "SQ11") {
    ndmi <- extrNDMIsub.sq11[, id]
  } else if(acc[[i,"Scene"]] == "SQ13") {
    ndmi <- extrNDMIsub.sq13[, id]
  }
  
  ndmi.notNA <- ndmi[!is.na(ndmi)]
  date.notNA <- index(ndmi.notNA)
  
  bfm.date <- acc[[i,"bfm.date.confirmed"]]
  bfm.date.is.in.extrNDMI <- bfm.date %in% date.notNA 
  # I see, there is a difference by 1-day (all scenes when bfm.date %in% date.notNA = FALSE) caused by something somewhere!!
  # bfm.date.is.in.extrNDMI <- bfm.date %in% date.notNA | (bfm.date-1) %in% date.notNA | (bfm.date+1) %in% date.notNA
  
  acc[i,"bfm.date.is.in.extrNDMI"] <- bfm.date.is.in.extrNDMI
}

acc_snap <- acc
acc_noSnap <- acc
View(cbind(acc_snap$bfm.date.confirmed, acc_noSnap$bfm.date.is.in.extrNDMI, acc_noSnap$bfm.date.confirmed))
acc_snap$bfm.date.confirmed - acc_noSnap$bfm.date.confirmed
# the snap works fine!