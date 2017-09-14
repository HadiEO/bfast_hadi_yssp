# The Landsat NDMI time stack --------------------------------------------- 
NDMI.DG1 <- read_rds(paste(path, "/raster_time_stack/ndmi_rds/NDMITimeStack_L578_KalArea1_selectDG_1.rds", sep = ""))
NDMI.DG2 <- read_rds(paste(path, "/raster_time_stack/ndmi_rds/NDMITimeStack_L578_KalArea1_selectDG_2.rds", sep = ""))
NDMI.sq9 <- read_rds(paste(path, "/raster_time_stack/ndmi_rds/NDMITimeStack_L578_sq_9.rds", sep = ""))
NDMI.sq10 <- read_rds(paste(path, "/raster_time_stack/ndmi_rds/NDMITimeStack_L578_sq_10.rds", sep = ""))
NDMI.sq11 <- read_rds(paste(path, "/raster_time_stack/ndmi_rds/NDMITimeStack_L578_sq_11.rds", sep = ""))
NDMI.sq13 <- read_rds(paste(path, "/raster_time_stack/ndmi_rds/NDMITimeStack_L578_sq_13.rds", sep = ""))



# Now import the selected mesh points (= pixels) --------
# to extract the spectral time series
shp.folder <- paste(path, "/vector_data", sep = "")
selectLandsatPixels.DG1 <- readOGR(dsn = shp.folder, layer = "meshSelect_prevDG1_label_ok")
selectLandsatPixels.DG2 <- readOGR(dsn = shp.folder, layer = "meshSelect_prevDG2_label_ok")
selectLandsatPixels.sq9 <- readOGR(dsn = shp.folder, layer = "meshSelect_sq_9_label_ok")
selectLandsatPixels.sq10 <- readOGR(dsn = shp.folder, layer = "meshSelect_sq_10_label_ok")
selectLandsatPixels.sq11 <- readOGR(dsn = shp.folder, layer = "meshSelect_sq_11_label_ok")
selectLandsatPixels.sq13 <- readOGR(dsn = shp.folder, layer = "meshSelect_sq_13_label_ok")


# Summary of the reference samples ----------------------------------------
table.DG1 <- table(selectLandsatPixels.DG1$Visual); write.csv2(table.DG1, paste(path, "/table/reference_DG1.csv", sep = ""))
table.DG2 <- table(selectLandsatPixels.DG2$Visual); write.csv2(table.DG2, paste(path, "/table/reference_DG2.csv", sep = ""))
table.sq9 <- table(selectLandsatPixels.sq9$Visual); write.csv2(table.sq9, paste(path, "/table/reference_sq9.csv", sep = ""))
table.sq10 <- table(selectLandsatPixels.sq10$Visual); write.csv2(table.sq10, paste(path, "/table/reference_sq10.csv", sep = ""))
table.sq11 <- table(selectLandsatPixels.sq11$Visual); write.csv2(table.sq11, paste(path, "/table/reference_sq11.csv", sep = ""))
table.sq13 <- table(selectLandsatPixels.sq13$Visual); write.csv2(table.sq13, paste(path, "/table/reference_sq13.csv", sep = ""))


