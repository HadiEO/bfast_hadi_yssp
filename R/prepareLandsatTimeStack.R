## This code takes the Landsat image time stack downloaded from GEE and prepare them to be ready for running BFAST Spatial
## Update: err not quite ready actually needs to run makeUniqueDates() later

# Read the image time stack downloaded from GEE
# imgTimeStack_L5 <- brick(paste(path, "/raster_time_stack/ndmi_geotiff/SC_1/landsat5NDMI_SC_1.tif", sep = ""))   # Landsat-5

imgTimeStack_L7 <- brick(paste(path, "/raster_time_stack/ndmi_geotiff/DG_1/landsat7toaNDMI_duringSentinel2_DG_1.tif", sep = ""))   # Landsat-7    

imgTimeStack_L8 <- brick(paste(path, "/raster_time_stack/ndmi_geotiff/DG_1/landsat8toaNDMI_duringSentinel2_DG_1.tif", sep = ""))   # Landsat-8


# Read the scene ID saved by copy-pasting from GEE console
# sceneId_L5 <- read_csv(paste(path, "/raster_time_stack/scene_id/SC_1_L5.csv", sep = ""))
# sceneId_L5 <- sceneId_L5[seq(2,nrow(sceneId_L5),by=2),]

sceneId_L7 <- read_csv(paste(path, "/raster_time_stack/scene_id/DG_1_L7_toa_duringSentinel2.csv", sep = ""))
sceneId_L7 <- sceneId_L7[seq(2,nrow(sceneId_L7),by=2),]

sceneId_L8 <- read_csv(paste(path, "/raster_time_stack/scene_id/DG_1_L8_toa_duringSentinel2.csv", sep = ""))
sceneId_L8 <- sceneId_L8[seq(2,nrow(sceneId_L8),by=2),]

# names(imgTimeStack_L5) <- sceneId_L5$header             ## Rename the brick with scene id
names(imgTimeStack_L7) <- sceneId_L7$header
names(imgTimeStack_L8) <- sceneId_L8$header

# Stack across sensors
# imgTimeStack_L578 <- addLayer(imgTimeStack_L5, imgTimeStack_L7, imgTimeStack_L8)
imgTimeStack_L78 <- addLayer(imgTimeStack_L7, imgTimeStack_L8)

# imgTimeStack_L578 <- setZ(imgTimeStack_L578, getSceneinfo(names(imgTimeStack_L578))$date, name = 'time')    # Set time attribute in z slot

dates.imgTimeStack_L78 <- as.Date(substr(names(imgTimeStack_L78), 13, 20), format = "%Y%m%d")
imgTimeStack_L78 <- setZ(imgTimeStack_L78, dates.imgTimeStack_L78, name = 'time')    # Set time attribute in z slot



# Sort raster layers by dates
# imgTimeStack_L578 <- subset(imgTimeStack_L578, order(getZ(imgTimeStack_L578)))
imgTimeStack_L78 <- subset(imgTimeStack_L78, order(getZ(imgTimeStack_L78)))
# getZ(imgTimeStack_L578)

# Write to disk, change the output file name
# write_rds(imgTimeStack_L578, 
#           paste(path, "/raster_time_stack/ndmi_rds/NDMITimeStack_L578_SC_1.rds", sep = ""))

write_rds(imgTimeStack_L78, 
          paste(path, "/raster_time_stack/ndmi_rds/toaNDMITimeStack_L78_DG_1_duringS2.rds", sep = ""))
