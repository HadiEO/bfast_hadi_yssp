# Read the image time stack downloaded from GEE
L5 <- brick(paste(path, "/raster_time_stack/ndmi_geotiff/DG_1/landsat5thermal_DG_1.tif", sep = ""))   # Landsat-5

L7 <- brick(paste(path, "/raster_time_stack/ndmi_geotiff/DG_1/landsat7thermal_DG_1.tif", sep = ""))   # Landsat-7    

L8 <- brick(paste(path, "/raster_time_stack/ndmi_geotiff/DG_1/landsat8thermal_DG_1.tif", sep = ""))   # Landsat-8

# Read the scene ID saved by copy-pasting from GEE console
L5.id <- read_csv(paste(path, "/raster_time_stack/scene_id/DG_1_L5_thermal_tier1.csv", sep = ""))
L5.id <- L5.id[seq(2,nrow(L5.id),by=2),]

L7.id <- read_csv(paste(path, "/raster_time_stack/scene_id/DG_1_L7_thermal_tier1.csv", sep = ""))
L7.id <- L7.id[seq(2,nrow(L7.id),by=2),]

L8.id <- read_csv(paste(path, "/raster_time_stack/scene_id/DG_1_L8_thermal_tier1.csv", sep = ""))
L8.id <- L8.id[seq(2,nrow(L8.id),by=2),]

# 
