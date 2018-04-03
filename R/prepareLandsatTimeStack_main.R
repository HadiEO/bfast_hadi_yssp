## This code takes the Landsat image time stack downloaded from GEE and prepare them to be ready for running BFAST Spatial
## Update: err not quite ready actually needs to run makeUniqueDates() later. Update: this is implemented already

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

# Output file
outdir1 <- str_c(path, "/raster_time_stack/ndmi_rds/ThermalTimeStack_L578_DG_1.rds")
ourdir2 <-  str_c(path, "/raster_time_stack/ndmi_rds/thermal_DG_1_unique.rds")

# Source the function
source("R/Rfunction/prepareLandsatTimeStack.R")

# Execute the function
prepareLandsatTimeStack(imgTimeStack_L5 = L5, imgTimeStack_L7 = L7, imgTimeStack_L8 = L8,
                        sceneId_L5 = L5.id, sceneId_L7 = L7.id, sceneId_L8 = L8.id,
                        outName = outdir1, uniqueOutName = ourdir2, collection = "Tier 1")

