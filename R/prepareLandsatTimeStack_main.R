## This code takes the Landsat image time stack downloaded from GEE and prepare them to be ready for running BFAST Spatial
## Update: err not quite ready actually needs to run makeUniqueDates() later

# Read the image time stack downloaded from GEE
L5 <- brick(paste(path, "/raster_time_stack/ndmi_geotiff/SC_1/landsat5NDMI_SC_1.tif", sep = ""))   # Landsat-5

L7 <- brick(paste(path, "/raster_time_stack/ndmi_geotiff/SC_1/landsat7NDMI_SC_1.tif", sep = ""))   # Landsat-7    

L8 <- brick(paste(path, "/raster_time_stack/ndmi_geotiff/SC_1/landsat8NDMI_SC_1.tif", sep = ""))   # Landsat-8

# Read the scene ID saved by copy-pasting from GEE console
L5.id <- read_csv(paste(path, "/raster_time_stack/scene_id/SC_1_L5.csv", sep = ""))
L5.id <- L5.id[seq(2,nrow(L5.id),by=2),]

L7.id <- read_csv(paste(path, "/raster_time_stack/scene_id/SC_1_L7.csv", sep = ""))
L7.id <- L7.id[seq(2,nrow(L7.id),by=2),]

L8.id <- read_csv(paste(path, "/raster_time_stack/scene_id/SC_1_L8.csv", sep = ""))
L8.id <- L8.id[seq(2,nrow(L8.id),by=2),]

# Output file
outdir1 <- str_c(path, "/raster_time_stack/ndmi_rds/NDMITimeStack_L578_SC_1.rds")
ourdir2 <-  str_c(path, "/raster_time_stack/ndmi_rds/ndmi_SC_1_unique.rds")

# Source the function
source("R/Rfunction/prepareLandsatTimeStack.R")

# Execute the function
prepareLandsatTimeStack(imgTimeStack_L5 = L5, imgTimeStack_L7 = L7, imgTimeStack_L8 = L8,
                        sceneId_L5 = L5.id, sceneId_L7 = L7.id, sceneId_L8 = L8.id,
                        outName = outdir1, uniqueOutName = ourdir2)