# The Landsat NDMI time stack 
NDMI.DG1 <- read_rds(paste(path, "/raster_time_stack/ndmi_rds/NDMITimeStack_L578_KalArea1_selectDG_1.rds", sep = ""))

# Sentinel-1 VH time stack (todo: make below  a function)
S1.VH_DG1 <- brick(paste(path, "/raster_time_stack/sentinel_1_geotiff/Sentinel1VH_DG_1.tif", sep = ""))
sceneId_S1.VH_DG1 <- read_csv(paste(path, "/raster_time_stack/scene_id/DG_1_Sentinel1_VH.csv", sep = "")) 
sceneId_S1.VH_DG1 <- sceneId_S1.VH_DG1[seq(2,nrow(sceneId_S1.VH_DG1),by=2),]
names(S1.VH_DG1) <- sceneId_S1.VH_DG1$header             ##*** Rename the brick with scene ids, they both are already ordered 1,2,3,...
date_S1.VH_DG1 <- as.Date(substr(names(S1.VH_DG1), 18, 25), format = "%Y%m%d")
S1.VH_DG1 <- setZ(S1.VH_DG1, date_S1.VH_DG1)

# Sentinel-1 VV time stack
S1.VV_DG1 <- brick(paste(path, "/raster_time_stack/sentinel_1_geotiff/Sentinel1VV_DG_1.tif", sep = ""))
sceneId_S1.VV_DG1 <- read_csv(paste(path, "/raster_time_stack/scene_id/DG_1_Sentinel1_VV.csv", sep = "")) 
sceneId_S1.VV_DG1 <- sceneId_S1.VV_DG1[seq(2,nrow(sceneId_S1.VV_DG1),by=2),]
names(S1.VV_DG1) <- sceneId_S1.VV_DG1$header             ##*** Rename the brick with scene ids, they both are already ordered 1,2,3,...
date_S1.VV_DG1 <- as.Date(substr(names(S1.VV_DG1), 18, 25), format = "%Y%m%d")
S1.VV_DG1 <- setZ(S1.VV_DG1, date_S1.VV_DG1)


# Make RasterTS with one observation per date 
source("R/Rfunction/makeUniqueDates.R")
# ! be sure that there are at most 2 layers per date table(table(getZ()))
NDMI.DG1.unique <- makeUniqueDates(x = NDMI.DG1, sensor = "Landsat")
S1.VH_DG1.unique <- makeUniqueDates(x = S1.VH_DG1, sensor = "Sentinel-1")

# Import the selected mesh points (= pixels) to extract the time series
shp.folder <- paste(path, "/vector_data", sep = "")
selectLandsatPixels.DG1 <- readOGR(dsn = shp.folder, layer = "meshSelect_prevDG1_label_ok")

# Extract the NDMI at select Landsat pixels
extrNDMI.DG1 <- bfastSpatial::zooExtract(x = subsetRasterTS(NDMI.DG1.unique, minDate = c(2015,1)), 
                                         sample = selectLandsatPixels.DG1,                               
                                         method = "simple")            # no need buffer to account geometric error cause we are concerned with pixel-specific time series and relative changes
colnames(extrNDMI.DG1) <- selectLandsatPixels.DG1$Id_1
                    

# Extract the VH backscatter at select Landsat pixels (this takes a bit of time)
extrVH.DG1 <- bfastSpatial::zooExtract(x = S1.VH_DG1.unique, 
                                       sample = selectLandsatPixels.DG1,                               
                                       buffer = 15, fun = mean, small = TRUE)           # The Sentinel-1 pixel is 10m, so buffer 15m to extract 30x30m

colnames(extrVH.DG1) <- selectLandsatPixels.DG1$Id_1
saveRDS(extrVH.DG1, str_c(path, "/extracted_time_series/extrVH_DG1.rds"))

# Create gap-less time series
intact.ids <- selectLandsatPixels.DG1@data %>% dplyr::filter(Visual == "Intact (20020929 - 20150815)") %>% 
  dplyr::select(Id_1)


ts1vh <- bfastts(extrVH.DG1)


# Plot time series
bayts::plotts

