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
NDMI.DG1.unique <- makeUniqueDates(x = NDMI.DG1, sensor = "Landsat")       # This takes a while
S1.VH_DG1.unique <- makeUniqueDates(x = S1.VH_DG1, sensor = "Sentinel-1")
saveRDS(NDMI.DG1.unique, str_c(path, "/raster_time_stack/ndmi_rds/ndmi_DG_1_unique.rds"))
saveRDS(S1.VH_DG1.unique, str_c(path, "/raster_time_stack/ndmi_rds/S1VH_DG_1_unique.rds"))

# Import the selected mesh points (= pixels) to extract the time series
shp.folder <- paste(path, "/vector_data", sep = "")
selectLandsatPixels.DG1 <- readOGR(dsn = shp.folder, layer = "meshSelect_prevDG1_label_ok")

# Extract the NDMI at select Landsat pixels
# NDMI.DG1.unique.subset <- subsetRasterTS(NDMI.DG1.unique, minDate = c(2014,1))
# extrNDMI.DG1 <- bfastSpatial::zooExtract(x = NDMI.DG1.unique.subset,
#                                          sample = selectLandsatPixels.DG1,
#                                          method = "simple")            # no need buffer to account geometric error cause we are concerned with pixel-specific time series and relative changes
# colnames(extrNDMI.DG1) <- selectLandsatPixels.DG1$Id_1
# saveRDS(extrNDMI.DG1, str_c(path, "/extracted_time_series/extrNDMI_DG1.rds"))
extrNDMI.DG1 <- readRDS(str_c(path, "/extracted_time_series/extrNDMI_DG1.rds"))

# Extract the VH backscatter at select Landsat pixels (this takes a bit of time)
# extrVH.DG1 <- bfastSpatial::zooExtract(x = S1.VH_DG1.unique, 
#                                        sample = selectLandsatPixels.DG1,                               
#                                        buffer = 15, fun = mean, small = TRUE)           # The Sentinel-1 pixel is 10m, so buffer 15m to extract 30x30m

# colnames(extrVH.DG1) <- selectLandsatPixels.DG1$Id_1
# saveRDS(extrVH.DG1, str_c(path, "/extracted_time_series/extrVH_DG1.rds"))
extrVH.DG1 <- readRDS(str_c(path, "/extracted_time_series/extrVH_DG1.rds"))

# Create gap-less time series
intact.ids <- selectLandsatPixels.DG1@data %>% dplyr::filter(Visual == "Intact (20020929 - 20150815)") %>% 
  dplyr::select(Id_1)

(which.id <- intact.ids$Id_1[7])

tlndmi <- bfastts(extrNDMI.DG1[, as.character(which.id)], dates = getZ(NDMI.DG1.unique.subset), type = "irregular")
ts1vh <- bfastts(extrVH.DG1[, as.character(which.id)], dates = getZ(S1.VH_DG1.unique), type = "irregular")  


# Plot time series
bayts::plotts(tsL = list(tlndmi, ts1vh), labL = list("Landsat NDMI", "Sentinel-1 VH [dB]"))   # ylimL = list(c(-0.2,0.8), c(-19,-6))


# For print to pdf
# which.columns <- which(names(extrVH.DG1) %in% as.character(intact.ids$Id_1))
# which.extrTS <- extrVH.DG1[, which.columns]
print.plotts <- function(k) {
  which.id <- intact.ids$Id_1[k]
  tlndmi <- bfastts(extrNDMI.DG1[, as.character(which.id)], dates = getZ(NDMI.DG1.unique.subset), type = "irregular")
  ts1vh <- bfastts(extrVH.DG1[, as.character(which.id)], dates = getZ(S1.VH_DG1.unique), type = "irregular")  
  bayts::plotts(tsL = list(tlndmi, ts1vh), labL = list("Landsat NDMI", "Sentinel-1 VH [dB]"), xlab = "Date")   # ylimL = list(c(-0.2,0.8), c(-19,-6))
}

# length(intact.ids$Id_1)
for(k in seq(1, 17, by = 4)) {                                                                                 # just do manually for now
  filename <- str_c(k, "_", k+3, ".pdf")
  pdf(str_c(path, "/prelim_figs/pdf/", filename),    # Change output dir here
      width = 7, height = 8, pointsize = 10)  
  par(mfrow = c(4,1), oma = c(0, 0, 0, 0), mai = c(0.45, 0.35, 0.1, 0.1),
      ps = 10, mgp = c(1.7, 0.5, 0), mar =c(2.7, 2.7, 0.5, 0.5))
  
  for(x in k:(k+3)) print.plotts(x)                   
  dev.off()
}  


# Manually do the rest
from <- k+4
to <- length(intact.ids$Id_1)
filename <- str_c(from, "_", to, ".pdf")
pdf(str_c(path, "/prelim_figs/pdf/", filename),    # Change output dir here
    width = 7, height = 9, pointsize = 10)  
par(mfrow = c(4,1))
for(x in from:to) print.plotts(x)                           
dev.off()

