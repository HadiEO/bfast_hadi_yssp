# The Landsat NDMI time stack 
NDMI.DG1.unique <- read_rds(str_c(path, "/raster_time_stack/ndmi_rds/ndmi_DG_1_unique.rds"))

# Sentinel-2  time stack (todo: make below  a function)
S2.NDMI_DG1 <- brick(str_c(path, "/raster_time_stack/ndmi_geotiff/Sentinel2_DG_1/Sentinel2NDMI_cloudMasked_DG_1.tif"))
sceneId_S2.NDMI_DG1 <- read_csv(str_c(path, "/raster_time_stack/scene_id/DG_1_Sentinel2.csv")) 
sceneId_S2.NDMI_DG1 <- sceneId_S2.NDMI_DG1[seq(2,nrow(sceneId_S2.NDMI_DG1),by=2),]
names(S2.NDMI_DG1) <- sceneId_S2.NDMI_DG1$header             ##*** Rename the brick with scene ids, they both are already ordered 1,2,3,...
date_S2.NDMI_DG1 <- as.Date(substr(names(S2.NDMI_DG1), 2, 9), format = "%Y%m%d")
S2.NDMI_DG1 <- setZ(S2.NDMI_DG1, date_S2.NDMI_DG1)

# Make RasterTS with one observation per date 
source("R/Rfunction/makeUniqueDates.R")
# ! be sure that there are at most 2 layers per date table(table(getZ()))

S2.NDMI_DG1.unique <- makeUniqueDates(x = S2.NDMI_DG1, sensor = "Sentinel-2")       # This takes a while
write_rds(S2.NDMI_DG1.unique, str_c(path, "/raster_time_stack/ndmi_rds/Sentinel2NDMI_cloudMasked_DG_1_unique.tif"))


# Import the selected mesh points (= pixels) to extract the time series
shp.folder <- paste(path, "/vector_data", sep = "")
selectLandsatPixels.DG1 <- readOGR(dsn = shp.folder, layer = "meshSelect_prevDG1_label_ok")

# Extract Landsat NDMI at select Landsat pixels
L78.NDMI_DG1.unique <- subsetRasterTS(NDMI.DG1.unique, minDate = c(2014,1))

extrNDMI.L78 <- bfastSpatial::zooExtract(x = L78.NDMI_DG1.unique,
                                         sample = selectLandsatPixels.DG1,
                                         method = "simple")            # no need buffer to account geometric error cause we are concerned with pixel-specific time series and relative changes
colnames(extrNDMI.L78) <- selectLandsatPixels.DG1$Id_1
write_rds(extrNDMI.L78, str_c(path, "/extracted_time_series/extrNDMI_duringS2_DG1.rds"))
# extrNDMI.L78 <- read_rds(str_c(path, "/extracted_time_series/extrNDMI_duringS2_DG1.rds"))

# Extract Sentinel-2 NDMI (this takes a bit of time)
extrNDMI.S2 <- bfastSpatial::zooExtract(x = S2.NDMI_DG1.unique,
                                        sample = selectLandsatPixels.DG1,
                                        method = "simple")           # The Sentinel-1 pixel is 10m, so buffer 15m to extract 30x30m

colnames(extrNDMI.S2) <- selectLandsatPixels.DG1$Id_1
write_rds(extrNDMI.S2, str_c(path, "/extracted_time_series/extrNDMI_S2_DG1.rds"))
# extrNDMI.S2 <- readRDS(str_c(path, "/extracted_time_series/extrNDMI_S2_DG1.rds"))


# Create gap-less time series
intact.ids <- selectLandsatPixels.DG1@data %>% dplyr::filter(Visual == "Intact (20020929 - 20150815)") %>% 
  dplyr::select(Id_1)

(which.id <- intact.ids$Id_1[2])

tl78ndmi <- bfastts(extrNDMI.L78[, as.character(which.id)], dates = getZ(L78.NDMI_DG1.unique), type = "irregular")
ts2ndmi <- bfastts(extrNDMI.S2[, as.character(which.id)], dates = getZ(S2.NDMI_DG1.unique), type = "irregular")  


# Plot time series
# x11()
bayts::plotts(tsL = list(tl78ndmi, ts2ndmi), labL = list("Landsat-7,8 NDMI (BOA)", "Sentinel-2 NDMI (TOA)"))   # ylimL = list(c(-0.2,0.8), c(-19,-6))


# For print to pdf
# which.columns <- which(names(extrVH.DG1) %in% as.character(intact.ids$Id_1))
# which.extrTS <- extrVH.DG1[, which.columns]
print.plotts <- function(k) {
  which.id <- intact.ids$Id_1[k]
  tl78ndmi <- bfastts(extrNDMI.L78[, as.character(which.id)], dates = getZ(L78.NDMI_DG1.unique), type = "irregular")
  ts2ndmi <- bfastts(extrNDMI.S2[, as.character(which.id)], dates = getZ(S2.NDMI_DG1.unique), type = "irregular")  
  bayts::plotts(tsL = list(tl78ndmi, ts2ndmi), labL = list("Landsat-7,8 NDMI (BOA)", "Sentinel-2 NDMI (TOA)"))   # ylimL = list(c(-0.2,0.8), c(-19,-6))
}

# length(intact.ids$Id_1)
for(k in seq(1, 17, by = 4)) {                                                                                 # just do manually for now
  filename <- str_c(k, "_", k+3, ".pdf")
  pdf(str_c(path, "/prelim_figs/pdf/Landsat_vs_Sentinel2/", filename),    # Change output dir here
      width = 7, height = 9, pointsize = 10)  
  par(mfrow = c(4,1))
  
  for(x in k:(k+3)) print.plotts(x)                   
  dev.off()
}  


# Manually do the rest
from <- k+4
to <- length(intact.ids$Id_1)
filename <- str_c(from, "_", to, ".pdf")
pdf(str_c(path, "/prelim_figs/pdf/Landsat_vs_Sentinel2/", filename),    # Change output dir here
    width = 7, height = 9, pointsize = 10)  
par(mfrow = c(4,1))
for(x in from:to) print.plotts(x)                           
dev.off()

