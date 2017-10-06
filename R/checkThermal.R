# Import the selected mesh points (= pixels) to extract the time series
shp.folder <- paste(path, "/vector_data", sep = "")
selectLandsatPixels.DG1 <- readOGR(dsn = shp.folder, layer = "meshSelect_prevDG1_label_ok")

# Last VHSR date
lastDate <- as.Date("2015-08-15")
lastDate.doy <- c(year(lastDate), yday(lastDate))

# Extract the NDMI at select Landsat pixels 
NDMI.DG1.unique <- read_rds(str_c(path, "/raster_time_stack/ndmi_rds/ndmi_DG_1_unique.rds"))
NDMI.DG1.unique.subset <- subsetRasterTS(NDMI.DG1.unique, maxDate = lastDate.doy)
extrNDMI.DG1 <- bfastSpatial::zooExtract(x = NDMI.DG1.unique.subset,
                                         sample = selectLandsatPixels.DG1,
                                         method = "simple")            # no need buffer to account geometric error cause we are concerned with pixel-specific time series and relative changes
colnames(extrNDMI.DG1) <- selectLandsatPixels.DG1$Id_1

# Extract the thermal value at select Landsat pixels
thermal.DG1.unique <- read_rds(str_c(path, "/raster_time_stack/ndmi_rds/thermal_DG_1_unique.rds"))
thermal.DG1.unique.subset <- subset(thermal.DG1.unique, which(getZ(thermal.DG1.unique) <= lastDate))
extrThermal.DG1 <- bfastSpatial::zooExtract(x = thermal.DG1.unique.subset,
                                       sample = selectLandsatPixels.DG1,
                                       method = "simple")           
colnames(extrThermal.DG1) <- selectLandsatPixels.DG1$Id_1
write_rds(extrThermal.DG1, str_c(path, "/extracted_time_series/extrThermal_DG1.rds"))

# Create gap-less time series
intact.ids <- selectLandsatPixels.DG1@data %>% dplyr::filter(Visual == "Intact (20020929 - 20150815)") %>% 
  dplyr::select(Id_1)

(which.id <- intact.ids$Id_1[1])

tlndmi <- bfastts(extrNDMI.DG1[, as.character(which.id)], dates = getZ(NDMI.DG1.unique.subset), type = "irregular")
tlthermal <- bfastts(extrThermal.DG1[, as.character(which.id)], dates = getZ(thermal.DG1.unique.subset), type = "irregular")  


# Plot time series
# x11()
bayts::plotts(tsL = list(tlndmi, tlthermal), labL = list("Landsat NDMI", "Landsat thermal"), ylimL = list(c(-0.2,0.8)))   # ylimL = list(c(-0.2,0.8), c(-19,-6))


# For print to pdf
# which.columns <- which(names(extrVH.DG1) %in% as.character(intact.ids$Id_1))
# which.extrTS <- extrVH.DG1[, which.columns]
print.plotts <- function(k) {
  which.id <- intact.ids$Id_1[k]
  tlndmi <- bfastts(extrNDMI.DG1[, as.character(which.id)], dates = getZ(NDMI.DG1.unique.subset), type = "irregular")
  tlthermal <- bfastts(extrThermal.DG1[, as.character(which.id)], dates = getZ(thermal.DG1.unique.subset), type = "irregular")  
  bayts::plotts(tsL = list(tlndmi, tlthermal), labL = list("Landsat NDMI", "Landsat thermal"), ylimL = list(c(-0.2,0.8)))   # ylimL = list(c(-0.2,0.8), c(-19,-6))
}

# length(intact.ids$Id_1)
for(k in seq(1, 17, by = 4)) {                                                                                 # just do manually for now
  filename <- str_c("checkThermal_", k, "_", k+3, ".pdf")
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
filename <- str_c("checkThermal_", from, "_", to, ".pdf")
pdf(str_c(path, "/prelim_figs/pdf/", filename),    # Change output dir here
    width = 7, height = 9, pointsize = 10)  
par(mfrow = c(4,1))
for(x in from:to) print.plotts(x)                           
dev.off()

