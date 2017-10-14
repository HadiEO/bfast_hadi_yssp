# Check if regrowth can really be detected,
# Set monitoring start in 2007 to make sure disturbance detected at correct date

NDMI.unique <- read_rds(str_c(path, "/raster_time_stack/ndmi_rds/ndmi_DG_2_unique.rds"))

# Subset raster date
DG.lastDate <- as.Date("2015-08-08")
jday.monitEnd <- c(year(DG.lastDate),yday(DG.lastDate))             # Date of latest VHSR
NDMI.unique.sub <- subsetRasterTS(NDMI.unique, maxDate = jday.monitEnd)

# Import shapefile
shp.folder <- paste(path, "/vector_data", sep = "")
demoPixels <- readOGR(dsn = shp.folder, layer = "meshSelect_DG2_label_justNatReveg")

# Extract time series
extrNDMI <- bfastSpatial::zooExtract(x = NDMI.unique.sub,
                                    sample = demoPixels,
                                    method = "simple")           
colnames(extrNDMI) <- as.character(demoPixels$Id_1)
write_rds(extrNDMI, str_c(path, "/extracted_time_series/extrNDMIsub_DG2_justNatReveg.rds"))

# Create gap-less time series
extrNDMI.ls <- as.list(extrNDMI)
demo.bts.ls <- lapply(extrNDMI.ls, FUN = function(z) bfastts(z, dates = getZ(NDMI.unique.sub), type = "irregular"))


# Run bfastmonitor 
DG.firstDate <- as.Date("2002-09-29")
DG.lastDate <- as.Date("2015-08-08")
jday.monitStart <- c(year(DG.firstDate), yday(DG.firstDate))          # Date of earliest VHSR showing still forested area

# If arbitrary start
jday.monitStart <- c(2007,1)

# To monitor regrowth, we fitted only harmonic model to the history as the baseline the regrowth signal
# is assessed against. In doing so, we avoided the impact of noise on the historical trend.
# However, using historic harmonic model seems to cause delayed detection.
# Changing the harmonic term doesn't seem to overcome it.
# Seems best result with order = 1, and h = 0.25

demo.bfm.ls <- lapply(demo.bts.ls,     # Check formula, with or without trend!
                          FUN = function(z) bfastmonitor(z, start = jday.monitStart, 
                                                          formula = response~harmon, order = 1, plot = FALSE, h = 0.25, # default h = 0.25
                                                          history = "all", level = 0.05))
# Plot bfm result 

for(i in seq(1, length(demo.bfm.ls)-length(demo.bfm.ls)%%4, by=4)) {
  my.bfmPlot <- bfmPlot(list(demo.bfm.ls[[i]], demo.bfm.ls[[i+1]], demo.bfm.ls[[i+2]], demo.bfm.ls[[i+3]]),
                        plotlabs = c(names(demo.bfm.ls)[i], names(demo.bfm.ls)[i+1], names(demo.bfm.ls)[i+2], names(demo.bfm.ls)[i+3]), 
                        ncols = 1, displayMagn = TRUE) + 
    theme_bw() + labs(y = "NDMI", x = "Date") + scale_y_continuous(limits = c(-0.2, 0.6)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggsave(str_c("justNatReveg_bfm_harmon_",i,"to",i+3,".pdf"), plot = my.bfmPlot, device = "pdf",
         path = str_c(path, "/prelim_figs/pdf"), width = 170, height = 40*4, unit = "mm")
}

# IDs with unsuccessful disturbance detection
bfm.fail.id <- c("471", "498", "547", "572", "620", "764", "836", "914", "982", "1138")
bfm.fail.idx <- which(names(extrNDMI.ls) %in% bfm.fail.id)
extrNDMI.ls.ok <- extrNDMI.ls[-bfm.fail.idx]
demo.bfm.ls.ok <- demo.bfm.ls[-bfm.fail.idx]

# Run regrowth

# reg <- tsreg(x = as.numeric(extrNDMI.ls[[i]]), change = demo.bfm.ls[[i]]$breakpoint, dates = getZ(NDMI.unique.sub),
#                 type = "irregular", h = 0.25, w = 2,          # default h = 0.5, w = 3
#                 formula = response~harmon, history = "BP",   # default history = "BP"
#                 plot = FALSE, ylabs = c("NDMI", "|MOSUM|"))            
# print(reg)

myRegFun <- failwith(NULL, f = function(z1, z2) {
  tsreg(x = as.numeric(z1), change = z2$breakpoint, dates = getZ(NDMI.unique.sub),
        type = "irregular", h = 0.25, w = 1, s = 0,       # w = ?
        formula = response~harmon, history = "BP", 
        plot = FALSE, ylabs = c("NDMI", "|MOSUM|"))
}, quiet = TRUE)

# myRegFun <- failwith(default = NULL, f = myRegFun, quiet = TRUE)       # If error, outputs NULL, and continue to next item
myRegFun <- tryCatch(myRegFun, error = function(e) NULL) 
# 5 regrowth detected


demo.reg.ls <- mapply(myRegFun, extrNDMI.ls.ok, demo.bfm.ls.ok)   # Only those with successful disturbance detection

# Remove those results in NULL
demo.reg.ls.ok <- demo.reg.ls
demo.reg.ls.ok[sapply(demo.reg.ls.ok, is.null)] <- NULL


source("R/Rfunction/plot_tsreg_mod.R")
# i = 11
# plot.tsreg.mod(demo.reg.ls[[i]], ylabs = c("NDMI", "|MOSUM|"))
# demo.reg.ls[[i]]

for(i in 1:length(demo.reg.ls.ok)) {
  pdf(str_c(path, "/prelim_figs/pdf/regrowth_experiment/param4/", 
            "DG2_justNatReveg_", names(demo.reg.ls.ok)[i], ".pdf"),
      width = 7, height = 3)
  plot.tsreg.mod(demo.reg.ls.ok[[i]], ylabs = c("NDMI", "|MOSUM|"), 
                 main = NULL,
                 text.x = as.Date("1990-01-01"), text.y = -0.2, text.text = names(demo.reg.ls.ok)[i])
  dev.off()
}



 