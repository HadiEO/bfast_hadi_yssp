#################################################################################################################
# DG1 --------------------------------
##################################################################################################################
# Import raster time stack with unique dates
NDMI.unique <- read_rds(str_c(path, "/raster_time_stack/ndmi_rds/ndmi_DG_1_unique.rds"))

# Subset raster date
DG.lastDate <- as.Date("2015-08-15")
jday.monitEnd <- c(year(DG.lastDate),yday(DG.lastDate))             # Date of latest VHSR
NDMI.unique.sub <- subsetRasterTS(NDMI.unique, maxDate = jday.monitEnd)

# Import shapefile
shp.folder <- paste(path, "/vector_data", sep = "")
demoPixels <- readOGR(dsn = shp.folder, layer = "meshSelect_prevDG1_label_forDemo_ok")

# Extract time series
extrNDMI <- bfastSpatial::zooExtract(x = NDMI.unique.sub,
                                    sample = demoPixels,
                                    method = "simple")            # no need buffer to account geometric error cause we are concerned with pixel-specific time series and relative changes
colnames(extrNDMI) <- as.character(demoPixels$DemoId)
write_rds(extrNDMI, str_c(path, "/extracted_time_series/extrNDMIsub_DG1_demo.rds"))

# Create gap-less time series
extrNDMI.ls <- as.list(extrNDMI)
# demo.bts <- bfastts(extrNDMI[, "B5"], dates = getZ(NDMI.unique.sub), type = "irregular")
demo.bts.ls <- lapply(extrNDMI.ls, FUN = function(z) bfastts(z, dates = getZ(NDMI.unique.sub), type = "irregular"))

                  
# Run bfastmonitor 
DG.firstDate <- as.Date("2002-09-29")
DG.lastDate <- as.Date("2015-08-15")
jday.monitStart <- c(year(DG.firstDate), yday(DG.firstDate))          # Date of earliest VHSR showing still forested area

demo.bfm.TH.ls <- lapply(demo.bts.ls, 
        FUN = function(z) bfastmonitor(z, start = jday.monitStart, 
                          formula = response~harmon+trend, order = 1, plot = FALSE, h = 0.25, history = "all"))

# Plot bfm result
x11()
my.bfmPlot.1 <- bfmPlot(list(demo.bfm.TH.ls$A1, demo.bfm.TH.ls$A2, demo.bfm.TH.ls$A3),
        plotlabs = c("A1", "A2", "A3"), ncols = 1, displayMagn = TRUE) + 
  theme_bw() + labs(y = "NDMI", x = "Date") + scale_y_continuous(limits = c(-0.2, 0.6)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

my.bfmPlot.2 <- bfmPlot(list(demo.bfm.TH.ls$B1, demo.bfm.TH.ls$B2, demo.bfm.TH.ls$B3, demo.bfm.TH.ls$B4, demo.bfm.TH.ls$B5),
        plotlabs = c("B1", "B2", "B3", "B4", "B5"), ncols = 1, displayMagn = TRUE) + 
  theme_bw() + labs(y = "NDMI", x = "Date") + scale_y_continuous(limits = c(-0.2, 0.6)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# For C was forest in 2002
jday.monitStart.C <- c(year(as.Date("2002-09-29")), yday(as.Date("2002-09-29"))) 

demo.bfm.TH.ls.C <- lapply(list(demo.bts.ls$C1, demo.bts.ls$C2), 
                         FUN = function(z) bfastmonitor(z, start = jday.monitStart.C, 
                                                        formula = response~harmon+trend, order = 1, plot = FALSE, h = 0.25, history = "all"))

my.bfmPlot.3 <- bfmPlot(demo.bfm.TH.ls.C,
        plotlabs = c("C1", "C2"), ncols = 1, displayMagn = TRUE) + 
  theme_bw() + labs(y = "NDMI", x = "Date") + scale_y_continuous(limits = c(-0.2, 0.6)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Save plots
ggsave("bfmPlot_DG1_demo_A_start2002.pdf", plot = my.bfmPlot.1, device = "pdf",
       path = str_c(path, "/prelim_figs/pdf"), width = 180, height = 40*3, unit = "mm")

ggsave("bfmPlot_DG1_demo_B_start2002.pdf", plot = my.bfmPlot.2, device = "pdf",
       path = str_c(path, "/prelim_figs/pdf"), width = 180, height = 40*5, unit = "mm")

ggsave("bfmPlot_DG1_demo_C.pdf", plot = my.bfmPlot.3, device = "pdf",
       path = str_c(path, "/prelim_figs/pdf"), width = 180, height = 40*2, unit = "mm")


# Plot raw times series
default.par <- par()
v <- as.numeric(as.Date(paste0(seq(1988,2016,by=1),'-01-01')))

pdf(str_c(path, "/prelim_figs/pdf", "/tsPlot_DG1_demo_B.pdf"), width = 7.4, height = 12.5)
par(mfrow = c(5,1))
plot(extrNDMI.ls$B1, type = "p", pch = 19, xlab = "Date", ylab = "NDMI", ylim = c(-0.3, 0.7), main = "B1")
abline(v = v,lty = "dotted",col = "gray20",lwd = 1)
plot(extrNDMI.ls$B2, type = "p", pch = 19, xlab = "Date", ylab = "NDMI", ylim = c(-0.3, 0.7), main = "B2")
abline(v = v,lty = "dotted",col = "gray20",lwd = 1)
plot(extrNDMI.ls$B3, type = "p", pch = 19, xlab = "Date", ylab = "NDMI", ylim = c(-0.3, 0.7), main = "B3")
abline(v = v,lty = "dotted",col = "gray20",lwd = 1)
plot(extrNDMI.ls$B4, type = "p", pch = 19, xlab = "Date", ylab = "NDMI", ylim = c(-0.3, 0.7), main = "B4")
abline(v = v,lty = "dotted",col = "gray20",lwd = 1)
plot(extrNDMI.ls$B5, type = "p", pch = 19, xlab = "Date", ylab = "NDMI", ylim = c(-0.3, 0.7), main = "B5")
abline(v = v,lty = "dotted",col = "gray20",lwd = 1)

dev.off()


#################################################################################################################
# DG2 --------------------------------
##################################################################################################################
# Import raster time stack with unique dates
NDMI.unique <- read_rds(str_c(path, "/raster_time_stack/ndmi_rds/ndmi_DG_2_unique.rds"))

# Subset raster date
DG.lastDate <- as.Date("2015-08-08")
jday.monitEnd <- c(year(DG.lastDate),yday(DG.lastDate))             # Date of latest VHSR
NDMI.unique.sub <- subsetRasterTS(NDMI.unique, maxDate = jday.monitEnd)

# Import shapefile
shp.folder <- paste(path, "/vector_data", sep = "")
demoPixels.1 <- readOGR(dsn = shp.folder, layer = "meshSelect_prevDG2_label_forDemo_ok")
demoPixels.2 <- readOGR(dsn = shp.folder, layer = "meshSelect_prev_prevDG2_label_select4")

# Extract time series
extrNDMI.1 <- bfastSpatial::zooExtract(x = NDMI.unique.sub,
                                     sample = demoPixels.1,
                                     method = "simple")            # no need buffer to account geometric error cause we are concerned with pixel-specific time series and relative changes
colnames(extrNDMI.1) <- as.character(demoPixels.1$DemoID)
write_rds(extrNDMI.1, str_c(path, "/extracted_time_series/extrNDMIsub_DG2_demo1.rds"))

extrNDMI.2 <- bfastSpatial::zooExtract(x = NDMI.unique.sub,
                                       sample = demoPixels.2,
                                       method = "simple")            # no need buffer to account geometric error cause we are concerned with pixel-specific time series and relative changes
colnames(extrNDMI.2) <- as.character(demoPixels.2$DemoId)
write_rds(extrNDMI.2, str_c(path, "/extracted_time_series/extrNDMIsub_DG2_demo2.rds"))


# Create gap-less time series
extrNDMI.1.ls <- as.list(extrNDMI.1)
extrNDMI.2.ls <- as.list(extrNDMI.2)

# demo.bts <- bfastts(extrNDMI[, "B5"], dates = getZ(NDMI.unique.sub), type = "irregular")
demo.bts.1.ls <- lapply(extrNDMI.1.ls, FUN = function(z) bfastts(z, dates = getZ(NDMI.unique.sub), type = "irregular"))
demo.bts.2.ls <- lapply(extrNDMI.2.ls, FUN = function(z) bfastts(z, dates = getZ(NDMI.unique.sub), type = "irregular"))


# Run bfastmonitor 
DG.firstDate <- as.Date("2002-09-29")
DG.lastDate <- as.Date("2015-08-15")
jday.monitStart <- c(year(DG.firstDate), yday(DG.firstDate))          # Date of earliest VHSR showing still forested area


demo.bfm.TH.1.ls <- lapply(demo.bts.1.ls, 
                         FUN = function(z) bfastmonitor(z, start = jday.monitStart, 
                                                        formula = response~harmon+trend, order = 6, plot = FALSE, h = 0.25, history = "all", level = 0.05))

demo.bfm.TH.2.ls <- lapply(demo.bts.2.ls, 
                           FUN = function(z) bfastmonitor(z, start = jday.monitStart, 
                                                          formula = response~harmon+trend, order = 6, plot = FALSE, h = 0.25, history = "all", level = 0.05))

# Sequential by 1 year
# years <- c(2002:2016)
# demo.bfm.TH.seq.1.ls <- lapply(demo.bts.1.ls, 
#                            FUN = function(z) bfastmonitor(window(z, start = jday.monitStart, 
#                                                           formula = response~harmon+trend, order = 6, plot = FALSE, h = 0.25, history = "all", level = 0.05))
# 
# demo.bfm.TH.seq.2.ls <- lapply(demo.bts.2.ls, 
#                            FUN = function(z) bfastmonitor(z, start = jday.monitStart, 
#                                                           formula = response~harmon+trend, order = 6, plot = FALSE, h = 0.25, history = "all", level = 0.05))




# For S start monitor 06 Jul 2011?
# demo.bfm.TH.S.ls <- lapply(list(S1 = demo.bts.1.ls$S1, S2 = demo.bts.1.ls$S2, S3 = demo.bts.1.ls$S3), 
#                            FUN = function(z) bfastmonitor(z, start = c(year(as.Date("2011-07-06")), yday(as.Date("2011-07-06"))) , 
#                                                           formula = response~harmon+trend, order = 1, plot = FALSE, h = 0.25, history = "all"))
# 


# Plot bfm result
my.bfmPlot.S <- bfmPlot(list(demo.bfm.TH.1.ls$S1, demo.bfm.TH.1.ls$S2, demo.bfm.TH.1.ls$S3),
                        plotlabs = c("S1", "S2", "S3"), ncols = 1, displayMagn = TRUE) + 
  theme_bw() + labs(y = "NDMI", x = "Date") + scale_y_continuous(limits = c(-0.2, 0.6)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

my.bfmPlot.ION <- bfmPlot(list(I1 = demo.bfm.TH.1.ls$I1, O1 = demo.bfm.TH.1.ls$O1, N1 = demo.bfm.TH.1.ls$N1,
                                N2 = demo.bfm.TH.2.ls$N2),
                        plotlabs = c("I1", "O1", "N1", "N2"), ncols = 1, displayMagn = TRUE) + 
  theme_bw() + labs(y = "NDMI", x = "Date") + scale_y_continuous(limits = c(-0.2, 0.6)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


my.bfmPlot.BR <- bfmPlot(list(B1 = demo.bfm.TH.2.ls$B1, R1 = demo.bfm.TH.2.ls$R1),
                          plotlabs = c("B1", "R1"), ncols = 1, displayMagn = TRUE) + 
  theme_bw() + labs(y = "NDMI", x = "Date") + scale_y_continuous(limits = c(-0.2, 0.6)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Save plots
ggsave("bfmPlot_DG2_demo_S.pdf", plot = my.bfmPlot.S, device = "pdf",
       path = str_c(path, "/prelim_figs/pdf"), width = 180, height = 40*3, unit = "mm")

ggsave("bfmPlot_DG2_demo_ION.pdf", plot = my.bfmPlot.ION, device = "pdf",
       path = str_c(path, "/prelim_figs/pdf"), width = 180, height = 40*4, unit = "mm")

ggsave("bfmPlot_DG2_demo_BR.pdf", plot = my.bfmPlot.BR, device = "pdf",
       path = str_c(path, "/prelim_figs/pdf"), width = 180, height = 40*2, unit = "mm")


#################################################################################################################
# DG1 with segmentation --------------------------------
##################################################################################################################
demo.bts.ls <- lapply(extrNDMI.ls, FUN = function(z) bfastts(z, dates = getZ(NDMI.unique.sub), type = "irregular"))
demo.bts.ls.int <- lapply(demo.bts.ls, FUN = function(z) na.approx(z))

# t.segment <- system.time(
#   bf <- bfast(demo.bts.ls.int$B1, h = 0.15, season = "harmonic", max.iter = 1, breaks = 1)
# )

t.bf01 <- system.time(
  # bf01 <- lapply(demo.bts.ls.int, FUN = function(z) bfast01(z, order = 1, bandwidth = 0.15))
  bf01_h25 <- lapply(demo.bts.ls.int, FUN = function(z) bfast01(z, order = 1, bandwidth = 0.25))
)

# write_rds(bf01, str_c(path, "/extracted_time_series/bf01_extrNDMIsub_DG1_demo.rds"))
write_rds(bf01_h25, str_c(path, "/extracted_time_series/bf01h25_extrNDMIsub_DG1_demo.rds"))

pdf(str_c(path, "/prelim_figs/pdf", "/bf01Plot_DG1_demo.pdf"), width = 7.4, height = 7.5)
par(mfrow = c(3,1))
plot(bf01_h25$B4, regular = TRUE, ylim = c(-0.2, 0.6), main = "B4", ylab = "NDMI", xlab = "Date")
plot(bf01_h25$B5, regular = TRUE, ylim = c(-0.2, 0.6), main = "B5", ylab = "NDMI", xlab = "Date")
plot(bf01_h25$A1, regular = TRUE, ylim = c(-0.2, 0.6), main = "A1", ylab = "NDMI", xlab = "Date")
dev.off()




#################################################################################################################
# SQ10 pulpwood --------------------------------
##################################################################################################################
# Import raster time stack with unique dates
NDMI.unique <- read_rds(str_c(path, "/raster_time_stack/ndmi_rds/ndmi_sq_10_unique.rds"))

# Subset raster date
DG.lastDate <- as.Date("2015-08-17")
jday.monitEnd <- c(year(DG.lastDate),yday(DG.lastDate))             # Date of latest VHSR
NDMI.unique.sub <- subsetRasterTS(NDMI.unique, maxDate = jday.monitEnd)

# Import shapefile
shp.folder <- paste(path, "/vector_data", sep = "")
demoPixels <- readOGR(dsn = shp.folder, layer = "meshSelect_sq_10_label_forDemo_ok")

# Extract time series
extrNDMI <- bfastSpatial::zooExtract(x = NDMI.unique.sub,
                                     sample = demoPixels,
                                     method = "simple")            # no need buffer to account geometric error cause we are concerned with pixel-specific time series and relative changes
colnames(extrNDMI) <- as.character(demoPixels$DemoId)
write_rds(extrNDMI, str_c(path, "/extracted_time_series/extrNDMIsub_sq10_demo.rds"))

# Create gap-less time series
extrNDMI.ls <- as.list(extrNDMI)
# demo.bts <- bfastts(extrNDMI[, "B5"], dates = getZ(NDMI.unique.sub), type = "irregular")
demo.bts.ls <- lapply(extrNDMI.ls, FUN = function(z) bfastts(z, dates = getZ(NDMI.unique.sub), type = "irregular"))

# Plot raw times series
default.par <- par()
v <- as.numeric(as.Date(paste0(seq(1988,2016,by=1),'-01-01')))

pdf(str_c(path, "/prelim_figs/pdf", "/tsPlot_sq10_demo.pdf"), width = 7.4, height = 10)
par(mfrow = c(4,1))
plot(extrNDMI.ls$P1, type = "p", pch = 19, xlab = "Date", ylab = "NDMI", ylim = c(-0.3, 0.7), main = "P1")
abline(v = v,lty = "dotted",col = "gray20",lwd = 1)
plot(extrNDMI.ls$P2, type = "p", pch = 19, xlab = "Date", ylab = "NDMI", ylim = c(-0.3, 0.7), main = "P2")
abline(v = v,lty = "dotted",col = "gray20",lwd = 1)
plot(extrNDMI.ls$P3, type = "p", pch = 19, xlab = "Date", ylab = "NDMI", ylim = c(-0.3, 0.7), main = "P3")
abline(v = v,lty = "dotted",col = "gray20",lwd = 1)
plot(extrNDMI.ls$P4, type = "p", pch = 19, xlab = "Date", ylab = "NDMI", ylim = c(-0.3, 0.7), main = "P4")
abline(v = v,lty = "dotted",col = "gray20",lwd = 1)

dev.off()

# Test segmentation
demo.bts.ls.int <- lapply(demo.bts.ls, FUN = function(z) na.approx(z))
test <- demo.bts.ls.int$P4
require(xts)
test.month <- apply.monthly(as.xts(zoo.test), FUN=mean)






t.bf01 <- system.time(
  # bf01 <- lapply(demo.bts.ls.int, FUN = function(z) bfast01(z, order = 1, bandwidth = 0.15))
  bf01_h25 <- bfast(test, order = 1, bandwidth = 0.25)
)
write_rds(bf01_h25, str_c(path, "/extracted_time_series/bf01h25_extrNDMIsub_sq10_demo_P4.rds"))

# Test sequential bfm
# p <- 2; years <- seq(2005, 2015, by = p)              
# 
# bfmSeq.H <- lapply(years, 
#                    FUN = function(z) bfastmonitor(window(bts, end = c(z + p, 1)), start = c(z, 1), history = "all", 
#                                                   formula = response ~ harmon, order = 1, h = 0.25))
# # history = "ROC" gives too few observations to fit a new history
# 
# # Plot the result
# plot.bfmSeq <- bfmPlot(bfmSeq.H, plotlabs = years, displayTrend = TRUE, displayMagn = TRUE, displayResiduals = "monperiod") + 
#   theme_bw() + scale_y_continuous(limits = c(-0.4,0.8))
# plot.bfmSeq





