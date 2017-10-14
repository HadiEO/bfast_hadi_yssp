# Select oil palm (DG1) and small clearing samples -------------------------
bfm.DG1.ls <- read_rds(str_c(path, "/extracted_time_series/bfm_justHarmon_extrNDMIsub_DG1.rds"))
bfm.SC1.ls <- read_rds(str_c(path, "/extracted_time_series/bfm_justHarmon_extrNDMIsub_SC1.rds"))
# Other small clearing in DG2 and SQ10
bfm.DG2.ls <- read_rds(str_c(path, "/extracted_time_series/bfm_justHarmon_extrNDMIsub_DG2.rds"))
bfm.sq10.ls <- read_rds(str_c(path, "/extracted_time_series/bfm_justHarmon_extrNDMIsub_sq10.rds"))



# The shapefile
shp.folder <- paste(path, "/vector_data", sep = "")
refPixels.DG1 <- readOGR(dsn = shp.folder, layer = "meshSelect_prevDG1_label_ok")
refPixels.SC1 <- readOGR(dsn = shp.folder, layer = "meshSelect_SC_1_label_ok")
# Other small clearing in DG2 and SQ10
refPixels.DG2 <- readOGR(dsn = shp.folder, layer = "meshSelect_prevDG2_label_ok")
refPixels.sq10 <- readOGR(dsn = shp.folder, layer = "meshSelect_sq_10_label_ok")



# Get oil palm from DG1
refPixels.DG1.oilPalm <- refPixels.DG1[refPixels.DG1$Visual %in% 
                                         c("Oil palm (visible 20150815) older",
                                           "Oil palm (visible 20150815) others",
                                           "Oil palm (visible 20150815) younger"), ]  # 49 samples

oilPalm.idx <- which(names(bfm.DG1.ls) %in% refPixels.DG1.oilPalm$Id_1)
bfm.DG1OilPalm.ls <- bfm.DG1.ls[oilPalm.idx]
identical(as.character(refPixels.DG1.oilPalm$Id_1), names(bfm.DG1OilPalm.ls))

# Get small clearing (N = 13)
refPixels.SC1.sm <- refPixels.SC1[refPixels.SC1$Visual == "Small clearing 20140204", ]
sm.idx <- which(names(bfm.SC1.ls) %in% refPixels.SC1.sm$Id_1)
bfm.SC1sm.ls <- bfm.SC1.ls[sm.idx]
identical(as.character(refPixels.SC1.sm$Id_1), names(bfm.SC1sm.ls))


# Get very small clearing (N = 31)
refPixels.SC1.vsm <- refPixels.SC1[refPixels.SC1$Visual == "Very small clearing 20140204", ]
vsm.idx <- which(names(bfm.SC1.ls) %in% refPixels.SC1.vsm$Id_1)
bfm.SC1vsm.ls <- bfm.SC1.ls[vsm.idx]
identical(as.character(refPixels.SC1.vsm$Id_1), names(bfm.SC1vsm.ls))


# Get other small clearing
# in DG1
refPixels.DG1.rd <- refPixels.DG1[refPixels.DG1$Visual == "Dirt road", ]
DG1rd.idx <- which(names(bfm.DG1.ls) %in% refPixels.DG1.rd$Id_1)
bfm.DG1rd.ls <- bfm.DG1.ls[DG1rd.idx]
identical(as.character(refPixels.DG1.rd$Id_1), names(bfm.DG1rd.ls))

# in DG2
refPixels.DG2.sm <- refPixels.DG2[refPixels.DG2$Visual == "Forest partially cleared 20150808", ]
DG2sm.idx <- which(names(bfm.DG2.ls) %in% refPixels.DG2.sm$Id_1)
bfm.DG2sm.ls <- bfm.DG2.ls[DG2sm.idx]
identical(as.character(refPixels.DG2.sm$Id_1), names(bfm.DG2sm.ls))

# in sq10
refPixels.sq10.rd <- refPixels.sq10[refPixels.sq10$Visual == "Acacia road (sub-pixel) from 20050726", ]
sq10rd.idx <- which(names(bfm.sq10.ls) %in% refPixels.sq10.rd$Id_1)
bfm.sq10rd.ls <- bfm.sq10.ls[sq10rd.idx]
identical(as.character(refPixels.sq10.rd$Id_1), names(bfm.sq10rd.ls))


# Check bfm results: animate


# Oil palm ----------------------------------------------------------------

bfmPlot.ls <- list()
for(i in 1:length(bfm.DG1OilPalm.ls)) {
  bfmPlot.ls[[i]] <- bfmPlot(bfm.DG1OilPalm.ls[[i]],
                             plotlabs = names(bfm.DG1OilPalm.ls)[i], displayMagn = TRUE) + 
    theme_bw() + labs(y = "NDMI", x = "Date") + scale_y_continuous(limits = c(-0.4, 0.7)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  
}

# saveHTML({                                             # Doesn't work for ggplot?
#   oopt <- ani.options(interval = 0.5, nmax = 50)
#   ani.options(oopt)
#   
#   for(i in 1:length(bfmPlot.ls)) { 
#     bfmPlot.ls[[i]]
#     ani.pause()
#   } 
# }, img.name = "bfm_plot", ani.height = 550, ani.width = 1000,
# title = "Title here", 
# description = "Description here"
# )

# So save manually to disk
for(i in 1:length(bfmPlot.ls)) { 
  ggsave(str_c(path, "/animation/oil_palm/bfm_H/", "Rplot_", i, ".png"), plot = bfmPlot.ls[[i]],   #TODO: save filename with the plot ID
         width = 140, height = 80, units = "mm")
}


# Manually make mp4 -------------------------------------------------------

# convert -delay 1x2 -limit memory 512mb -limit map 64mb *.png movie.mp4
# 'convert *.png -delay 5 -loop 0 stick_figure.gif'
# Just do it in command line.



# Small clearing ----------------------------------------------------------------

bfmPlot.ls <- list()
for(i in 1:length(bfm.SC1sm.ls)) {
  bfmPlot.ls[[i]] <- bfmPlot(bfm.SC1sm.ls[[i]],
                             plotlabs = str_c(names(bfm.SC1sm.ls)[i], " : small clearing between 20120325 & 20140204"), displayMagn = TRUE) + 
    theme_bw() + labs(y = "NDMI", x = "Date") + scale_y_continuous(limits = c(-0.4, 0.7)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  
}

# So save manually to disk
for(i in 1:length(bfmPlot.ls)) { 
  ggsave(str_c(path, "/animation/small_clearing/bfm_H/", "Rplot_", i, ".png"), plot = bfmPlot.ls[[i]],   #TODO: save filename with the plot ID
         width = 140, height = 80, units = "mm")
}

# Very small clearing ----------------------------------------------------------------

bfmPlot.ls <- list()
for(i in 1:length(bfm.SC1vsm.ls)) {
  bfmPlot.ls[[i]] <- bfmPlot(bfm.SC1vsm.ls[[i]],
                             plotlabs = str_c(names(bfm.SC1vsm.ls)[i], " : very small clearing between 20120325 & 20140204"), displayMagn = TRUE) + 
    theme_bw() + labs(y = "NDMI", x = "Date") + scale_y_continuous(limits = c(-0.4, 0.7)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  
}

# So save manually to disk
for(i in 1:length(bfmPlot.ls)) { 
  ggsave(str_c(path, "/animation/very_small_clearing/bfm_H/", "Rplot_", i, ".png"), plot = bfmPlot.ls[[i]],   #TODO: save filename with the plot ID
         width = 140, height = 80, units = "mm")
}


# Other small clearing ----------------------------------------------------------------
# require(rlist)
# bfm.other.sm <- list.append(bfm.DG1rd.ls, bfm.DG2sm.ls, bfm.sq10rd.ls)  # N = 11, N = 8, N = 8

# Do separately
bfm.other.sm <- bfm.DG1rd.ls



bfmPlot.ls <- list()
for(i in 1:length(bfm.other.sm)) {
  bfmPlot.ls[[i]] <- bfmPlot(bfm.other.sm[[i]],
                             plotlabs = str_c(names(bfm.other.sm)[i], " : acacia road visible from 2005"), displayMagn = TRUE) + 
    theme_bw() + labs(y = "NDMI", x = "Date") + scale_y_continuous(limits = c(-0.4, 0.7)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  
}

# So save manually to disk
for(i in 1:length(bfmPlot.ls)) { 
  ggsave(str_c(path, "/animation/other_small_clearing/bfm_H/", "Rplot_", i, ".png"), plot = bfmPlot.ls[[i]],   #TODO: save filename with the plot ID
         width = 140, height = 80, units = "mm")
}
