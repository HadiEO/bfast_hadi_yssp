# Select oil palm (DG1) and natural reveg samples -------------------------
bf.DG1.ls <- read_rds(str_c(path, "/extracted_time_series/bf_DG1_subTS_h015.rds"))
bf.natReveg.ls <- read_rds(str_c(path, "/extracted_time_series/bf_justNatReveg_subTS_h015.rds"))


# The shapefile
shp.folder <- paste(path, "/vector_data", sep = "")
refPixels.DG1 <- readOGR(dsn = shp.folder, layer = "meshSelect_prevDG1_label_ok")
refPixels.justNatReveg <- readOGR(dsn = shp.folder, layer = "meshSelect_DG2_label_justNatReveg")  # 41 samples

# bf.natReveg.ls is already for only natural revegetation samples. Now get bf results for oil palm samples.
refPixels.DG1.oilPalm <- refPixels.DG1[refPixels.DG1$Visual %in% 
                                         c("Oil palm (visible 20150815) older",
                                           "Oil palm (visible 20150815) others",
                                           "Oil palm (visible 20150815) younger"), ]  # 49 samples


oilPalm.idx <- which(names(bf.DG1.ls) %in% refPixels.DG1.oilPalm$Id_1)
bf.DG1OilPalm.ls <- bf.DG1.ls[oilPalm.idx]



# Check bf results: animate


# Oil palm ----------------------------------------------------------------

# Run ImageMagick within R
# my_command <- 'convert *.png -delay 5 -loop 0 stick_figure.gif'
# system(my_command)

require(animation)
# Save as HTML
saveHTML({
  oopt <- ani.options(interval = 0.5, nmax = 50)
  ani.options(oopt)
  for(i in 1:length(bf.DG1OilPalm.ls)) {
    plot(bf.DG1OilPalm.ls[[i]], type="trend", largest=TRUE, main = names(bf.DG1OilPalm.ls)[i], ylim = c(-0.4, 0.7))
    ani.pause()
    } 
  }, img.name = "bf_plot", ani.height = 550, ani.width = 1000,
  title = "Title here", 
  description = "Description here"
)

# This creates a file "index.html" with associated folders 'images', 'js' and 'css'

# Save as GIF (doesn't work :())
# Need to install ImageMagick. During installation, tick "Install legacy utilities!
# ani.options(convert = 'C:/Program Files/ImageMagick-7.0.7-Q16/convert.exe')
# saveGIF({
#   oopt <- ani.options(interval = 0.5, nmax = 50)
#   ani.options(oopt)
#   for(i in 1:length(bf.DG1OilPalm.ls)) {
#     plot(bf.DG1OilPalm.ls[[i]], type="trend", largest=TRUE, main = names(bf.DG1OilPalm.ls)[i])
#     ani.pause()
#   } 
# }, movie.name = "bf_plot.gif", img.name = "Rplot", convert  = "convert",
# cmd.fun = system, clean = TRUE, ani.height = 300, ani.width = 550,
# title = "Title here", 
# description = "Description here"
# )


# Try save as video 
# Need to install FFMPEG (see animation package vignette)
# There are two locations of ffmpeg.exe
# ani.options(ffmpeg = 'C:/LocalUserData/User-data/hadi1/PHD_RESEARCH/STUDY_IIASA/tools/ffmpeg-20170921-183fd30-win64-static/bin/ffmpeg.exe')
# ani.options(ffmpeg = 'C:/Program Files/ImageMagick-7.0.7-Q16/ffmpeg.exe')


saveVideo({
  oopt <- ani.options(interval = 0.5, nmax = 50)
  ani.options(oopt)
  for(i in 1:length(bf.DG1OilPalm.ls)) {
    plot(bf.DG1OilPalm.ls[[i]], type="trend", largest=TRUE, main = names(bf.DG1OilPalm.ls)[i])
    ani.pause()
  }
}, video.name = "bf_plot.mp4", img.name = "Rplot", 
ffmpeg  = 'C:/LocalUserData/User-data/hadi1/PHD_RESEARCH/STUDY_IIASA/tools/ffmpeg-20170921-183fd30-win64-static/bin/ffmpeg.exe',
clean = TRUE, ani.height = 300, ani.width = 550,
title = "Title here",
description = "Description here"
)



# Manually make mp4 -------------------------------------------------------

# convert -delay 1x2 -limit memory 512mb -limit map 64mb *.jpg movie.mp4
# 'convert *.png -delay 5 -loop 0 stick_figure.gif'
# Just do it in command line.

# Natural revegetation ----------------------------------------------------

# Save as HTML
saveHTML({
  oopt <- ani.options(interval = 0.5, nmax = 50)
  ani.options(oopt)
  for(i in 1:length(bf.natReveg.ls)) {
    plot(bf.natReveg.ls[[i]], type="trend", largest=TRUE, main = names(bf.natReveg.ls)[i], ylim = c(-0.4, 0.7))
    ani.pause()
  } 
}, img.name = "bf_plot", ani.height = 550, ani.width = 1000,
title = "Title here", 
description = "Description here"
)



