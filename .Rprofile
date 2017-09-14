# Set library path as I am mostly working in Aalto laptop anyway
.libPaths("C:/Program Files/R/R-3.3.1/library")

# Load libraries
require(ggplot2)
require(raster)
require(plyr)
require(dplyr)
require(stringr)
require(tidyverse)
require(lubridate)
require(strucchange)
require(zoo)
require(bfastSpatial)  
require(sp)
require(rgrowth)
require(bfastPlot)
require(mapedit)
require(mapview)
require(sf)
require(dtwSat)
require(shiny)
require(timesyncR)



# Set path for all machines

if(.Platform$OS.type == 'windows') {
  path <- 'C:/LocalUserData/User-data/hadi1/PHD_RESEARCH/STUDY_IIASA/bfast_hadi_yssp_data'
} else {
#   info <- Sys.info()
#   if (info['nodename'] == 'vanoise') {
#     path <- '/media/dutri001/LP_DUTRIEUX_Data/RS'
#   } else if (info['nodename'] == 'papaya') {
#     path <- '/media/DATA3/dutri001'
#   } else if (info['nodename'] == 'tanargue') {
#     path <- 'media/whatever/'
#   }
  
} # For some reasons the empty line at the bottom is important