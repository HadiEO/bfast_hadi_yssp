## THIS SCRIPT NEED TO BE MODIFIED FOR REPO ##


library(timeSyncR)

setwd("H:/MyDocuments/DATA/ImageTimeStack/bestDG")

list.files("RGB")



# Skip (1)-(3) and read rds in (4) -----------------------------------------------



# (1) Read the images ---------------------------------------------------------------
# Downloads from GEE is automatically in UTM 50. Is it for all? Yes in this case of small area

L5.r <- stack("RGB/landsat5red_bestDG_KalArea1.tif")
L5.g <- stack("RGB/landsat5green_bestDG_KalArea1.tif")
L5.b <- stack("RGB/landsat5blue_bestDG_KalArea1.tif")

L7.r <- stack("RGB/landsat7red_bestDG_KalArea1.tif")
L7.g <- stack("RGB/landsat7green_bestDG_KalArea1.tif")
L7.b <- stack("RGB/landsat7blue_bestDG_KalArea1.tif")

L8.r <- stack("RGB/landsat8red_bestDG_KalArea1.tif")
L8.g <- stack("RGB/landsat8green_bestDG_KalArea1.tif")
L8.b <- stack("RGB/landsat8blue_bestDG_KalArea1.tif")



# (2) Read the scene ids ------------------------------------------------------

sceneId_L5 <- read_csv("sceneId_bestDG_KalArea1_L5.csv")   
sceneId_L5 <- sceneId_L5[seq(2,nrow(sceneId_L5),by=2),]                            

sceneId_L7 <- read_csv("sceneId_bestDG_KalArea1_L7.csv")   
sceneId_L7 <- sceneId_L7[seq(2,nrow(sceneId_L7),by=2),]                            

sceneId_L8 <- read_csv("sceneId_bestDG_KalArea1_L8.csv")   
sceneId_L8 <- sceneId_L8[seq(2,nrow(sceneId_L8),by=2),]                            


names(L5.r) <- sceneId_L5$header             ##*** Rename the brick with scene ids, they both are already ordered 1,2,3,...
names(L5.g) <- sceneId_L5$header
names(L5.b) <- sceneId_L5$header

names(L7.r) <- sceneId_L7$header
names(L7.g) <- sceneId_L7$header
names(L7.b) <- sceneId_L7$header

names(L8.r) <- sceneId_L8$header
names(L8.g) <- sceneId_L8$header
names(L8.b) <- sceneId_L8$header



# (3) Need to change value "0" to "NA" ----------------------------------------
L5.r[L5.r == 0] <- NA                                    # L5
write_rds(L5.r, "RGB/rds/Area1_L5_red.rds")

L5.g[L5.g == 0] <- NA
write_rds(L5.g, "RGB/rds/Area1_L5_green.rds")

L5.b[L5.b == 0] <- NA
write_rds(L5.b, "RGB/rds/Area1_L5_blue.rds")


L7.r[L7.r == 0] <- NA                                    # L7
write_rds(L7.r, "RGB/rds/Area1_L7_red.rds")

L7.g[L7.g == 0] <- NA
write_rds(L7.g, "RGB/rds/Area1_L7_green.rds")

L7.b[L7.b == 0] <- NA
write_rds(L7.b, "RGB/rds/Area1_L7_blue.rds")


L8.r[L8.r == 0] <- NA                                    # L8
write_rds(L8.r, "RGB/rds/Area1_L8_red.rds")

L8.g[L8.g == 0] <- NA
write_rds(L8.g, "RGB/rds/Area1_L8_green.rds")

L8.b[L8.b == 0] <- NA
write_rds(L8.b, "RGB/rds/Area1_L8_blue.rds")


# More efficient approach, example:
# cloud2NA <- function(x, y){
#   x[y != 0] <- NA
#   return(x)
# }
# 
# tahitiCloudFree <- overlay(x = tahiti6_2, y = fmask, fun = cloud2NA)






# (4) Read rds (this takes time coz large files!) ------------------------------------------------------------

L5.r <- read_rds("RGB/rds/Area1_L5_red.rds")
L5.g <- read_rds("RGB/rds/Area1_L5_green.rds")
L5.b <- read_rds("RGB/rds/Area1_L5_blue.rds")

# L7.r <- read_rds("RGB/rds/Area1_L7_red.rds")
# L7.g <- read_rds("RGB/rds/Area1_L7_green.rds")
# L7.b <- read_rds("RGB/rds/Area1_L7_blue.rds")

L8.r <- read_rds("RGB/rds/Area1_L8_red.rds")
L8.g <- read_rds("RGB/rds/Area1_L8_green.rds")
L8.b <- read_rds("RGB/rds/Area1_L8_blue.rds")



# SetZ --------------------------------------------------------------------

L5.r <- setZ(L5.r, getSceneinfo(names(L5.r))$date, name = 'time')    # Set time attribute in z slot
L5.g <- setZ(L5.g, getSceneinfo(names(L5.g))$date, name = 'time') 
L5.b <- setZ(L5.b, getSceneinfo(names(L5.b))$date, name = 'time') 

L7.r <- setZ(L7.r, getSceneinfo(names(L7.r))$date, name = 'time')    # Set time attribute in z slot
L7.r <- setZ(L7.g, getSceneinfo(names(L7.g))$date, name = 'time')
L7.r <- setZ(L7.b, getSceneinfo(names(L7.b))$date, name = 'time')

L8.r <- setZ(L8.r, getSceneinfo(names(L8.r))$date, name = 'time')    # Set time attribute in z slot
L8.r <- setZ(L8.g, getSceneinfo(names(L8.g))$date, name = 'time') 
L8.r <- setZ(L8.b, getSceneinfo(names(L8.b))$date, name = 'time') 



# Stack sensors -----------------------------------------------------------

L578.r <- addLayer(L5.r, L7.r, L8.r)

L578.g <- addLayer(L5.g, L7.g, L8.g)

L578.b <- addLayer(L5.b, L7.b, L8.b)

# The @z$time somewhat not carried to the addLayer output. So setZ again:
L578.r <- setZ(L578.r, getSceneinfo(names(L578.r))$date, name = 'time') 
L578.g <- setZ(L578.g, getSceneinfo(names(L578.g))$date, name = 'time') 
L578.b <- setZ(L578.b, getSceneinfo(names(L578.b))$date, name = 'time') 

# Sort raster layers by dates
L578.r <- subset(L578.r, order(getZ(L578.r)))
getZ(L578.r)

L578.g <- subset(L578.g, order(getZ(L578.g)))

L578.b <- subset(L578.b, order(getZ(L578.b)))


# Visualize with timeSyncR ------------------------------------------------
# chips <- tsChipsRGB(xr = L5.r, xg = L5.g, xb = L5.b, loc = pix, start = "2005-01-01", plot = TRUE, percNA = 0, buff = 50, exportChips = TRUE)
# timeSeries <- tsChipsRGB(xr = r, xg = g, xb = b, loc = pix, start = "2005-01-01", plot = TRUE, percNA = 0, buff = 50, exportZoo = TRUE)

op <- par()

getRasterCenter <- function(img) {                             ## Function to get raster center coordinate
  xcent <- extent(img)@xmin + 0.5 * (extent(img)@xmax - extent(img)@xmin)
  ycent <- extent(img)@ymin + 0.5 * (extent(img)@ymax - extent(img)@ymin)
  xycent <- c(xcent, ycent)
  
  xbuff <- 0.5 * (extent(img)@xmax - extent(img)@xmin)
  ybuff <- 0.5 * (extent(img)@ymax - extent(img)@ymin)
  
  res <- list(xycent = xycent, xbuff = xbuff, ybuff = ybuff)
  return(res)
}


# First, see whole image and dates, overlaid with DG extents, to d --------

xy.cent <- getRasterCenter(L5.r)$xycent                   ## Pixel coordinate, can we get coordinates from clicking the raster?
pix.cent <- pixelToPolygon(x = L5.r, cell = xy.cent)

getRasterCenter(L5.r)                             # Check buffer

range(L5.r@z$time)                                # Range of raster dates
range(L8.r@z$time)


plot(pix.cent)
tsChipsRGB(xr = L5.r, xg = L5.g, xb = L5.b, loc = pix.cent, start = "1988-01-31", plot = TRUE, 
           buff = 8955, percNA = 20) # buffer from getRasterCenter(L5.r)

# tsChipsRGB(xr = L8.r, xg = L8.g, xb = L8.b, loc = pix.cent, start = "2013-05-27", plot = TRUE, buff = 20, percNA = 20) # 6:80 - 

par(op)                                                           # Recover par to default



# See chips based on DG scenes --------------------------------------------

bestDG.coords <- read_csv("H:/MyDocuments/GIS/VHR/DG_Indonesia_Dates/rasters/rastersToPoints/csv/more17_KalArea1_spJoin_utm50N.csv")
nrow(bestDG.coords)



# Landsat-7 ---------------------------------------------------------------
# [outdated] Interesting rows (i.e. DG scene center) = 120, 80, 20, 88


row <- 88                # Change which DG scene center to view Landsat for **********************
xy.cent <- c(unlist(bestDG.coords[row,"X"]), unlist(bestDG.coords[row,"Y"]))  
pix.cent <- pixelToPolygon(x = L7.r, cell = xy.cent)

range(L7.r@z$time)

plot(pix.cent)
tsChipsRGB(xr = L7.r, xg = L7.g, xb = L7.b, loc = pix.cent, start = "1999-06-30", plot = TRUE,
           buff = 20, percNA = 20, textcol = "red") # buffer 20 pixel as one DG scene is 40 x 40 Landsat pixels

# If export time series
myTS <- tsChipsRGB(xr = L7.r, xg = L7.g, xb = L7.b, loc = pix.cent, start = "1999-06-30", plot = TRUE,
           buff = 20, percNA = 20, textcol = "red", exportZoo = TRUE) # buffer 20 pixel as one DG scene is 40 x 40 Landsat pixels

# myTS.R <- myTS$R
# str(myTS.R)
# myTS.R[myTS.R > 15000] <- NA
# 
# x11()
# plot(myTS.R)



# Landsat-5,7,8 stacked ---------------------------------------------------

# NEW 20170726 interesting rows from "H:/MyDocuments/GIS/VHR/DG_Indonesia_Dates/rasters/rastersToPoints/csv/more17_KalArea1_spJoin_utm50N.csv":
# Note change must be after 2002
# 88 deforested into plantation
# 9 subtle degradation (bare soil), but before 2000 also and recovers
# 13 road appearing early 2000, we can detect roads (linear series of pixels) that persists to be anomaly?
# 14 possible clearing in 2015, roads which recover
# 16 plantations appear in 2014
# 40 roads appear in 2001, plantation appears in 2013 (but small area)
# 47 clearing already in 1998, and plantation starts in 2011 and spreads
# 58 cleared before 2000, regrowth, then cleared again in 2011. Clearing visible as brown areas vs green areas.
# This (58) is nice time series showing first disturbance, then regrowth, then again disturbance into plantations.
# 65, 75 maybe one pixel degradation?
# 76 there is burn scar but before 2000, then plantations also like the others
# 89, plantation in 2012 and less noise
# 97, plantation in 2012 and less noise
# 92, road evolution
# 107, for some time bare soil visible
# 121, cleared patchi-ly (in 1998) but not converted to plantation            **
# 126, same as 121, what is the greener area in the end?                      **
# 128, same as 126, also an interesting patch
# 129, same as 126, is it burn scar which recovers?


row <- 129
xy.cent <- c(unlist(bestDG.coords[row,"X_utm50N"]), unlist(bestDG.coords[row,"Y_utm50N"]))  
pix.cent <- pixelToPolygon(x = L578.r, cell = xy.cent)

range(L578.r@z$time)

plot(pix.cent)
tsChipsRGB(xr = L578.r, xg = L578.g, xb = L578.b, loc = pix.cent, start = "1988-01-31", plot = TRUE,
           buff = 20, percNA = 20, textcol = "red") # buffer 20 pixel as one DG scene is 40 x 40 Landsat pixels

# Change buff = 100 to see larger area

interesting.rows <- c(9,13,14,16,40,47,58,65,75,76,88,89,92,97,107,121,126,128,129)
bestDG.interesting <- bestDG.coords[interesting.rows,]
bestDG.interesting$row <- interesting.rows

write_csv(bestDG.interesting, "H:/MyDocuments/DATA/VHR/exampleDGtif/checkTimeSyncR/selected_KalArea1/bestDG_interesting.csv")




# TimeSync export ---------------------------------------------------------

# Export zoo --------------------------------------------------------------
# The export keeps all image dates (dis-regarding percNA used in visualization)

expZoo <- tsChipsRGB(xr = L578.r, xg = L578.g, xb = L578.b, loc = pix.cent, start = "1988-01-31", plot = FALSE, show = FALSE,
           buff = 20, percNA = 20, textcol = "red", exportZoo = TRUE)


expZoo$R[which(expZoo$R > 2000)] <- NA                     # Further noise removal is needed! In the visible bands, remnant clouds are visible. Less influenced are NIR and SWIR (thus NBR)?


x11()
plot(expZoo$R, ylim = c(0,2000), type = "b")





# Export chips ------------------------------------------------------------

t.expChips <- system.time(
  expChips <- tsChipsRGB(xr = L578.r, xg = L578.g, xb = L578.b, loc = pix.cent, start = "1988-01-31", plot = FALSE,
                         buff = 20, percNA = 20, textcol = "red", exportChips = TRUE) # buffer 20 pixel as one DG scene is 40 x 40 Landsat pixels
)

write_rds(expChips, "RGB/chips_rds_etc/tsChips_pointId80.rds")

# writeRaster(x=turaStack, filename='turaStack.grd', datatype='INT2S')
t.writeChips <- system.time(
  writeRaster(expChips$R, "RGB/chips_rds_etc/tsChips_pointId80_red.tif", datatype = "INT2U")   # Red
)

writeRaster(expChips$G, "RGB/chips_rds_etc/tsChips_pointId80_green.tif", datatype = "INT2U")   # Green

writeRaster(expChips$B, "RGB/chips_rds_etc/tsChips_pointId80_blue.tif", datatype = "INT2U")   # Blue

# Write as single layers to preserve layer names
# writeRaster(stk, filename=names(stk), bylayer=TRUE,format="GTiff")
ori.wd <- getwd()

setwd("H:/MyDocuments/DATA/ImageTimeStack/bestDG/RGB/chips_rds_etc/single/R")
t.writeChipsSingle <- system.time(
  writeRaster(expChips$R, filename = names(expChips$R), bylayer = TRUE, format = "GTiff", datatype = "INT2U", overwrite = TRUE)   # Red
)

setwd("H:/MyDocuments/DATA/ImageTimeStack/bestDG/RGB/chips_rds_etc/single/G")
writeRaster(expChips$G, filename = names(expChips$G), bylayer = TRUE, format = "GTiff", datatype = "INT2U", overwrite = TRUE)   # Green

setwd("H:/MyDocuments/DATA/ImageTimeStack/bestDG/RGB/chips_rds_etc/single/B")
writeRaster(expChips$B, filename = names(expChips$B), bylayer = TRUE, format = "GTiff", datatype = "INT2U", overwrite = TRUE)   # Blue






