## This will give an idea of how good is the cloud QA in Sentinel-2 Level 1 C product


# Read the images ---------------------------------------------------------------
S2.b <- raster::stack(paste(path, "/raster_time_stack/rgb/Sentinel2_cloudMasked_DG_1_blue_10m.tif", sep = ""))
S2.g <- raster::stack(paste(path, "/raster_time_stack/rgb/Sentinel2_cloudMasked_DG_1_green_10m.tif", sep = ""))
S2.r <- raster::stack(paste(path, "/raster_time_stack/rgb/Sentinel2_cloudMasked_DG_1_red_10m.tif", sep = ""))

# Read the scene ids ------------------------------------------------------
sceneId_S2 <- read_csv(paste(path, "/raster_time_stack/scene_id/DG_1_Sentinel2.csv", sep = ""))
sceneId_S2 <- sceneId_S2[seq(2,nrow(sceneId_S2),by=2),]

names(S2.r) <- sceneId_S2$header             ##*** Rename the brick with scene ids, they both are already ordered 1,2,3,...
names(S2.g) <- sceneId_S2$header
names(S2.b) <- sceneId_S2$header

# SetZ --------------------------------------------------------------------
source("R/Rfunction/parseSentinel2SceneId.R")
temp <- sceneId_S2 %>% rowwise() %>% mutate(date = parseSentinel2SceneId(header))

S2.r <- setZ(S2.r, temp$date, name = 'time')    # Set time attribute in z slot
S2.g <- setZ(S2.g, temp$date, name = 'time') 
S2.b <- setZ(S2.b, temp$date, name = 'time') 

# Sort raster layers by dates ---------------------------------------------
S2.r <- subset(S2.r, order(getZ(S2.r)))
getZ(S2.r)

S2.g <- subset(S2.g, order(getZ(S2.g)))

S2.b <- subset(S2.b, order(getZ(S2.b)))



# Rescale values and remove nodata fill -----------------------------------
# calc may be not efficient for small raster? 
S2.r <- raster::calc(S2.r, fun = function(x) {x * 0.0001})
S2.g <- raster::calc(S2.g, fun = function(x) {x * 0.0001})
S2.b <- raster::calc(S2.b, fun = function(x) {x * 0.0001})

S2.r[S2.r == 0] <- NA
S2.g[S2.g == 0] <- NA
S2.b[S2.b == 0] <- NA

# SetZ *again* --------------------------------------------------------------------
source("R/Rfunction/parseSentinel2SceneId.R")
temp <- sceneId_S2 %>% rowwise() %>% mutate(date = parseSentinel2SceneId(header))

S2.r <- setZ(S2.r, temp$date, name = 'time')    # Set time attribute in z slot
S2.g <- setZ(S2.g, temp$date, name = 'time') 
S2.b <- setZ(S2.b, temp$date, name = 'time') 


# Show chips --------------------------------------------------------------
source("R/Rfunction/getRasterCenter.R")
xy.cent <- getRasterCenter(S2.r[[1]])$xycent
pix.cent <- pixelToPolygon(x = S2.r, cell = xy.cent)

date.start <- min(getZ(S2.r))
date.end <- max(getZ(S2.r))
raster.dim <- max(dim(S2.r)) / 2 

source("R/Rfunction/tsChipsRGB_Sentinel2.R")
# Oh well, there is an error somewhere in the tsChipsRG_Sentinel2 :/ 
tsChipsRG_Sentinel2(xr = S2.r, xg = S2.g, xb = S2.b, loc = pix.cent,
           buff = raster.dim, percNA = 20, textcol = "red", nr = 3, nc = 3) 


