# Read the scene ID saved by copy-pasting from GEE console
sceneId_L7 <- read_csv(paste(path, "/raster_time_stack/scene_id/DG_1_L7_duringSentinel2.csv", sep = ""))
sceneId_L7 <- sceneId_L7[seq(2,nrow(sceneId_L7),by=2),]

sceneId_L8 <- read_csv(paste(path, "/raster_time_stack/scene_id/DG_1_L8_duringSentinel2.csv", sep = ""))
sceneId_L8 <- sceneId_L8[seq(2,nrow(sceneId_L8),by=2),]

sceneId_S2 <- read_csv(paste(path, "/raster_time_stack/scene_id/DG_1_Sentinel2.csv", sep = ""))
sceneId_S2 <- sceneId_S2[seq(2,nrow(sceneId_S2),by=2),]

date.L7 <- unique(getSceneinfo(sceneId_L7$header)$date)
date.L8 <- unique(getSceneinfo(sceneId_L8$header)$date)
source("R/Rfunction/parseSentinel2SceneId.R")
temp <- sceneId_S2 %>% rowwise() %>% mutate(date = parseSentinel2SceneId(header))
date.S2 <- unique(temp$date) 

date.merged <- sort(c(date.L7, date.L8, date.S2))

# How many scenes 

# Plot 
x <- seq(as_date("2015-01-01"), as_date("2017-09-16"), by = 1)
plot(x, rep(2, length(x)), ylim = c(0,1))
# points(date.merged, rep(1.5, length(date.merged)))
# Colour by sensors
points(date.L7, rep(0.5, length(date.L7)), col = "blue", cex = 2, pch = 19)
points(date.L8, rep(0.5, length(date.L8)), col = "dark orange", cex = 2, pch = 19)
points(date.S2, rep(0.5, length(date.S2)), col = "magenta", cex = 2, pch = 19)
