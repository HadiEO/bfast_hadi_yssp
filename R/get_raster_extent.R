myRaster.dir <- paste(path, "/digital_globe/DG_1/", sep = "")
list.files(myRaster.dir)
myRaster <- brick(paste(myRaster.dir, "77ff77b3-172f-cb3f-44c2-be8a48df72c8_110_0_20150815_024102.jpg", sep = ""))

e <- extent(myRaster)
p <- as(e, 'SpatialPolygons')
crs(p) <- crs(myRaster)

raster::plot(myRaster)
raster::plot(p, add = TRUE)

p.spdf <- SpatialPolygonsDataFrame(p, data = data.frame(x = 0))

writeOGR(p.spdf, dsn = paste(path, "/vector_data", sep = ""), layer = "extent_DG_1",  driver = "ESRI Shapefile", overwrite_layer = TRUE)
