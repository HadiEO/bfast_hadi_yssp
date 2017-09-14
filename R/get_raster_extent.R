myRaster.dir <- paste(path, "/digital_globe/SC_1/", sep = "")
list.files(myRaster.dir)
myRaster <- brick(paste(myRaster.dir, "371440b0-cb85-bc34-0d00-1ff0ffee5c38_110_0_20140204_033630.jpg", sep = ""))

e <- extent(myRaster)
p <- as(e, 'SpatialPolygons')
crs(p) <- crs(myRaster)

raster::plot(myRaster)
raster::plot(p, add = TRUE)

p.spdf <- SpatialPolygonsDataFrame(p, data = data.frame(x = 0))

writeOGR(p.spdf, dsn = paste(path, "/vector_data", sep = ""), layer = "extent_SC_1",  driver = "ESRI Shapefile", overwrite_layer = TRUE)
