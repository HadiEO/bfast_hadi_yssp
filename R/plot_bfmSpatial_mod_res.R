bfmSpatial.DG1.run27 <- read_rds(str_c(path, "/bfmSpatial_results/bfmSpatial_DG1_run27.rds"))
bfmSpatial.DG2.run27 <- read_rds(str_c(path, "/bfmSpatial_results/bfmSpatial_DG2_run27.rds"))
bfmSpatial.SC1.run27 <- read_rds(str_c(path, "/bfmSpatial_results/bfmSpatial_SC1_run27.rds"))
bfmSpatial.SQ9.run27 <- read_rds(str_c(path, "/bfmSpatial_results/bfmSpatial_SQ9_run27.rds"))

# **********************************************************************************
# DG1
# **********************************************************************************
change.DG1.run27 <- bfmSpatial.DG1.run27$breakpoint

# Plot decimal date of change
raster::plot(change.DG1.run27)

# Plot years of change 
change.DG1.run27.year <- calc(change.DG1.run27, floor)
min.year <- summary(change.DG1.run27.year)[1,]
max.year <- summary(change.DG1.run27.year)[5,] + 1
  
brks <- c(min.year:max.year); brks.char <- as.character(brks)
col <- "RdYlBu"   # "RdBu"
raster::plot(change.DG1.run27.year, col=rev(brewer.pal(8, col)), legend=F, main="")
raster::plot(change.DG1.run27.year, col=rev(brewer.pal(8, col)), legend.only=T, legend.width=1, legend.shrink=1, side=4, cex=1.25,
     axis.args=list(at=brks, labels=brks.char, cex.axis=1.25))


# **********************************************************************************
# Decision, plot the year of change, export the geotiff to make map in Arcmap
# **********************************************************************************

# **********************************************************************************
# DG1
# **********************************************************************************
change.DG1.run27 <- bfmSpatial.DG1.run27$breakpoint
change.DG1.run27.year <- calc(change.DG1.run27, floor)
summary(change.DG1.run27.year)[1,]
summary(change.DG1.run27.year)[5,] + 1

writeRaster(change.DG1.run27.year, format = "GTiff",
            filename = str_c(path, "/bfmSpatial_results/geotiff/bfmSpatial_DG1_run27_change_year.tif"))

# **********************************************************************************
# DG2
# **********************************************************************************
change.DG2.run27 <- bfmSpatial.DG2.run27$breakpoint
change.DG2.run27.year <- calc(change.DG2.run27, floor)
summary(change.DG2.run27.year)[1,]
summary(change.DG2.run27.year)[5,] + 1
unique(values(change.DG2.run27.year))

writeRaster(change.DG2.run27.year, format = "GTiff",
            filename = str_c(path, "/bfmSpatial_results/geotiff/bfmSpatial_DG2_run27_change_year.tif"))

# **********************************************************************************
# SC1
# **********************************************************************************
change.SC1.run27 <- bfmSpatial.SC1.run27$breakpoint
change.SC1.run27.year <- calc(change.SC1.run27, floor)
summary(change.SC1.run27.year)[1,]
summary(change.SC1.run27.year)[5,] + 1

writeRaster(change.SC1.run27.year, format = "GTiff",
            filename = str_c(path, "/bfmSpatial_results/geotiff/bfmSpatial_SC1_run27_change_year.tif"))

# **********************************************************************************
# SQ9
# **********************************************************************************
change.SQ9.run27 <- bfmSpatial.SQ9.run27$breakpoint
change.SQ9.run27.year <- calc(change.SQ9.run27, floor)
summary(change.SQ9.run27.year)[1,]
summary(change.SQ9.run27.year)[5,] + 1

writeRaster(change.SQ9.run27.year, format = "GTiff",
            filename = str_c(path, "/bfmSpatial_results/geotiff/bfmSpatial_SQ9_run27_change_year.tif"))
