# The intact forests
# Check visually
# Plot annual cycle

# Read zoo time series
(path)
path2 <- "/extracted_time_series/FINALLY_USED/"
extrNDMIsub_DG1 <- read_rds(str_c(path, path2, "extrNDMIsub_DG1.rds"))        # Checked these are unique dates
extrNDMIsub_DG2 <- read_rds(str_c(path, path2, "extrNDMIsub_DG2.rds"))
extrNDMIsub_SC1 <- read_rds(str_c(path, path2, "extrNDMIsub_SC1.rds"))
extrNDMIsub_sq9 <- read_rds(str_c(path, path2, "extrNDMIsub_sq9.rds"))
extrNDMIsub_sq10 <- read_rds(str_c(path, path2, "extrNDMIsub_sq10.rds"))
extrNDMIsub_sq11 <- read_rds(str_c(path, path2, "extrNDMIsub_sq11.rds"))
extrNDMIsub_sq13 <- read_rds(str_c(path, path2, "extrNDMIsub_sq13.rds"))

# Make list
# extrNDMIsub_DG1_ls <- as.list(extrNDMIsub_DG1)
# extrNDMIsub_DG2_ls <- as.list(extrNDMIsub_DG2)
# extrNDMIsub_SC1_ls <- as.list(extrNDMIsub_SC1)
# extrNDMIsub_sq9_ls <- as.list(extrNDMIsub_sq9)
# extrNDMIsub_sq10_ls <- as.list(extrNDMIsub_sq10)
# extrNDMIsub_sq11_ls <- as.list(extrNDMIsub_sq11)
# extrNDMIsub_sq13_ls <- as.list(extrNDMIsub_sq13)

# Read interpreted Landsat pixels (shapefile)
shp.folder <- paste(path, "/vector_data/FINALLY_USED", sep = "")
pixels_DG1 <- readOGR(dsn = shp.folder, layer = "meshSelect_prevDG1")
pixels_DG2 <- readOGR(dsn = shp.folder, layer = "meshSelect_prevDG2")
pixels_SC1 <- readOGR(dsn = shp.folder, layer = "meshSelect_SC_1")
pixels_sq9 <- readOGR(dsn = shp.folder, layer = "meshSelect_sq_9")
pixels_sq10 <- readOGR(dsn = shp.folder, layer = "meshSelect_sq_10")
pixels_sq11 <- readOGR(dsn = shp.folder, layer = "meshSelect_sq_11")
pixels_sq13 <- readOGR(dsn = shp.folder, layer = "meshSelect_sq_13")


# Get Id of "intact forest"
intactId_DG1 <- as.character(pixels_DG1$Id[pixels_DG1$Visual == "Intact (20020929 - 20150815)"]) 
intactId_DG2 <- as.character(pixels_DG2$Id[pixels_DG2$Visual == "Intact forest (20020929 - 20150808)"])
intactId_SC1 <- as.character(pixels_SC1$Id[pixels_SC1$Visual == "Intact forest 20050726 - 20140204"])
intactId_sq9 <- as.character(pixels_sq9$Id[pixels_sq9$Visual == "Intact forest 20020818-20140513"])
intactId_sq13 <- as.character(pixels_sq13$Id[pixels_sq13$Visual == "Intact forest 20140513"])

(length(intactId_DG1)) # 23
(length(intactId_DG2)) # 34
(length(intactId_SC1)) # 28->100
(length(intactId_sq9)) # 26
(length(intactId_sq13)) # 52
# total 163->235 intact forest pixels


# The DG scene clusters: (1) DG1, DG2; (2) SQ9, SQ13; (3)  SC1, SQ10, SQ11

# Subset the ts with the intact forest Id
extrNDMIsub_DG1_intact <- extrNDMIsub_DG1[, intactId_DG1]
extrNDMIsub_DG2_intact <- extrNDMIsub_DG2[, intactId_DG2]
extrNDMIsub_SC1_intact <- extrNDMIsub_SC1[, intactId_SC1]
extrNDMIsub_sq9_intact <- extrNDMIsub_sq9[, intactId_sq9]
extrNDMIsub_sq13_intact <- extrNDMIsub_sq13[, intactId_sq13]

# Convert to xts object
xts_DG1_intact <- as.xts(extrNDMIsub_DG1_intact)
xts_DG2_intact <- as.xts(extrNDMIsub_DG2_intact)
xts_SC1_intact <- as.xts(extrNDMIsub_SC1_intact)
xts_sq9_intact <- as.xts(extrNDMIsub_sq9_intact)
xts_sq13_intact <- as.xts(extrNDMIsub_sq13_intact)


# Check for intact forest pixels that show disturbance signal in time series to be removed
x <- xts_SC1_intact
for(i in 1:nrow(x)) {
   plot(index(x[,i]), x[,i], ylim = c(-0.2, 0.7), pch = 19, main = colnames(x)[i])
   locator(1)
}

# Below is not finalised!
# It's hard to decide from the time series, but taking median across pixels should prevent registering disturbance signal in the seasonal profile
# DG1: zero high confidence disturbance visually
# DG2: 294, 442, 515, 596, 665, 813, 967, 1008, 1158, 1172, 1197, 1272, 1322
# SC1: 913, 1069, 1377, 1385, 1245, (1312), (1387), (1393)
# sq9: 1337, 1339, 1341
# sq13: 


# Todo
# Check non-intact
# Check DG scenes not yet here



# Plot time series 
# par(mfrow = c(2,1))
# # The plot.zoo way
# tsRainbow <- rainbow(ncol(xts_DG1_intact))
# plot.zoo(xts_DG1_intact, plot.type = "single", type = "p", main = "plot.zoo",
#          col = tsRainbow)
# # The plot.xts way
# plot.xts(xts_DG1_intact, plot.type = "single", type = "p", main = "plot.xts",
#          grid.ticks.on = "year")

# ********************************************************
# Plot seasonality
# ********************************************************
# xts_DG1_intact_byYear <- split(xts_DG1_intact, f = "years")
# xts_DG2_intact_byYear <- split(xts_DG2_intact, f = "years")
# xts_SC1_intact_byYear <- split(xts_SC1_intact, f = "years")
# xts_sq9_intact_byYear <- split(xts_sq9_intact, f = "years")
# xts_sq13_intact_byYear <- split(xts_sq13_intact, f = "years")
# 
# 
# # Visualize year by year
# x <- xts_sq13_intact_byYear
# type <- "l"
# naApprox <- FALSE
# for(i in 1:length(x)) {
#   if(naApprox) {
#     plot.zoo(na.approx(x[[i]]), plot.type = "single", type = type, main = "plot.zoo",
#              col = tsRainbow, ylim = c(-0.2, 0.7))
#     locator(1)
#   }
#   plot.zoo(x[[i]], plot.type = "single", type = type, main = "plot.zoo",
#            col = tsRainbow, ylim = c(-0.2, 0.7))
#   locator(1)
# }

# Visualize all years overlain  
# Average across space or
# Average across time?
# Ok, for each observation date, average (median) across pixels AND across years
# The DG scene clusters: (1) DG1, DG2; (2) SQ9, SQ13; (3) SC1, SQ10, SQ11
xts_DG1_DG2_intact <- merge.xts(xts_DG1_intact, xts_DG2_intact, join = "outer")
xts_sq9_sq13_intact <- merge.xts(xts_sq9_intact, xts_sq13_intact, join = "outer")
xts_SC1_intact <- xts_SC1_intact

# DG1, DG2
# Median
median_DG1_DG2 <- apply(xts_DG1_DG2_intact, 1, median, na.rm = TRUE)
median_DG1_DG2 <- as.xts(median_DG1_DG2)
# plot(index(xts_DG1_DG2_intact), median_DG1_DG2)
# Sd
sd_DG1_DG2 <- apply(xts_DG1_DG2_intact, 1, sd, na.rm = TRUE)
sd_DG1_DG2 <- as.xts(sd_DG1_DG2)
# Split by year
median_DG1_DG2_byYear <- split(median_DG1_DG2, f = "years")

# SQ9, SQ13
median_sq9_sq13 <- apply(xts_sq9_sq13_intact, 1, median, na.rm = TRUE)
median_sq9_sq13 <- as.xts(median_sq9_sq13)

# SC1
median_SC1 <- apply(xts_SC1_intact, 1, median, na.rm = TRUE)
median_SC1 <- as.xts(median_SC1)

# Plot by zoo or xts way (can't get it to work aargh!)
# x <- median_DG1_DG2_byYear
# x1 <- x[[1]]; x2 <- x[[2]]
# indexFormat(x1) <- "%m"; indexFormat(x2) <- "%m"
# plot.zoo(x2, type = "p", xlab = "Date", ylab = "NDMI", ylim = c(-0.2, 0.7))
# points(x1, col = "red")
# walk()
 
# Plot the ggplot way
# Need data frame
# DG1, DG2
df_DG1_DG2 <- tibble(date = index(median_DG1_DG2), value = as.numeric(median_DG1_DG2))
df_DG1_DG2 <- df_DG1_DG2 %>% mutate(
  doy = yday(date),
  dom = mday(date),
  month = month(date),
  year = year(date)
)

# SQ9, SQ13
df_sq9_sq13 <- tibble(date = index(median_sq9_sq13), value = as.numeric(median_sq9_sq13))
df_sq9_sq13 <- df_sq9_sq13 %>% mutate(
  doy = yday(date),
  dom = mday(date),
  month = month(date),
  year = year(date)
)

# SC1
df_SC1 <- tibble(date = index(median_SC1), value = as.numeric(median_SC1))
df_SC1 <- df_SC1 %>% mutate(
  doy = yday(date),
  dom = mday(date),
  month = month(date),
  year = year(date)
)


# Choose the year with >= N obs ?
# df %>%  group_by(year) %>% summarize(count = n()) %>% select(count) %>% table
# 
# df %>% dplyr::filter(year >= 1995, year <= 2000) %>% 
#   ggplot(aes(x = doy, y = value, group = year, col = year)) + 
#   geom_point(na.rm = TRUE)  + 
#   scale_color_gradientn(colours = rainbow(5))


# Or maybe just average across years. Update: YES, 2000-2004; 2005-2009; 2010-2014 *********

# ***************************************************************************
# Source function
source("R/Rfunction/plot_seasonality.R")
# ***************************************************************************

# Use the function
plotSeasonality_DG1_DG2 <- plotSeasonality(df_DG1_DG2, legend.pos = c(0.5, 0.15), showX = FALSE, yLab = "")
plotSeasonality_sq9_sq13 <- plotSeasonality(df_sq9_sq13, legend.pos = "none", showX = FALSE, yLab = "NDMI")
plotSeasonality_SC1 <- plotSeasonality(df_SC1, legend.pos = "none", showX = TRUE, yLab = "")

# The multiplot way
# pdf("report/figs/intactForestSeasonality.pdf", width = 7, height = 5, pointsize = 10) 
# multiplot(plotSeasonality_DG1_DG2, plotSeasonality_sq9_sq13, plotSeasonality_SC1, cols = 1)
# dev.off()

# The grid.draw way
pdf("report/figs/intactForestSeasonality_Kalimantan.pdf", width = 7, height = 5, pointsize = 10)
grid.newpage()
grid.draw(rbind(ggplotGrob(plotSeasonality_DG1_DG2), 
                ggplotGrob(plotSeasonality_sq9_sq13), 
                ggplotGrob(plotSeasonality_SC1),
                size = "last"))
dev.off()


# Todo: if we fit seasonal model what is R2



# ********************************************************************************
# Check seasonality of other spectral variables
# ********************************************************************************
# Update: Save center of intact forest pixels as point vector data (shapefile) for use in GEE
pixels_DG1_intact <- pixels_DG1[pixels_DG1$Visual == "Intact (20020929 - 20150815)", ]
pixels_DG2_intact <- pixels_DG2[pixels_DG2$Visual == "Intact forest (20020929 - 20150808)", ]
pixels_SC1_intact <- pixels_SC1[pixels_SC1$Visual == "Intact forest 20050726 - 20140204", ]
pixels_sq9_intact <- pixels_sq9[pixels_sq9$Visual == "Intact forest 20020818-20140513", ]
pixels_sq13_intact <- pixels_sq13[pixels_sq13$Visual == "Intact forest 20140513", ]

# Make centroid spatial points
pixels_DG1_intact_centers <- SpatialPointsDataFrame(
  coords = coordinates(pixels_DG1_intact), proj4string = crs(pixels_DG1_intact),
  data = pixels_DG1_intact@data)
pixels_DG2_intact_centers <- SpatialPointsDataFrame(
  coords = coordinates(pixels_DG2_intact), proj4string = crs(pixels_DG2_intact),
  data = pixels_DG2_intact@data)
pixels_SC1_intact_centers <- SpatialPointsDataFrame(
  coords = coordinates(pixels_SC1_intact), proj4string = crs(pixels_SC1_intact),
  data = pixels_SC1_intact@data)
pixels_sq9_intact_centers <- SpatialPointsDataFrame(
  coords = coordinates(pixels_sq9_intact), proj4string = crs(pixels_sq9_intact),
  data = pixels_sq9_intact@data)
pixels_sq13_intact_centers <- SpatialPointsDataFrame(
  coords = coordinates(pixels_sq13_intact), proj4string = crs(pixels_sq13_intact),
  data = pixels_sq13_intact@data)

# Project to WGS1984 (same name)
pixels_DG1_intact_centers <- spTransform(pixels_DG1_intact_centers, CRS("+init=epsg:4326"))
pixels_DG2_intact_centers <- spTransform(pixels_DG2_intact_centers, CRS("+init=epsg:4326"))
pixels_SC1_intact_centers <- spTransform(pixels_SC1_intact_centers, CRS("+init=epsg:4326"))
pixels_sq9_intact_centers <- spTransform(pixels_sq9_intact_centers, CRS("+init=epsg:4326"))
pixels_sq13_intact_centers <- spTransform(pixels_sq13_intact_centers, CRS("+init=epsg:4326"))

# Add a new attribute column containing scene id to avoid confusion with same numeric id with other scenes
pixels_DG1_intact_centers$Id_new <- str_c("DG1_", pixels_DG1_intact_centers$Id)
pixels_DG2_intact_centers$Id_new <- str_c("DG2_", pixels_DG2_intact_centers$Id)
pixels_SC1_intact_centers$Id_new <- str_c("SC1_", pixels_SC1_intact_centers$Id)
pixels_sq9_intact_centers$Id_new <- str_c("SQ9_", pixels_sq9_intact_centers$Id)
pixels_sq13_intact_centers$Id_new <- str_c("SQ13_", pixels_sq13_intact_centers$Id)

# Write out as shapefile
path2 <- "/vector_data/FINALLY_USED/intact_forest_points"
dsn <- str_c(path, path2)  

writeOGR(pixels_DG1_intact_centers, dsn, "pixels_DG1_intact_centers", driver="ESRI Shapefile")
writeOGR(pixels_DG2_intact_centers, dsn, "pixels_DG2_intact_centers", driver="ESRI Shapefile")
writeOGR(pixels_SC1_intact_centers, dsn, "pixels_SC1_intact_centers", driver="ESRI Shapefile")
writeOGR(pixels_sq9_intact_centers, dsn, "pixels_sq9_intact_centers", driver="ESRI Shapefile")
writeOGR(pixels_sq13_intact_centers, dsn, "pixels_sq13_intact_centers", driver="ESRI Shapefile")


# Read rds made in analyzeExtrTimeSeries_newLandsatCollection.R
extr.ts.DG1 <- read_rds(str_c(path, "/Amazon/extrL5n7RawBandsKalimantan2000to2004_DG1.rds"))
extr.ts.DG2 <- read_rds(str_c(path, "/Amazon/extrL5n7RawBandsKalimantan2000to2004_DG2.rds"))
extr.ts.SC1 <- read_rds(str_c(path, "/Amazon/extrL5n7RawBandsKalimantan2000to2004_SC1.rds"))
extr.ts.SQ9 <- read_rds(str_c(path, "/Amazon/extrL5n7RawBandsKalimantan2000to2004_SQ9.rds"))
extr.ts.SQ13 <- read_rds(str_c(path, "/Amazon/extrL5n7RawBandsKalimantan2000to2004_SQ13.rds"))

# Combine extr.ts
# extr.ts <- bind_rows(extr.ts.DG1, extr.ts.DG2, extr.ts.SC1, extr.ts.SQ9, extr.ts.SQ13)
extr.ts.DG1.DG2 <- bind_rows(extr.ts.DG1, extr.ts.DG2)
extr.ts.SQ9.SQ13 <- bind_rows(extr.ts.SQ9, extr.ts.SQ13)
extr.ts.SC1 <- extr.ts.SC1

# Tidy up 
extr.ts.DG1.DG2.Melt <- reshape2::melt(extr.ts.DG1.DG2, id.vars = c("Id_new", "Visual", "date", "sensor"), 
                  measure.vars = c("blue", "green", "red", "nir", "swir1", "swir2", "nbr", "ndmi", "ndvi"))

extr.ts.SQ9.SQ13.Melt <- reshape2::melt(extr.ts.SQ9.SQ13, id.vars = c("Id_new", "Visual", "date", "sensor"), 
                  measure.vars = c("blue", "green", "red", "nir", "swir1", "swir2", "nbr", "ndmi", "ndvi"))

extr.ts.SC1.Melt <- reshape2::melt(extr.ts.SC1, id.vars = c("Id_new", "Visual", "date", "sensor"), 
                  measure.vars = c("blue", "green", "red", "nir", "swir1", "swir2", "nbr", "ndmi", "ndvi"))


now.variable <- "nir"                                 # what variable? ****************************

df.DG1.DG2 <- as_tibble(extr.ts.DG1.DG2.Melt) %>% 
  filter(variable == now.variable) %>% 
  mutate(doy = yday(date),
         dom = mday(date),
         month = month(date),
         year = year(date),
         site = "DG1, DG2")

df.SQ9.SQ13 <- as_tibble(extr.ts.SQ9.SQ13.Melt) %>% 
  filter(variable == now.variable) %>% 
  mutate(doy = yday(date),
         dom = mday(date),
         month = month(date),
         year = year(date),
         site = "SQ9, SQ13")

df.SC1 <- as_tibble(extr.ts.SC1.Melt) %>% 
  filter(variable == now.variable) %>% 
  mutate(doy = yday(date),
         dom = mday(date),
         month = month(date),
         year = year(date),
         site = "SC1")


# Get median across space (pixel ID) and years
# ************************************************************
# (1) Aggregate spatially and temporally (simultaneously)
# ************************************************************
df.DG1.DG2.medianByDoy <- df.DG1.DG2 %>%
  group_by(doy) %>% summarise(median_value = median(value, na.rm = TRUE)) %>%   # mean or median? ***
  ungroup()
  
df.SQ9.SQ13.medianByDoy <- df.SQ9.SQ13 %>%
  group_by(doy) %>% summarise(median_value = median(value, na.rm = TRUE)) %>%   # mean or median? ***
  ungroup()
  
df.SC1.medianByDoy <- df.SC1 %>%
  group_by(doy) %>% summarise(median_value = median(value, na.rm = TRUE)) %>%   # mean or median? ***
  ungroup()

# plot(df_medianByDoy$doy, df_medianByDoy$median_value, ylim = c(-0.2, 0.6)) # NDMI
# plot(df_medianByDoy$doy, df_medianByDoy$median_value, ylim = c(0.4, 1)) # NDVI
# plot(df_medianByDoy$doy, df_medianByDoy$median_value, ylim = c(0, 0.5)) # NIR
# plot(df_medianByDoy$doy, df_medianByDoy$median_value, ylim = c(0, 0.1)) # red
# plot(df_medianByDoy$doy, df_medianByDoy$median_value, ylim = c(0, 0.3)) # SWIR1
# plot(df_medianByDoy$doy, df_medianByDoy$median_value, ylim = c(0, 0.6)) # NDMI narrower ylim


# ggplot
# Merge the three sites keeping a site column
df.DG1.DG2.medianByDoy$site <- "DG1, DG2"
df.SQ9.SQ13.medianByDoy$site <- "SQ9, SQ13"
df.SC1.medianByDoy$site <- "SC1"

df.merged.medianByDoy <- bind_rows(df.DG1.DG2.medianByDoy, 
                            df.SQ9.SQ13.medianByDoy,
                            df.SC1.medianByDoy)

# Make dummy date for plotting purpose
df.merged.medianByDoy <- df.merged.medianByDoy %>% 
  mutate(dummy_date = as.Date(doy, origin = "2014-12-31"))


# To plot Apr to Mar, add 365 days to Jan-Mar
df.merged.medianByDoy[which(df.merged.medianByDoy$doy <= 90), "dummy_date"] <-
  df.merged.medianByDoy[which(df.merged.medianByDoy$doy <= 90), "dummy_date"] + 365


# Plot
myPlot <- ggplot(df.merged.medianByDoy, aes(x = dummy_date, y = median_value, col = site, shape = site)) +
  geom_point(na.rm = TRUE, size = 1) +
  theme_bw() + labs(y = "NIR", x = "") + scale_y_continuous(limits = c(0, 0.5)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b", date_minor_breaks = "1 month",
               limits = c(as.Date("2015-03-21"), as.Date("2016-04-10")) , 
               expand = c(0,0)) 

pdf("report/figs/intactForestSeasonality_Kalimantan_2000to2004_SWIR1.pdf", width = 7, height = 3, pointsize = 10)
myPlot
dev.off()

# ************************************************************
# Plot to compare different aggregation level
# ************************************************************
# (a) Visualize for each doy, all obs of different pixels (ID) and years
# data point is one original obs (not aggregated by any means)
df.medianBy.Year.Doy.Id <- df.SC1 %>%                                     # Which VHSR scenes?
  mutate(year = factor(year, c(2000, 2001, 2002, 2003, 2004))) %>% 
  group_by(Id_new, year, doy) %>%  
  summarise(median_value = median(value, na.rm = TRUE)) %>% 
  ungroup()

plot(df.medianBy.Year.Doy.Id$doy, df.medianBy.Year.Doy.Id$median_value, 
     ylim = c(0, 0.5), col = "#00000060", pch = 19) # NIR

# (b1) then for each doy, all obs of different pixels (ID) i.e. aggregate across years
# data point is median of all years, for each pixel (ID), for each doy
df.medianBy.Year.Doy.Id.thenAggrYear <- df.medianBy.Year.Doy.Id %>% 
  group_by(Id_new, doy) %>% 
  summarise(median_value = median(median_value, na.rm = TRUE)) %>% 
  ungroup()

points(df.medianBy.Year.Doy.Id.thenAggrYear$doy, df.medianBy.Year.Doy.Id.thenAggrYear$median_value,
       col = "blue", pch = 19)   # blue "#2b8cbe"

# (b2) then for each doy, all obs of different years i.e. aggregate across pixels (ID)
# data point is median of all pixels, for each date (not aggregate across years)
df.medianBy.Year.Doy.Id.thenAggrId <- df.medianBy.Year.Doy.Id %>% 
  group_by(year, doy) %>% 
  summarise(median_value = median(median_value, na.rm = TRUE)) %>% 
  ungroup()

points(df.medianBy.Year.Doy.Id.thenAggrId$doy, df.medianBy.Year.Doy.Id.thenAggrId$median_value,
       col = "red", pch = 19)


# (c1) for each doy, aggregate across years (b1), then aggregate across pixels (ID)
df.medianBy.Year.Doy.Id.thenAggrYear.thenAggrId <- df.medianBy.Year.Doy.Id.thenAggrYear %>% 
  group_by(doy) %>% 
  summarise(median_value = median(median_value, na.rm = TRUE)) %>% 
  ungroup()

points(df.medianBy.Year.Doy.Id.thenAggrYear.thenAggrId$doy, df.medianBy.Year.Doy.Id.thenAggrYear.thenAggrId$median_value,
       col = "green", pch = 19)

# (c2) for each doy, aggregate across pixels/ID (b2), then aggregate across years 
df.medianBy.Year.Doy.Id.thenAggrId.thenAggrYear <- df.medianBy.Year.Doy.Id.thenAggrId %>% 
  group_by(doy) %>% 
  summarise(median_value = median(median_value, na.rm = TRUE)) %>% 
  ungroup() 

points(df.medianBy.Year.Doy.Id.thenAggrId.thenAggrYear$doy, df.medianBy.Year.Doy.Id.thenAggrId.thenAggrYear$median_value,
       col = "orange", pch = 19)

# Make dummy date for plotting purpose
df.medianBy.Year.Doy.Id <- df.medianBy.Year.Doy.Id %>% 
  mutate(dummy_date = as.Date(doy, origin = "2014-12-31"))

df.medianBy.Year.Doy.Id.thenAggrYear <- df.medianBy.Year.Doy.Id.thenAggrYear %>% 
  mutate(dummy_date = as.Date(doy, origin = "2014-12-31"))

df.medianBy.Year.Doy.Id.thenAggrId <- df.medianBy.Year.Doy.Id.thenAggrId %>% 
  mutate(dummy_date = as.Date(doy, origin = "2014-12-31"))

df.medianBy.Year.Doy.Id.thenAggrYear.thenAggrId <- df.medianBy.Year.Doy.Id.thenAggrYear.thenAggrId %>% 
  mutate(dummy_date = as.Date(doy, origin = "2014-12-31"))

df.medianBy.Year.Doy.Id.thenAggrId.thenAggrYear <- df.medianBy.Year.Doy.Id.thenAggrId.thenAggrYear %>% 
  mutate(dummy_date = as.Date(doy, origin = "2014-12-31"))

# To plot June to May, add 365 days to Jan-May
df.medianBy.Year.Doy.Id[which(df.medianBy.Year.Doy.Id$doy <= 90), "dummy_date"] <-
  df.medianBy.Year.Doy.Id[which(df.medianBy.Year.Doy.Id$doy <= 90), "dummy_date"] + 365

df.medianBy.Year.Doy.Id.thenAggrYear[which(df.medianBy.Year.Doy.Id.thenAggrYear$doy <= 90), "dummy_date"] <-
  df.medianBy.Year.Doy.Id.thenAggrYear[which(df.medianBy.Year.Doy.Id.thenAggrYear$doy <= 90), "dummy_date"] + 365

df.medianBy.Year.Doy.Id.thenAggrId[which(df.medianBy.Year.Doy.Id.thenAggrId$doy <= 90), "dummy_date"] <-
  df.medianBy.Year.Doy.Id.thenAggrId[which(df.medianBy.Year.Doy.Id.thenAggrId$doy <= 90), "dummy_date"] + 365

df.medianBy.Year.Doy.Id.thenAggrYear.thenAggrId[which(df.medianBy.Year.Doy.Id.thenAggrYear.thenAggrId$doy <= 90), "dummy_date"] <-
  df.medianBy.Year.Doy.Id.thenAggrYear.thenAggrId[which(df.medianBy.Year.Doy.Id.thenAggrYear.thenAggrId$doy <= 90), "dummy_date"] + 365

df.medianBy.Year.Doy.Id.thenAggrId.thenAggrYear[which(df.medianBy.Year.Doy.Id.thenAggrId.thenAggrYear$doy <= 90), "dummy_date"] <-
  df.medianBy.Year.Doy.Id.thenAggrId.thenAggrYear[which(df.medianBy.Year.Doy.Id.thenAggrId.thenAggrYear$doy <= 90), "dummy_date"] + 365



# Show year
# cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# ggplot(df.medianBy.Year.Doy.Id.thenAggrId, aes(dummy_date, median_value, col = year)) +
#   geom_point(na.rm = TRUE, size = 1) +
#   theme_bw() + labs(y = "NIR", x = "") + scale_y_continuous(limits = c(0, 0.5)) +
#   scale_x_date(date_breaks = "1 month", date_labels = "%b", date_minor_breaks = "1 month",
#                limits = c(as.Date("2015-05-21"), as.Date("2016-06-10")) ,
#                expand = c(0,0)) +
#   scale_colour_brewer(palette="Set1")   # scale_color_manual(values = cbPalette)

# Show different aggregation levels
# (a) Original sample (not aggregated)
a <- select(df.medianBy.Year.Doy.Id, dummy_date, median_value) %>% 
  mutate(aggregation = "Individual observation")

b <- select(df.medianBy.Year.Doy.Id.thenAggrYear, dummy_date, median_value) %>% 
  mutate(aggregation = "For each DOY, median across years")

c <- select(df.medianBy.Year.Doy.Id.thenAggrYear.thenAggrId, dummy_date, median_value) %>% 
  mutate(aggregation = "For each DOY, median across years, then median across pixels")

d <- select(df.medianBy.Year.Doy.Id.thenAggrId.thenAggrYear, dummy_date, median_value) %>% 
  mutate(aggregation = "For each DOY, median across pixels, then median across years")

abcd <- bind_rows(a, b, c, d) %>% 
  mutate(aggregation = factor(aggregation,
                              c("Individual observation", 
                                "For each DOY, median across years",
                                "For each DOY, median across years, then median across pixels",
                                "For each DOY, median across pixels, then median across years")))

print(pts.cols)
print(pts.pchs)
col2hex("orange")

myPlot <- ggplot(abcd, aes(dummy_date, median_value, col = aggregation, shape = aggregation)) +
  geom_point(na.rm = TRUE, size = 1) +
  theme_bw() + labs(y = "NIR", x = "") + scale_y_continuous(limits = c(0, 0.5)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b", date_minor_breaks = "1 month",
               limits = c(as.Date("2015-03-21"), as.Date("2016-04-10")) ,
               expand = c(0,0)) +
  # scale_colour_manual(values = c("dark orange", "dodgerblue", "chartreuse4", "dark blue")) + 
  scale_colour_manual(values = c("#FFA50060", "#1E90FF60", "red", "dark blue")) + 
  scale_shape_manual(values = c(21, 21, 21, 21)) +
  scale_fill_manual(values = c(NA, NA, NA, NA)) +
  theme(axis.line = element_line(colour = "black"), panel.background = element_blank(), 
        axis.text = element_text(size = 8), axis.title.x = element_text(size = 10,face = "bold"), 
        axis.title.y = element_text(size = 10,face="bold"), 
        legend.text = element_text(size = 8), legend.title = element_blank(), 
        legend.background = element_rect(fill = "transparent"),
        legend.key = element_rect(fill = NA, colour = NA),
        legend.position = "top") +
  guides(col = guide_legend(nrow = 2, override.aes = list(size=1.5)), title = NULL, title.position = NULL)


pdf("report/figs/intactForestSeasonality_Kalimantan_SC1_2000to2004_NIR_compareAggregations.pdf", width = 7, height = 5, pointsize = 10)
myPlot
dev.off()

