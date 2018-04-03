# Read rds made in analyzeExtrTimeSeries_newLandsatCollection.R
extr.ts <- read_rds(str_c(path, "/Amazon/extrL5n7RawBandsAmazonOneTile2000to2004Random1000.rds"))

# Tidy up 
extr.ts.Melt <- reshape2::melt(extr.ts, id.vars = c("ID", "date", "sensor"), 
                               measure.vars = c("blue", "green", "red", "nir", "swir1", "swir2", "nbr", "ndmi", "ndvi"))

df <- as_tibble(extr.ts.Melt)
df <- df %>% filter(variable == "ndmi")
df <- df %>% mutate(
  doy = yday(date),
  dom = mday(date),
  month = month(date),
  year = year(date)
)

# Get median across space (pixel ID) and years
# Aggregate spatially AND temporally (simultaneously)
df_medianByDoy <- df %>% 
  group_by(doy) %>% summarise(median_value = median(value, na.rm = TRUE))  # mean or median? ***

df_medianByDoyYear <- df %>% 
  group_by(doy, year) %>% summarise(median_value = median(value, na.rm = TRUE))  # mean or median? ***
df_medianByDoyYear <- df_medianByDoyYear %>% 
  mutate(year = factor(year, level = c("2000", "2001", "2002", "2003", "2004")))

# plot(df_medianByDoy$doy, df_medianByDoy$median_value, ylim = c(-0.2, 0.6)) # NDMI
# plot(df_medianByDoy$doy, df_medianByDoy$median_value, ylim = c(0.4, 1)) # NDVI
# plot(df_medianByDoy$doy, df_medianByDoy$median_value, ylim = c(0, 0.5)) # NIR
# plot(df_medianByDoy$doy, df_medianByDoy$median_value, ylim = c(0, 0.1)) # red
# plot(df_medianByDoy$doy, df_medianByDoy$median_value, ylim = c(0, 0.3)) # SWIR1
plot(df_medianByDoy$doy, df_medianByDoy$median_value, ylim = c(0, 0.6)) # NDMI narrower ylim

# ggplot
# Make dummy date for plotting purpose
df_medianByDoy <- df_medianByDoy %>% 
  mutate(dummy_date = as.Date(doy, origin = "2014-12-31"))

df_medianByDoyYear <- df_medianByDoyYear %>% 
  mutate(dummy_date = as.Date(doy, origin = "2014-12-31"))

# To plot June to May, add 365 days to Jan-May
df_medianByDoyYear[which(df_medianByDoyYear$doy <= 151), "dummy_date"] <-
  df_medianByDoyYear[which(df_medianByDoyYear$doy <= 151), "dummy_date"] + 365

df_medianByDoy[which(df_medianByDoy$doy <= 151), "dummy_date"] <-
  df_medianByDoy[which(df_medianByDoy$doy <= 151), "dummy_date"] + 365


# Plot
plot_df_medianByDoy_showYear <- ggplot(df_medianByDoyYear, aes(x = dummy_date, y = median_value, col = year, shape = year)) +
  geom_point(na.rm = TRUE, size = 1) +
  theme_bw() + labs(y = "NDVI", x = "") + scale_y_continuous(limits = c(0.4, 1)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b", date_minor_breaks = "1 month",
               limits = c(as.Date("2015-05-21"), as.Date("2016-06-10")) , 
               # c(as.Date("2015-06-01"), as.Date("2016-05-31")) or as.Date("2014-12-21"), as.Date("2016-01-10")
               expand = c(0,0)) 

pdf("report/figs/intactForestSeasonality_Amazon_2000to2004_showYear.pdf", width = 7, height = 3, pointsize = 10)
plot_df_medianByDoy_showYear
dev.off()


plot_df_medianByDoy <- ggplot(df_medianByDoy, aes(x = dummy_date, y = median_value)) +
  geom_point(na.rm = TRUE, size = 1) +
  theme_bw() + labs(y = "NDMI", x = "") + scale_y_continuous(limits = c(0, 0.6)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b", date_minor_breaks = "1 month",
               limits = c(as.Date("2015-05-21"), as.Date("2016-06-10")) , 
               # c(as.Date("2015-06-01"), as.Date("2016-05-31")) or as.Date("2014-12-21"), as.Date("2016-01-10")
               expand = c(0,0)) 

pdf("report/figs/intactForestSeasonality_Amazon_2000to2004_NDMI.pdf", width = 7, height = 3, pointsize = 10)
plot_df_medianByDoy
dev.off()

# ************************************************************
# Plot to compare different aggregation level
# ************************************************************
df <- as_tibble(extr.ts.Melt)
df <- df %>% filter(variable == "nir")

# (a) Visualize for each doy, all obs of different pixels (ID) and years
# data point is one original obs (not aggregated by any means)
df.medianBy.Year.Doy.Id <- df %>% 
  mutate(year = factor(year, c(2000, 2001, 2002, 2003, 2004))) %>% 
  group_by(ID, year, doy) %>%  
  summarise(median_value = median(value, na.rm = TRUE)) %>% 
  ungroup()

plot(df.medianBy.Year.Doy.Id$doy, df.medianBy.Year.Doy.Id$median_value, 
     ylim = c(0, 0.5), col = "#00000060", pch = 19) # NIR

# (b1) then for each doy, all obs of different pixels (ID) i.e. aggregate across years
# data point is median of all years, for each pixel (ID), for each doy
df.medianBy.Year.Doy.Id.thenAggrYear <- df.medianBy.Year.Doy.Id %>% 
  group_by(ID, doy) %>% 
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
df.medianBy.Year.Doy.Id[which(df.medianBy.Year.Doy.Id$doy <= 151), "dummy_date"] <-
  df.medianBy.Year.Doy.Id[which(df.medianBy.Year.Doy.Id$doy <= 151), "dummy_date"] + 365

df.medianBy.Year.Doy.Id.thenAggrYear[which(df.medianBy.Year.Doy.Id.thenAggrYear$doy <= 151), "dummy_date"] <-
  df.medianBy.Year.Doy.Id.thenAggrYear[which(df.medianBy.Year.Doy.Id.thenAggrYear$doy <= 151), "dummy_date"] + 365

df.medianBy.Year.Doy.Id.thenAggrId[which(df.medianBy.Year.Doy.Id.thenAggrId$doy <= 151), "dummy_date"] <-
  df.medianBy.Year.Doy.Id.thenAggrId[which(df.medianBy.Year.Doy.Id.thenAggrId$doy <= 151), "dummy_date"] + 365

df.medianBy.Year.Doy.Id.thenAggrYear.thenAggrId[which(df.medianBy.Year.Doy.Id.thenAggrYear.thenAggrId$doy <= 151), "dummy_date"] <-
  df.medianBy.Year.Doy.Id.thenAggrYear.thenAggrId[which(df.medianBy.Year.Doy.Id.thenAggrYear.thenAggrId$doy <= 151), "dummy_date"] + 365

df.medianBy.Year.Doy.Id.thenAggrId.thenAggrYear[which(df.medianBy.Year.Doy.Id.thenAggrId.thenAggrYear$doy <= 151), "dummy_date"] <-
  df.medianBy.Year.Doy.Id.thenAggrId.thenAggrYear[which(df.medianBy.Year.Doy.Id.thenAggrId.thenAggrYear$doy <= 151), "dummy_date"] + 365



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
               limits = c(as.Date("2015-05-21"), as.Date("2016-06-10")) ,
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


pdf("report/figs/intactForestSeasonality_Amazon_2000to2004_NIR_compareAggregations.pdf", width = 7, height = 5, pointsize = 10)
myPlot
dev.off()

# Without (b) to show individual obs data points
acd <- filter(abcd, aggregation != "For each DOY, median across years")

myPlot <- ggplot(acd, aes(dummy_date, median_value, col = aggregation, shape = aggregation)) +
  geom_point(na.rm = TRUE, size = 1) +
  theme_bw() + labs(y = "NIR", x = "") + scale_y_continuous(limits = c(0, 0.5)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b", date_minor_breaks = "1 month",
               limits = c(as.Date("2015-05-21"), as.Date("2016-06-10")) ,
               expand = c(0,0)) +
  # scale_colour_manual(values = c("dark orange", "chartreuse4", "dark blue")) + 
  scale_colour_manual(values = c("#FFA50060", "red", "dark blue")) + 
  scale_shape_manual(values = c(21,21, 21)) +
  scale_fill_manual(values = c(NA, NA, NA)) +
  theme(axis.line = element_line(colour = "black"), panel.background = element_blank(), 
        axis.text = element_text(size = 8), axis.title.x = element_text(size = 10,face = "bold"), 
        axis.title.y = element_text(size = 10,face="bold"), 
        legend.text = element_text(size = 8), legend.title = element_blank(), 
        legend.background = element_rect(fill = "transparent"),
        legend.key = element_rect(fill = NA, colour = NA),
        legend.position = "top") +
  guides(col = guide_legend(nrow = 2, override.aes = list(size=1.5)), title = NULL, title.position = NULL)

pdf("report/figs/intactForestSeasonality_Amazon_2000to2004_NIR_compareAggregations_acd.pdf", width = 7, height = 5, pointsize = 10)
myPlot
dev.off()