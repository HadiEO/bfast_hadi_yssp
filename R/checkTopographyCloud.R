# temp <- read_csv(str_c(path, "/amazon/extrSRTM90vsL5n7CountsKal99to12Random1000.csv"))
# temp <- read_csv(str_c(path, "/amazon/extrSRTM90vsL5n7CountsKal99to12Manual275.csv"))
temp_undisturbed <- read_csv(str_c(path, "/amazon/extrSRTM90vsL5n7CountsKal99to12Undisturbed1000.csv"))
temp_oilpalm <- read_csv(str_c(path, "/amazon/extrSRTM90vsL5n7CountsKal99to12Oilpalm1000.csv"))
temp_concession <- read_csv(str_c(path, "/amazon/extrSRTM90vsL5n7CountsKal99to12Concession1000.csv"))


# Heh the export is a mess i.e. a string of bracketed numeric values (vector)
# So convert to vector of numeric values
# x <- temp$SRTM

tidyGEEDictExport <- function(x) {
  a <- str_sub(x, 2, nchar(x)-1)
  b <- str_split(a, ",")[[1]]
  c <- as.numeric(b)
  return(c)
}

getTidiedGEEDictExport <- function(temp) {
  tempTidied <- data.frame(SRTM = tidyGEEDictExport(temp$SRTM),
                           clearCount = tidyGEEDictExport(temp$clearCount),
                           cloudCount = tidyGEEDictExport(temp$cloudCount),
                           shadowCount = tidyGEEDictExport(temp$shadowCount))
  # Replace fill (9999) with NA
  tempTidied[tempTidied == 9999] <- NA    # This works for data.frame object
  tempTidied <- as_tibble(tempTidied)
  return(tempTidied)
}


# For several land use
tempTidied_undisturbed <- getTidiedGEEDictExport(temp_undisturbed)
tempTidied_undisturbed$landuse <- "Undisturbed"

tempTidied_oilpalm <- getTidiedGEEDictExport(temp_oilpalm)
tempTidied_oilpalm$landuse <- "Oil palm"

tempTidied_concession <- getTidiedGEEDictExport(temp_concession)
tempTidied_concession$landuse <- "Concession"


# Merge
tempTidied <- bind_rows(tempTidied_undisturbed,
          tempTidied_oilpalm,
          tempTidied_concession)

# Plot
tempTidiedForPlot <- tempTidied %>% 
  gather(-SRTM, -landuse, key = "var", value = "value")

ggplot(tempTidiedForPlot, aes(x = SRTM, y = value, col = landuse, shape = landuse)) +
  geom_point() + 
  scale_shape_manual(values = c(1, 1, 1)) +
  stat_smooth() +
  facet_wrap(~ var, scales = "free", ncol = 1) +
  theme_bw()


