ref.lastDate <- list(DG1 = as.Date("2015-08-15"), DG2 = as.Date("2015-08-08"),
                     sq9 = as.Date("2014-05-13"), sq10 = as.Date("2015-08-17"),
                     sq11 = as.Date("2014-02-04"), sq13 = as.Date("2014-05-13"),
                     SC1 = as.Date("2014-02-04"))



# Merge extracted time series  --------------------------------------------
# First, dg2
dg2 <- read_rds(str_c(path, "/extracted_time_series/FINALLY_USED/before_merging/extrNDMIsub_DG2.rds"))
dg2.justNatReveg <- read_rds(str_c(path, "/extracted_time_series/FINALLY_USED/before_merging/extrNDMIsub_DG2_justNatReveg.rds"))

# Remove columns in dg2 that are the same as dg2.justNatReveg
dg2.names <- names(dg2)
dg2.justNatReveg.names <- names(dg2.justNatReveg)
toRemove <- dg2.names[which(dg2.names %in% dg2.justNatReveg.names)]
dg2.withoutNatReveg <- dg2[, -which(dg2.names %in% dg2.justNatReveg.names)]

# Merge dg2.withoutNatReveg and dg2.justNatReveg
dg2.merged <- cbind(dg2.withoutNatReveg, dg2.justNatReveg)
dimnames(dg2.merged)[[1]] <- dimnames(dg2)[[1]]
write_rds(dg2.merged, str_c(path, "/extracted_time_series/FINALLY_USED/extrNDMIsub_DG2.rds"))

# Second, sq13
sq13 <- read_rds(str_c(path, "/extracted_time_series/FINALLY_USED/before_merging/extrNDMIsub_sq13.rds"))
sq13.addIntact <- read_rds(str_c(path, "/extracted_time_series/FINALLY_USED/before_merging/extrNDMIsub_sq13_addIntact.rds"))
sq13.merged <- cbind(sq13, sq13.addIntact)
dimnames(sq13.merged)[[1]] <- dimnames(sq13)[[1]]
write_rds(sq13.merged, str_c(path, "/extracted_time_series/FINALLY_USED/extrNDMIsub_sq13.rds"))

a <- getSceneinfo(dimnames(sq13.merged)[[1]])
nrow(a)
length(unique(a$date))



# ***********************************************************************
# 20180207
# SC1 (added more intact forest)
# ***********************************************************************
# Need to extract ts
NDMI.SC1.unique <- read_rds(str_c(path, "/raster_time_stack/ndmi_rds/ndmi_SC_1_unique.rds"))
pixels.SC1.addIntact <- readOGR(dsn = paste(path, "/vector_data/FINALLY_USED/before_merging", sep = ""), 
                                layer = "meshSelect_SC_1_addIntactForest")
samples <- coordinates(pixels.SC1.addIntact)

NDMI.unique.sub <- subsetRasterTS(NDMI.SC1.unique, maxDate = ref.lastDate$SC1)

extrNDMI <- zooExtract(NDMI.unique.sub, samples, method = "simple")
colnames(extrNDMI) <- as.character(pixels.SC1.addIntact$Id)
write_rds(extrNDMI, str_c(path, "/extracted_time_series/FINALLY_USED/before_merging/extrNDMIsub_SC1_addIntact.rds"))

# Merge with former SC1
SC1 <- read_rds(str_c(path, "/extracted_time_series/FINALLY_USED/before_merging/extrNDMIsub_SC1.rds"))
SC1.addIntact <- read_rds(str_c(path, "/extracted_time_series/FINALLY_USED/before_merging/extrNDMIsub_SC1_addIntact.rds"))
SC1.merged <- cbind(SC1, SC1.addIntact) # use bind_cols instead to keep dimnames?
dimnames(SC1.merged)[[1]] <- dimnames(SC1)[[1]]
write_rds(SC1.merged, str_c(path, "/extracted_time_series/FINALLY_USED/extrNDMIsub_SC1.rds"))




# ***********************************************************************
# Remove reference pixels with un-verified disturbance ---------------------
# ***********************************************************************
# oldDating was dated not at earliest obs of detected disturbance. 
# Update: re-done so earliest dating in reference
DG1.oldDating <- read_rds(str_c(path, "/from_shiny/old/", "DG1.rds"))
DG2.part1.oldDating <- read_rds(str_c(path, "/from_shiny/old/", "DG2_part1.rds"))
DG2.part2.oldDating <- read_rds(str_c(path, "/from_shiny/old/", "DG2_part2.rds"))
DG2.oldDating <- bind_rows(DG2.part1.oldDating, DG2.part2.oldDating)

# Get Id to remove
DG1.toRemove <- DG1.oldDating %>% filter(Comment == "to_remove") %>% select(Id)
DG2.toRemove <- DG2.oldDating %>% filter(Comment == "to_remove") %>% select(Id)

# Before removal
DG1.beforeRemove <- read_rds(str_c(path, "/extracted_time_series/FINALLY_USED/before_remove_unverified_change/extrNDMIsub_DG1.rds"))
DG2.beforeRemove <- read_rds(str_c(path, "/extracted_time_series/FINALLY_USED/before_remove_unverified_change/extrNDMIsub_DG2.rds"))

# After removal
colnames(DG1.beforeRemove)
DG1.afterRemove <- DG1.beforeRemove[, which(!colnames(DG1.beforeRemove) %in% DG1.toRemove$Id)]
colnames(DG1.afterRemove)
  
DG2.afterRemove <- DG2.beforeRemove[, which(!colnames(DG2.beforeRemove) %in% DG2.toRemove$Id)]

# Write to disk
write_rds(DG1.afterRemove, str_c(path, "/extracted_time_series/FINALLY_USED/extrNDMIsub_DG1.rds"))
write_rds(DG2.afterRemove, str_c(path, "/extracted_time_series/FINALLY_USED/extrNDMIsub_DG2.rds"))

  
# ****************************************************************************
# Extend end of ts for SC1 ------------------------------------------------
# Cause hard to visually confirm change without a little bit more new obs (after the latest VHSR date)
# ****************************************************************************
NDMI.SC1.unique <- read_rds(str_c(path, "/raster_time_stack/ndmi_rds/ndmi_SC_1_unique.rds"))
pixels.SC1 <- readOGR(dsn = paste(path, "/vector_data/FINALLY_USED", sep = ""), 
                                layer = "meshSelect_SC_1")

samples <- coordinates(pixels.SC1)

# Test add 30 days, 90 days, 180 days, 365 days
# 365 days it is! based on visual interpretation in app
NDMI.unique.sub <- subsetRasterTS(NDMI.SC1.unique, maxDate = ref.lastDate$SC1 + 365)

extrNDMI <- zooExtract(NDMI.unique.sub, samples, method = "simple")
colnames(extrNDMI) <- as.character(pixels.SC1$Id)
write_rds(extrNDMI, str_c(path, "/extracted_time_series/FINALLY_USED/extrNDMIsub_SC1_add365Days.rds"))

# for(i in 1:ncol(extrNDMI)) {
#   x <- extrNDMI[, i]
#   plot(index(x), x, ylim = c(-0.2, 0.6))
#   locator(1)
# }

# ***********************************************************************
# Fix mistakes during app interpretation ----------------------------------
# ***********************************************************************
DG1.dating <- read_rds(str_c(path, "/from_shiny/before_remove_unverified/", "DG1_ok.rds"))
DG2.dating <- read_csv2(str_c(path, "/from_shiny/before_remove_unverified/", "DG2_ok.csv"),
                        col_types = "ccc")
SC1.dating <- read_rds(str_c(path, "/from_shiny/before_remove_unverified/", "SC1_ok.rds"))
SQ9.dating <- read_csv2(str_c(path, "/from_shiny/before_remove_unverified/", "SQ9_ok.csv"),
                        col_types = "ccc")
SQ11.dating <- read_rds(str_c(path, "/from_shiny/before_remove_unverified/", "SQ11_ok.rds"))
SQ13.dating <- read_rds(str_c(path, "/from_shiny/before_remove_unverified/", "SQ13_ok.rds"))

# Make date "Date" object
DG1.dating <- DG1.dating %>% mutate(Date = as.Date(Date))
DG2.dating <- DG2.dating %>% mutate(Date = as.Date(Date, format = "%d.%m.%Y"))
SC1.dating <- SC1.dating %>% mutate(Date = as.Date(Date))
SQ9.dating <- SQ9.dating %>% mutate(Date = as.Date(Date, format = "%d.%m.%Y"))
SQ11.dating <- SQ11.dating %>% mutate(Date = as.Date(Date))
SQ13.dating <- SQ13.dating %>% mutate(Date = as.Date(Date))

# Convert to csv, write to disk, and fix it in excel, easier
write.csv2(DG1.dating, str_c(path, "/from_shiny/", "DG1_refChangeDate.csv"))
write.csv2(DG2.dating, str_c(path, "/from_shiny/", "DG2_refChangeDate.csv"))
write.csv2(SC1.dating, str_c(path, "/from_shiny/", "SC1_refChangeDate.csv"))
write.csv2(SQ9.dating, str_c(path, "/from_shiny/", "SQ9_refChangeDate.csv"))
write.csv2(SQ11.dating, str_c(path, "/from_shiny/", "SQ11_refChangeDate.csv"))
write.csv2(SQ13.dating, str_c(path, "/from_shiny/", "SQ13_refChangeDate.csv"))

# Ok, mistakes have been fixed by opening in excel. Now, read them and combine into .rds.
DG1.dating <- read_csv2(str_c(path, "/from_shiny/", "DG1_refChangeDate.csv"), col_types = "cccc")  #
DG2.dating <- read_csv2(str_c(path, "/from_shiny/", "DG2_refChangeDate.csv"), col_types = "cccc")  #
SC1.dating <- read_csv2(str_c(path, "/from_shiny/", "SC1_refChangeDate.csv"), col_types = "cccc")   #
SQ9.dating <- read_csv2(str_c(path, "/from_shiny/", "SQ9_refChangeDate.csv"), col_types = "ccDc")
SQ11.dating <- read_csv2(str_c(path, "/from_shiny/", "SQ11_refChangeDate.csv"), col_types = "ccDc")
SQ13.dating <- read_csv2(str_c(path, "/from_shiny/", "SQ13_refChangeDate.csv"), col_types = "cccc") #

# Somehow read as number for these scenes (seem like because we edited them in excel)
SC1.dating <- SC1.dating %>% mutate(Date = as.Date(Date, format = "%d.%m.%Y"))
SQ13.dating <- SQ13.dating %>% mutate(Date = as.Date(Date, format = "%d.%m.%Y"))
DG2.dating <- DG2.dating %>% mutate(Date = as.Date(Date, format = "%d.%m.%Y"))
DG1.dating <- DG1.dating %>% mutate(Date = as.Date(Date, format = "%d.%m.%Y"))

# Combine into one table
DG1.dating <- DG1.dating %>% mutate(Scene = "DG1")
DG2.dating <- DG2.dating %>% mutate(Scene = "DG2")
SC1.dating <- SC1.dating %>% mutate(Scene = "SC1")
SQ9.dating <- SQ9.dating %>% mutate(Scene = "SQ9")
SQ11.dating <- SQ11.dating %>% mutate(Scene = "SQ11")
SQ13.dating <- SQ13.dating %>% mutate(Scene = "SQ13")

all.dating <- bind_rows(DG1.dating,
                        DG2.dating,
                        SC1.dating,
                        SQ9.dating,
                        SQ11.dating,
                        SQ13.dating)

# 443 samples

# Remove "to_remove"
table(all.dating$Comment)  # 8 "to_remove" samples
to_remove <- all.dating %>% filter(Comment == "to_remove") 
all.dating <- all.dating %>% filter(!Id %in% to_remove$Id)    # 435 samples
table(all.dating$Comment)

# Let's add column of disturbed (value = 1) or not disturbed (value = 0)
# NA means disturbed (large clearing)
all.dating <- all.dating %>% 
  mutate(Disturbed = ifelse(is.na(Comment), 1,
                            ifelse(Comment == "undisturbed_throughout", 0, 1)))
table(all.dating$Disturbed)
# 0   1 
# 209 226

# Save
write_rds(all.dating, str_c(path, "/from_shiny/", "all_refChangeDate_final.rds"))
write.csv2(all.dating, str_c(path, "/from_shiny/", "all_refChangeDate_final.csv"))

# ***********************************************************************
# Keep extracted time series of the final Id
# ***********************************************************************
DG1.beforeRemove <- read_rds(str_c(path, "/extracted_time_series/FINALLY_USED/before_remove_unverified_change/",
                                   "extrNDMIsub_DG1.rds"))
DG2.beforeRemove <- read_rds(str_c(path, "/extracted_time_series/FINALLY_USED/before_remove_unverified_change/",
                                   "extrNDMIsub_DG2.rds"))
SC1.beforeRemove <- read_rds(str_c(path, "/extracted_time_series/FINALLY_USED/before_remove_unverified_change/",
                                   "extrNDMIsub_SC1.rds"))
SQ9.beforeRemove <- read_rds(str_c(path, "/extracted_time_series/FINALLY_USED/before_remove_unverified_change/",
                                   "extrNDMIsub_SQ9.rds"))
SQ11.beforeRemove <- read_rds(str_c(path, "/extracted_time_series/FINALLY_USED/before_remove_unverified_change/",
                                   "extrNDMIsub_SQ11.rds"))
SQ13.beforeRemove <- read_rds(str_c(path, "/extracted_time_series/FINALLY_USED/before_remove_unverified_change/",
                                   "extrNDMIsub_SQ13.rds"))

dim(DG1.beforeRemove)[2]
dim(DG2.beforeRemove)[2]
dim(SC1.beforeRemove)[2]
dim(SQ9.beforeRemove)[2]
dim(SQ11.beforeRemove)[2]
dim(SQ13.beforeRemove)[2]
# 445 total

all.dating <- read_rds(str_c(path, "/from_shiny/", "all_refChangeDate_final.rds")) # 435

DG1.afterRemove <- DG1.beforeRemove[, which(colnames(DG1.beforeRemove) %in% filter(all.dating, Scene == "DG1")$Id)]
DG2.afterRemove <- DG2.beforeRemove[, which(colnames(DG2.beforeRemove) %in% filter(all.dating, Scene == "DG2")$Id)]
SC1.afterRemove <- SC1.beforeRemove[, which(colnames(SC1.beforeRemove) %in% filter(all.dating, Scene == "SC1")$Id)]
SQ9.afterRemove <- SQ9.beforeRemove[, which(colnames(SQ9.beforeRemove) %in% filter(all.dating, Scene == "SQ9")$Id)]
SQ11.afterRemove <- SQ11.beforeRemove[, which(colnames(SQ11.beforeRemove) %in% filter(all.dating, Scene == "SQ11")$Id)]
SQ13.afterRemove <- SQ13.beforeRemove[, which(colnames(SQ13.beforeRemove) %in% filter(all.dating, Scene == "SQ13")$Id)]

dim(DG1.afterRemove)[2]
dim(DG2.afterRemove)[2]
dim(SC1.afterRemove)[2]
dim(SQ9.afterRemove)[2]
dim(SQ11.afterRemove)[2]
dim(SQ13.afterRemove)[2]
# 435 total 

write_rds(DG1.afterRemove, str_c(path, "/extracted_time_series/FINALLY_USED/extrNDMIsub_DG1.rds"))
write_rds(DG2.afterRemove, str_c(path, "/extracted_time_series/FINALLY_USED/extrNDMIsub_DG2.rds"))
write_rds(SC1.afterRemove, str_c(path, "/extracted_time_series/FINALLY_USED/extrNDMIsub_SC1.rds"))
write_rds(SQ9.afterRemove, str_c(path, "/extracted_time_series/FINALLY_USED/extrNDMIsub_SQ9.rds"))
write_rds(SQ11.afterRemove, str_c(path, "/extracted_time_series/FINALLY_USED/extrNDMIsub_SQ11.rds"))
write_rds(SQ13.afterRemove, str_c(path, "/extracted_time_series/FINALLY_USED/extrNDMIsub_SQ13.rds"))


# ***********************************************************************
# Keep reference pixels (shp) of the final Id
# ***********************************************************************
shp.folder <- paste(path, "/vector_data/FINALLY_USED/before_remove_unverified_change", sep = "")
shp.DG1.beforeRemove <- readOGR(dsn = shp.folder, layer = "meshSelect_prevDG1")
shp.DG2.beforeRemove <- readOGR(dsn = shp.folder, layer = "meshSelect_prevDG2")
shp.SC1.beforeRemove <- readOGR(dsn = shp.folder, layer = "meshSelect_SC_1")
shp.SQ9.beforeRemove <- readOGR(dsn = shp.folder, layer = "meshSelect_sq_9")
# shp.SQ10.beforeRemove <- readOGR(dsn = shp.folder, layer = "meshSelect_sq_10")  # acacia
shp.SQ11.beforeRemove <- readOGR(dsn = shp.folder, layer = "meshSelect_sq_11")
shp.SQ13.beforeRemove <- readOGR(dsn = shp.folder, layer = "meshSelect_sq_13")


shp.DG1.afterRemove <- shp.DG1.beforeRemove[which(as.character(shp.DG1.beforeRemove$Id) %in% colnames(DG1.afterRemove)),]
shp.DG2.afterRemove <- shp.DG2.beforeRemove[which(as.character(shp.DG2.beforeRemove$Id) %in% colnames(DG2.afterRemove)),]
shp.SC1.afterRemove <- shp.SC1.beforeRemove[which(as.character(shp.SC1.beforeRemove$Id) %in% colnames(SC1.afterRemove)),]
shp.SQ9.afterRemove <- shp.SQ9.beforeRemove[which(as.character(shp.SQ9.beforeRemove$Id) %in% colnames(SQ9.afterRemove)),]
shp.SQ11.afterRemove <- shp.SQ11.beforeRemove[which(as.character(shp.SQ11.beforeRemove$Id) %in% colnames(SQ11.afterRemove)),]
shp.SQ13.afterRemove <- shp.SQ13.beforeRemove[which(as.character(shp.SQ13.beforeRemove$Id) %in% colnames(SQ13.afterRemove)),]

# ***********************************************************************
# Add disturbed or not
# ***********************************************************************
shp.DG1.afterRemove$Id <- as.character(shp.DG1.afterRemove$Id) 
shp.DG1.afterRemove <- sp::merge(shp.DG1.afterRemove, filter(all.dating, Scene == "DG1"), by = "Id")

shp.DG2.afterRemove$Id <- as.character(shp.DG2.afterRemove$Id) 
shp.DG2.afterRemove <- sp::merge(shp.DG2.afterRemove, filter(all.dating, Scene == "DG2"), by = "Id")

shp.SC1.afterRemove$Id <- as.character(shp.SC1.afterRemove$Id) 
shp.SC1.afterRemove <- sp::merge(shp.SC1.afterRemove, filter(all.dating, Scene == "SC1"), by = "Id")

shp.SQ9.afterRemove$Id <- as.character(shp.SQ9.afterRemove$Id) 
shp.SQ9.afterRemove <- sp::merge(shp.SQ9.afterRemove, filter(all.dating, Scene == "SQ9"), by = "Id")

shp.SQ11.afterRemove$Id <- as.character(shp.SQ11.afterRemove$Id) 
shp.SQ11.afterRemove <- sp::merge(shp.SQ11.afterRemove, filter(all.dating, Scene == "SQ11"), by = "Id")

shp.SQ13.afterRemove$Id <- as.character(shp.SQ13.afterRemove$Id) 
shp.SQ13.afterRemove <- sp::merge(shp.SQ13.afterRemove, filter(all.dating, Scene == "SQ13"), by = "Id")

# Save shp as shapefile and rds
shp.folder <- paste(path, "/vector_data/FINALLY_USED/before_add_adjusted_reference_date", sep = "")
writeOGR(shp.DG1.afterRemove, shp.folder, layer = "meshSelect_prevDG1", driver = "ESRI Shapefile")
writeOGR(shp.DG2.afterRemove, shp.folder, layer = "meshSelect_prevDG2", driver = "ESRI Shapefile")
writeOGR(shp.SC1.afterRemove, shp.folder, layer = "meshSelect_SC_1", driver = "ESRI Shapefile")
writeOGR(shp.SQ9.afterRemove, shp.folder, layer = "meshSelect_sq_9", driver = "ESRI Shapefile")
writeOGR(shp.SQ11.afterRemove, shp.folder, layer = "meshSelect_sq_11", driver = "ESRI Shapefile")
writeOGR(shp.SQ13.afterRemove, shp.folder, layer = "meshSelect_sq_13", driver = "ESRI Shapefile")


# ***********************************************************************
# UPDATE 2018-03-02: add adjusted reference date to shapefile
# ***********************************************************************
shp.folder <- paste(path, "/vector_data/FINALLY_USED/before_add_adjusted_reference_date", sep = "")
shp.DG1.afterRemove <- readOGR(dsn = shp.folder, layer = "meshSelect_prevDG1")
shp.DG2.afterRemove <- readOGR(dsn = shp.folder, layer = "meshSelect_prevDG2")    
shp.SC1.afterRemove <- readOGR(dsn = shp.folder, layer = "meshSelect_SC_1")
shp.SQ9.afterRemove <- readOGR(dsn = shp.folder, layer = "meshSelect_sq_9")
shp.SQ11.afterRemove <- readOGR(dsn = shp.folder, layer = "meshSelect_sq_11")
shp.SQ13.afterRemove <- readOGR(dsn = shp.folder, layer = "meshSelect_sq_13")

# Adjusted ref date
all.dating.adj <- read_rds(str_c(path, "/from_shiny/", "all_refChangeDate_final_adj.rds")) # 435

# Replace "Date_adj" and "Date" with NA for pixels with Comment = "undisturbed_throughout"
all.dating <- read_rds(str_c(path, "/from_shiny/", "all_refChangeDate_final.rds")) # 435
all.dating.adj.intactNoDate <- left_join(all.dating, all.dating.adj, by = c("Id", "Scene"))
# all.dating.adj.intactNoDate <- all.dating.adj.intactNoDate %>% 
#   mutate(Date = if_else(Comment == "undisturbed_throughout", NA, Date),
#          Date_adj = if_else(Comment == "undisturbed_throughout", NA, Date_adj
# doesn't work!!

all.dating.adj.intactNoDate[which(all.dating.adj.intactNoDate$Comment == "undisturbed_throughout"), "Date"] <- NA
all.dating.adj.intactNoDate[which(all.dating.adj.intactNoDate$Comment == "undisturbed_throughout"), "Date_adj"] <- NA
write_rds(all.dating.adj.intactNoDate, str_c(path, "/from_shiny/", "all_refChangeDate_final_adj_intactNoDate.rds"))



# Attach to shapefile attribute data
shp.DG1.afterRemove$Id <- as.character(shp.DG1.afterRemove$Id) 
DG1.dating.adj <- all.dating.adj.intactNoDate %>% filter(Scene == "DG1") %>% select(Id, Date_adj)
shp.DG1.afterRemove <- sp::merge(shp.DG1.afterRemove, DG1.dating.adj, by = "Id")

shp.DG2.afterRemove$Id <- as.character(shp.DG2.afterRemove$Id) 
DG2.dating.adj <- all.dating.adj.intactNoDate %>% filter(Scene == "DG2") %>% select(Id, Date_adj)
shp.DG2.afterRemove <- sp::merge(shp.DG2.afterRemove, DG2.dating.adj, by = "Id")

shp.SC1.afterRemove$Id <- as.character(shp.SC1.afterRemove$Id) 
SC1.dating.adj <- all.dating.adj.intactNoDate %>% filter(Scene == "SC1") %>% select(Id, Date_adj)
shp.SC1.afterRemove <- sp::merge(shp.SC1.afterRemove, SC1.dating.adj, by = "Id")

shp.SQ9.afterRemove$Id <- as.character(shp.SQ9.afterRemove$Id) 
SQ9.dating.adj <- all.dating.adj.intactNoDate %>% filter(Scene == "SQ9") %>% select(Id, Date_adj)
shp.SQ9.afterRemove <- sp::merge(shp.SQ9.afterRemove, SQ9.dating.adj, by = "Id")

shp.SQ11.afterRemove$Id <- as.character(shp.SQ11.afterRemove$Id) 
SQ11.dating.adj <- all.dating.adj.intactNoDate %>% filter(Scene == "SQ11") %>% select(Id, Date_adj)
shp.SQ11.afterRemove <- sp::merge(shp.SQ11.afterRemove, SQ11.dating.adj, by = "Id")

shp.SQ13.afterRemove$Id <- as.character(shp.SQ13.afterRemove$Id) 
SQ13.dating.adj <- all.dating.adj.intactNoDate %>% filter(Scene == "SQ13") %>% select(Id, Date_adj)
shp.SQ13.afterRemove <- sp::merge(shp.SQ13.afterRemove, SQ13.dating.adj, by = "Id")

# Save shp as shapefile and rds
shp.folder <- paste(path, "/vector_data/FINALLY_USED", sep = "")
writeOGR(shp.DG1.afterRemove, shp.folder, layer = "meshSelect_prevDG1", driver = "ESRI Shapefile")
writeOGR(shp.DG2.afterRemove, shp.folder, layer = "meshSelect_prevDG2", driver = "ESRI Shapefile")
writeOGR(shp.SC1.afterRemove, shp.folder, layer = "meshSelect_SC_1", driver = "ESRI Shapefile")
writeOGR(shp.SQ9.afterRemove, shp.folder, layer = "meshSelect_sq_9", driver = "ESRI Shapefile")
writeOGR(shp.SQ11.afterRemove, shp.folder, layer = "meshSelect_sq_11", driver = "ESRI Shapefile")
writeOGR(shp.SQ13.afterRemove, shp.folder, layer = "meshSelect_sq_13", driver = "ESRI Shapefile")


# ***********************************************************************
# Save final reference date by scene for use in shiny app
# ***********************************************************************
all.dating.adj <- read_rds(str_c(path, "/from_shiny/", "all_refChangeDate_final_adj_intactNoDate.rds"))

writeSplitDF <- function(DF) {
  write_rds(DF, str_c(path, "/from_shiny/", unique(DF$Scene),       # Scene is the group_by var
                      "_refChangeDate_final_adj_intactNoDate.rds"))
  return(DF)
}

all.dating.adj %>% group_by(Scene) %>% 
  do(writeSplitDF(.))


# ***********************************************************************
# Save extrNDMI which bfm.date.confirmed < ref.date
# ***********************************************************************
# all.bfmFlag <- read_rds(str_c(path, "/accuracy_results/accuracy_run11_start2000_all_df.rds"))   # which experiment run? ***
# toReviseRefDate <- all.bfmFlag %>% filter(bfm.date.confirmed < ref.date, Comment != "unidsturbed_throughout") 

# Revise for cons = 3 first. Then do for cons = 2 (run11, need to rerun), find the Id that 
# has not been revised.
all.bfmFlag <- read_rds(str_c(path, "/accuracy_results/accuracy_run10_start2000_all_df.rds"))   # which experiment run? ***

toReviseRefDate <- all.bfmFlag %>% filter(bfm.date.confirmed < ref.date.adj) 

# **********************************************************
# Check 
toReviseRefDate %>% filter(Scene == "SC1", Id == "112") %>% 
  select(Scene, Id, ref.date.adj, bfm.date.confirmed)
# ref.date.adj is ok, bfm.date.confirmed = 2013-07-22 is strange it's not obs date (= 2013-08-18)! 
bfm.res <- read_rds(str_c(path, "/bfm_results/bfm_run10_start2000_SC1.rds"))   # which experiment run? ***
bfm.res$`112`
as.Date(203, origin = "2013-01-01")
bfm.res$`112`$breakpoint
dec2date(2013.553)
dec2date(bfm.res$`112`$mefp$dataDf$time)

# If 16 days window
all.bfmFlag %>% mutate(lag = bfm.date.confirmed - ref.date.adj)  %>%  
  filter(lag < -30) 

# Check in bfastmonitor_mod() and monitor_mod()
data <- extrNDMIsub.SC1[,'112']
class(data)
index(data)
length(unique(index(data))) == length(index(data))


data.bts <- bfastts(data, dates = index(data), type = "irregular")
data[which(!index(data) %in% dec2date(index(data.bts)))]
# The issue: bfastts introduces duplicate dates and missing dates!!!!!!!!!
# Try a quick solution now:
# "snap" breakpoint (confirmed and first flagged) dates in bfm results
# to closest obs date in time series.


# Decision: should revise the cases where bfm date < ref date
# for cons=3 (run10) or cons=2 (run11) ?
bfmRes.run10 <- read_rds(str_c(path, "/accuracy_results/accuracy_run10_start2000_all_df.rds"))   # which experiment run? ***
toReviseRefDate.run10 <- bfmRes.run10 %>% filter(bfm.date.confirmed < ref.date.adj) 

bfmRes.run11 <- read_rds(str_c(path, "/accuracy_results/accuracy_run11_start2000_all_df.rds"))   # which experiment run? ***
toReviseRefDate.run11 <- bfmRes.run11 %>% filter(bfm.date.confirmed < ref.date.adj) 

toReviseRefDate.run11 %>% 
  filter(Scene == "SC1", 
         !Id %in% filter(toReviseRefDate.run10, Scene == "SC1")$Id) %>% 
  select(Scene, Id)

# Ok, need to revise the run11 (cons=2), there are quite some cases where bfm date is not really before apparent change date


# **********************************************************

# The extrNDMI
(path)
path2 <- "/extracted_time_series/FINALLY_USED/"
extrNDMIsub.DG1 <- read_rds(str_c(path, path2, "extrNDMIsub_DG1.rds"))      
extrNDMIsub.DG2 <- read_rds(str_c(path, path2, "extrNDMIsub_DG2.rds"))
extrNDMIsub.SC1 <- read_rds(str_c(path, path2, "extrNDMIsub_SC1.rds"))
extrNDMIsub.sq9 <- read_rds(str_c(path, path2, "extrNDMIsub_sq9.rds"))
# extrNDMIsub.sq10 <- read_rds(str_c(path, path2, "extrNDMIsub_sq10.rds"))    # acacia
extrNDMIsub.sq11 <- read_rds(str_c(path, path2, "extrNDMIsub_sq11.rds"))
extrNDMIsub.sq13 <- read_rds(str_c(path, path2, "extrNDMIsub_sq13.rds"))


# Keep columns based on Id and Scene in toReviseRefDate
extrNDMIsub.DG1.toRevise <- extrNDMIsub.DG1[, which(colnames(extrNDMIsub.DG1) %in% filter(toReviseRefDate.run11, Scene == "DG1")$Id)]
extrNDMIsub.DG2.toRevise <- extrNDMIsub.DG2[, which(colnames(extrNDMIsub.DG2) %in% filter(toReviseRefDate.run11, Scene == "DG2")$Id)]
extrNDMIsub.SC1.toRevise <- extrNDMIsub.SC1[, which(colnames(extrNDMIsub.SC1) %in% filter(toReviseRefDate.run11, Scene == "SC1")$Id)]
extrNDMIsub.sq9.toRevise <- extrNDMIsub.sq9[, which(colnames(extrNDMIsub.sq9) %in% filter(toReviseRefDate.run11, Scene == "SQ9")$Id)]
extrNDMIsub.sq11.toRevise <- extrNDMIsub.sq11[, which(colnames(extrNDMIsub.sq11) %in% filter(toReviseRefDate.run11, Scene == "SQ11")$Id)]
extrNDMIsub.sq13.toRevise <- extrNDMIsub.sq13[, which(colnames(extrNDMIsub.sq13) %in% filter(toReviseRefDate.run11, Scene == "SQ13")$Id)]
# DG1 12 samples, DG2 7, SC1 36, SQ9 9, SQ11 17, SQ13 15


write_rds(extrNDMIsub.DG1.toRevise, str_c(path, "/extracted_time_series/FINALLY_USED/toReviseRefDate/extrNDMIsub_DG1.rds"))
write_rds(extrNDMIsub.DG2.toRevise, str_c(path, "/extracted_time_series/FINALLY_USED/toReviseRefDate/extrNDMIsub_DG2.rds"))
write_rds(extrNDMIsub.SC1.toRevise, str_c(path, "/extracted_time_series/FINALLY_USED/toReviseRefDate/extrNDMIsub_SC1.rds"))
write_rds(extrNDMIsub.sq9.toRevise, str_c(path, "/extracted_time_series/FINALLY_USED/toReviseRefDate/extrNDMIsub_SQ9.rds"))
write_rds(extrNDMIsub.sq11.toRevise, str_c(path, "/extracted_time_series/FINALLY_USED/toReviseRefDate/extrNDMIsub_SQ11.rds"))
write_rds(extrNDMIsub.sq13.toRevise, str_c(path, "/extracted_time_series/FINALLY_USED/toReviseRefDate/extrNDMIsub_SQ13.rds"))


# ***********************************************************************
# Update the reference date accordingly with the revised ref date
# ***********************************************************************

all.dating.adj.beforeRevise <- read_rds(str_c(path, "/from_shiny/before_revise_date/", "all_refChangeDate_final_adj_intactNoDate.rds"))

# Samples to revise date
DG1.dating.revised <- read_rds(str_c(path, "/from_shiny/revised/", "DG1_revised.rds"))
DG2.dating.revised <- read_rds(str_c(path, "/from_shiny/revised/", "DG2_revised.rds"))
SC1.dating.revised <- read_rds(str_c(path, "/from_shiny/revised/", "SC1_revised.rds"))
SQ9.dating.revised <- read_rds(str_c(path, "/from_shiny/revised/", "SQ9_revised.rds"))
SQ11.dating.revised <- read_rds(str_c(path, "/from_shiny/revised/", "SQ11_revised.rds"))
SQ13.dating.revised <- read_rds(str_c(path, "/from_shiny/revised/", "SQ13_revised.rds"))

# Make date "Date" object
DG1.dating.revised <- DG1.dating.revised %>% mutate(Date = as.Date(Date))
DG2.dating.revised <- DG2.dating.revised %>% mutate(Date = as.Date(Date))
SC1.dating.revised <- SC1.dating.revised %>% mutate(Date = as.Date(Date))
SQ9.dating.revised <- SQ9.dating.revised %>% mutate(Date = as.Date(Date))
SQ11.dating.revised <- SQ11.dating.revised %>% mutate(Date = as.Date(Date))
SQ13.dating.revised <- SQ13.dating.revised %>% mutate(Date = as.Date(Date))


# Update the ref date of the revised samples
beforeRevise <- all.dating.adj.beforeRevise            # just to shorten the object name temporarily

# DG1
beforeRevise[which(beforeRevise$Scene == "DG1" & 
                     match(beforeRevise$Id, DG1.dating.revised$Id)), 
             "Date"] <- DG1.dating.revised$Date

# DG2
beforeRevise[which(beforeRevise$Scene == "DG2" & 
                     match(beforeRevise$Id, DG2.dating.revised$Id)), 
             "Date"] <- DG2.dating.revised$Date

# SC1
beforeRevise[which(beforeRevise$Scene == "SC1" & 
                     match(beforeRevise$Id, SC1.dating.revised$Id)), 
             "Date"] <- SC1.dating.revised$Date

# SQ9
beforeRevise[which(beforeRevise$Scene == "SQ9" & 
                     match(beforeRevise$Id, SQ9.dating.revised$Id)), 
             "Date"] <- SQ9.dating.revised$Date

# SQ11
beforeRevise[which(beforeRevise$Scene == "SQ11" & 
                     match(beforeRevise$Id, SQ11.dating.revised$Id)), 
             "Date"] <- SQ11.dating.revised$Date

# SQ13
beforeRevise[which(beforeRevise$Scene == "SQ13" & 
                     match(beforeRevise$Id, SQ13.dating.revised$Id)), 
             "Date"] <- SQ13.dating.revised$Date


# ***********************************************************************
# Re-calculate adjusted ref date cause some ref date revised
# ***********************************************************************
(path)
path2 <- "/extracted_time_series/FINALLY_USED/"
extrNDMIsub.DG1 <- read_rds(str_c(path, path2, "extrNDMIsub_DG1.rds"))      
extrNDMIsub.DG2 <- read_rds(str_c(path, path2, "extrNDMIsub_DG2.rds"))
extrNDMIsub.SC1 <- read_rds(str_c(path, path2, "extrNDMIsub_SC1.rds"))
extrNDMIsub.sq9 <- read_rds(str_c(path, path2, "extrNDMIsub_sq9.rds"))
extrNDMIsub.sq11 <- read_rds(str_c(path, path2, "extrNDMIsub_sq11.rds"))
extrNDMIsub.sq13 <- read_rds(str_c(path, path2, "extrNDMIsub_sq13.rds"))



for(i in 1:NROW(beforeRevise)) {
  
  id <- beforeRevise[[i,"Id"]]
  
  # NDMI for which scene?
  if(beforeRevise[[i,"Scene"]] == "DG1") {
    ndmi <- extrNDMIsub.DG1[, id]
  } else if(beforeRevise[[i,"Scene"]] == "DG2") {
    ndmi <- extrNDMIsub.DG2[, id]
  } else if(beforeRevise[[i,"Scene"]] == "SC1") {
    ndmi <- extrNDMIsub.SC1[, id]
  } else if(beforeRevise[[i,"Scene"]] == "SQ9") {
    ndmi <- extrNDMIsub.sq9[, id]
  } else if(beforeRevise[[i,"Scene"]] == "SQ11") {
    ndmi <- extrNDMIsub.sq11[, id]
  } else if(beforeRevise[[i,"Scene"]] == "SQ13") {
    ndmi <- extrNDMIsub.sq13[, id]
  }
  
  ndmi.notNA <- ndmi[!is.na(ndmi)]
  date.notNA <- index(ndmi.notNA)
  
  ref.date <- beforeRevise[[i,"Date"]]
  ref.date.id <- which(date.notNA == ref.date)[1]   # [1] cause just in case there are date duplicates
  ref.date.prev <- date.notNA[ref.date.id - 1]
  
  ref.date.adj <- ref.date - floor(abs( (ref.date - ref.date.prev)/2 ))
  beforeRevise[i,"Date_adj"] <- ref.date.adj
}


write_rds(beforeRevise, str_c(path, "/from_shiny/", "all_refChangeDate_final_adj_intactNoDate.rds"))



# ***********************************************************************
# Split by scene and write to disk
# ***********************************************************************
writeSplitDF <- function(DF) {
  write_rds(DF, str_c(path, "/from_shiny/", unique(DF$Scene),       # Scene is the group_by var
                      "_refChangeDate_final_adj_intactNoDate.rds"))
  return(DF)
}

beforeRevise %>% group_by(Scene) %>% 
  do(writeSplitDF(.))


# ***********************************************************************
# UPDATE 2018-03-05: add the revised ref date, and thus revised adjusted ref date to shapefile
# ***********************************************************************
shp.folder <- paste(path, "/vector_data/FINALLY_USED/before_add_adjusted_reference_date", sep = "")
shp.DG1.afterRemove <- readOGR(dsn = shp.folder, layer = "meshSelect_prevDG1")
shp.DG2.afterRemove <- readOGR(dsn = shp.folder, layer = "meshSelect_prevDG2")    
shp.SC1.afterRemove <- readOGR(dsn = shp.folder, layer = "meshSelect_SC_1")
shp.SQ9.afterRemove <- readOGR(dsn = shp.folder, layer = "meshSelect_sq_9")
shp.SQ11.afterRemove <- readOGR(dsn = shp.folder, layer = "meshSelect_sq_11")
shp.SQ13.afterRemove <- readOGR(dsn = shp.folder, layer = "meshSelect_sq_13")

# Adjusted ref date
all.dating.adj.intactNoDate <- read_rds(str_c(path, "/from_shiny/", "all_refChangeDate_final_adj_intactNoDate.rds"))


# Attach to shapefile attribute data
shp.DG1.afterRemove$Id <- as.character(shp.DG1.afterRemove$Id) 
DG1.dating.adj <- all.dating.adj.intactNoDate %>% filter(Scene == "DG1") %>% select(Id, Date_adj)
shp.DG1.afterRemove <- sp::merge(shp.DG1.afterRemove, DG1.dating.adj, by = "Id")

shp.DG2.afterRemove$Id <- as.character(shp.DG2.afterRemove$Id) 
DG2.dating.adj <- all.dating.adj.intactNoDate %>% filter(Scene == "DG2") %>% select(Id, Date_adj)
shp.DG2.afterRemove <- sp::merge(shp.DG2.afterRemove, DG2.dating.adj, by = "Id")

shp.SC1.afterRemove$Id <- as.character(shp.SC1.afterRemove$Id) 
SC1.dating.adj <- all.dating.adj.intactNoDate %>% filter(Scene == "SC1") %>% select(Id, Date_adj)
shp.SC1.afterRemove <- sp::merge(shp.SC1.afterRemove, SC1.dating.adj, by = "Id")

shp.SQ9.afterRemove$Id <- as.character(shp.SQ9.afterRemove$Id) 
SQ9.dating.adj <- all.dating.adj.intactNoDate %>% filter(Scene == "SQ9") %>% select(Id, Date_adj)
shp.SQ9.afterRemove <- sp::merge(shp.SQ9.afterRemove, SQ9.dating.adj, by = "Id")

shp.SQ11.afterRemove$Id <- as.character(shp.SQ11.afterRemove$Id) 
SQ11.dating.adj <- all.dating.adj.intactNoDate %>% filter(Scene == "SQ11") %>% select(Id, Date_adj)
shp.SQ11.afterRemove <- sp::merge(shp.SQ11.afterRemove, SQ11.dating.adj, by = "Id")

shp.SQ13.afterRemove$Id <- as.character(shp.SQ13.afterRemove$Id) 
SQ13.dating.adj <- all.dating.adj.intactNoDate %>% filter(Scene == "SQ13") %>% select(Id, Date_adj)
shp.SQ13.afterRemove <- sp::merge(shp.SQ13.afterRemove, SQ13.dating.adj, by = "Id")


# The undisturbed sample has Date (in app the date of last obs was registered) need to remove
shp.DG1.afterRemove$Comment <- as.character(shp.DG1.afterRemove$Comment)
shp.DG1.afterRemove[which(shp.DG1.afterRemove$Comment == "undisturbed_throughout"), "Date"] <- NA

shp.DG2.afterRemove$Comment <- as.character(shp.DG2.afterRemove$Comment)
shp.DG2.afterRemove[which(shp.DG2.afterRemove$Comment == "undisturbed_throughout"), "Date"] <- NA

shp.SC1.afterRemove$Comment <- as.character(shp.SC1.afterRemove$Comment)
shp.SC1.afterRemove[which(shp.SC1.afterRemove$Comment == "undisturbed_throughout"), "Date"] <- NA

shp.SQ9.afterRemove$Comment <- as.character(shp.SQ9.afterRemove$Comment)
shp.SQ9.afterRemove[which(shp.SQ9.afterRemove$Comment == "undisturbed_throughout"), "Date"] <- NA

shp.SQ11.afterRemove$Comment <- as.character(shp.SQ11.afterRemove$Comment)
shp.SQ11.afterRemove[which(shp.SQ11.afterRemove$Comment == "undisturbed_throughout"), "Date"] <- NA

shp.SQ13.afterRemove$Comment <- as.character(shp.SQ13.afterRemove$Comment)
shp.SQ13.afterRemove[which(shp.SQ13.afterRemove$Comment == "undisturbed_throughout"), "Date"] <- NA


# Save shp as shapefile and rds
shp.folder <- paste(path, "/vector_data/FINALLY_USED", sep = "")
writeOGR(shp.DG1.afterRemove, shp.folder, layer = "meshSelect_prevDG1", driver = "ESRI Shapefile")
writeOGR(shp.DG2.afterRemove, shp.folder, layer = "meshSelect_prevDG2", driver = "ESRI Shapefile")
writeOGR(shp.SC1.afterRemove, shp.folder, layer = "meshSelect_SC_1", driver = "ESRI Shapefile")
writeOGR(shp.SQ9.afterRemove, shp.folder, layer = "meshSelect_sq_9", driver = "ESRI Shapefile")
writeOGR(shp.SQ11.afterRemove, shp.folder, layer = "meshSelect_sq_11", driver = "ESRI Shapefile")
writeOGR(shp.SQ13.afterRemove, shp.folder, layer = "meshSelect_sq_13", driver = "ESRI Shapefile")








  