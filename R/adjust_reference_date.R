# ***********************************************************************
# Check considering adjusting reference date ---------------------------------------------------
# ***********************************************************************
# Ref change date
all.dating <- read_rds(str_c(path, "/from_shiny/", "all_refChangeDate_final.rds")) # 435

# bfm results (doesn't matter which run)
temp <- str_c(path, "/bfm_results/bfm_run8")
bfmRes.DG1 <- read_rds(str_c(temp, "_DG1.rds"))
bfmRes.DG2 <- read_rds(str_c(temp, "_DG2.rds"))
bfmRes.SC1 <- read_rds(str_c(temp, "_SC1.rds"))
bfmRes.SQ9 <- read_rds(str_c(temp, "_SQ9.rds"))
bfmRes.SQ11 <- read_rds(str_c(temp, "_SQ11.rds"))
bfmRes.SQ13 <- read_rds(str_c(temp, "_SQ13.rds"))

# Get dataDf$time
notNATime.DG1 <- lapply(bfmRes.DG1, FUN = function(z) z$mefp$dataDf$time) # This can be wrong though cause z$mefp$dataDf$time may not match the obs date, possibly due to bfastts() issue
notNATime.DG2 <- lapply(bfmRes.DG2, FUN = function(z) z$mefp$dataDf$time)
notNATime.SC1 <- lapply(bfmRes.SC1, FUN = function(z) z$mefp$dataDf$time)
notNATime.SQ9 <- lapply(bfmRes.SQ9, FUN = function(z) z$mefp$dataDf$time)
notNATime.SQ11 <- lapply(bfmRes.SQ11, FUN = function(z) z$mefp$dataDf$time)
notNATime.SQ13 <- lapply(bfmRes.SQ13, FUN = function(z) z$mefp$dataDf$time)

# Convert time to date
notNATime.DG1 <- lapply(notNATime.DG1, FUN = function(z) dec2date(z))
notNATime.DG2 <- lapply(notNATime.DG2, FUN = function(z) dec2date(z))
notNATime.SC1 <- lapply(notNATime.SC1, FUN = function(z) dec2date(z))
notNATime.SQ9 <- lapply(notNATime.SQ9, FUN = function(z) dec2date(z))
notNATime.SQ11 <- lapply(notNATime.SQ11, FUN = function(z) dec2date(z))
notNATime.SQ13 <- lapply(notNATime.SQ13, FUN = function(z) dec2date(z))

# Get the date before ref change date
# scene <- "DG1"
# notNATime <- notNATime.DG1

getLagPrev <- function(notNATime, scene) {
  out <- list()
  for(i in 1:length(names(notNATime))) {
    nowNotNATime <- notNATime[[i]]
    nowId <- names(notNATime)[i]
    nowRefDate <- all.dating %>% dplyr::filter(Scene == scene, Id == nowId) %>% .[["Date"]]
    nowNotNATimeBeforeRef <- nowNotNATime[nowNotNATime < nowRefDate]
    nowLag <- as.numeric(nowRefDate - nowNotNATimeBeforeRef[length(nowNotNATimeBeforeRef)])
    out[[nowId]] <- nowLag
  }
  return(out)
}

lagPrev.DG1 <- getLagPrev(notNATime.DG1, "DG1")
lagPrev.DG2 <- getLagPrev(notNATime.DG2, "DG2")
lagPrev.SC1 <- getLagPrev(notNATime.SC1, "SC1")
lagPrev.SQ9 <- getLagPrev(notNATime.SQ9, "SQ9")
lagPrev.SQ11 <- getLagPrev(notNATime.SQ11, "SQ11")
lagPrev.SQ13 <- getLagPrev(notNATime.SQ13, "SQ13")


lagPrev.all <- c(unlist(lagPrev.DG1),
                 unlist(lagPrev.DG2),
                 unlist(lagPrev.SC1),
                 unlist(lagPrev.SQ9),
                 unlist(lagPrev.SQ11),
                 unlist(lagPrev.SQ13))

hist(lagPrev.all)
par(mfrow = c(1,2))
boxplot(lagPrev.all)
vioplot(lagPrev.all)
# The lag between ref date and previous date can be very long!
# But,
length(lagPrev.all[lagPrev.all > 30]) # 69
length(lagPrev.all[lagPrev.all > 90]) # 44
length(lagPrev.all[lagPrev.all > 180]) # 22
length(lagPrev.all[lagPrev.all > 365]) # 5
table(lagPrev.all)
# 1   8   9  17  24  33  40  65  88  96 104 113 128 136 177 209 265 305 337 433 449 489 
# 245  92   2  26   1   4   6   3  12   7   1   2   8   3   1   6   3   3   5   1   3   1




# ********************************************************************************
# Let's try calculate MTL with adjusted reference date
# ********************************************************************************
# Get date of previous obs before ref date
getDatePrev <- function(notNATime, scene) {
  out <- list()
  for(i in 1:length(names(notNATime))) {
    nowNotNATime <- notNATime[[i]]
    nowId <- names(notNATime)[i]
    nowRefDate <- all.dating %>% dplyr::filter(Scene == scene, Id == nowId) %>% .[["Date"]]
    nowNotNATimeBeforeRef <- nowNotNATime[nowNotNATime < nowRefDate]
    nowPrevDate <- nowNotNATimeBeforeRef[length(nowNotNATimeBeforeRef)]
    out[[nowId]] <- nowPrevDate
  }
  return(out)
}

datePrev.DG1 <- getDatePrev(notNATime.DG1, "DG1")
datePrev.DG2 <- getDatePrev(notNATime.DG2, "DG2")
datePrev.SC1 <- getDatePrev(notNATime.SC1, "SC1")
datePrev.SQ9 <- getDatePrev(notNATime.SQ9, "SQ9")
datePrev.SQ11 <- getDatePrev(notNATime.SQ11, "SQ11")
datePrev.SQ13 <- getDatePrev(notNATime.SQ13, "SQ13")

# Make a tibble with columns Id, Scene, ref.date.prev 
datePrev.DG1.tb <- tibble(Scene = "DG1", Id = names(datePrev.DG1), ref.date.prev = as.Date(unlist(datePrev.DG1)))
datePrev.DG2.tb <- tibble(Scene = "DG2", Id = names(datePrev.DG2), ref.date.prev = as.Date(unlist(datePrev.DG2)))
datePrev.SC1.tb <- tibble(Scene = "SC1", Id = names(datePrev.SC1), ref.date.prev = as.Date(unlist(datePrev.SC1)))
datePrev.SQ9.tb <- tibble(Scene = "SQ9", Id = names(datePrev.SQ9), ref.date.prev = as.Date(unlist(datePrev.SQ9)))
datePrev.SQ11.tb <- tibble(Scene = "SQ11", Id = names(datePrev.SQ11), ref.date.prev = as.Date(unlist(datePrev.SQ11)))
datePrev.SQ13.tb <- tibble(Scene = "SQ13", Id = names(datePrev.SQ13), ref.date.prev = as.Date(unlist(datePrev.SQ13)))

datePrev.all.tb <- bind_rows(datePrev.DG1.tb,
                             datePrev.DG2.tb,
                             datePrev.SC1.tb,
                             datePrev.SQ9.tb,
                             datePrev.SQ11.tb,
                             datePrev.SQ13.tb)


# Then, left_join(all.bfmFlag, by = c(Id, Scene))
all.bfmFlag.TP.bfmDateAfterRef <- left_join(
  all.bfmFlag.TP.bfmDateAfterRef, datePrev.all.tb, by = c("Id", "Scene"))

# Calculate ref.date.adj
all.bfmFlag.TP.bfmDateAfterRef <- all.bfmFlag.TP.bfmDateAfterRef %>% 
  mutate(ref.date.adj = ref.date - floor(0.5*(ref.date - ref.date.prev)))


all.bfmFlag.TP.bfmDateAfterRef <- all.bfmFlag.TP.bfmDateAfterRef %>% 
  mutate(lag.confirmed.adj = bfm.date.confirmed - ref.date.adj,
         lag.firstFlagged.adj = bfm.date.firstFlagged - ref.date.adj)

# ******************************************************
# MTL adjusted
# ******************************************************
median(all.bfmFlag.TP.bfmDateAfterRef$lag.confirmed, na.rm = TRUE)
median(all.bfmFlag.TP.bfmDateAfterRef$lag.firstFlagged, na.rm = TRUE)
# ******************************************************


# ******************************************************
# Save the adjusted date (for all pixels not TP here!)
# ******************************************************
# Then, left_join(all.bfmFlag, by = c(Id, Scene))
all.bfmFlag <- left_join(
  all.bfmFlag, datePrev.all.tb, by = c("Id", "Scene"))

# Calculate ref.date.adj
all.bfmFlag <- all.bfmFlag %>% 
  mutate(ref.date.adj = ref.date - floor(0.5*(ref.date - ref.date.prev)))


all.bfmFlag %>% select(Id, Scene, ref.date.adj) %>% rename(Date_adj = ref.date.adj) %>% 
  write_rds(str_c(path, "/from_shiny/", "all_refChangeDate_final_adj.rds")) # Save the adjusted reference date
all.bfmFlag %>% select(Id, Scene, ref.date.adj) %>% rename(Date_adj = ref.date.adj) %>% 
  write.csv2(str_c(path, "/from_shiny/", "all_refChangeDate_final_adj.csv")) # Save the adjusted reference date


