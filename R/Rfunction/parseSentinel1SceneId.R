# SceneId is asset Id in google earth engine
# sceneId <- "S1A_IW_GRDH_1SDV_20150324T214954_20150324T215019_005179_006887_854C" # Example

parseSentinel1SceneId <- function(sceneId) {
  temp1 <- str_split(sceneId, "_")[[1]][1]
  yyyy <- substr(temp1, 1, 4)
  mm <- substr(temp1, 5, 6)
  dd <- substr(temp1, 7, 8)
  yyyymmdd <- str_c(yyyy, "-", mm, "-", dd)
  date <- as_date(yyyymmdd)
  return(date)
} 