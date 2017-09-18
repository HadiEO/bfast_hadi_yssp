# SceneId is asset Id in google earth engine
# sceneId <- "20151130T022042_20170512T203612_T50NPH" # Example

parseSentinel2SceneId <- function(sceneId) {
  temp1 <- str_split(sceneId, "_")[[1]][1]
  yyyy <- substr(temp1, 1, 4)
  mm <- substr(temp1, 5, 6)
  dd <- substr(temp1, 7, 8)
  yyyymmdd <- str_c(yyyy, "-", mm, "-", dd)
  date <- as_date(yyyymmdd)
  return(date)
} 