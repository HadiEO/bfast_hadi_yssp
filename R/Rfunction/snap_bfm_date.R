# # ref.date.adj is ok, bfm.date.confirmed = 2013-07-22 is strange it's not obs date (= 2013-08-18)! 
# The issue: bfastts introduces duplicate dates and missing dates!!!!!!!!!
# Try a quick solution now:
# "snap" breakpoint (confirmed and first flagged) dates in bfm results
# to closest obs date in time series.


# ExtrNDMI has the correct date
(path)
path2 <- "/extracted_time_series/FINALLY_USED/"
extrNDMIsub.DG1 <- read_rds(str_c(path, path2, "extrNDMIsub_DG1.rds"))      
extrNDMIsub.DG2 <- read_rds(str_c(path, path2, "extrNDMIsub_DG2.rds"))
extrNDMIsub.SC1 <- read_rds(str_c(path, path2, "extrNDMIsub_SC1.rds"))
extrNDMIsub.sq9 <- read_rds(str_c(path, path2, "extrNDMIsub_sq9.rds"))
# extrNDMIsub.sq10 <- read_rds(str_c(path, path2, "extrNDMIsub_sq10.rds"))    # acacia
extrNDMIsub.sq11 <- read_rds(str_c(path, path2, "extrNDMIsub_sq11.rds"))
extrNDMIsub.sq13 <- read_rds(str_c(path, path2, "extrNDMIsub_sq13.rds"))


snap_bfm_date <- function(correct_date, bfm_date) {
  
}