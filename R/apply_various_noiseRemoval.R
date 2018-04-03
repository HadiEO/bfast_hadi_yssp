# Function removedips() and removedips_mod() sourced in .Rprofile source("R/Rfunction/removedips.R")

# Outlier detection approaches:
# (1) Compare obs with neighbour obs
# (2) Compare obs with percentile of distribution
# (3) Standard deviation

# Test data
(path)
path2 <- "/extracted_time_series/FINALLY_USED/"
extrNDMIsub_DG1 <- read_rds(str_c(path, path2, "extrNDMIsub_DG1.rds"))
x <- extrNDMIsub_DG1[, "1131"]

# x is a zoo ts object (irregular). Convert to ts object (regular) for plotting
x_bts <- bfastts(x, dates = time(x), type = "irregular")


# (1) Compare obs with neighbour obs --------------------------------------
# Apply original removedips function
x_rmDips <- removedips(x_bts)              # removedips can be applied to either irregular zoo or regular ts                       

# Apply modified removedips function
x_rmDipsModNoUpd <- removedips_mod(x_bts, updateX = FALSE, searchWindow = 1)  
x_rmDipsModYesUpd <- removedips_mod(x_bts, updateX = TRUE, searchWindow = 1)  

# Need to convert date to not in decimal date for plotting
x_bts_date <- as.Date(format(date_decimal(as.numeric(time(x_bts))), "%d-%m-%Y"), "%d-%m-%Y")
# shorter: as.Date(format(date_decimal((index(x_bts))), "%d-%m-%Y"), "%d-%m-%Y")
# or could also use lubridate::ymd
x_rmDips_date <- as.Date(format(date_decimal(as.numeric(time(x_rmDips))), "%d-%m-%Y"), "%d-%m-%Y")
x_rmDipsMod_date <- as.Date(format(date_decimal(as.numeric(time(x_rmDipsModNoUpd))), "%d-%m-%Y"), "%d-%m-%Y")

# Plot time series
# plot(x_bts_date, x_bts, pch = 16) # Raw time series
# # points(x_rmDips_date, x_rmDips, col = "red", pch = 0, cex = 2) # Original removedips function applied
# points(x_rmDipsMod_date, x_rmDipsModNoUpd, col = "blue", pch = 2, cex = 1.5) # Modified removedips function applied
# points(x_rmDipsMod_date, x_rmDipsModYesUpd, col = "magenta", pch = 7, cex = 1.5)

# Plot time series (the plot.zoo way)
df <- data.frame(ori = x_bts,
                 rmDips = x_rmDips,
                 rmDipsModNoUpd = x_rmDipsModNoUpd,
                 rmDipsModYesUpd = x_rmDipsModYesUpd)
z <- zoo(df, order.by = x_bts_date)
plot(na.omit(z), plot.type = "single", col = c("grey", "green", "blue", "red"), lwd = c(8,2,4,2), type = "o")
legend("topright", inset=c(0,0), y.intersp = 1, legend = c("ori", "rmDips", "rmDipsModNoUpd", "rmDipsModYesUpd"),  
       lwd = c(8,2,4,2), bty = "n", col = c("grey", "green", "blue", "red"))




# (2) Compare obs with percentile of distribution -------------------------
# Hamunyela et al. found 5th percentile optimal for outlier detection in monitoring period
x_bts_stable <- window(x_bts, end = c(2010,1), frequency = 365)
hist(x_bts_stable)
quantile(x_bts_stable, .05, na.rm = TRUE) 


















