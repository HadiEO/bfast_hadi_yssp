# False positive cases (also when fit harmon to history) in DG1 image:
# 1346 intact forest
# 654 oil palm

extrNDMI <- read_rds(str_c(path, "/extracted_time_series/FINALLY_USED/extrNDMIsub_DG1.rds"))
extrNDMI.ls <- as.list(extrNDMI)
demo.bts.ls <- lapply(extrNDMI.ls, FUN = function(z) bfastts(z, dates = time(z), type = "irregular"))


for(i in 1:length(demo.bts.ls)) {
  plot(index(demo.bts.ls[[i]]), demo.bts.ls[[i]], ylim = c(-0.2, 0.6),
       main = str_c("pixel id: ", names(demo.bts.ls)[i]))
  locator(1)
}


data <- demo.bts.ls$`515`                       # Which time series Id ? DG1 1346 intact forest with noise; 
                                                 # DG1 982 oil palm with noise
                                                 # DG1 1131 successful case
                                                 # SC1 1393 to test ROC (same as "all")
                                                 # DG1 756 to test ROC

DG.firstDate <- as.Date("2000-01-15")             # 2000-01-15 "2002-09-29" "2006-01-01"
DG.lastDate <- as.Date("2015-08-15")
start <- c(year(DG.firstDate), yday(DG.firstDate))
formula <- response ~ trend 
history <- "all"
order <- 1
plot <- FALSE
h <- 0.25
historyNoiseRemoved <- TRUE
updateMOSUM <- TRUE
allNoiseRemoved <- FALSE
cons <- 3                     
maxTimeSpan <- 2
updateMOSUM <- TRUE
factorRMSE <- 3
boundaryRMSE <- TRUE  
originalTs <- extrNDMI.ls$`515`
originalDateNoNA <- index(originalTs[!is.na(originalTs)])

# Test original bfm algorithm
# ori_bfm_out <- bfastmonitor(data = data, 
#                             start = start, 
#                             formula = formula,
#                             order = order, 
#                             plot = TRUE, 
#                             h = h, 
#                             history = history)


# default bfastmonitor arguments (run when debug)
lag = NULL
slag = NULL
verbose = TRUE
type = "OLS-MOSUM"
hpc = "none"
end = 10
level = 0.05


# ***********************************************************************************
# Fix date mismatch: use the bp obs number to index the original dates in extrNMDI (before regularized by bfastts)
# ***********************************************************************************
ori.ts <- extrNDMI.ls$'515'
ori.ts.df <- tibble(date = decimal_date(index(ori.ts)) , value = ori.ts)
ori.ts.noNA <- ori.ts[!is.na(ori.ts)]
length(ori.ts.noNA)
originalDateNoNA <- index(ori.ts.noNA)

bts <- demo.bts.ls$`515`   # bts is "data" object in bfastmonitor_mod(), which is input to dips removal
bts.noNA <- bts[!is.na(bts)]
length(bts.noNA) # length is the same as length(ori.ts.noNA)
plot(ori.ts.noNA, bts.noNA); abline(0,1) # values are the same

bts.histDenoised <- data_bind_backTs  # data_bind_backTs from bfastmonitor_mod()
bts.histDenoised.noNA <- bts.histDenoised[!is.na(bts.histDenoised)]
length(bts.histDenoised.noNA) # length is shorter than bts cause some outliers are removed

bts.histDenoised.noNA %in% bts.noNA # some values change cause interpolated between neighbours during noise removal

NROW(data_tspp)  # the same as length(bts.histDenoised.noNA)




# Maybe fix bfastts?
tso.noNA <- tso[!is.na(tso)]
# NO, ts needs to be regular i.e. freq=365

# removedips always add 1 date in the end?
temp <- demo.bts.ls$`515`
temp.dipsRm <- removedips(temp)
temp.dipsRmMod <- removedips_mod(temp, searchWindow = 1, updateX = TRUE)
length(temp)
length(temp.dipsRm)
length(temp.dipsRmMod)



# ***********************************************************************************

# Run the modified bfm function
source("R/Rfunction/bfastmonitor_mod.R")

mod_bfm_out_dipsNotRm_notCons <- bfastmonitor_mod(data = data, 
                                          start = start, 
                                          formula = formula,
                                          order = order, 
                                          plot = FALSE, 
                                          h = h, 
                                          history = history,
                                          historyNoiseRemoved = FALSE, 
                                          cons = 1,
                                          updateMOSUM = updateMOSUM)

# ************************************************************************
# Test different history
mod_bfm_out_dipsNotRm_notCons_roc <- bfastmonitor_mod(data = data, 
                                                  start = start, 
                                                  formula = formula,
                                                  order = order, 
                                                  plot = FALSE, 
                                                  h = h, 
                                                  history = "ROC",
                                                  historyNoiseRemoved = FALSE, 
                                                  cons = 1,
                                                  updateMOSUM = updateMOSUM)

mod_bfm_out_dipsNotRm_notCons_bp <- bfastmonitor_mod(data = data, 
                                                      start = start, 
                                                      formula = formula,
                                                      order = order, 
                                                      plot = FALSE, 
                                                      h = h, 
                                                      history = "BP",
                                                      historyNoiseRemoved = FALSE, 
                                                      cons = 1,
                                                      updateMOSUM = updateMOSUM)
# ****************************************************************************

mod_bfm_out_historyDipsRm_notCons <- bfastmonitor_mod(data = data, 
                                              start = start, 
                                              formula = formula,
                                              order = order, 
                                              plot = FALSE, 
                                              h = h, 
                                              history = history,
                                              historyNoiseRemoved = TRUE,
                                              cons = 1,
                                              updateMOSUM = updateMOSUM)

mod_bfm_out_historyDipsRm_Cons2_Span2 <- bfastmonitor_mod(data = data, 
                                                          start = start, 
                                                          formula = formula,
                                                          order = order, 
                                                          plot = FALSE, 
                                                          h = h, 
                                                          history = history,
                                                          historyNoiseRemoved = TRUE,
                                                          cons = 2,
                                                          maxTimeSpan = 2,
                                                          updateMOSUM = updateMOSUM)

mod_bfm_out_historyDipsRm_Cons3_Span2 <- bfastmonitor_mod(data = data, 
                                                          start = start, 
                                                          formula = formula,
                                                          order = order, 
                                                          plot = FALSE, 
                                                          h = h, 
                                                          history = history,
                                                          historyNoiseRemoved = TRUE,
                                                          cons = 3,
                                                          maxTimeSpan = 2,
                                                          updateMOSUM = updateMOSUM)

mod_bfm_out_historyDipsRm_Cons4_Span2 <- bfastmonitor_mod(data = data, 
                                              start = start, 
                                              formula = formula,
                                              order = order, 
                                              plot = FALSE, 
                                              h = h, 
                                              history = history,
                                              historyNoiseRemoved = TRUE,
                                              cons = 4,
                                              maxTimeSpan = 2,
                                              updateMOSUM = updateMOSUM)

mod_bfm_out_allDipsRm_notCons <- bfastmonitor_mod(data = data, 
                                                  start = start, 
                                                  formula = formula,
                                                  order = order, 
                                                  plot = FALSE, 
                                                  h = h, 
                                                  history = history,
                                                  historyNoiseRemoved = FALSE,
                                                  allNoiseRemoved = TRUE,
                                                  cons = 1,
                                                  updateMOSUM = updateMOSUM)

# This seems to be the best, but seems like "cheating"?
# I mean, shouldn't it be the history is fit only once, then any new obs
# is compared against history
# Solution is: must confirm disturbance only if they are consecutive in the monitoring period!


# ***************************************************
# RMSE as boundary
# ***************************************************
mod_bfm_rmseBound_out_historyDipsRm_Cons3_Span2 <- bfastmonitor_mod(data = data, 
                                                  start = start, 
                                                  formula = formula,
                                                  order = order, 
                                                  plot = FALSE, 
                                                  h = h, 
                                                  history = history,
                                                  historyNoiseRemoved = TRUE,
                                                  allNoiseRemoved = FALSE,
                                                  cons = 3, maxTimeSpan = 2,
                                                  updateMOSUM = TRUE,
                                                  boundaryRMSE = TRUE,
                                                  factorRMSE = 3,
                                                  originalDateNoNA = originalDateNoNA)


dec2date(mod_bfm_rmseBound_out_historyDipsRm_Cons3_Span2$breakpoint) %in% 
  index(extrNDMI.ls$`515`)







# Plot to compare original and modified bfastmonitor
# base way
par(mfrow = c(3,1))
plot(mod_bfm_out_dipsNotRm_notCons)
plot(mod_bfm_out_historyDipsRm_notCons)
plot(mod_bfm_out_historyDipsRm_Cons4_Span2)  

# ggplot way
tempPlot <- bfmPlot(
  list(mod_bfm_out_dipsNotRm_notCons, 
       mod_bfm_out_historyDipsRm_notCons,
       mod_bfm_out_historyDipsRm_Cons3_Span2,
       mod_bfm_out_historyDipsRm_Cons4_Span2,
       mod_bfm_out_allDipsRm_notCons),
  plotlabs = c("dipsNotRm_notCons", 
               "historyDipsRm_notCons", 
               "historyDipsRm_Cons3_Span2",
               "historyDipsRm_Cons4_Span2",
               "allDipsRm_notCons"), 
  ncols = 1, displayMagn = FALSE, displayResiduals = 'monperiod') + 
  theme_bw() + labs(y = "NDMI", x = "Date") + scale_y_continuous(limits = c(-0.2, 0.6)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

tempPlot

# Modified bfmPlot
# The version for input as list doesn't work with the modified bfmPlot arggghhhh!!!!
# So just use multiplot() for now
# tempPlotMod <- bfmPlot_mod(
#   list(mod_bfm_out_dipsNotRm_notCons, 
#        mod_bfm_out_historyDipsRm_notCons,
#        mod_bfm_out_historyDipsRm_Cons3_Span2,
#        mod_bfm_out_historyDipsRm_Cons4_Span2,
#        mod_bfm_out_allDipsRm_notCons),
#   plotlabs = c("dipsNotRm_notCons", 
#                "historyDipsRm_notCons", 
#                "historyDipsRm_Cons3_Span2",
#                "historyDipsRm_Cons4_Span2",
#                "allDipsRm_notCons"), 
#   ncols = 1, displayMagn = FALSE, displayResiduals = 'monperiod', displayOldFlag = TRUE) + 
#   theme_bw() + labs(y = "NDMI", x = "Date") + scale_y_continuous(limits = c(-0.2, 0.6)) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))
# 
# tempPlotMod

plot1 <- bfmPlot_mod(mod_bfm_out_dipsNotRm_notCons, plotlabs = "Original bfastmonitor",
            ncols = 1, displayMagn = FALSE, displayResiduals = 'monperiod', displayOldFlag = TRUE,
            circleVersion = TRUE) 
plot1 <- plot1 + 
  theme_bw() + labs(y = "NDMI", x = "Date") + scale_y_continuous(limits = c(-0.2, 0.6)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot2 <- bfmPlot_mod(mod_bfm_out_historyDipsRm_notCons, plotlabs = "History denoised",
            ncols = 1, displayMagn = FALSE, displayResiduals = 'monperiod', displayOldFlag = TRUE,
            circleVersion = TRUE) 
plot2 <- plot2 + 
  theme_bw() + labs(y = "NDMI", x = "Date") + scale_y_continuous(limits = c(-0.2, 0.6)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot3 <- bfmPlot_mod(mod_bfm_out_historyDipsRm_Cons3_Span2, plotlabs = "History denoised, cons=3, span=2",
            ncols = 1, displayMagn = FALSE, displayResiduals = 'monperiod', displayOldFlag = TRUE,
            circleVersion = TRUE) 
plot3 <- plot3 + 
  theme_bw() + labs(y = "NDMI", x = "Date") + scale_y_continuous(limits = c(-0.2, 0.6)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot4 <- bfmPlot_mod(mod_bfm_out_historyDipsRm_Cons4_Span2, plotlabs = "mod_bfm_out_historyDipsRm_Cons4_Span2",
            ncols = 1, displayMagn = FALSE, displayResiduals = 'monperiod', displayOldFlag = TRUE,
            circleVersion = TRUE) 
plot4 <- plot4 + 
  theme_bw() + labs(y = "NDMI", x = "Date") + scale_y_continuous(limits = c(-0.2, 0.6)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot5 <- bfmPlot_mod(mod_bfm_out_allDipsRm_notCons, plotlabs = "Whole ts denoised",
            ncols = 1, displayMagn = FALSE, displayResiduals = 'monperiod', displayOldFlag = TRUE,
            circleVersion = TRUE) 
plot5 <- plot5 + 
  theme_bw() + labs(y = "NDMI", x = "Date") + scale_y_continuous(limits = c(-0.2, 0.6)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot6 <- bfmPlot_mod(mod_bfm_out_historyDipsRm_Cons2_Span2, plotlabs = "History denoised, cons=2, span=2",
            ncols = 1, displayMagn = FALSE, displayResiduals = 'monperiod', displayOldFlag = TRUE,
            circleVersion = TRUE) 
plot6 <- plot6 + 
  theme_bw() + labs(y = "NDMI", x = "Date") + scale_y_continuous(limits = c(-0.2, 0.6)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# *********************************************************************
# RMSE as boundary
# **********************************************************************
plot7 <- bfmPlot_mod(mod_bfm_rmseBound_out_historyDipsRm_Cons3_Span2, plotlabs = "History denoised, cons=3, span=2, boundary=3*histRMSE",
            ncols = 1, displayMagn = FALSE, displayResiduals = 'monperiod', displayOldFlag = TRUE,
            circleVersion = TRUE) 
plot7 <- plot7 + 
  theme_bw() + labs(y = "NDMI", x = "Date") + scale_y_continuous(limits = c(-0.2, 0.6)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




# Need to adjust sizes for saving plot in pdf format
pdf("report/figs/demo_modified_bfm_updateMOSUMTrue.pdf", width = 5.4, height = 7, pointsize = 8) 
multiplot(plot1, plot2, plot6, plot3, plot5, cols = 1)
dev.off()



# To delete
temp <- bfastmonitor_mod(data = data, 
                        start = start, 
                        formula = response ~ trend,
                        plot = FALSE, 
                        h = 0.25, 
                        history = "all",
                        historyNoiseRemoved = FALSE, 
                        allNoiseRemoved = FALSE,
                        cons = 3, maxTimeSpan = 2,
                        updateMOSUM = FALSE,
                        boundaryRMSE = FALSE,
                        factorRMSE = 3)


tempPlot <- bfmPlot_mod(temp, plotlabs = c("9999"),
                             ncols = 1, displayMagn = FALSE, displayResiduals = 'monperiod', displayOldFlag = TRUE,
                             circleVersion = TRUE) +
  theme_bw() + labs(y = "NDMI", x = "Date") + scale_y_continuous(limits = c(-0.2, 0.6)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


tempPlot




