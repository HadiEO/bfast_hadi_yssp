# Extracted zoo ts
extrNDMI <- read_rds(str_c(path, "/extracted_time_series/FINALLY_USED/extrNDMIsub_DG1.rds"))
extrNDMI.ls <- as.list(extrNDMI)
demo.bts.ls <- lapply(extrNDMI.ls, FUN = function(z) bfastts(z, dates = time(z), type = "irregular"))

# Store the original obs date of non-NA obs
extrNDMI.dateNoNA.ls <- lapply(extrNDMI.ls, 
                               FUN = function(z) index(z[!is.na(z)]))

# Reference date
ref.date.all <- read_rds(str_c(path, "/from_shiny/", "all_refChangeDate_final_adj_intactNoDate.rds"))


# ********************************************************************
# Which time series Id ? 
# DG1 (677, 904, 908, 1131, 725, 603)
# ********************************************************************
now.ts <- demo.bts.ls$`603`                       
now.dateNoNA <- extrNDMI.dateNoNA.ls$`603` 
now.refDate <- ref.date.all %>% 
  filter(Scene == "DG1", Id == "603") %>% 
  .[["Date"]]

now.ts.raw <- extrNDMI.ls$'603'

# ********************************************************************
# Scenarios for comparison
# ********************************************************************
# Fixed setting
searchWindow <- 1
maxTimeSpan <- 2
start <- c(2000,1)
history = "all"



# Run 1: Original bfastmonitor *without* history noise removal
run <- "run1"
historyNoiseRemoved <- FALSE
allNoiseRemoved = FALSE
boundaryRMSE <- FALSE
factorRMSE <- NA
cons <- 1
updateMOSUM <- FALSE    
factorRMSE_immediate <- NULL
allowImmediateConfirm <- FALSE


# Run 5: Modified bfastmonitor *without* history noise removal, cons = 3, updateMOSUM = TRUE
run <- "run5"
historyNoiseRemoved <- FALSE
allNoiseRemoved = FALSE
boundaryRMSE <- FALSE
factorRMSE <- NA
cons <- 3 
updateMOSUM <- TRUE    
factorRMSE_immediate <- NULL
allowImmediateConfirm <- FALSE

# Run 6: Modified bfastmonitor *without* history noise removal, cons = 3, updateMOSUM = FALSE
run <- "run6"
historyNoiseRemoved <- FALSE
allNoiseRemoved = FALSE
boundaryRMSE <- FALSE
factorRMSE <- NA
cons <- 3 
updateMOSUM <- FALSE    
factorRMSE_immediate <- NULL
allowImmediateConfirm <- FALSE

# Run 27: boundary 4 * histRMSE, *without* history noise removal, cons = 3
run <- "run27"
historyNoiseRemoved <- FALSE
allNoiseRemoved = FALSE
boundaryRMSE <- TRUE
factorRMSE <- 4
cons <- 3 
updateMOSUM <- FALSE    
factorRMSE_immediate <- NULL
allowImmediateConfirm <- FALSE

# ********************************************************************
# Run bfastmonitor for different scenarios
# ********************************************************************

# ********************************************************************
# Run 1 -------------------------------------------------------------------
# ********************************************************************
run1 <- bfastmonitor_mod(
  now.ts, # 
  start = start,
  formula = response~trend,      
  plot = FALSE, 
  h = 0.25, 
  history = history,
  historyNoiseRemoved = historyNoiseRemoved,
  allNoiseRemoved = allNoiseRemoved,
  searchWindow = searchWindow,
  cons = cons,
  maxTimeSpan = maxTimeSpan,
  updateMOSUM = updateMOSUM,
  boundaryRMSE = boundaryRMSE,
  factorRMSE = factorRMSE,
  originalDateNoNA = now.dateNoNA, #
  allowImmediateConfirm = allowImmediateConfirm,
  factorRMSE_immediate = factorRMSE_immediate
)

# ********************************************************************
# Run 5 -------------------------------------------------------------------
# ********************************************************************
run5 <- bfastmonitor_mod(
  now.ts, # 
  start = start,
  formula = response~trend,      
  plot = FALSE, 
  h = 0.25, 
  history = history,
  historyNoiseRemoved = historyNoiseRemoved,
  allNoiseRemoved = allNoiseRemoved,
  searchWindow = searchWindow,
  cons = cons,
  maxTimeSpan = maxTimeSpan,
  updateMOSUM = updateMOSUM,
  boundaryRMSE = boundaryRMSE,
  factorRMSE = factorRMSE,
  originalDateNoNA = now.dateNoNA, #
  allowImmediateConfirm = allowImmediateConfirm,
  factorRMSE_immediate = factorRMSE_immediate
)

# ********************************************************************
# Run 6 -------------------------------------------------------------------
# ********************************************************************
run6 <- bfastmonitor_mod(
  now.ts, # 
  start = start,
  formula = response~trend,      
  plot = FALSE, 
  h = 0.25, 
  history = history,
  historyNoiseRemoved = historyNoiseRemoved,
  allNoiseRemoved = allNoiseRemoved,
  searchWindow = searchWindow,
  cons = cons,
  maxTimeSpan = maxTimeSpan,
  updateMOSUM = updateMOSUM,
  boundaryRMSE = boundaryRMSE,
  factorRMSE = factorRMSE,
  originalDateNoNA = now.dateNoNA, #
  allowImmediateConfirm = allowImmediateConfirm,
  factorRMSE_immediate = factorRMSE_immediate
)

# ********************************************************************
# Run 27 -------------------------------------------------------------------
# ********************************************************************
run27 <- bfastmonitor_mod(
  now.ts, # 
  start = start,
  formula = response~trend,      
  plot = FALSE, 
  h = 0.25, 
  history = history,
  historyNoiseRemoved = historyNoiseRemoved,
  allNoiseRemoved = allNoiseRemoved,
  searchWindow = searchWindow,
  cons = cons,
  maxTimeSpan = maxTimeSpan,
  updateMOSUM = updateMOSUM,
  boundaryRMSE = boundaryRMSE,
  factorRMSE = factorRMSE,
  originalDateNoNA = now.dateNoNA, #
  allowImmediateConfirm = allowImmediateConfirm,
  factorRMSE_immediate = factorRMSE_immediate
)


# ********************************************************************
# bfm plot -------------------------------------------------------------------
# ********************************************************************
# run1, run5, run6, run27
source("R/Rfunction/bfmPlot_mod.R")
source("R/Rfunction/MOSUM_plot.R")






# ********************************************************************
# run 1
# ********************************************************************
plot.run1 <- bfmPlot_mod(run1, plotlabs = "Run 1",
                     ncols = 1, displayMagn = FALSE, displayResiduals = 'monperiod', displayOldFlag = TRUE,
                     circleVersion = TRUE, displayRefDate = TRUE, refDate = now.refDate) +
  theme_bw() + labs(y = "NDMI", x = "Date") + scale_y_continuous(limits = c(-0.2, 0.6)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank(),
          strip.background = element_blank(),
          strip.text.x = element_blank()
        ) +
  annotate("text", label = "(a)", x = as.Date("1989-01-01"), y = -0.15)

# ********************************************************************
# run 5
# ********************************************************************
plot.run5 <- bfmPlot_mod(run5, plotlabs = "Run 5",
                         ncols = 1, displayMagn = FALSE, displayResiduals = 'monperiod', displayOldFlag = TRUE,
                         circleVersion = TRUE, displayRefDate = TRUE, refDate = now.refDate) +
  theme_bw() + labs(y = "NDMI", x = "Date") + scale_y_continuous(limits = c(-0.2, 0.6)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_blank()) +
  annotate("text", label = "(b)", x = as.Date("1989-01-01"), y = -0.15)

# ********************************************************************
# run 6
# ********************************************************************
plot.run6 <- bfmPlot_mod(run6, plotlabs = "Run 6",
                         ncols = 1, displayMagn = FALSE, displayResiduals = 'monperiod', displayOldFlag = TRUE,
                         circleVersion = TRUE, displayRefDate = TRUE, refDate = now.refDate) +
  theme_bw() + labs(y = "NDMI", x = "Date") + scale_y_continuous(limits = c(-0.2, 0.6)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_blank())

# ********************************************************************
# run 27
# ********************************************************************
plot.run27 <- bfmPlot_mod(run27, plotlabs = "Run 27",
                         ncols = 1, displayMagn = FALSE, displayResiduals = 'monperiod', displayOldFlag = TRUE,
                         circleVersion = TRUE, displayRefDate = TRUE, refDate = now.refDate) +
  theme_bw() + labs(y = "NDMI", x = "Date") + scale_y_continuous(limits = c(-0.2, 0.6)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_blank())



# ********************************************************************
# run 5 MOSUM
# ********************************************************************
plot.run5.process <- plotMOSUM(run5, plotlabs = "Run 5",
                         ncols = 1, displayMagn = FALSE, displayResiduals = 'monperiod', displayOldFlag = TRUE,
                         circleVersion = TRUE, displayRefDate = TRUE, refDate = now.refDate) +
  theme_bw() + labs(y = "|MOSUM|", x = "Date") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0))) +
  annotate("text", label = "(c)", x = as.Date("1989-01-01"), y = 0.5)

# ********************************************************************
# run 27 boundary RMSE, k = 1 and k = factRMSE
# ********************************************************************
plot.run27.boundaryRMSE <- bfmPlot_mod(run27, plotlabs = "Run 27",
                          ncols = 1, displayMagn = FALSE, displayResiduals = 'monperiod', displayOldFlag = TRUE,
                          circleVersion = TRUE, displayRefDate = TRUE, refDate = now.refDate,
                          displayBoundaryRMSE = TRUE) +
  theme_bw() + labs(y = "NDMI", x = "Observation date") + scale_y_continuous(limits = c(-0.2, 0.6)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.background = element_blank(),
        strip.text.x = element_blank()) +
  annotate("text", label = "(d)", x = as.Date("1989-01-01"), y = -0.15)





# ********************************************************************
# Print the plot
# ********************************************************************
# plot.run1
# plot.run5
# plot.run5.process
# plot.run27
# plot.run27.boundaryRMSE


pdf(str_c(final.fig.path, "demo_algorithm_comparison.pdf"), 
    width = 7, height = 7.5, pointsize = 12)
multiplot(plot.run1,
          plot.run5,
          plot.run5.process,
          plot.run27.boundaryRMSE,
          cols = 1)
dev.off()

