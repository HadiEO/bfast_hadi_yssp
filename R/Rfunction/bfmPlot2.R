bfmPlot2 <- function (bfm, plotlabs = NULL, ncols = 1, rescale = 1, ylab = "response", 
          displayMagn = FALSE, magn_ypos = 0.3, magn_xoffset = -0.45, 
          magn_digits = 3, displayTrend = TRUE, displayResiduals = c("none", 
                                                                     "all", "monperiod", "history"), type = "irregular") 
{
  allData <- bfmPredict(bfm, type = type, plotlabs = plotlabs)
  xbks <- as.Date(c(floor(min(allData$time)):ceiling(max(allData$time))))
  p <- ggplot(data = allData, aes(x = as.Date(time), y = response)) + 
    geom_point(na.rm = TRUE) + geom_line(aes(y = prediction), 
                                         col = "blue", na.rm = TRUE) + labs(y = ylab) + scale_x_continuous(breaks = xbks) + 
    geom_vline(aes(xintercept = start), na.rm = TRUE)
  if (length(levels(allData$lab) > 1)) 
    p <- p + facet_wrap(~lab, ncol = ncols)
  if (!all(is.na(unique(allData$breakpoint)))) {
    p <- p + geom_vline(aes(xintercept = breakpoint), na.rm = TRUE, 
                        col = "red", lty = 2)
  }
  if (displayTrend) {
    p <- p + geom_line(aes(y = predictionTrend), col = "blue", 
                       lty = 2, na.rm = TRUE)
  }
  if (displayMagn) {
    magn_ypos <- min(allData$response, na.rm = TRUE) + magn_ypos * 
      diff(range(allData$response, na.rm = TRUE))
    magns <- unique(allData$magnitude)
    xpos <- unique(allData$start) + magn_xoffset
    magn <- data.frame(magn = round(magns * rescale, magn_digits), 
                       x = xpos, y = magn_ypos, lab = unique(allData$lab))
    p <- p + geom_text(data = magn, aes(x = x, y = y, label = paste("m = ", 
                                                                    magn, sep = ""), group = NULL), size = 5)
  }
  if (displayResiduals[1] != "none" & ("monperiod" %in% displayResiduals | 
                                       "all" %in% displayResiduals)) {
    p <- p + geom_segment(data = allData[allData$time >= 
                                           allData$start, ], aes(x = time, xend = time, y = response, 
                                                                 yend = prediction), col = "grey", lty = 5, na.rm = TRUE)
  }
  if (displayResiduals[1] != "none" & ("history" %in% displayResiduals | 
                                       "all" %in% displayResiduals)) {
    p <- p + geom_segment(data = allData[allData$time < 
                                           allData$start, ], aes(x = time, xend = time, y = response, 
                                                                 yend = prediction), col = "grey", lty = 5, na.rm = TRUE)
  }
  return(p)
}
