plot.tsreg.mod <- function (x, ylabs = c("data", "|MOSUM|"), legend = TRUE) 
{
  lo <- matrix(c(1:2), nr = 2, nc = 1)
  layout(lo)
  # op <- par(mar = c(0, 5, 0, 5), oma = c(3, 3, 3, 3))                        # Modify OP
  op <- par(mar = c(0, 4, 0, 3), oma = c(2, 0, 0.5, 0))  
  # op <- par(oma = c(0, 0, 0, 0), mai = c(0, 0.35, 0, 0.1),                # mai = c(0.45, 0.35, 0.1, 0.1)
  #           ps = 12, mgp = c(1.7, 0.5, 0), mar =c(2.7, 2.6, 0.5, 1.5))
  
  plot(x$data, xlab = "", xaxt = "n", ylab = ylabs[1])
  lines(x$fit, col = "blue")
  points(x$data[time(x$data) >= min(time(x$fit)) & time(x$data) <= 
                  max(time(x$fit))], type = "p", pch = "*", cex = 0.7, 
         col = "blue")
  if (x$start != x$disturbance) {
    abline(v = as.numeric(x$disturbance), lty = 2, col = "red")
    abline(v = as.numeric(x$start), lty = 2)
  }
  else {
    abline(v = as.numeric(x$start), lty = 2)
  }
  if (!is.na(x$regrowth_onset) & x$prereg_check > 0) 
    abline(v = as.numeric(x$regrowth_onset), col = "blue", 
           lty = 3)
  ymax <- max(abs(x$MOSUM), x$bound)
  plot(x$data, col = "white", yaxt = "n", ylim = c(min(abs(x$MOSUM)), 
                                                   ymax), ylab = ylabs[2])
  lines(abs(x$MOSUM), yaxt = "n", lty = 2, col = "blue")
  lines(zoo(x$bound, time(x$MOSUM)), col = "green")
  if (x$start != x$disturbance) {
    abline(v = as.numeric(x$disturbance), lty = 2, col = "red")
    abline(v = as.numeric(x$start), lty = 2)
  }
  else {
    abline(v = as.numeric(x$start), lty = 2)
  }
  if (!is.na(x$regrowth_onset) & x$prereg_check > 0) 
    abline(v = as.numeric(x$regrowth_onset), col = "blue", 
           lty = 3)
  axis(4)
  if (legend) {
  }
  layout(1)
  par(op)
}