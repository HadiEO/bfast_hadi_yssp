plot_bfast01_2 <- function(x, breaks = NULL, which = c("response", "fitted", "trend"),
                         plot.type = "single", panel = NULL, screens = NULL,
                         col = NULL, lwd = NULL,
                         main = "", xlab = "Time", ylab = NULL, ci = NULL, regular = TRUE, ...)
{
  ## set up zoo series and select series to be plotted
  breaks <- .breaks01(x, breaks)
  z <- as.zoo(x, breaks = breaks)
  which <- sapply(which, function(x) match.arg(x, colnames(z)))
  z <- z[, which]
  
  ## try making intelligent guesses about default col/lwd
  plot.type <- match.arg(plot.type, c("single", "multiple"))
  if(is.null(col)) {
    col0 <- c("gray", "black", "blue", "red", "green", "black")
    if(plot.type == "single") {
      col <- col0
      names(col) <- c("response", "fitted", "trend", "season", "reg", "residuals")
      col <- col[which]
    } else {
      col <- if(is.null(screens)) 1 else unlist(lapply(unique(screens),
                                                       function(i) if((n <- sum(screens == i)) == 1) "black" else rep(col0, length.out = n)))
    }
  }
  if(is.null(lwd)) {
    if(plot.type == "single") {
      lwd <- c(2, 1, 2, 1, 1, 1)
      names(lwd) <- c("response", "fitted", "trend", "season", "reg", "residuals")
      lwd <- lwd[which]
    } else {
      lwd <- 1
    }
  }
  
  ## default y-axis labels
  if(is.null(ylab)) {
    ylab <- which
    for(i in seq_along(ylab)) substr(ylab[i], 1, 1) <- toupper(substr(ylab[i], 1, 1))
    if(plot.type == "single") {
      ylab <- paste(ylab, collapse = " / ")
    } else {
      if(!is.null(screens)) ylab <- sapply(unique(screens), function(i) paste(ylab[screens == i], collapse = " / "))
    }
  }
  
  ## set up panel function with confidence intervals
  if(is.null(panel)) panel <- .make_confint_lines01(x, breaks = breaks, ci = ci)
  
  if(regular) z <- as.zoo(as.ts(z))
  
  plot(z, plot.type = plot.type, panel = panel, screens = screens,
       col = col, lwd = lwd, main = main, xlab = xlab, ylab = ylab, ...)
  points()
}


#######################################################################################

.breaks01 <- function(object, breaks) {
  if(is.null(breaks)) breaks <- object$breaks
  breaks <- breaks[1]
  if(!breaks %in% 0:1) stop("breaks can only be 0 or 1")
  breaks
}

.make_confint_lines01 <- function(object, breaks = NULL, col = 1, lty = 2, lwd = 1, ci = list(), ...)
{
  breaks <- .breaks01(object, breaks)
  if(breaks < 1) return(lines)
  
  function(x, y, ...) {
    lines(x, y, ...)
    abline(v = breakdates(object, breaks = breaks), lty = lty, col = col, lwd = lwd)
    if(!identical(ci, FALSE)) {
      if(!is.list(ci)) ci <- list()
      if(is.null(ci$col)) ci$col <- 2
      if(is.null(ci$angle)) ci$angle <- 90
      if(is.null(ci$length)) ci$length <- 0.05
      if(is.null(ci$code)) ci$code <- 3
      if(is.null(ci$at)) {
        at <- par("usr")[3:4]
        at <- diff(at)/1.08 * 0.02 + at[1]
      } else {
        at <- ci$at
      }
      ci$at <- NULL
      do.call("arrows", c(list(
        x0 = object$data$time[object$confint[1]],
        y0 = at,
        x1 = object$data$time[object$confint[3]],
        y1 = at),
        ci))
    }
  }
}

.confint01 <- function(object, level = 0.95, het.reg = TRUE, het.err = TRUE)
{
  ## data and arguments
  X <- model.matrix(object$model[[1]])
  y <- model.response(model.frame(object$model[[1]]))
  n <- nrow(object$data)
  a2 <- (1 - level)/2
  bp <- c(0, object$breakpoints, n)
  
  ## auxiliary functions
  myfun <- function(x, level = 0.975, xi = 1, phi1 = 1, phi2 = 1)
    (pargmaxV(x, xi = xi, phi1 = phi1, phi2 = phi2) - level)
  myprod <- function(delta, mat) as.vector(crossprod(delta, mat) %*% delta)
  
  ## overall fits
  res <- residuals(object$model[[2]])
  beta <- coef(object, breaks = 1)
  sigma1 <- sigma2 <- sum(res^2)/n
  Q1 <- Q2 <- crossprod(X)/n
  Omega1 <- Omega2 <- sigma1 * Q1
  
  ## subsample fits
  X1 <- X[(bp[1]+1):bp[2],,drop = FALSE]
  X2 <- X[(bp[2]+1):bp[3],,drop = FALSE]
  y1 <- y[(bp[1]+1):bp[2]]
  y2 <- y[(bp[2]+1):bp[3]]
  beta1 <- beta[1,]
  beta2 <- beta[2,]
  if(het.reg) {
    Q1 <- crossprod(X1)/nrow(X1)
    Q2 <- crossprod(X2)/nrow(X2)
  }
  if(het.err) {
    sigma1 <- sum(res[(bp[1]+1):(bp[2])]^2)/nrow(X1)
    sigma2 <- sum(res[(bp[2]+1):(bp[3])]^2)/nrow(X2)
    Omega1 <- sigma1 * Q1
    Omega2 <- sigma2 * Q2
  }
  delta <- beta2 - beta1
  
  Oprod1 <- myprod(delta, Omega1)
  Oprod2 <- myprod(delta, Omega2)
  Qprod1 <- myprod(delta, Q1)
  Qprod2 <- myprod(delta, Q2)
  
  xi <- if(het.reg) Qprod2/Qprod1 else 1
  phi1 <- sqrt(Oprod1/Qprod1)
  phi2 <- sqrt(Oprod2/Qprod2)
  
  p0 <- pargmaxV(0, phi1 = phi1, phi2 = phi2, xi = xi)
  if(is.nan(p0) || p0 < a2 || p0 > (1-a2)) {
    warning(paste("Confidence interval cannot be computed: P(argmax V <= 0) =", round(p0, digits = 4)))
    upper <- NA
    lower <- NA
  } else {
    ub <- lb <- 0
    while(pargmaxV(ub, phi1 = phi1, phi2 = phi2, xi = xi) < (1 - a2)) ub <- ub + 1000
    while(pargmaxV(lb, phi1 = phi1, phi2 = phi2, xi = xi) > a2) lb <- lb - 1000
    
    upper <- uniroot(myfun, c(0, ub), level = (1-a2), xi = xi, phi1 = phi1, phi2 = phi2)$root
    lower <- uniroot(myfun, c(lb, 0), level = a2, xi = xi, phi1 = phi1, phi2 = phi2)$root
    
    upper <- upper * phi1^2 / Qprod1
    lower <- lower * phi1^2 / Qprod1
  }
  
  bp <- c(bp[2] - ceiling(upper), bp[2], bp[2] - floor(lower))
  a2 <- round(a2 * 100, digits = 1)
  names(bp) <- c(paste(a2, "%"), "breakpoints", paste(100 - a2, "%"))
  bp
}