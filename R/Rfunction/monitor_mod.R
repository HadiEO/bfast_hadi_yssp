obj <- test_mefp


monitor_mod <- function (obj, data = NULL, verbose = TRUE) 
{
  if (!is.na(obj$breakpoint)) 
    return(TRUE)
  if (missing(data)) {
    if (is.null(obj$data)) {
      data <- list()
    }
    else {
      data <- get(obj$data)
    }
  }
  mf <- model.frame(obj$formula, data = data)
  y <- as.matrix(model.response(mf))
  modelterms <- terms(obj$formula, data = data)
  x <- model.matrix(modelterms, data = data)
  if (nrow(x) <= obj$last) 
    return(obj)
  if (nrow(x) != nrow(y)) 
    stop("response and regressors must have the same number of rows")
  if (ncol(y) != 1) 
    stop("multivariate response not implemented yet")
  foundBreak <- FALSE
  if ((obj$type == "OLS-MOSUM") | (obj$type == "OLS-CUSUM")) {
    if (obj$type == "OLS-CUSUM") {
      obj$process <- obj$computeEmpProc(x, y)[-(1:obj$histsize)]     # computeEmpProc(x, y) is a function?
    }
    else {   # else (obj$type == "OLS-MOSUM")
      obj$process <- obj$computeEmpProc(x, y)[-(1:length(obj$efpprocess))]     # What is the ( ) indexing?
    }
    boundary <- obj$border((obj$histsize + 1):nrow(x))                  # boundary; border() is a function?
    obj$statistic <- max(abs(obj$process))
    if (!foundBreak & any(abs(obj$process) > boundary)) {
      foundBreak <- TRUE
      obj$breakpoint <- min(which(abs(obj$process) > boundary)) +        # process > boundary
        obj$histsize
      if (verbose) 
        cat("Break detected at observation #", obj$breakpoint, 
            "\n")
    }
    obj$lastcoef <- NULL
  }
  else {
    for (k in (obj$last + 1):nrow(x)) {
      newestims <- obj$computeEstims(x, y, k)
      obj$process <- rbind(obj$process, obj$computeEmpProc(newestims$coef, 
                                                           newestims$Qr12, k))
      stat <- obj$computeStat(obj$process)
      obj$statistic <- c(obj$statistic, stat)
      if (!foundBreak & (stat > obj$border(k))) {
        foundBreak <- TRUE
        obj$breakpoint <- k
        if (verbose) 
          cat("Break detected at observation #", k, 
              "\n")
      }
    }
    obj$lastcoef <- newestims$coef
  }
  obj$last <- nrow(x)
  obj$call <- match.call()
  obj
}
