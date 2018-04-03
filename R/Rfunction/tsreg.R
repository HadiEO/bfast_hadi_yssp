# Code source: https://github.com/bendv/rgrowth/blob/master/R/tsreg.R

tsreg <- function(x, change, h, dates = NULL, type = c("irregular", "16-day"), startOffset = 0, formula=response~harmon, order=1, history='BP', level=0.05, w = 3, s = 1, plot = FALSE, ylabs = c('data', '|MOSUM|')) {
  
  # make a regularlized bfastts
  type <- type[1]                                   # HH: Aha, so "16-day" is not yet implemented
  if(is.null(dates)) {
    z <- bfastts(x, dates = time(x), type = type)
  } else {
    z <- bfastts(x, dates, type = type)
  }
  
  # error message (to be included in output)
  err <- NULL
  
  # set start period
  if(startOffset == "floor"){
    start <- floor(change)
  } else {
    start <- change - startOffset                  # HH: so startOffset is backward in time?
  }
  
  # derive history model parameters
  df <- bfastpp(z, order = order)        
  history_pp <- stableHistory(df, start=start, formula=formula, history=history, level=level, hpc="none")
  mod <- lm(formula=formula, data=history_pp)
  
  # predict values for all times
  df$prediction <- predict(mod, newdata=df)
  df$prediction[df$time < min(history_pp$time)] <- NA
  df$residual <- df$response - df$prediction
  
  # initiate mefp with historical data
  y <- ts(df$residual[which(df$time < start)])
  m <- mefp(y ~ 1, type = "OLS-MOSUM", h = h)
  
  # add all data
  y <- ts(df$residual)
  m <- monitor(m, verbose = FALSE)                                  # HH: monitor()
  
  # just take lowest bound for now (this can change over time)
  bound <- min(boundary(m))
  
  # subset df$time to overlap with abs(m$process)
  mostime <- df$time[which(df$time >= start)]            # HH: mostime = mosum time i.e. monitoring time
  
  ### conditions for reg to be assigned:
  # 1) (tR - tB) >= w
  suppressWarnings(reg1 <- min(mostime[which(mostime >= change + w & abs(m$process) < bound)])) # HH: here, process < boundary because it is detecting REGROWTH
  if(reg1 == Inf | is.na(reg1) | length(reg1) == 0) {
    reg1 <- NA
    err <- "no regrowth detected"
  }
  
  # 2) stable period after tR >= S
  ## for how long do MOSUMS stay under bound?
  ## choose the first 'stable' segment with duration > s
  ### TODO: can we speed this up by combining this with condition (1)?
  if(!is.na(reg1)){
    
    ## get time segments where abs(m$process) < bound
    st <- abs(m$process[which(mostime >= reg1)]) < bound  # HH: reg1 is the time when process < bound is firstly detected, after w start offset years
                                                          # HH: st is vector of TRUE or FALSE
    if(!all(st) & !all(!st) & length(st) > 0) {           # HH: doesn't allow all obs during monitoring period to have process < bound
      seg <- lapply(rle(st)$lengths, seq_len)
      for(i in 2:length(seg))
        seg[[i]] <- seg[[i]] + seg[[i-1]][length(seg[[i-1]])]                # HH: segment obs index (order)
      segt <- lapply(seg, FUN=function(x) mostime[mostime > reg1][x])        # HH: segment time
      segdur <- sapply(segt, FUN=function(x) x[length(x)] - x[1])            # HH: segment duration (time difference)
      finalseg <- min(which(segdur > s & rle(st)$values))                    # HH: rle(st)$values is to check the time is not 0 or NA ??
      ## TODO: fix bug in above line:
      ## tsreg(ndmi, change=2005, h=0.25)
      ## Warning message:
      ##In min(which(segdur > s & rle(st)$values)) :
      ##no non-missing arguments to min; returning Inf
      
      if(finalseg != Inf) {
        reg2 <- segdur[finalseg]
        reg1 <- min(segt[[finalseg]]) ## revised reg1
      } else {
        reg2 <- NA
        err <- "stability parameter (s) not computed"
      }
    } else if(all(!st)) {
      reg2 <- NA
      err <- "no regrowth detected (2)"
    } else {
      sttime <- mostime[which(mostime > reg1)]
      reg2 <- sttime[length(sttime)] - sttime[1]
    }
  } else {
    reg2 <- NA
  }
  
  if(!is.na(reg2) & reg2 < s) {
    reg1 <- reg2 <- NA
    err <- "stability criterion not met"
  }
  
  # 3) at least some MOSUMs between tB and (tB + tR) are greater than bound
  if(!is.na(reg1)){
    reg3 <- sum(abs(m$process[which(mostime > change & mostime < reg1)]) > bound) / length(m$process[which(mostime > change & mostime < reg1)])
    # expressed as a proportion of total observations in that time
    ## FIX ME: use w (fails if set to 0) or reg1
  } else {
    reg3 <- NA
  }
  
  # object to return
  res <- list(start=start,
              disturbance=change,
              regrowth_onset=reg1,       #
              s=reg2,                    #
              prereg_check=reg3,         #
              data=zoo(z[!is.na(z)], time(z)[!is.na(z)]),
              fit=zoo(df$prediction[!is.na(df$prediction)], df$time[!is.na(df$prediction)]),
              MOSUM=zoo(m$process, mostime),
              bound=bound,
              error_message=err)
  
  class(res) <- "tsreg"
  
  # plot
  if(plot) 
    plot(res)
  
  return(res)
}