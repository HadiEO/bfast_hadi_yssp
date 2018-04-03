# Original code source: https://github.com/jreiche/bayts/blob/master/R/detectBayts.R

detectBayts <- function (bayts, chi = 0.5, PNFmin = 0.5, start = NULL, end = NULL) 
{
  #check for observations in the monitoring period with PNF >= PNFmin
  ind <- which(bayts$PNF >= PNFmin)
  ind <- ind[which(ind >= min(which(index(bayts) >= start)))]
  
  if (length(ind) > 0) { # HH: else bayts is returned as it was when given as input i.e. PChange, Flag = NA
    ################################
    # step 1: set start and end date
    if (is.null(start)) {st <- 1} else {st <- min(which(index(bayts) > start))}  # index of first obs after monitoring start date
    if (is.null(end)) {en <- length(index(bayts))} else {en <- max(which(index(bayts) < end))}
    # in case of start is beginning of "bayts" time series, a zero element is added
    if (st == 1) {
      bayts_zero <- bayts[1]
      index(bayts_zero) <- index(bayts_zero) - 0.1
      bayts_zero$PNF <- 0.5
      bayts <- rbind(bayts_zero, bayts)
      st <- st + 1
      en <- en + 1
      bayts$Flag[(st - 1)] <- "0"
    } else {
      bayts$Flag[(st - 1)] <- "0"
    }
    
    ####################
    # step 2: Monitoring
    if (en >= st) {
      bayts$Flag[which(bayts$PNF<PNFmin)]<-0         # HH: flag NA -> 0, if PNF less than 0.5
      for (r in 1:length(ind)){
        for (t in ind[r]:en) {
          #############################################################
          # step 2.1: Update Flag and PChange for current time step (t)
          # (case 1) No confirmed or flagged change: 
          if (bayts$Flag[(t - 1)] == "0" || bayts$Flag[(t - 
                                                        1)] == "oldFlag") {   # 
            i <- 0
            prior <- as.double(bayts$PNF[t - 1])
            likelihood <- as.double(bayts$PNF[t])
            postieror <- (prior * likelihood)/((prior * 
                                                  likelihood) + ((1 - prior) * (1 - likelihood)))
            bayts$Flag[t] <- "Flag"
            bayts$PChange[t] <- postieror
          }
          # (case 2) Flagged change at preveous time step: update PChange
          if (bayts$Flag[(t - 1)] == "Flag") {
            prior <- as.double(bayts$PChange[t - 1])
            likelihood <- as.double(bayts$PNF[t])
            postieror <- (prior * likelihood)/((prior * likelihood) + 
                                                 ((1 - prior) * (1 - likelihood)))
            bayts$PChange[t] <- postieror
            bayts$Flag[t] <- "Flag"
            i <- i + 1
          }
          ###############################################
          # step 2.2: Confirm and reject flagged changes
          if (bayts$Flag[(t)] == "Flag") {
            if ((i > 0)) {
              if ((as.double(bayts$PChange[t])) < 0.5) {
                bayts$Flag[(t - i):t] <- 0
                bayts$Flag[(t - i)] <- "oldFlag"
                break 
              }
            }
            # confirm change in case PChange >= chi
            if ((as.double(bayts$PChange[t])) >= chi) {
              if ((as.double(bayts$PNF[t])) >= 0.5) {
                bayts$Flag[min(which(bayts$Flag == "Flag")):t] <- "Change"  # min(which(bayts$Flag == "Flag")) is "flagged" change, t is time of "confirmed" change
                return(bayts)
              }
            }
          } # end of loop L58
        }
      }
    }
  }
  return(bayts)
}