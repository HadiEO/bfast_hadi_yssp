
# Attempt 1 ---------------------------------------------------------------

for(i in mosTime_idx[1:(length(mosTime_idx)-cons+1)]) {
  nowIdx <- i:(i+cons-1)
  nowProcess <- obj$dataDf$process_upd[nowIdx]
  nowBoundary <- obj$dataDf$boundary[nowIdx]
  nowCond <- abs(nowProcess) > nowBoundary
  if(all(nowCond) == TRUE) {
    timeSpan <- (obj$dataDf$time[nowIdx[length(nowIdx)]]) - (obj$dataDf$time[nowIdx[1]])
    if(timeSpan <= maxTimeSpan) {
      obj$dataDf$flag[nowIdx] <- "change"                      # change at time i
      break                                                           # detect one earliest event only
    }
  } else {
    trueIdx <- nowIdx[which(nowCond == TRUE)]
    obj$dataDf$flag[trueIdx] <- "oldFlag"
    # Omit the oldFlag, recompute process. See R/Rfunction/computeEmpProc_mod.R for more details.
    y[trueIdx] <- NA  # Should also update obj$dataDf$response?
    obj$dataDf$response_upd[trueIdx] <- NA
    $process_upd[mosTime_idx] <- obj$computeEmpProc_mod(x, y)[-(1:length(obj$efpprocess))] 
  }
} # end for(i) loop



# Attempt 2 ---------------------------------------------------------------


# for(i in mosTime_idx[1:(length(mosTime_idx)-cons+1)]) {
for(i in 20:31) {
  nowIdx <- i:(i+cons-1)
  nowProcess <- obj$dataDf$process_upd[nowIdx]
  nowBoundary <- obj$dataDf$boundary[nowIdx]
  nowCond <- abs(nowProcess) > nowBoundary
  # Confirm change if all consecutive obs are anomalies
  if(all(nowCond) == TRUE) {
    timeSpan <- (obj$dataDf$time[nowIdx[length(nowIdx)]]) - (obj$dataDf$time[nowIdx[1]])
    if(timeSpan <= maxTimeSpan) {
      obj$dataDf$flag[nowIdx] <- "change"                      # change at time i (min() i.e. earliest change flag in daat frame)
      break                                                    # detect one earliest event only
    }
  }
  
  # Mark as "olfFlag" if obs i (first in current window) is anomaly but the consecutive ones are not anomalies (else to above if)
  if(nowCond[1] == TRUE) {
    obj$dataDf$flag[i] <- "oldFlag"
    # Omit the oldFlag, recompute process. See R/Rfunction/computeEmpProc_mod.R for more details.
    y[i] <- NA  # y is used in computeEmpProc_mod()
    obj$dataDf$response_upd[i] <- NA
    obj$dataDf$process_upd[mosTime_idx] <- obj$computeEmpProc_mod(x, y)[-(1:length(obj$efpprocess))]
  }
  
} # end for(i) loop



