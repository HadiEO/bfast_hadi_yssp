# Original code source: https://github.com/jreiche/bayts/blob/master/R/createBayts.R

createBayts <- function (tsL=list(NULL,...), pdfL=list(NULL,...), bwf = c(0.1, 0.9)) {
  
  #######################################
  # step 1: add time series to data frame
  
  #number of ts
  l <- length(tsL)
  #get first ts as zoo object
  ts1 <- merge.zoo(na.omit(as.zoo(tsL[[1]])))
  
  #add remaining ts as zoo objects
  if (l>1){
    for(i in 2:l){
      #if ts is empty add empty ts
      if (length(na.omit(as.zoo(tsL[[i]])))==0){
        ts2 <- as.double(merge.zoo(na.omit(as.zoo(tsL[[1]]))))
        ts2[!is.na(ts2)] <- NA
        ts1 <- merge.zoo(ts1, ts2)
        names(ts1)[i] <- paste("ts", i, sep = "")
        #otherwise add ts
      } else {
        ts1 <- merge.zoo(ts1, na.omit(as.zoo(tsL[[i]])))
        names(ts1)[i] <- paste("ts", i, sep = "")
      }
    }
  }
  
  ##################################################################
  # step 2: calc conditional non-forest probability (PNF) 
  #         update PNF using Bayesian updating in case of multiple 
  #         observations at same date
  
  #calc PNF for the first ts and add
  PNF <- calcPNF(na.omit(as.zoo(tsL[[1]])), pdfL[[1]], bwf)
  ts1 <- merge.zoo(ts1,PNF)
  
  #calc PNF for remaining ts &
  #update PNF in case of multiple observations at the same date
  if (l>1){
    for(i in 2:l){
      ts2 <- merge.zoo(ts1, calcPNF(ts1[,i], pdfL[[i]], bwf))
      names(ts2)[l+2] <- paste("PNF2")
      #updating PNF using Bayesioan updating
      ts1$PNF[which(!is.na(ts2$PNF)==!is.na(ts2$PNF2))] <- calcPosterior(ts2$PNF[which(!is.na(ts2$PNF)==!is.na(ts2$PNF2))],ts2$PNF2[which(!is.na(ts2$PNF)==!is.na(ts2$PNF2))])
      ts1$PNF[is.na(ts1$PNF)] <- ts2$PNF2[is.na(ts1$PNF)]
      remove(ts2)
    }
  }
  
  ######################################################
  # step 3: add empty flag and Pchange row to data frame
  
  Flag <- ts1[, 1]
  Flag[!is.na(Flag)] <- NA
  PChange <- ts1[, 1]
  PChange[!is.na(PChange)] <- NA
  ts1 <- merge(ts1, PChange, Flag)
  
  return(ts1)
}