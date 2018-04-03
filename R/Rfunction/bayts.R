# Original source code: https://github.com/jreiche/bayts/blob/master/R/bayts.R


bayts <- function(tsL=list(NULL,...), pdfL=list(NULL,...), bwf=c(0.1, 0.9), chi=0.9, PNFmin=0.5, start=NULL, end=NULL){
  
  bayts <- createBayts(tsL=tsL, pdfL=pdfL, bwf=bwf)
  
  bayts <- detectBayts(bayts, chi=chi, PNFmin=PNFmin, start=start, end=end)  
  # HH: this returns bayts data frame but with columns PChange and Flag updated
  
  #create output list
  baytsL <- list(
    bayts = bayts,
    flag = index(bayts[min(which(bayts$Flag=="Flag"))]),
    change.flagged = index(bayts[min(which(bayts$Flag=="Change"))]),    # HH: min
    change.confirmed = index(bayts[max(which(bayts$Flag=="Change"))]),  # HH: max
    oldflag = index(bayts[which(bayts$Flag=="oldFlag")]),
    vchange = na.omit(bayts$PChange[which(bayts$Flag=="Change")]),
    vflag = na.omit(bayts$PChange[which(bayts$Flag=="Flag")])
  )
  return(baytsL)
}
