# Original code source: https://github.com/hamun001/STEF/blob/master/R/removedips.R

# Checked, this is code for each time series i.e. observation time sequence i, c, b
# Update: maybe need to modify so to search temporal neighbours up to 8 days i.e. c-8, c, c+8 ?
# 8-days certainly not enough as date between valid observations is spread out > 8 days
# So best thing to try now is to make the function works with zoo ts object.
# But doesn't make sense to use neighbouring observation at say >1 year gap!
# Write removedips_mod below

# Update: this may not be the most updated version on github ********************************
removedips <- function (x) {
  # Convert to t (regular) ts object if zoo (irregular)
  if(class(x) == "zoo") {
    x <- bfastts(x, dates = time(x), type = "irregular")
  }
  
  
  #x <- na. approx (x, rule = 2)
  y <- as.numeric (x)
  leng <- length (x) - 2
  
  for(i in 1: leng )
  {
    ## moving window - check distance
    b <- i + 2
    c <- b - 1
    if(any(is.na(x[b]) ,is.na(x[c]) ,is.na(x[i])))
      next
    mida <- x[c] - x[i]
    midc <- x[c] - x[b]
    
    # Find 20 percent
    threshold1 <- ( -1/100) * x[i] # Original: threshold1 <- ( -1/100) * x[i]
    threshold2 <- ( -1/100) * x[b]
    # check threshold
    
    if( mida < 0 & midc < 0 & mida < threshold1 | midc < threshold2 ) {
      y[c] <- (x[b] + x[i]) / 2}
  }
  
  yts <- ts(data = y, start = start(x), frequency = frequency(x))
  
  return (yts)
  
}


# Modify:
# (a) search until it finds non-NA neighbour in the raw (irregular) time series
# Fig. 5 in Hamunyela et al. (2016) seems to indicate the immediate neighbours are non-NA neighbours beyond 
# (t-1) and (t+1). In other words, t is t-th observation order.
# (b) search non-NA neighbour within n years

removedips_mod <- function (x, updateX, searchWindow) {
  # Convert to t (regular) ts object if zoo (irregular)
  if(class(x) == "zoo") {
    x <- bfastts(x, dates = time(x), type = "irregular")
  }
  
  #x <- na. approx (x, rule = 2)
  y <- as.numeric(x)
  
  leng <- length(x) - 2
  for(i in 1: leng) {                           # The loop <<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    ## moving window - check distance
    b <- i + 2
    c <- b - 1
    if(is.na(x[c]))  next            # if midpoint value is NA, loop the next date
    
    while(is.na(x[i])) {         # While the left neighbour is NA, go back in time until find non-NA
      i <- i-1
      if(i == 0)
        break
    }
    
    if(i == 0) next
    
    while(is.na(x[b])) {
      b <- b+1
      if(b == length(x)) break
    }
    
    if(b == length(x)) next
    
    mida <- x[c] - x[i]
    midc <- x[c] - x[b]
    
    # Find 20 percent
    threshold1 <- ( -1/100) * x[i] # Original: threshold1 <- ( -1/100) * x[i]
    threshold2 <- ( -1/100) * x[b]
    # check threshold
    if( mida < 0 & midc < 0 & mida < threshold1 | midc < threshold2 ) {
      if( ((index(x)[c]-index(x)[i]) >= searchWindow) |         # If exceeded search window.
          ((index(x)[b]-index(x)[c]) >= searchWindow) ) {       # index(x) is decimal date, so diff=1 means 1 year     
        y[c] <- NA
      } else {
        y[c] <- (x[b] + x[i]) / 2 
      }
      
      if(updateX) x[c] <- y[c]        # update the value in ts before assessing next obs
    }
  }

  # Thinking corner:  
  # In the above: maybe just make NA? i.e. remove dips, not replace dips with average of neighbours
  # No, it removes disturbance signal points too
  # But this would mean no gap-filling.
  # But if we know it's ephemeral noise, then we know there is no change at the noisy date,
  # That it maybe ok to interpolate from the two neighbour dates
  # To be safe just do what has been published i.e. peer-reviewed!
  
  yts <- ts(data = y, start = start(x), frequency = frequency(x))
  
  return (yts)
  
}

