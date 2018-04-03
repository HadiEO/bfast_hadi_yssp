# Checking internal calculation and functions

df1 <- data.frame(y=rnorm(300))
df1[150:300,"y"] <- df1[150:300,"y"]+1
me1 <- mefp(y~1, data=df1[1:50,,drop=FALSE], type="OLS-MOSUM", h=1,
            alpha=0.05)
me2 <- monitor(me1, data=df1)

x11()
plot(me2, boundary=FALSE)
lines(boundary(me2), col="green", lty="44") 

## set up time series
library(zoo)
ndvi <- as.ts(zoo(cbind(a = som$NDVI.a, b = som$NDVI.b), som$Time))
ndvi <- window(ndvi, start = c(2006, 1), end = c(2009, 23))

## parametric season-trend model
d1 <- bfastpp(ndvi, order = 2)
d1lm <- lm(response ~ trend + harmon, data = d1)
summary(d1lm)

## autoregressive model (after nonparametric season-trend adjustment)
d2 <- bfastpp(ndvi, stl = "both", lag = 1:2)
d2lm <- lm(response ~ lag, data = d2)
summary(d2lm)



## UK Seatbelt data: a SARIMA(1,0,0)(1,0,0)_12 model
## (fitted by OLS) is used and reveals (at least) two
## breakpoints - one in 1973 associated with the oil crisis and
## one in 1983 due to the introduction of compulsory
## wearing of seatbelts in the UK.
data("UKDriverDeaths")
seatbelt <- log10(UKDriverDeaths)
seatbelt <- cbind(seatbelt, lag(seatbelt, k = -1), lag(seatbelt, k = -12))
colnames(seatbelt) <- c("y", "ylag1", "ylag12")
seatbelt <- window(seatbelt, start = c(1970, 1), end = c(1984,12))
plot(seatbelt[,"y"], ylab = expression(log[10](casualties)))

## testing
re.seat <- efp(y ~ ylag1 + ylag12, data = seatbelt, type = "RE")
plot(re.seat)

## dating
bp.seat <- breakpoints(y ~ ylag1 + ylag12, data = seatbelt, h = 0.1,
                       breaks = NULL)
summary(bp.seat)
lines(bp.seat, breaks = 2)


# monitor() ---------------------------------------------------------------
# process
computeEmpProc <- function (X, y) {
  e <- as.vector(y - X %*% histcoef)  # length(e) = 73  # error = difference between historical model predictions and historical observations
  process <- rep(0, nrow(X) - K + 1)  # -K coz MOSUM(i, i+1, ..., i+K); +1 coz i starts from 0
  for (i in 0:(nrow(X) - K)) {
    process[i + 1] <- sum(e[(i + 1):(i + K)])    # starts from 0 allows MOSUM window of exactly K size i.e. (i+K)-(i+1)
  }
  process/(sigmahat * sqrt(histsize))    # The enumerator is the factor to standardize residuals
}



# obj$computeEmpProc
X <- x   # x has columns: (Intercept) trend    harmoncos    harmonsin
         # subset data to monitoring period ? it starts from 1988. Aha, if history = "all" basically monitors from the beginning.
y <- y   # y is reponse as matrix (N x 1)
histcoef <- obj$histcoef
histsize <- obj$histsize


# K is number of independent variables + 1 (the first component equal to unity) i.e. K <- 3 [Eq. 4 in strucchange white paper]
# or MOSUM window size? [Eq. 12] i.e. K = n*h, where n is number of dates (Eq.)
X <- x
y <- y
n <- obj$histsize
h <- h
K <- floor(n * h)
histcoef <- obj$histcoef
e <- as.vector(y - X %*% histcoef)
(process_idx <- ((0:(nrow(X) - K))+1)[-(1:length(obj$efpprocess))])     # This indicates search forward
e_process_idx <- e[process_idx]

df_residual <- length(process_idx) - length(histcoef) # or length(y) - length(histcoef) ?
sigmahat <- sqrt(sum(e^2)/df_residual)
mosum <- obj$process  * (sigmahat * sqrt(obj$histsize)) 

# obj$histsize+1 = 20
sum(e[20:23]) == mosum[20] # FALSE
sum(e[17:20]) == mosum[20] # FALSE


# Sum the e by h=4, forward
sum_e_h4_forward <- c()
for(i in 1:(length(e)-3)) {
  sum_e_h4_forward[i] <- sum(e[i:(i+3)]) 
}

# Sum the e by h=4, backward
sum_e_h4_backward <- c()
for(i in length(e):(1+3)) {
  sum_e_h4_backward[i] <- sum(e[i:(i-3)]) 
}
sum_e_h4_backward <- sum_e_h4_backward[-(1:3)]  # Remove first 3 NAs


# Plot
par(mfrow = c(3,1))
plot(1:length(e), e, xlim = c(0, length(e)), pch = 19, xaxp = c(1, 73, 72))
abline(v = seq(1, length(e), 1), col = "grey", lty = 2)
plot(1:(length(e)-3), sum_e_h4_forward, col = "dark orange", xlim = c(0, length(e)), pch = 19, xaxp = c(1, 73, 72))
abline(v = seq(1, length(e), 1), col = "grey", lty = 2)
plot((1+3):length(e), sum_e_h4_backward, col = "dark green", xlim = c(0, length(e)), pch = 19, xaxp = c(1, 73, 72))
abline(v = seq(1, length(e), 1), col = "grey", lty = 2)
# plot(process_idx, mosum, col = "red", xlim = c(0, length(e)), pch = 19)
# mosTime_idx <- (obj$histsize+1):nrow(data)   # This is search backward
# plot(mosTime_idx, mosum, col = "blue", xlim = c(0, length(e)), pch = 19)


# the x-axis (time index) for mosum plot is process_idx or (obj$histsize+1):nrow(data)
# Is the window backward or forward??? I DON'T KNOW HUHU :(
# **************************************************************************
# obj$breakpoint <- min(which(abs(obj$process) > boundary)) + obj$histsize
# **************************************************************************


# default_op <- par()
lo <- matrix(c(1:2), nr=2, nc=1)
layout(lo)
op <- par(mar = c(0, 5, 0, 5), oma = c(3, 3, 3, 3))
plot(data[process_idx, "time"], e_process_idx, col = "red", cex = 2, type = "b", main = "Residual", xlim = c(1999, 2016), xlab = '', xaxt = 'n')
grid()
plot(data[process_idx,"time"], mosum, cex = 2, col = "blue", type = "b", main = "MOSUM", xlim = c(1999, 2016))   
grid()
layout(1)
par(default_op)




# Outer
x <- 1:9; names(x) <- x
# Multiplication & Power Tables
x %o% x
y <- 2:8; names(y) <- paste(y,":", sep = "")
outer(y, x, "^")


# To show different lines in different facets, use aesthetics
temp <- ggplot(mtcars, aes(mpg, wt)) +
  geom_point() +
  facet_wrap(~ cyl)

mean_wt <- data.frame(cyl = c(4, 6, 8), wt = c(2.28, 3.11, 4.00))
temp + geom_hline(aes(yintercept = wt), mean_wt)


# create new dataframe
intercept <- data.frame(vs=c(rep(0, 2), rep(1, 2), rep(0,3), rep(1,3)), 
                        am = c(rep(0, 4), rep(1, 6)), 
                        int = c(10, 20, 10, 20, 15, 25, 35, 15, 25, 35))
# add vline to plot
p + geom_vline(aes(xintercept=int), intercept)




# switch()
arg <- "ROC"
switch(arg, 
       all = "history is 'all'", 
       ROC = "history is 'ROC'", 
       BP = "history is 'BP'")
