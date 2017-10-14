makeMonthlyTs <- function(x){
  temp.xts <- xts(as.numeric(x), 
                  date_decimal(index(x)))
  temp.month <- apply.monthly(temp.xts, FUN=mean)
  # Need to convert back to ts for bfast
  temp.month.ts <- ts(as.numeric(temp.month), start = c(year(start(temp.month)),month(start(temp.month))), 
                      frequency = 12)     # Freq = 12 cause monthly, 23 if 16-days
  return(temp.month.ts)
}