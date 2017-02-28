ComputeQuartersFromIXDAYS <- function(x) {
  
  
  x$QUARTER <- ceiling(as.numeric(x$IXDAYS)/90)
  
  # shift history by -1, so that:
  # old value "0" gets "-1"
  # old value "-1" gets "-2"
  # ...
  # this allows an easier intepretation
  
  history   <- x$QUARTER <= 0
  x$QUARTER[history] <- x$QUARTER[history]-1
  
  # substitue the minus "-" with "m"
  x$QUARTER <- gsub("-","m",x$QUARTER)
  
  return(x)
}