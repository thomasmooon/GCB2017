GetHOSPFL.DAYS <- function(x) {
  # get sum of time-unique-HOSPFL events per patient per quarter
  
  cat("Computing HOSPFL DAYS feature ....","\n")
  hospfl <- 
    x %>% 
    filter(HOSPFL == TRUE) %>% 
    dplyr::select(ENROLID, QUARTER, IXDAYS, DAYS) %>% distinct()
  
  hospfl <- aggregate(DAYS ~., hospfl %>% dplyr::select(-IXDAYS), FUN = sum) # exclude IXDAYS because you want to sum over it
  
  # create feature name
  hospfl$colnames <- paste("HOSPFL.DAYS", hospfl$QUARTER, sep = ".x.")
  hospfl <- hospfl %>% dplyr::select(-QUARTER) 
  
  # create QUARTQTY features: spread rows into columns 
  cat("Reshape data ....","\n")
  hospfl   <- spread(hospfl, key = colnames, val = DAYS, fill = 0)
  
  return(hospfl)
  
}