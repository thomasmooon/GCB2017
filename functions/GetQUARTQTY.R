
GetQUARTQTY <- function(patient_data, onlyAEDs = TRUE) {
  require(parallel)
  
  #######################
  getQUARTQTY.s <- function(x){
    
    
    # shift history by -1, so that:
    # old value "0" gets "-1"
    # old value "-1" gets "-2"
    # ...
    # this allows an easier intepretation
      quarters          <- ceiling((x$START:x$END)/90)
      history           <- quarters <= 0
      quarters[history] <- quarters[history]-1
    
    dosage.quarter.daily <- rep(x$DAYQTY, length(quarters))
    dosage.quarter.daily <- cbind(quarter = quarters, QUARTQTY = dosage.quarter.daily, row.names = NULL)
    dosage.quarter       <- aggregate(QUARTQTY ~ quarter, data=dosage.quarter.daily, sum )  
    dosage.quarter       <- aggregate(QUARTQTY ~ quarter, data=dosage.quarter.daily, sum )
    QUARTQTY             <- data.frame(cbind(x$ENROLID,x$SUBSTANCENAME,dosage.quarter, row.names=NULL), stringsAsFactors = FALSE)
    return(QUARTQTY)
  }
  #######################
  
  # preprocessing
  cat("Preprocessing ....","\n")
  
  if(onlyAEDs) {
    cat("   - only AEDs ....","\n")
    data <- 
      patient_data %>%
      filter(isAED == TRUE) %>%
      filter(METQTY > 0) %>% # 0 omitted to avoid aggregation() errors
      mutate(DAYQTY = METQTY / DAYSUPP) %>% # daily dosage = metric quantitiy / daily supply
      select(ENROLID, SUBSTANCENAME, STARTDT, ENDDT, INDEXDT, DAYQTY) %>% 
      distinct()
    
  } else {
    cat("   - all SUBSTANCES ....","\n")
    data <- 
      patient_data %>%
      filter(SUBSTANCENAME != "") %>%
      filter(METQTY > 0) %>% # 0 omitted to avoid aggregation() errors
      mutate(DAYQTY = METQTY / DAYSUPP) %>% # daily dosage = metric quantitiy / daily supply
      select(ENROLID, SUBSTANCENAME, STARTDT, ENDDT, INDEXDT, DAYQTY) %>% 
      distinct()
  }
  
  
  data$START  <- as.numeric(data$INDEXDT - data$STARTDT)
  data$END    <- as.numeric(data$INDEXDT - data$ENDDT)
  data        <- data %>% select(ENROLID, SUBSTANCENAME, DAYQTY, START, END) %>% distinct()
  data$DAYQTY <- as.numeric(data$DAYQTY)
  
  # compute QUARTQTY
  cat("Split data to list ....","\n")
  data.list <- split(data, 1:nrow(data))
  
  cat("Compute QUARTQTY ....","\n")
  y1 <- mclapply(data.list,FUN = function(x) getQUARTQTY.s(x), mc.cores = 10)
  
  cat("Transform List to data.frame ....","\n")
  y2 <- do.call(rbind.data.frame, y1)
  
  # label columns
  colnames(y2) <- c("ENROLID", "SUBSTANCENAME", "quarter", "QUARTQTY")
  
  # create feature name
  y2$quarter <- paste("QUARTQTY", y2$SUBSTANCENAME, y2$quarter, sep = ".x.")
  
  # reshape
  y2$quarter  <- make.names(gsub("-","m",y2$quarter))
  y2          <- y2 %>% select(-SUBSTANCENAME) %>% distinct()
  y2$QUARTQTY <- as.numeric(y2$QUARTQTY)
  y2          <- aggregate(QUARTQTY ~ ., data = y2, FUN = sum)
  
  # create QUARTQTY features: spread rows into columns 
  cat("Reshape data ....","\n")
  QUARTQTY   <- spread(y2, key = quarter, val = QUARTQTY, fill = 0)
  
  return(QUARTQTY)
  
}