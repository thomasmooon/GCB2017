AddRegion <- function(x) {
  
  x$ENROLID <- as.numeric(x$ENROLID)
  load("~/epilepsie_preprocessing_descrStat/data/re-do_epd183/enrollment_demographic.RData")
  enrollment_demographic$ENROLID <- as.numeric(enrollment_demographic$ENROLID)
  load("~/epilepsie_preprocessing_descrStat/data/re-do_epd183/region.RData")
  region$REGION <- as.character(region$REGION)
  
  y <-
    x %>% select(ENROLID) %>% distinct() %>% 
    left_join(enrollment_demographic %>% select(ENROLID, REGION) %>% distinct(), by = "ENROLID") %>% 
    left_join(region, by = "REGION") %>% select(-REGION)
  
  return(y)
}

#######################

HospDaysPerQuarter <- function(P) {
  
  # hospitalization days per quarter
  P.hosp <- P %>% filter(HOSPFL == TRUE) %>%  select(ENROLID, DAYS, IXDAYS)  %>% distinct()
  P.hosp$IXDAYS.end <- P.hosp$IXDAYS + P.hosp$DAYS -1 # get last hospital day
  start <- which(colnames(P.hosp) == "IXDAYS")
  stop <- which(colnames(P.hosp) == "IXDAYS.end")
  P.hosp$hospix <- apply(P.hosp, 1,FUN = function(x) x[start]:x[stop]) # get hospitalisation interval
  
  # compute quarters
  setDT(P.hosp)
  P.hosp <- P.hosp[, lapply(.SD, function(x) c(unlist(x))), by = ENROLID, .SDcols = "hospix"]
  P.hosp <- unique(P.hosp) # avoid doubles (-> overcount days per quarter)
  # drop observations beyond 1st quarter
  drop <- which(P.hosp$hospix >= 91.25)
  P.hosp <- P.hosp[-drop,]
  P.hosp[, QUARTER := floor(hospix/91.25)] # compute quarters from interval
  P.hosp[QUARTER >= 0, QUARTER := QUARTER +1] # shift by 1: positive quarters start from 1, instead of 0
  
  # nice names
  P.hosp[, QUARTER := paste("HospDays",gsub("-","m",QUARTER), sep = ".x.")]
  
  # aggregate
  P.hosp[,count := 1] # value.var for dcast aggregation via sum
  P.hosp <- dcast(P.hosp, ENROLID ~ QUARTER, fill = 0, fun=sum, value.var = "count")
  
  #
  return(P.hosp)
}

#######################

HospDaysSlidingWindow <- function(P) {
  
  # hospitalization days per quarter
  P.hosp <- P %>% filter(HOSPFL == TRUE) %>%  select(ENROLID, DAYS, IXDAYS)  %>% distinct()
  P.hosp$IXDAYS.end <- P.hosp$IXDAYS + P.hosp$DAYS -1 # get last hospital day
  start <- which(colnames(P.hosp) == "IXDAYS")
  stop <- which(colnames(P.hosp) == "IXDAYS.end")
  P.hosp$hospix <- apply(P.hosp, 1,FUN = function(x) x[start]:x[stop]) # get hospitalisation interval
  setDT(P.hosp)
  P.hosp <- P.hosp[, lapply(.SD, function(x) c(unlist(x))), by = ENROLID, .SDcols = "hospix"]
  P.hosp <- unique(P.hosp) # avoid doubles (-> overcount days per quarter)
  
  # compute sliding windows masked as "QUARTER" column
  P.hosp <- Ixdays2SlidingWindow(P.hosp, "hospix")
  
  # nice names
  setDT(P.hosp)
  P.hosp[, QUARTER := paste("HospDays",QUARTER, sep = ".x.")]
  
  # aggregate
  P.hosp[,count := 1] # value.var for dcast aggregation via sum
  P.hosp <- dcast(P.hosp, ENROLID ~ QUARTER, fill = 0, fun=sum, value.var = "count")
  
  #
  return(P.hosp)
}


#######################

HospDaysAllInOne <- function(P) {
  
  # hospitalization days per quarter
  P.hosp <- P %>% filter(HOSPFL == TRUE) %>%  select(ENROLID, DAYS, IXDAYS)  %>% distinct()
  P.hosp$IXDAYS.end <- P.hosp$IXDAYS + P.hosp$DAYS -1 # get last hospital day
  start <- which(colnames(P.hosp) == "IXDAYS")
  stop <- which(colnames(P.hosp) == "IXDAYS.end")
  P.hosp$hospix <- apply(P.hosp, 1,FUN = function(x) x[start]:x[stop]) # get hospitalisation interval
  setDT(P.hosp)
  P.hosp <- P.hosp[, lapply(.SD, function(x) c(unlist(x))), by = ENROLID, .SDcols = "hospix"]
  P.hosp <- unique(P.hosp) # avoid doubles (-> overcount days per quarter)
  
  # compute sliding windows masked as "QUARTER" column
  P.hosp <- Ixdays2AllInOne(P.hosp, "hospix")
  
  # nice names
  setDT(P.hosp)
  P.hosp[, QUARTER := paste("HospDays",QUARTER, sep = ".x.")]
  
  # aggregate
  P.hosp[,count := 1] # value.var for dcast aggregation via sum
  P.hosp <- dcast(P.hosp, ENROLID ~ QUARTER, fill = 0, fun=sum, value.var = "count")
  
  #
  return(P.hosp)
}



#######################

Ixdays2Quarter <- function(P) {
  library(data.table)
  setDT(P)
  # compute quarters
  P[, QUARTER := floor(IXDAYS/91.25)] # compute quarters from interval
  P[QUARTER >= 0, QUARTER := QUARTER +1] # shift by 1: positive quarters start from 1, instead of 0
  
  #
  return(setDF(P))
}

#######################

Ixdays2SlidingWindow <- function(P, ixday.name) {
  library(data.table)
  setDT(P)
  
  # compute months
  idx <- which(colnames(P) == ixday.name)
  P$MONTH <- floor(P[,idx, with = F]/30)
  P[MONTH >= 0, MONTH := MONTH +1]
  P[MONTH == 4, MONTH := 3]
  setDF(P)
  
  # 6 months, window size 3 => 4 windows
  # window 1: months -3, -2, -1
  # window 2: months -2, -1, 1
  # window 3: months -1, 1, 2
  # window 4: months 1, 2, 3
  # QUARTER is used as dummy variable for the particular window to avoid 
  
  P.w1 <- P %>% filter(MONTH %in% c(-3,-2,-1)) %>% mutate(QUARTER = "w_m2")
  P.w2 <- P %>% filter(MONTH %in% c(-2,-1,1)) %>% mutate(QUARTER = "w_m1")
  P.w3 <- P %>% filter(MONTH %in% c(-1, 1, 2)) %>% mutate(QUARTER = "w_1")
  P.w4 <- P %>% filter(MONTH %in% c(1,2,3)) %>% mutate(QUARTER = "w_2")
  
  P.slidingWindow <- rbind(P.w1, P.w2, P.w3, P.w4) %>% select(-MONTH)
  
  #
  return(P.slidingWindow)
}

#######################

Ixdays2AllInOne <- function(P, ixday.name) {
  P$QUARTER <- "allInOne"
  return(P)
}

#######################

AedQuartQty <- function(P) {
  # metqty by aed by quarter
  P.QUART.METQTY.AED <- 
    P %>% filter(isAED == TRUE ) %>%  select(ENROLID, NDCNUM, IXDAYS, METQTY, DAYSUPP) %>% distinct() %>% 
    mutate(DAYQTY = METQTY / DAYSUPP)
  
  # retrieve NDCNUM features
  load("~/epilepsie_preprocessing_descrStat/data/re-do_epd183/pharm_class_mapping.RData")
  pharmClassMapping <- 
    pharmClassMapping %>% filter(NDCNUM %in% P.QUART.METQTY.AED$NDCNUM) %>% select(NDCNUM, GENNME, ROACD, STRNGTH) %>%
    distinct() %>% mutate(AED = make.names(paste(GENNME,ROACD, STRNGTH, sep="_")))
  
  # join NDCNUM features to patients treatment data
  P.QUART.METQTY.AED <- left_join(P.QUART.METQTY.AED, pharmClassMapping %>% select(NDCNUM, AED), by = "NDCNUM") %>% select(-NDCNUM)
  
  # treatment days per quarter
  P.QUART.METQTY.AED$IXDAYS.end <- P.QUART.METQTY.AED$IXDAYS + P.QUART.METQTY.AED$DAYSUPP -1 # get last treatment prescription day
  start <- which(colnames(P.QUART.METQTY.AED) == "IXDAYS")
  stop <- which(colnames(P.QUART.METQTY.AED) == "IXDAYS.end")
  P.QUART.METQTY.AED$treatix <- apply(P.QUART.METQTY.AED, 1,FUN = function(x) x[start]:x[stop]) # get treatment period
  
  setDT(P.QUART.METQTY.AED)
  P.QUART.METQTY.AED[, c("IXDAYS","METQTY","DAYSUPP","IXDAYS.end") := NULL ] # drop useless cols
  P.QUART.METQTY.AED <- P.QUART.METQTY.AED[, lapply(.SD, function(x) c(unlist(x))), by = .(ENROLID, DAYQTY, AED)]
  
  # compute quarters
  # drop observations beyond 1st quarter
  drop <- which(P.QUART.METQTY.AED$treatix >= 91.25)
  P.QUART.METQTY.AED <- P.QUART.METQTY.AED[-drop,]
  P.QUART.METQTY.AED[, QUARTER := floor(treatix/91.25)] # compute quarters from interval
  P.QUART.METQTY.AED[QUARTER >= 0, QUARTER := QUARTER +1] # shift by 1: positive quarters start from 1, instead of 0
  
  # aggregate
  P.QUART.METQTY.AED <- dcast(P.QUART.METQTY.AED, ENROLID ~ AED + QUARTER, fill = 0, fun=sum, value.var = "DAYQTY", sep = ".x.")
  
  # nice colnames
  colnames(P.QUART.METQTY.AED) <- gsub("-","m",colnames(P.QUART.METQTY.AED))
  idx <- colnames(P.QUART.METQTY.AED) != "ENROLID"
  colnames(P.QUART.METQTY.AED)[idx] <- paste("QUARTQTY",colnames(P.QUART.METQTY.AED)[idx], sep = ".x.")
  
  #
  return(P.QUART.METQTY.AED)
}

#######################

AedSlidingWindow <- function(P) {
  # metqty by aed by quarter
  P.QUART.METQTY.AED <- 
    P %>% filter(isAED == TRUE ) %>%  select(ENROLID, NDCNUM, IXDAYS, METQTY, DAYSUPP) %>% distinct() %>% 
    mutate(DAYQTY = METQTY / DAYSUPP)
  
  # retrieve NDCNUM features
  load("~/epilepsie_preprocessing_descrStat/data/re-do_epd183/pharm_class_mapping.RData")
  pharmClassMapping <- 
    pharmClassMapping %>% filter(NDCNUM %in% P.QUART.METQTY.AED$NDCNUM) %>% select(NDCNUM, GENNME, ROACD, STRNGTH) %>%
    distinct() %>% mutate(AED = make.names(paste(GENNME,ROACD, STRNGTH, sep="_")))
  
  # join NDCNUM features to patients treatment data
  P.QUART.METQTY.AED <- left_join(P.QUART.METQTY.AED, pharmClassMapping %>% select(NDCNUM, AED), by = "NDCNUM") %>% select(-NDCNUM)
  
  # treatment days per quarter
  P.QUART.METQTY.AED$IXDAYS.end <- P.QUART.METQTY.AED$IXDAYS + P.QUART.METQTY.AED$DAYSUPP -1 # get last treatment prescription day
  start <- which(colnames(P.QUART.METQTY.AED) == "IXDAYS")
  stop <- which(colnames(P.QUART.METQTY.AED) == "IXDAYS.end")
  P.QUART.METQTY.AED$treatix <- apply(P.QUART.METQTY.AED, 1,FUN = function(x) x[start]:x[stop]) # get treatment period
  
  # compute quarters
  setDT(P.QUART.METQTY.AED)
  P.QUART.METQTY.AED[, c("IXDAYS","METQTY","DAYSUPP","IXDAYS.end") := NULL ] # drop useless cols
  P.QUART.METQTY.AED <- P.QUART.METQTY.AED[, lapply(.SD, function(x) c(unlist(x))), by = .(ENROLID, DAYQTY, AED)]
  
  # compute sliding window masked as "QUARTER" column
  P.QUART.METQTY.AED <- Ixdays2SlidingWindow(P.QUART.METQTY.AED, "treatix")
  setDT(P.QUART.METQTY.AED)
  
  # aggregate
  P.QUART.METQTY.AED <- dcast(P.QUART.METQTY.AED, ENROLID ~ AED + QUARTER, fill = 0, fun=sum, value.var = "DAYQTY", sep = ".x.")
  
  # nice colnames
  colnames(P.QUART.METQTY.AED) <- gsub("-","m",colnames(P.QUART.METQTY.AED))
  idx <- colnames(P.QUART.METQTY.AED) != "ENROLID"
  colnames(P.QUART.METQTY.AED)[idx] <- paste("QUARTQTY",colnames(P.QUART.METQTY.AED)[idx], sep = ".x.")
  
  #
  return(P.QUART.METQTY.AED)
}

#######################

AedAllInOne <- function(P) {
  # metqty by aed by quarter
  P.QUART.METQTY.AED <- 
    P %>% filter(isAED == TRUE ) %>%  select(ENROLID, NDCNUM, IXDAYS, METQTY, DAYSUPP) %>% distinct() %>% 
    mutate(DAYQTY = METQTY / DAYSUPP)
  
  # retrieve NDCNUM features
  load("~/epilepsie_preprocessing_descrStat/data/re-do_epd183/pharm_class_mapping.RData")
  pharmClassMapping <- 
    pharmClassMapping %>% filter(NDCNUM %in% P.QUART.METQTY.AED$NDCNUM) %>% select(NDCNUM, GENNME, ROACD, STRNGTH) %>%
    distinct() %>% mutate(AED = make.names(paste(GENNME,ROACD, STRNGTH, sep="_")))
  
  # join NDCNUM features to patients treatment data
  P.QUART.METQTY.AED <- left_join(P.QUART.METQTY.AED, pharmClassMapping %>% select(NDCNUM, AED), by = "NDCNUM") %>% select(-NDCNUM)
  
  # treatment days per quarter
  P.QUART.METQTY.AED$IXDAYS.end <- P.QUART.METQTY.AED$IXDAYS + P.QUART.METQTY.AED$DAYSUPP -1 # get last treatment prescription day
  start <- which(colnames(P.QUART.METQTY.AED) == "IXDAYS")
  stop <- which(colnames(P.QUART.METQTY.AED) == "IXDAYS.end")
  P.QUART.METQTY.AED$treatix <- apply(P.QUART.METQTY.AED, 1,FUN = function(x) x[start]:x[stop]) # get treatment period
  
  # compute quarters
  setDT(P.QUART.METQTY.AED)
  P.QUART.METQTY.AED[, c("IXDAYS","METQTY","DAYSUPP","IXDAYS.end") := NULL ] # drop useless cols
  P.QUART.METQTY.AED <- P.QUART.METQTY.AED[, lapply(.SD, function(x) c(unlist(x))), by = .(ENROLID, DAYQTY, AED)]
  
  # compute all-in-one history, masked by QUARTER
  P.QUART.METQTY.AED <- Ixdays2AllInOne(P.QUART.METQTY.AED, "treatix")
  setDT(P.QUART.METQTY.AED)
  
  # aggregate
  P.QUART.METQTY.AED <- dcast(P.QUART.METQTY.AED, ENROLID ~ AED + QUARTER, fill = 0, fun=sum, value.var = "DAYQTY", sep = ".x.")
  
  # nice colnames
  colnames(P.QUART.METQTY.AED) <- gsub("-","m",colnames(P.QUART.METQTY.AED))
  idx <- colnames(P.QUART.METQTY.AED) != "ENROLID"
  colnames(P.QUART.METQTY.AED)[idx] <- paste("QUARTQTY",colnames(P.QUART.METQTY.AED)[idx], sep = ".x.")
  
  #
  return(P.QUART.METQTY.AED)
}


#######################

GetHltplnPlantypCov <- function() {
  load("~/epilepsie_preprocessing_descrStat/data/re-do_epd183/enrollment_demographic.RData")
  load("~/epilepsie_preprocessing_descrStat/data/re-do_epd183/plantyp.RData")
  load("~/epilepsie_preprocessing_descrStat/data/re-do_epd183/hlthpln.RData")
  enrollment_demographic$HLTHPLAN <- as.numeric(enrollment_demographic$HLTHPLAN)
  
  enrollment_demographic <- 
    enrollment_demographic %>% filter(ENROLID %in% P$ENROLID) %>% 
    left_join(plantyp) %>% left_join(hlthpln, by = c("HLTHPLAN" = "HLTHPLN")) %>% 
    select(ENROLID, COVERAGE_OF_PRESCRIPTIONS, HLTHPLNC, PLANTYPC) %>% 
    distinct()
  
  setDT(enrollment_demographic)
  enrollment_demographic[COVERAGE_OF_PRESCRIPTIONS != "Yes", COVERAGE_OF_PRESCRIPTIONS := "No"]
  setDF(enrollment_demographic)
  enrollment_demographic$ENROLID <- as.numeric(enrollment_demographic$ENROLID)
  return(enrollment_demographic)
}