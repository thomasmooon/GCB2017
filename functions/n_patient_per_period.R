# A gap is defined as a missing visit (IXDAY-event) during the treatment period.
# E.g. if a patient is treated between quartile -4 to quartile 8 and and has an missing
# visit in any quartile between those, then he is flaged as "gapped"

filter_patient_gapFreePeriod <- function(PATIENTS_AGE_REGION,gapFreePeriod=NULL) {
  
if(!is.null(gapFreePeriod)) {

# min. 1 event per quarter
x <- 
  PATIENTS_AGE_REGION %>% 
  dplyr::select(ENROLID, IXDAYS) %>% 
  mutate(quart = floor(IXDAYS/90)) %>% 
  mutate(hy = floor(IXDAYS/180)) %>% 
  mutate(y = floor(IXDAYS/360)) %>% 
  dplyr::select(-IXDAYS) %>% 
  distinct()

q_seq  <- seq(4*-1,4*3)
hy_seq <- seq(2*-1,2*3)
y_seq  <- seq(-1,3)
x_q      <- aggregate(quart ~ ENROLID,data = x,FUN = c)
x_hy     <- aggregate(hy ~ ENROLID,data = x,FUN = c)
x_y      <- aggregate(y ~ ENROLID,data = x,FUN = c)
xx       <- cbind(x_q,x_hy,x_y,inQ=NA,inHY=NA,inY=NA)

for (k in 1:nrow(xx)) {
  #if (k %% 1000 == 0) print(k)
  xx$inQ[k]  <- all(q_seq %in% xx$quart[[k]])
  xx$inHY[k] <- all(hy_seq %in% xx$hy[[k]])
  xx$inY[k]  <- all(y_seq %in% xx$y[[k]])
}

xx               <- xx[,-c(3,5)]
result           <- t(t(apply(xx[,5:7],2,sum)))
colnames(result) <- "nPatients"

knitr:::kable(rbind(result,""))

if (gapFreePeriod == "quarter") {
  period               <- xx %>% filter(inQ == TRUE) %>% dplyr::select(ENROLID) %>% distinct()
  PATIENTS_AGE_REGION  <- PATIENTS_AGE_REGION[PATIENTS_AGE_REGION$ENROLID %in% period$ENROLID,]
  cat("Patients remaining (no gaps in quarterly sequence):",n_distinct(PATIENTS_AGE_REGION$ENROLID))
}

if (gapFreePeriod == "halfyear") {
  period               <- xx %>% filter(inHY == TRUE) %>% dplyr::select(ENROLID) %>% distinct()
  PATIENTS_AGE_REGION  <- PATIENTS_AGE_REGION[PATIENTS_AGE_REGION$ENROLID %in% period$ENROLID,]
  cat("Patients remaining (no gaps in half-year sequence):",n_distinct(PATIENTS_AGE_REGION$ENROLID))
}

if (gapFreePeriod == "year"){
  period               <- xx %>% filter(inY == TRUE) %>% dplyr::select(ENROLID) %>% distinct()
  PATIENTS_AGE_REGION  <- PATIENTS_AGE_REGION[PATIENTS_AGE_REGION$ENROLID %in% period$ENROLID,]
  cat("Patients remaining (no gaps in yearly sequence):",n_distinct(PATIENTS_AGE_REGION$ENROLID))
}

}


return(PATIENTS_AGE_REGION)

}