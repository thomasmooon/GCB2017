GetCensoredIncidentSurvivalTime <- function(PATIENTS_AGE_REGION) {
  
  # patients without any main-comorbidity
  #######################################
  anyMc  <- PATIENTS_AGE_REGION %>% filter(!is.na(MAIN.COMORB)) %>% dplyr::select(ENROLID) %>% distinct()
  allPat <- PATIENTS_AGE_REGION %>% dplyr::select(ENROLID) %>% distinct()
  censored   <- setdiff(allPat$ENROLID, anyMc$ENROLID)
  
  # get censoring time
  censored <- 
    PATIENTS_AGE_REGION %>% filter(ENROLID %in% censored) %>% 
    select(ENROLID, IXDAYS) %>% 
    group_by(ENROLID) %>% mutate(time = max(IXDAYS)) %>% 
    ungroup() %>% select(ENROLID, time) %>% distinct() %>% 
    mutate(status = "censored")
  
  # patients with main-comorbidities
  ##################################
  main.comorb.first.ixday <- PATIENTS_AGE_REGION %>% filter(MAIN.COMORB != "") %>% dplyr::select(ENROLID, IXDAYS,  MAIN.COMORB) %>% distinct()
  main.comorb.first.ixday <- aggregate(IXDAYS ~., data = main.comorb.first.ixday, FUN = min)
  
  # with incident main-comorbidities
  incident <- main.comorb.first.ixday %>% filter(IXDAYS >= 180)
  incident <- incident %>% dplyr::rename(time = IXDAYS, status = MAIN.COMORB)
  
  return(rbind(censored,incident))
}