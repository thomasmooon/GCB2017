# how many patients get a NEW main-comorbidity after 3 years?
#############################################################
n_Patients_new_comorb <- function(PATIENTS_AGE_REGION,mainComorb) {

mainPHEW    <- as.character(phew2custom$PHEWAS_STRING_HL[phew2custom$customised_high_level2 %in% mainComorb])
  
pat_newTreat <- 
  PATIENTS_AGE_REGION %>%  
  filter(DIAG != "", PHEWAS_STRING %in% mainPHEW) %>% 
  dplyr::select(ENROLID,IXDAYS,PHEWAS_STRING) %>% 
  distinct()

n_distinct(pat_newTreat$ENROLID)

pat_newTreat$PHEWAS_STRING <- as.character(pat_newTreat$PHEWAS_STRING)

t                <- seq(0,3,0.25)
counter          <- 1
newCoMorb_byTime <- cbind(t,nPat_new_comorb=NA)

for (tt in t) {
  
  cat(counter,"of",length(t),"\n")

pat_hist <-
  pat_newTreat %>% 
  filter(IXDAYS <= tt*365) %>% 
  dplyr::select(-IXDAYS) %>% 
  distinct()

pat_hist <- aggregate(PHEWAS_STRING ~ ENROLID,data = pat_hist,FUN = c)

pat_fut <- 
  pat_newTreat %>% 
  filter(IXDAYS > tt*365) %>% 
  dplyr::select(-IXDAYS) %>% 
  distinct()

pat_fut <- aggregate(PHEWAS_STRING ~ ENROLID,data = pat_fut,FUN = c)
pat_fut <- cbind(pat_fut,anyOld=NA,anyNew=NA)

for (k in 1:nrow(pat_fut)) {
  id    <- which(pat_hist$ENROLID %in% pat_fut$ENROLID[k])
  if (length(id) > 0) {
  oldCM <- pat_hist$PHEWAS_STRING[[id]]
  pat_fut$anyNew[k] <- any(!(pat_fut$PHEWAS_STRING[[k]] %in% oldCM)) # new co-morbidity occurence in future
  pat_fut$anyOld[k] <- any((pat_fut$PHEWAS_STRING[[k]] %in% oldCM))  # persistent  co-morbidity from past
  } else {
    pat_fut$anyNew[k] <- TRUE
    pat_fut$anyOld[k] <- FALSE
  }
}

newCoMorb_byTime[counter,2] <- sum(pat_fut$anyNew)
counter                     <- counter + 1

}
return(as.data.frame(newCoMorb_byTime))
}