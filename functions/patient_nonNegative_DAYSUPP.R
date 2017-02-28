patient_nonNegative_DAYSUPP <- function(PATIENTS_AGE_REGION) {
  enrol_dsup0 <- PATIENTS_AGE_REGION %>% filter(DAYSUPP <0) %>% dplyr::select(ENROLID) %>%  distinct()
  PATIENTS_AGE_REGION <- PATIENTS_AGE_REGION %>% filter( !(ENROLID %in% enrol_dsup0$ENROLID) )
  return(PATIENTS_AGE_REGION)
}