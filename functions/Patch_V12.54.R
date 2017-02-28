# 16.01.2017
# ICD9 code V12.54 "Personal history of transient ischemic attack" workaround:
#############################################################################
# 
# This ICD9 code indicates a prevalence in the main-comorb. category "Stroke.IschemAttack".
# In some cases, if this code appears with an IXDAY >180, then the "personal history" will be
# flagged as an incidence due to that.
# 
# No supporting information for a precise history date are available.
# 
# Therefore all Stroke.IschemAttack events of a patient will be deleted,
# if the patient has a V12.54 event.
# In except to thoses cases in which V12.54 occurs at Indexdate x >= 180
# and some other Stroke.IschemAttack occured at   180 <= y < x
# 
# Step by Step:
# 1 
# 1.1 get all observations with "Stroke.IschemAttack" terms
# 1.2 delete all observations with "Stroke.IschemAttack" terms from PATIENTS_... table
# 2. separate the diagnosis "D"  information from the therapy information "TH"
# 3. 
# 3.1 if V12.54 occurs at IXDAY <= 180, then delete each "Stroke.IschemAttack" event of the patient
# 3.2 if V12.54 occurs at IXDAY >= 180 and no other "Stroke.IschemAttack" Attack occurs before, then do as in (3.1)
# 3.3 if V12.54 occurs at IXDAY x >= 180 and it exists an other "Stroke.IschemAttack" event at time
#     180 <= y < x, then delete the V12.54 event and keep the other

Patch_V12.54 <- function(PATIENTS_AGE_REGION_) {
  
  # 1.
  all <- PATIENTS_AGE_REGION_ %>% filter(MAIN.COMORB == "Stroke.IschemAttack") 
  PATIENTS_AGE_REGION_ <- PATIENTS_AGE_REGION_ %>% filter(MAIN.COMORB != "Stroke.IschemAttack" | is.na(MAIN.COMORB)) 
  
  # 2.
  # diagnosis only, without treatment information
  D <- all # 22693
  D$SUBSTANCENAME <- ""
  D$PROPRIETARYNAME <- ""
  D$PHARMCLS1 <- ""
  D$DAYSUPP <-  NA
  D$METQTY <- NA
  D$isAED <- NA
  D <- unique(D)
  
  
  # treatment only, without diagnosis information
  TH <- all 
  TH$DIAG <- ""
  TH$PHEWAS_STRING <- ""
  TH$PHEWAS_STRING_HL <- ""
  TH$MAIN.COMORB <- ""
  TH <- TH %>% filter(SUBSTANCENAME != "")
  
  # 3.
  
  # 3.1
  
  delme_3.1 <- 
    D %>% 
    filter(DIAG == "V1254") %>% 
    dplyr::select(ENROLID, IXDAYS) %>% 
    distinct() %>% 
    filter(IXDAYS <= 180) %>% 
    dplyr::select(ENROLID)
  
  D <- D %>% filter(! (ENROLID %in% delme_3.1$ENROLID) ) 
  
  # 3.2
  ixday.v1254 <- 
    D %>% 
    filter(DIAG == "V1254") %>% 
    dplyr::select(ENROLID, IXDAYS) %>% 
    distinct() %>% 
    dplyr::rename(V1254 = IXDAYS)
  
  ixday.min <- aggregate(IXDAYS ~. , data = D %>% dplyr::select(ENROLID,IXDAYS), min) %>% dplyr::rename(min = IXDAYS)
  
  ixday.v1254.is.smallest <- left_join(ixday.v1254, ixday.min) %>%  mutate(V.is.smallest = V1254 <= min) %>% filter(V.is.smallest == TRUE)
  
  D <- D %>% filter(! (ENROLID %in% unique(ixday.v1254.is.smallest$ENROLID)) ) 
  
  # 3.3
  
  ixday.v1254.notSmallest <- left_join(ixday.v1254, ixday.min) %>%  mutate(V.is.smallest = V1254 <= min) %>% filter(V.is.smallest == FALSE)
  delme <- which( (D$ENROLID %in% ixday.v1254.notSmallest$ENROLID) & D$DIAG == "V1254")
  D <- D[-delme,] 
  
  PATIENTS_AGE_REGION_ <- rbind(PATIENTS_AGE_REGION_, TH, D)
  
return(PATIENTS_AGE_REGION_)

}