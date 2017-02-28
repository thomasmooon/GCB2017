add_Phew_PhewHighlevel <- function(PATIENTS_AGE_REGION){
  
  # get high-level PHEWAS terms for each low-level PHEWAS term
  source("functions/ICD9_PHEWASHL.R")
  
  #
  ICD9GROUPING   <- tbl(src, "REFERENCE.ICD9GROUPING") %>% collect()
  ICD9PHEWASHL   <- ICD9_PHEWASHL(ICD9GROUPING)  %>% dplyr::rename(PHEWAS_STRING_HL = PHEWAS_STRING)
  ICD9GROUPING   <- ICD9GROUPING %>% dplyr::select(ICD9,PHEWAS_STRING) %>% distinct()
  ICD9PHEWASHL   <- left_join(ICD9PHEWASHL,ICD9GROUPING %>% dplyr::select(ICD9,PHEWAS_STRING),by="ICD9")
  
  ICD9PHEW       <- ICD9PHEWASHL %>% filter(ICD9 != "") %>% collect()
  ICD9PHEW$ICD9  <- as.character(ICD9PHEW$ICD9)
  ICD9PHEW$ICD9  <- gsub("\\.","",ICD9PHEW$ICD9) # remove delimiter
  ICD9PHEW       <- ICD9PHEW %>% dplyr::rename(DIAG = ICD9)
  
  # join Phewas and Phewas-highlevel-terms to PATIENTS_AGE_REGION
  PATIENTS_AGE_REGION  <-
    left_join(PATIENTS_AGE_REGION,
              ICD9PHEW %>%  dplyr::select(DIAG, PHEWAS_STRING, PHEWAS_STRING_HL) %>% distinct(),
              by = "DIAG")
  #
  return(PATIENTS_AGE_REGION)
}