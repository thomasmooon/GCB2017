setAEDFlag <- function(PATIENTS_AGE_REGION) {
  
  # add flag indicating, if a PHARMCLS1 is an AED
  PATIENTS_AGE_REGION <-
    PATIENTS_AGE_REGION %>% mutate(
      isAED = (
        PHARMCLS1 == "Anti-epileptic Agent [EPC]" |
          PHARMCLS1 == "Decreased Central Nervous System Disorganized Electrical Activity [PE]")
    ) 
  
  AEDs <-
    rbind(
      "Anti-epileptic Agent [EPC]",
      "Decreased Central Nervous System Disorganized Electrical Activity [PE]"
    )
  
  # print("Added column 'isAED' as indicator if PHARMCLS1 matches one of the following STRINGS")
  # print(AEDs)
  
  return(PATIENTS_AGE_REGION)
}