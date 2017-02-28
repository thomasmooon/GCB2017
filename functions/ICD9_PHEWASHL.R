ICD9_PHEWASHL <- function(ICD9GROUPING){

# get high-level PHEWAS terms for each low-level PHEWAS term
PHEW2HL               <- ICD9GROUPING %>%  dplyr::select(PHEWAS_CODE,PHEWAS_STRING) %>%  distinct() %>% collect()
tmp                   <- suppressWarnings(as.numeric(as.character(PHEW2HL$PHEWAS_CODE)))
PHEW2HL               <- PHEW2HL[complete.cases(tmp),]
PHEW2HL$PHEW_CODE_HL  <- floor(as.numeric(as.character(PHEW2HL$PHEWAS_CODE)))

keep         <- which(as.numeric(as.character(PHEW2HL$PHEWAS_CODE)) %in% PHEW2HL$PHEW_CODE_HL)
onlyHL       <- PHEW2HL[keep,] %>% dplyr::rename(PHEWAS_STRING_HL = PHEWAS_STRING) %>% dplyr::select(-PHEWAS_CODE)
PHEW2HL      <- left_join(PHEW2HL,onlyHL,by="PHEW_CODE_HL")

# use only PHEWAS_STRING_HL and rename it to PHEWAS_STRING to ease compatibilty in the "main" script
PHEW2HL <- PHEW2HL %>% dplyr::select(-PHEWAS_STRING,-PHEW_CODE_HL) %>% dplyr::rename(PHEWAS_STRING = PHEWAS_STRING_HL)


# download ICD9GROUPING and replace PHEWAS_STRING with high level PHEWAS STRING
ICD9GROUPING <- ICD9GROUPING %>%  dplyr::select(-PHEWAS_STRING) %>% collect()
ICD9GROUPING <- left_join(ICD9GROUPING,PHEW2HL,by="PHEWAS_CODE")

}