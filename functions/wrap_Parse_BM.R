
wrap_parse_BM <- function(src) {
  
  # Map diagnoses ICD9 codes via inner joins to
  # get description of each disease in terms of disease groups, 
  # biomarkers, disease genes & pathways and  symptoms
  ##############################################################
  
  
  
  #### ICD9GROUPING ####
  ######################
  
  # load data
  ICD9GROUPING <- tbl(src, "REFERENCE.ICD9GROUPING")
  ICD9GROUPING <- 
    dplyr::select(ICD9GROUPING, ICD9,PHEWAS_STRING,PHEWAS_CODE) # omit useless columns
  ICD9All <- dplyr::select(ICD9GROUPING, ICD9)
  
  
  #### DISEASE2BIOMARKER ####
  ###########################
  
  # load data
  DISEASE2BIOMARKER <- tbl(src, "REFERENCE.DISEASE2BIOMARKER" )
  
  # inner join to biomarkers
    
    # to-do: parse ICD9, because concatenated entities, e.g.:
    dplyr::select(DISEASE2BIOMARKER,ICD9) %>% summarise(n=n()) # number of rows
    count(DISEASE2BIOMARKER,ICD9) %>% summarise(n=n()) # number of unique rows 
    
      # parsing step-by-step:
      # pull data from database and parse, then put it back
      # 1. pull DISEASE2BIOMARKER
      # 2. identify rows with non-unique entities & store in new vector
      # 3. view the data again
      # 4. create parsing functions, return parsed output
      # 5. match unparsed data to parsed data (this will multiply the rows)
      # 6. remove non-unique entities from pulled source data, add matched parsed data
      # 7. put DISEASE2BIOMARKER back to netezza DB
    
    # 1.
    DISEASE2BIOMARKER_R <- 
      DISEASE2BIOMARKER %>% 
      collect() %>% 
      dplyr::select(BIOMARKER_REC_ID,BIOMARKERNAME,ICD9) 
    #' any special code "V..." or "E..."? 
    #' (compare http://www.icd9data.com/2015/Volume1/default.htm)
    if(any((grepl(pattern = "V",x = DISEASE2BIOMARKER_R$ICD9))))
      warning("Special Case 'V' recognized. Coercion to numeric forbidden")
    if(any((grepl(pattern = "E",x = DISEASE2BIOMARKER_R$ICD9))))
      warning("Special Case 'E' recognized. Coercion to numeric forbidden")
    
    # 2. to 4.
    source("functions/parse_DISEASE2BIOMARKER.R")
    parsed_D2BM <- tbl_df(parse_DISEASE2BIOMARKER(DISEASE2BIOMARKER_R,ICD9All)) 
    
    # 5.1 match parsed ICD9 Codes to biomarker table
    
      # rename to enable join
      DISEASE2BIOMARKER_R <- dplyr::rename(DISEASE2BIOMARKER_R,unparsedICD9 = ICD9) 
      D2B_ICD9 <- dplyr::rename(parsed_D2BM,unparsedICD9 = unparsed,parsedICD9 = parsed)
      
        # join with previous character to omit error-message
        DISEASE2BIOMARKER_R$unparsedICD9 <- 
          as.character(DISEASE2BIOMARKER_R$unparsedICD9)
        D2B_ICD9$unparsedICD9            <- as.character(D2B_ICD9$unparsedICD9)
        D2B_ICD9 <- inner_join(DISEASE2BIOMARKER_R,D2B_ICD9,"unparsedICD9")
        
        #
        return(D2B_ICD9)
}
  
  


  
  