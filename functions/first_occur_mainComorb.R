first_occur_mainComorb <- function(PATIENTS_AGE_REGION,main_Phew_highlvl,phew2custom) {
  
  pat_newTreat_hl <- 
    PATIENTS_AGE_REGION %>%  
    filter(DIAG != "", PHEWAS_STRING_HL %in% main_Phew_highlvl) %>% 
    dplyr::select(ENROLID,IXDAYS,PHEWAS_STRING_HL,DIAG) %>% 
    distinct()
  
  pat_newTreat <- 
    PATIENTS_AGE_REGION %>%  
    filter(DIAG != "", PHEWAS_STRING_HL %in% main_Phew_highlvl) %>% 
    dplyr::select(ENROLID,IXDAYS,PHEWAS_STRING,DIAG) %>% 
    distinct()
  
# first occurrence of a main-co-morbidity
  by_icd9 <- aggregate(data=pat_newTreat_hl, IXDAYS ~ DIAG + ENROLID  , FUN=min)
  by_icd9 <- left_join(by_icd9,pat_newTreat_hl %>% dplyr::select(-ENROLID,-IXDAYS) %>% distinct(), by="DIAG")
  by_icd9 <- left_join(by_icd9, phew2custom %>% dplyr::select(-customised_high_level1,-X), by="PHEWAS_STRING_HL")
  by_icd9_0 <- by_icd9 %>% filter(IXDAYS >0)
  
  by_phewas_hl <- aggregate(data=pat_newTreat_hl, IXDAYS ~ PHEWAS_STRING_HL + ENROLID, FUN=min)
  by_phewas_hl <- left_join(by_phewas_hl, phew2custom %>% dplyr::select(-customised_high_level1,-X), by="PHEWAS_STRING_HL")
  by_phewas_hl_0 <- by_phewas_hl %>% filter(IXDAYS >0)
  
  by_phewas <- aggregate(data=pat_newTreat, IXDAYS ~ PHEWAS_STRING + ENROLID, FUN=min)
  by_phewas_0 <- by_phewas %>% filter(IXDAYS >0)

# boxplots
  
  # reference ICD9
  subplot(1,2)
  p1 <- ggplot(by_icd9,aes(x=customised_high_level2,y=IXDAYS)) +
    geom_boxplot() +
    theme(axis.text.x = element_text(angle = 25, hjust = 1)) +
    geom_hline(yintercept = c(0.5*365,365),color="red",linetype="dashed") +
    ggtitle("First occurrence of new main-comorbidities, reference: ICD9 \n red line: 1/2 year, 1 year")
  print(p1,vp=vplayout(1,1))
  
  p2 <- ggplot(by_icd9,aes(x=customised_high_level2,y=IXDAYS)) +
    geom_violin() +
    theme(axis.text.x = element_text(angle = 25, hjust = 1)) +
    geom_hline(yintercept = c(0.5*365,365),color="red",linetype="dashed") +
    ggtitle("First occurrence of new main-comorbidities, reference: ICD9 \n red line: 1/2 year, 1 year")
  print(p2,vp=vplayout(1,2))
  
  # reference ICD9, Indexdate >0
  subplot(1,2)
  p1 <- ggplot(by_icd9_0,aes(x=customised_high_level2,y=IXDAYS)) +
    geom_boxplot() +
    theme(axis.text.x = element_text(angle = 25, hjust = 1)) +
    geom_hline(yintercept = c(0.5*365,365),color="red",linetype="dashed") +
    ggtitle("First occurrence of new main-comorbidities, reference: ICD9 \n red line: 1/2 year, 1 year \n only events Indexdate >0")
  print(p1,vp=vplayout(1,1))
  
  p2 <- ggplot(by_icd9_0,aes(x=customised_high_level2,y=IXDAYS)) +
    geom_violin() +
    theme(axis.text.x = element_text(angle = 25, hjust = 1)) +
    geom_hline(yintercept = c(0.5*365,365),color="red",linetype="dashed") +
    ggtitle("First occurrence of new main-comorbidities, reference: ICD9 \n red line: 1/2 year, 1 year \n only events Indexdate >0")
  print(p2,vp=vplayout(1,2))
  
  
  # reference PHEWAS highlevel
  subplot(1,2)
  p1 <- ggplot(by_phewas_hl,aes(x=customised_high_level2,y=IXDAYS)) +
    geom_boxplot() +
    theme(axis.text.x = element_text(angle = 25, hjust = 1)) +
    geom_hline(yintercept = c(0.5*365,365),color="red",linetype="dashed") +
    ggtitle("First occurrence of new main-comorbidities, reference: high-level PHEWAS \n red line: 1/2 year, 1 year")
    print(p1,vp=vplayout(1,1))
  
  p2 <- ggplot(by_phewas_hl,aes(x=customised_high_level2,y=IXDAYS)) +
    geom_violin() +
    theme(axis.text.x = element_text(angle = 25, hjust = 1)) +
    geom_hline(yintercept = c(0.5*365,365),color="red",linetype="dashed") +
    ggtitle("First occurrence of new main-comorbidities, reference: high-level PHEWAS \n red line: 1/2 year, 1 year")
  print(p2,vp=vplayout(1,2))
  
  # reference PHEWAS highlevel, Indexdate >0
  subplot(1,2)
  p1 <- ggplot(by_phewas_hl_0,aes(x=customised_high_level2,y=IXDAYS)) +
    geom_boxplot() +
    theme(axis.text.x = element_text(angle = 25, hjust = 1)) +
    geom_hline(yintercept = c(0.5*365,365),color="red",linetype="dashed") +
    ggtitle("First occurrence of new main-comorbidities, reference: high-level PHEWAS \n red line: 1/2 year, 1 year")
  print(p1,vp=vplayout(1,1))
  
  p2 <- ggplot(by_phewas_hl_0,aes(x=customised_high_level2,y=IXDAYS)) +
    geom_violin() +
    theme(axis.text.x = element_text(angle = 25, hjust = 1)) +
    geom_hline(yintercept = c(0.5*365,365),color="red",linetype="dashed") +
    ggtitle("First occurrence of new main-comorbidities, reference: high-level PHEWAS \n red line: 1/2 year, 1 year")
  print(p2,vp=vplayout(1,2))
  
  # by PHEWAS highlevel, Indexdate >0

  p1 <- ggplot(by_phewas_hl_0,aes(x=PHEWAS_STRING_HL,y=IXDAYS)) +
    geom_boxplot() +
    theme(axis.text.x = element_text(angle = 25, hjust = 1)) +
    geom_hline(yintercept = c(0.5*365,365,1.5*365,2*365),color="red",linetype="dashed") +
    ggtitle("First occurrence of new main-comorbidities, by high-level PHEWAS \n red line: 1/2 year, 1y, 1.5y, 2y  \n only events Indexdate >0")
  print(p1)
  
  p2 <- ggplot(by_phewas_hl_0,aes(x=PHEWAS_STRING_HL,y=IXDAYS)) +
    geom_violin() +
    theme(axis.text.x = element_text(angle = 25, hjust = 1)) +
    geom_hline(yintercept = c(0.5*365,365,1.5*365,2*365),color="red",linetype="dashed") +
    ggtitle("First occurrence of new main-comorbidities, by high-level PHEWAS \n red line: 1/2 year, 1y, 1.5y, 2y  \n only events Indexdate >0")
  print(p2)
  
  # by PHEWAS, Indexdate > 0 
  p1 <- ggplot(by_phewas_0,aes(x=PHEWAS_STRING,y=IXDAYS)) +
    geom_boxplot(color="blue") +
    theme(axis.text.x = element_text(angle = 25, hjust = 1,size=rel(0.5))) +
    geom_hline(yintercept = c(0.5*365,365,1.5*365,2*365),color="red",linetype="dashed") +
    ggtitle("First occurrence of new main-comorbidities, by PHEWAS \n red line: 1/2 year, 1y, 1.5y, 2y  \n only events Indexdate >0")
  print(p1)
  
  p2 <- ggplot(by_phewas_0,aes(x=PHEWAS_STRING,y=IXDAYS)) +
    geom_violin(color="blue") +
    theme(axis.text.x = element_text(angle = 25, hjust = 1,size=rel(0.5))) +
    geom_hline(yintercept = c(0.5*365,365,1.5*365,2*365),color="red",linetype="dashed") +
    ggtitle("First occurrence of new main-comorbidities, by PHEWAS \n red line: 1/2 year, 1y, 1.5y, 2y  \n only events Indexdate >0")
  print(p2)
  
  # view "outliers"
  outl <- aggregate(data=by_phewas_0, IXDAYS ~ PHEWAS_STRING, FUN=median)
  outl <- outl %>% filter(IXDAYS >= quantile(outl$IXDAYS,probs = seq(0.1,1,0.1))[8]) # 8 = 80% quantil
  
  p1 <- ggplot(by_phewas_0 %>% filter(PHEWAS_STRING %in% outl$PHEWAS_STRING),aes(x=PHEWAS_STRING,y=IXDAYS)) +
    geom_boxplot(color="blue") +
    theme(axis.text.x = element_text(angle = 25, hjust = 1)) +
    geom_hline(yintercept = c(0.5*365,365,1.5*365,2*365),color="red",linetype="dashed") +
    ggtitle("First occurrence of new main-comorbidities, by PHEWAS \n red line: 1/2 year, 1y, 1.5y, 2y  \n only events Indexdate >0 \n outliers (>= 80% quantile)")
  print(p1)
  
  p2 <- ggplot(by_phewas_0 %>% filter(PHEWAS_STRING %in% outl$PHEWAS_STRING),aes(x=PHEWAS_STRING,y=IXDAYS)) +
    geom_violin(color="blue") +
    theme(axis.text.x = element_text(angle = 25, hjust = 1)) +
    geom_hline(yintercept = c(0.5*365,365,1.5*365,2*365),color="red",linetype="dashed") +
    ggtitle("First occurrence of new main-comorbidities, by PHEWAS \n red line: 1/2 year, 1y, 1.5y, 2y  \n only events Indexdate >0 \n outliers (>= 80% quantile)")
  print(p2)
}


