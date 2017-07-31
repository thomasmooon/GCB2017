# date: 14.10.2016

load("~/epilepsie_preprocessing_descrStat/output/incidence_comorbidities_2016-10-12/comb_res.RData")

#' die unten stehende Auswahl beruht auf den Kriterin in 
#' https://ucb-my.sharepoint.com/personal/thomas_gerlach_ucb_com/Documents/PhD/descrStat/2016-10-12/incidence_comorbidities_2016-10-12/combined_results_2016-10-12.xlsx?web=1

main_comorb <- c(
"Essential hypertension",
"Hypertension",
"Type 2 diabetes",
"Secondary diabetes mellitus",
"Other headache syndromes",
"Tension headache",
"Migraine",
"Migrain with aura",
"Hyperlipidemia",
"Mixed hyperlipidemia",
"Depression",
"Major depressive disorder",
"Anxiety disorder",
"Generalized anxiety disorder",
"Anxiety, phobic and dissociative disorders",
"Bipolar",
"Schizophrenia and other psychotic disorders",
"Schizophrenia",
"Schizoid personality disorder")

# how many patients develop a main_comorb in the future?
x <- 
  comb_res %>% 
  filter(PHEWAS_STRING %in% main_comorb, inc == TRUE) %>% 
  select(ENROLID,AGE) %>% 
  distinct()

pop      <- c("all","adult (age >= 18)","children (age < 18)")

nComorb  <- 
  c(
  x %>% tally(), 
  x %>% filter(AGE >= 18) %>% tally(), 
  x %>% filter(AGE < 18) %>% tally())

xSummary <- as_tibble(cbind(pop,nComorb))

knitr::kable(xSummary)

# # what are the most frequent co-morbidities of these patients?
# # # - adult subpopulation
# # # - most frequent non-incident co-morbidities
# # # - most frequent incident co-morbidities

most_freq_comorb <- 
  comb_res %>% 
  filter(ENROLID %in% x$ENROLID) %>% 
  filter(AGE >= 18) %>% 
  select(ENROLID, PHEWAS_STRING,inc) %>% 
  distinct()

most_freq_incident <-
  most_freq_comorb %>% 
  filter(inc == TRUE) %>% 
  select(ENROLID, PHEWAS_STRING) %>% 
  group_by(PHEWAS_STRING) %>% 
  tally(sort = TRUE) %>% 
  rename(n_inc = n) %>% 
  mutate(rank_n_inc = rank(-n_inc))

most_freq_nonIncident <- 
  most_freq_comorb %>% 
  filter(inc == FALSE) %>% 
  select(ENROLID, PHEWAS_STRING) %>% 
  group_by(PHEWAS_STRING) %>% 
  tally(sort = TRUE) %>% 
  rename(n_noninc = n) %>% 
  mutate(rank_n_noninc = rank(-n_noninc))

# # combine incident and non-incident frequencies
comorb_inc_noninc <- 
  most_freq_comorb %>% 
  select(PHEWAS_STRING) %>% 
  distinct() 

comorb_inc_noninc <- 
  left_join(
    comorb_inc_noninc,
    most_freq_incident,
    by="PHEWAS_STRING")

comorb_inc_noninc <- 
  left_join(
    comorb_inc_noninc,
    most_freq_nonIncident,
    by="PHEWAS_STRING")

comorb_inc_noninc <- 
  comorb_inc_noninc[ order(comorb_inc_noninc$n_inc,decreasing = TRUE) ,]

# # add mainComorb flag column
comorb_inc_noninc$isMainComorb <- 
  comorb_inc_noninc$PHEWAS_STRING %in% main_comorb

# export as csv
filename <- paste("output/comorb_inc_noninc","2y_MH","adult",Sys.Date(),".csv",sep="_")
write.csv2(comorb_inc_noninc,filename,row.names=FALSE)

#########################
# substances frequency  #
#########################

load("data/TABLE04SAS7BDAT_SPARSE.RData")
source("functions/setAEDFlag.R")
TABLE04SAS7BDAT_SPARSE <- setAEDFlag(TABLE04SAS7BDAT_SPARSE)

subst_freq <- 
  TABLE04SAS7BDAT_SPARSE %>% 
  filter(ENROLID %in% x$ENROLID) %>% 
  filter(SUBSTANCENAME != "") %>% 
  select(ENROLID, SUBSTANCENAME) %>% 
  distinct() %>% 
  group_by(SUBSTANCENAME) %>% 
  tally(sort=TRUE) %>% 
  rename(n_patients = n) %>% 
  mutate(rank_n = rank(-n_patients))

subst_freq <- 
  left_join(
    subst_freq,
    TABLE04SAS7BDAT_SPARSE %>% select(SUBSTANCENAME,PHARMCLS1, isAED) %>% distinct(),
    by="SUBSTANCENAME")

subst_freq <- subst_freq[, c(1,4,5,2,3)]

# export as csv
filename <- paste("output/substance_freq_pat_with_mainComorb","2y_MH","adult",Sys.Date(),".csv",sep="_")
write.csv2(subst_freq,filename,row.names=FALSE)
