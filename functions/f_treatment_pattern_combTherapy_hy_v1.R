
# txvis visualization of treatment patterns
###########################################

# assign each element to it's corresponding time period
# (quarter, or maybe half-year)
# 2 variants:
# A: by SUBSTANCENAME
# B: by PHARMCLS1
#------------------------------------------------------

# step-by-step:
#--------------
# 1. compute time aggregators (quarters, half-year)
# 2. cutoff AEDs which < 5% occurence
# 3. aggregate treatments when they occur within the same treatment period, ensure that they are sorted
# 

#######

treat_pattern_combinationTherapy <- function(PATIENTS_AGE_REGION) {

library(txvis)


# transform dates to quartely and half_year time periods
dummy_date <- as.Date("2000-12-31","%Y-%m-%d")

samples <- 
  PATIENTS_AGE_REGION %>% 
  mutate(quarter=floor(IXDAYS/90)) %>%  # compute quarter sequences
  mutate(half_year=floor(IXDAYS/180)) %>%  # compute half-year sequences
  filter(quarter >= 0) %>% 
  mutate(quart_start = dummy_date + quarter*90) %>% 
  mutate(quart_end   = dummy_date + quarter*90 + 90 - 1) %>% 
  mutate(half_year_start = dummy_date + half_year*180) %>% 
  mutate(half_year_end   = dummy_date + half_year*180 + 180 - 1) %>% 
  dplyr::select(ENROLID,STARTDT,ENDDT,PHEWAS_STRING,SUBSTANCENAME,quarter,half_year,quart_start,quart_end,half_year_start,half_year_end) %>% 
  distinct() 

# order to provide an equal concatenation order in the aggregation function below
ordered <- order(samples$ENROLID,samples$quarter,samples$SUBSTANCENAME)
samples <- samples[ordered,]

# AEDs which occur in at least 5% of patients
print(" Cutoff AEDs which do not occur in at least 5% of patients.")
aed_subst_cutoff  <-
  PATIENTS_AGE_REGION %>%
  filter(isAED == TRUE) %>%
  dplyr::select(SUBSTANCENAME, ENROLID) %>%
  distinct() %>% 
  group_by(SUBSTANCENAME) %>%
  tally(sort =  TRUE) %>% 
  mutate(n_perc = round(n/nPatients,2)) %>% 
  filter(n_perc >= 0.05) %>% 
  dplyr::select(SUBSTANCENAME)


# create aggregated quarterly and half-year treatment events for txvis method
#--------------------------------------------------------------

samples_AEDs <- 
  samples %>% 
  filter(SUBSTANCENAME %in% aed_subst_cutoff$SUBSTANCENAME) %>% # apply cut-off
  dplyr::select(ENROLID, SUBSTANCENAME, quarter, half_year,quart_start,quart_end,half_year_start,half_year_end) %>% 
  distinct()

print("Create combination therapy events")

# half-year
samples_AEDs_hy <-
  samples_AEDs %>% 
  dplyr::select(ENROLID,SUBSTANCENAME, half_year, half_year_start,half_year_end) %>% 
  unique()

samples_AEDs_hy <-
  aggregate(
    SUBSTANCENAME ~ ENROLID + half_year + half_year_start + half_year_end,
    data=samples_AEDs_hy,
    paste,
    collapse = ","
  )

# create aggregated quarterly and half-year diagnosis events for txvis method
#--------------------------------------------------------------
sample_diags <- 
  samples %>% 
  filter(PHEWAS_STRING != "") %>% 
  dplyr::select(ENROLID,PHEWAS_STRING, quarter, half_year, quart_start,quart_end,half_year_start,half_year_end) %>% 
  distinct()

# half-year
sample_diags_hy <-
  sample_diags %>% 
  dplyr::select(ENROLID,PHEWAS_STRING, half_year, half_year_start,half_year_end) %>% 
  unique()


# create txvis objects
#######################

txdata_hy <- create_txvis(patient = samples_AEDs_hy$ENROLID,
                          treatment = samples_AEDs_hy$SUBSTANCENAME,
                          start     = samples_AEDs_hy$half_year_start,
                          end       = samples_AEDs_hy$half_year_end,
                          date_format = "%Y-%m-%d",
                          ev_patient = sample_diags_hy$ENROLID,
                          events     = sample_diags_hy$PHEWAS_STRING,
                          event_date = sample_diags_hy$half_year_start,
                          event_end_date = sample_diags_hy$half_year_end)

return(txdata_hy)
}

