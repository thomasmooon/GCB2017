patient_filter_healthCareEnrolment <- function(PATIENTS_AGE_REGION) {

# monthly enrolment of patients
# Dtstart:  Versicherungsabdeckungs Start
# Dtend:    Versicherungsabdeckungs Ende
# MDays:    Anzahl der Tage im Monat die Patient versichert war (dtend-dtstart)
# Rx/DRUGCOVG:       Wurde Verschreibungen auch übernommen, ja - nein
# Database: ist Patient in CCAE/MDCR oder MDCD versichert

# load
enrol_1yMH_2yFU_1DiagPerQ <- read.csv2(file = "~/epilepsie_preprocessing_descrStat/output/enrol_1yMH_2yFU_1DiagPerQ.csv")
load("data/monthly_enrolment.RData")
monthly_enrolment <- as_tibble(monthly_enrolment)

# fraction of patients where prescriptions were paid by the health insurance
monthly_enrolment$DRUGCOVG        <- as.numeric(monthly_enrolment$DRUGCOVG)
nas                               <- is.na(monthly_enrolment$DRUGCOVG)
monthly_enrolment$DRUGCOVG[nas]   <- 0
monthly_enrolment$insurPaidPrescr <- as.numeric(monthly_enrolment$RX) + as.numeric(monthly_enrolment$DRUGCOVG)
monthly_enrolment$insurPaidPrescr <- factor(monthly_enrolment$insurPaidPrescr, labels = c("No","Yes"))

insP <-
  as.data.frame(
      monthly_enrolment %>%
      dplyr::select(ENROLID, insurPaidPrescr) %>%
      distinct() %>%
      dplyr::select(insurPaidPrescr) %>%
      table()
    )

colnames(insP) <- c("Insurance_paid_prescriptions","number_of_patients")

# average memdays
#-----------------
mean_memDays              <- monthly_enrolment %>% dplyr::select(ENROLID, MEMDAYS) %>% group_by(ENROLID) %>% summarise(mean_memDays = mean(MEMDAYS))
mean_memDays$mean_memDays <- round(mean_memDays$mean_memDays,0)

# define filter 1: patients with private insurance
enrol_filter1 <- 
  monthly_enrolment %>% 
  filter(insurPaidPrescr == "Yes") %>% 
  dplyr::select(ENROLID) %>% 
  distinct()

# define filter 2: keep only patients with at least 29 days per month membership in average
enrol_filter2 <-
  mean_memDays %>% 
  filter(mean_memDays >= 30) %>% 
  dplyr::select(ENROLID) %>% 
  distinct()


# apply filters to data
health_insur_enrol_filter <- intersect(enrol_filter1$ENROLID,enrol_filter2$ENROLID)
idx                 <- with(PATIENTS_AGE_REGION, ENROLID %in% health_insur_enrol_filter)
PATIENTS_AGE_REGION <- PATIENTS_AGE_REGION[idx,]

# short message
cat("Patients remaining after filtering: \n(1) only CCAE/MDCR (additional private insurance) \n(2) gap-free insurance membership",
    "\n",n_distinct(PATIENTS_AGE_REGION$ENROLID))

#
return(PATIENTS_AGE_REGION)
}