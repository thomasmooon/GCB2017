n_distinct(PATIENTS_AGE_REGION$ENROLID)


# distribution of max(IXDAY | SUBSTANCE)
ix.subst <- 
  PATIENTS_AGE_REGION %>% 
  filter(SUBSTANCENAME != "") %>% 
  select(ENROLID, SUBSTANCENAME, IXDAYS) %>% 
  distinct()
ix.subst <- aggregate(IXDAYS ~., ix.subst %>% select(-SUBSTANCENAME) %>% distinct(), max)

# distribution of max(IXDAY | non-AED)
ix.nonAed <- 
  PATIENTS_AGE_REGION %>% 
  filter(SUBSTANCENAME != "", isAED != TRUE) %>% 
  select(ENROLID, SUBSTANCENAME, IXDAYS) %>% 
  distinct()
ix.nonAed <- aggregate(IXDAYS ~., ix.nonAed %>% select(-SUBSTANCENAME) %>% distinct(), max)

# distribution of max(IXDAY | AED)
ix.aed <- 
  PATIENTS_AGE_REGION %>% 
  filter(SUBSTANCENAME != "", isAED == TRUE) %>% 
  select(ENROLID, SUBSTANCENAME, IXDAYS) %>% 
  distinct()
ix.aed <- aggregate(IXDAYS ~., ix.aed %>% select(-SUBSTANCENAME) %>% distinct(), max)

# distribution of max(IXDAY | DIAG)
ix.diag <- 
  PATIENTS_AGE_REGION %>% 
  filter(DIAG != "") %>% 
  select(ENROLID, DIAG, IXDAYS) %>% 
  distinct()
ix.diag <- aggregate(IXDAYS ~., ix.diag %>% select(-DIAG) %>% distinct(), max)

par(mfrow=c(2,4))
hist(ix.subst$IXDAYS, freq = FALSE, xlim = c(-1500,1500))
hist(ix.nonAed$IXDAYS, freq = FALSE, xlim = c(-1500,1500))
hist(ix.aed$IXDAYS, freq = FALSE, xlim = c(-1500,1500))
hist(ix.diag$IXDAYS, freq = FALSE, xlim = c(-1500,1500))

hist(ix.subst$IXDAYS, xlim = c(-1500,1500))
hist(ix.nonAed$IXDAYS, xlim = c(-1500,1500))
hist(ix.aed$IXDAYS,xlim = c(-1500,1500))
hist(ix.diag$IXDAYS, xlim = c(-1500,1500))


# cohort with no main-comorbidities
###################################
MAIN.COMORB <- c("Hypertension","Diabetes","Hyperlipidemia","Anxiety","Migraine","Depression","Stroke.IschemAttack")
MAIN.COMORB.PHEWAS <- vector("list",length(MAIN.COMORB))
names(MAIN.COMORB.PHEWAS) <- MAIN.COMORB
MAIN.COMORB.PHEWAS$Hypertension <- c("Essential hypertension")
MAIN.COMORB.PHEWAS$Diabetes <- c("Type 2 diabetes")
MAIN.COMORB.PHEWAS$Hyperlipidemia <- c("Hyperlipidemia","Mixed hyperlipidemia")
MAIN.COMORB.PHEWAS$Anxiety <- c("Anxiety disorder","Generalized anxiety disorder","Anxiety, phobic and dissociative disorders","Agorophobia, social phobia, and panic disorder")
MAIN.COMORB.PHEWAS$Stroke.IschemAttack <- c("Ischemic stroke", "Transient cerebral ischemia")
MAIN.COMORB.PHEWAS$Migraine <- c("Migraine","Migrain with aura")
MAIN.COMORB.PHEWAS$Depression <- c("Depression","Major depressive disorder")
MAIN.COMORB.PHEWAS <-
  data.frame (cbind(
    MAIN.COMORB = as.character(unlist(mapply(rep, MAIN.COMORB, unlist(lapply( MAIN.COMORB.PHEWAS, length))))),
    PHEWAS_STRING = as.character(unlist(MAIN.COMORB.PHEWAS))
  ), row.names = NULL, stringsAsFactors = FALSE)

PATIENTS_AGE_REGION <- PATIENTS_AGE_REGION %>% mutate(isMainCM = PHEWAS_STRING %in% MAIN.COMORB.PHEWAS$PHEWAS_STRING)
patMainCM <- PATIENTS_AGE_REGION %>% filter(isMainCM == TRUE) %>% select(ENROLID) %>% distinct()
patNoMainCM <- PATIENTS_AGE_REGION %>% filter(!(ENROLID %in% patMainCM$ENROLID)) %>% select(ENROLID) %>% distinct() 

# distribution of max(IXDAY | SUBSTANCE)
ix.subst <- 
  PATIENTS_AGE_REGION %>% 
  filter(ENROLID %in% patNoMainCM$ENROLID) %>% 
  filter(SUBSTANCENAME != "") %>% 
  select(ENROLID, SUBSTANCENAME, IXDAYS) %>% 
  distinct()
ix.subst <- aggregate(IXDAYS ~., ix.subst %>% select(-SUBSTANCENAME) %>% distinct(), max)

# distribution of max(IXDAY | non-AED)
ix.nonAed <- 
  PATIENTS_AGE_REGION %>% 
  filter(ENROLID %in% patNoMainCM$ENROLID) %>% 
  filter(SUBSTANCENAME != "", isAED != TRUE) %>% 
  select(ENROLID, SUBSTANCENAME, IXDAYS) %>% 
  distinct()
ix.nonAed <- aggregate(IXDAYS ~., ix.nonAed %>% select(-SUBSTANCENAME) %>% distinct(), max)

# distribution of max(IXDAY | AED)
ix.aed <- 
  PATIENTS_AGE_REGION %>% 
  filter(ENROLID %in% patNoMainCM$ENROLID) %>% 
  filter(SUBSTANCENAME != "", isAED == TRUE) %>% 
  select(ENROLID, SUBSTANCENAME, IXDAYS) %>% 
  distinct()
ix.aed <- aggregate(IXDAYS ~., ix.aed %>% select(-SUBSTANCENAME) %>% distinct(), max)

# distribution of max(IXDAY | DIAG)
ix.diag <- 
  PATIENTS_AGE_REGION %>% 
  filter(ENROLID %in% patNoMainCM$ENROLID) %>% 
  filter(DIAG != "") %>% 
  select(ENROLID, DIAG, IXDAYS) %>% 
  distinct()
ix.diag <- aggregate(IXDAYS ~., ix.diag %>% select(-DIAG) %>% distinct(), max)

pdf(file="output/max.ixdays.PDF",width = 10,height = 7)
par(mfrow=c(3,4))
hist(ix.subst$IXDAYS, freq = FALSE, xlim = c(-1500,1500))
hist(ix.nonAed$IXDAYS, freq = FALSE, xlim = c(-1500,1500))
hist(ix.aed$IXDAYS, freq = FALSE, xlim = c(-1500,1500))
hist(ix.diag$IXDAYS, freq = FALSE, xlim = c(-1500,1500))

hist(ix.subst$IXDAYS, xlim = c(-1500,1500))
hist(ix.nonAed$IXDAYS, xlim = c(-1500,1500))
hist(ix.aed$IXDAYS,xlim = c(-1500,1500))
hist(ix.diag$IXDAYS, xlim = c(-1500,1500))

plot(ecdf(ix.subst$IXDAYS), xlim = c(-1500,1500))
abline(v=c(365,730), col="blue")
plot(ecdf(ix.nonAed$IXDAYS), xlim = c(-1500,1500))
abline(v=c(365,730), col="blue")
plot(ecdf(ix.aed$IXDAYS), xlim = c(-1500,1500))
abline(v=c(365,730), col="blue")
plot(ecdf(ix.diag$IXDAYS), xlim = c(-1500,1500))
abline(v=c(365,730), col="blue")

 dev.off()
 