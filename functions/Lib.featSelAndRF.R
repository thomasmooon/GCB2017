# function library for feature Selection and random forest
##########################################################

####################################################################################################

# create test and training dataset
GetTestTrainAssignment <- function(censored, comorb, seedCensored = 1, seedComorb = 2, train.fraction = 5/6, test.fraction = 1/6) {
  
  set.seed(seedCensored)
  train.censored.idx <- sample(1 : nrow(censored), size = train.fraction*nrow(censored))
  train.censored     <- censored[train.censored.idx,]
  test.censored      <- censored[-train.censored.idx,]
  
  set.seed(seedComorb)
  train.comorb.idx <- sample(1 : nrow(comorb), size = train.fraction*nrow(comorb))
  train.comorb     <- comorb[train.comorb.idx,]
  test.comorb      <- comorb[-train.comorb.idx,]
  
  train.data <- rbind(train.censored, train.comorb)
  test.data  <- rbind(test.censored, test.comorb)
  
  return(list(train.data = train.data, test.data = test.data))
}

####################################################################################################

# efficient attachment of raw drug features to patients treatment table

AddTreatmentFeatures <- function(rawDrugFeatures, patQuartTreat, enrolment) {
  
  # get training dataset
  train.idx <- patQuartTreat$ENROLID %in% enrolment$train.data$ENROLID
  training  <- patQuartTreat[train.idx, ] 
  
  # pre-filtering: drop rare features
  ###################################
  
  # threshold below features will be dropped
  nPat          <- n_distinct(training$ENROLID)
  nPatFeatThres <- ceiling(nPat * 0.05) 
  
  setDT(training)
  
  # maximum occurrence of a substance in any quarter in the training dataset
  treatMaxN <- 
    training %>% 
    group_by(SUBSTANCENAME, QUARTER) %>% 
    tally() %>% # n substance per quarter
    ungroup() %>% 
    group_by(SUBSTANCENAME) %>% 
    mutate(max = max(n)) %>%
    dplyr::select(SUBSTANCENAME, max) %>%
    distinct() # max(n) substance in any quarter
  
  # get non-sparse drug features (feature occurs in at least nPatFeatThres patients in *any* quarter)
  # -> drop features which don't fulfill this
  training.maxN <- treatMaxN[rawDrugFeatures, on = "SUBSTANCENAME", nomatch=0] # data.table inner join
  setDF(training.maxN)
  omitCols                  <- which(colnames(training.maxN) %in% c("SUBSTANCENAME","max"))
  training.maxN[,-omitCols] <- training.maxN[,-omitCols] * training.maxN$max
  training.maxN$max         <- c()
  omitCols                  <- which(colnames(training.maxN) %in% c("SUBSTANCENAME"))
  nonSparseFeatures         <- colSums(training.maxN[,-omitCols]) >= nPatFeatThres
  nonSparseFeatures         <- c(TRUE, nonSparseFeatures) # 1st TRUE = SUBSTANCENAME
  rawDrugFeatures.nonSparse <- rawDrugFeatures[,nonSparseFeatures] %>% filter(SUBSTANCENAME %in% training.maxN$SUBSTANCENAME)
  
  # match pre-filtered drug features to training dataset
  setDT(rawDrugFeatures.nonSparse)
  setDT(training)
  training <- training[rawDrugFeatures.nonSparse, on = "SUBSTANCENAME", nomatch = 0]
  training[,SUBSTANCENAME := NULL]
  
  # aggregate by .(enrolid, quarter) to retrieve quantitative features using sum()
  names <- colnames(training)[3:ncol(training)]
  training.feat <- 
    dcast(
      training, 
      ENROLID ~ QUARTER, 
      fun.aggregate = sum, # summarise frequency for quantitative features
      sep = ".x.", 
      value.var = names, 
      fill = 0) 
  
  # drop features with don't occur in at least nPatFeatThres cases
  colnames <- training.feat[, lapply(.SD, function(x) sum(x>0) < nPatFeatThres) , .SDcols = !"ENROLID"]
  delCols  <- as.numeric(which(unlist(colnames)))
  delCols  <- delCols +1 # shift by 1 because column ENROLID was omitted 2 rows above
  cat("\n","Treatment features:\n",ncol(training.feat)- 1 - length(delCols),"of",ncol(training.feat)-1,"feature columns appear in at least", nPatFeatThres,"patients","\n",sep = " ")
  training.feat <- training.feat[, -delCols, with = FALSE]
  
  return(training.feat)
}

####################################################################################################

# efficient attachment of raw drug features to patients treatment table

AddDiagnosisFeatures <- function(rawDiseaseFeatures, patQuartDiag, enrolment) {
  
  train.idx <- patQuartDiag$ENROLID %in% enrolment$train.data$ENROLID
  training  <- patQuartDiag[train.idx, ] 
  
  # pre-filtering: drop rare features
  ###################################
  nPat          <- n_distinct(training$ENROLID)
  nPatFeatThres <- ceiling(nPat * 0.05)
  
  setDT(training)
  setDT(rawDiseaseFeatures)
  
  # qualitative aggregation
  rawDiseaseFeatures$ICD9 <- as.numeric(rawDiseaseFeatures$ICD9)
  rawDiseaseFeatures      <- rawDiseaseFeatures[, lapply(.SD, max), by = ICD9]
  
  # maximum occurrence of a diagnosis in any quarter in the training dataset
  diagMaxN <-
    training %>% 
    group_by(DIAG, QUARTER) %>% 
    tally() %>% # n diagnosis per quarter
    ungroup() %>% 
    group_by(DIAG) %>% 
    mutate(max = max(n)) %>%
    dplyr::select(DIAG, max) %>%
    distinct() # max(n) diagnosis in any quarter
  
  suppressWarnings(diagMaxN$DIAG <- as.numeric(diagMaxN$DIAG))
  diagMaxN      <- diagMaxN[complete.cases(diagMaxN),]
  setDT(diagMaxN)
  
  # get non-sparse disease features (feature occurs in at least nPatFeatThres patients in *any* quarter)
  training.maxN <- merge(diagMaxN,rawDiseaseFeatures, by.x = "DIAG", by.y="ICD9", all.x = 0) 
  setDF(training.maxN)
  omitCols                  <- which(colnames(training.maxN) %in% c("DIAG","max"))
  training.maxN[,-omitCols] <- training.maxN[,-omitCols] * training.maxN$max
  training.maxN$max         <- c()
  omitCols                  <- which(colnames(training.maxN) %in% c("DIAG"))
  nonSparseFeatures         <- colSums(training.maxN[,-omitCols]) >= nPatFeatThres
  nonSparseFeatures         <- c(TRUE, nonSparseFeatures) # 1st TRUE = DIAG
  rawDiseaseFeatures.nonSparse <- 
    rawDiseaseFeatures[,nonSparseFeatures, with = FALSE] %>% 
    filter(ICD9 %in% training.maxN$DIAG)
  
  # match pre-filtered disease features to training dataset
  setDT(rawDiseaseFeatures.nonSparse)
  setDT(training)
  training$DIAG <- as.numeric(training$DIAG)
  training <- merge(training, rawDiseaseFeatures.nonSparse, by.x = "DIAG", by.y="ICD9", all.x = 0) # data.table inner join
  training[,DIAG := NULL]
  
  # aggregate by .(enrolid, quarter) to retrieve qualitative features using max()
  names <- colnames(training)[3:ncol(training)]
  training.feat <- 
    dcast(
      training, ENROLID ~ QUARTER, 
      fun.aggregate = max, # take max() of the boolean features to retrieve qualitative aggregation
      sep = ".x.", 
      value.var = names, 
      fill = 0) 
  
  ## drop features with don't occur in at least nPatFeatThres cases
  delCols <- training.feat[, lapply(.SD, function(x) sum(x>0) < nPatFeatThres) , .SDcols = !"ENROLID"]
  delCols <- as.numeric(which(unlist(delCols)))
  delCols <- delCols +1 # shift by 1 because column ENROLID was omitted 2 rows above
  cat("\n","Diagnosis features: \n",ncol(training.feat)- 1 - length(delCols),"of",ncol(training.feat)-1,"feature columns appear in at least", nPatFeatThres,"patients \n",sep = " ")
  training.feat <- training.feat[, -delCols, with = FALSE]
  
  return(training.feat)
}

####################################################################################################

# This is a convenience wrapper to compute the feature matrices for the particular main-comorbidity
# <enrolment> 

GetMCFeatureMatrix <- function(rawDrugFeatures, patQuartTreat, mainComorb.Enrolment, rawDiseaseFeatures, patQuartDiag, patFeatures) {
  # treatment features
  mainComorb.train.treat <- 
    AddTreatmentFeatures(
      rawDrugFeatures = drug.features,
      patQuartTreat = P.QUART.SUBST,
      enrolment = mainComorb.Enrolment)
  
  # disease features
  mainComorb.train.diag <- 
    AddDiagnosisFeatures(
      rawDiseaseFeatures = DISEASES.FEATMAT, 
      patQuartDiag = P.QUART.DIAG,
      enrolment = mainComorb.Enrolment)
  
  # patient features (age, sex, region, MHSAFL, HOSPFL, ...)
  mainComorb.train.pat <- patFeatures[ENROLID %in% mainComorb.Enrolment$train.data$ENROLID,]
  
  # patient survival data (ENROLID, time, status)
  mainComorb.train.surv <- mainComorb.Enrolment$train.data
  
  # create feature matrix
  mainComorb.featMat <- 
    mainComorb.train.surv %>% 
    inner_join(mainComorb.train.pat) %>%
    inner_join(mainComorb.train.diag) %>%
    inner_join(mainComorb.train.treat)
  
  # check for features later than quarter 2
  source("functions/NoMedicalHistoryColumns.R")
  noMHColumns <- NoMedicalHistoryColumns(mainComorb.featMat)
  
  return(
    list(
      featureMatrix = mainComorb.featMat,
      treatmentFeatures = mainComorb.train.treat,
      diagnosisFeatures = mainComorb.train.diag,
      patientFeatures = mainComorb.train.pat,
      survivalFeatures = mainComorb.train.surv
      )
    )
}


####################################################################################################


featureSelectionMRMR <- function(featureMatrix, feature_count = 500){
  setDF(featureMatrix)
  featureMatrix$time <- as.numeric(featureMatrix$time)
  
  # GENDER and REGIONC are inappropiate for mRMRe feature selection, because type must be either numeric, survival or ordered factor
  c1                 <- which(colnames(featureMatrix) %in% c("ENROLID","GENDER","REGIONC"))
  # featureMatrix.c1 <- featureMatrix[,c1]
  featureMatrix.fs <- featureMatrix[,-c1] # select columns appropiate for mRMRe algorithm
  
  # create mRMR.data object
  dd             <- mRMR.data(data = featureMatrix.fs)
  
  # uncomment to stratify by status for bootstrap
    # sampleStrata(dd) <-  as.factor(featureMatrix.fs$status)
  
  # select time and status as variables as reference variables to select features which correlated most to status and time,
  # but a minimum correlated to each other (= most redundant) 
  target_indices <- which(colnames(featureMatrix.fs) %in% c("time","status"))
  exect <- system.time(fs <- mRMR.ensemble(data = dd, target_indices = target_indices, feature_count = feature_count, solution_count = 1))
  # see also: http://stackoverflow.com/questions/36502796/using-mrmre-in-r
  
  ## print the names of the selected features for (each distinct) mRMR solutions
  featureNames <- print(apply(solutions(fs)[[1]], 2, function(x, y) { return(y[x]) }, y=featureNames(dd)))
  
  return(list(exec.time = exect, featureSelectionObject = fs, featureNames = featureNames))
}