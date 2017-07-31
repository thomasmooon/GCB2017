mRMR.Ranger <- function(train.data, test.data,  feature_count = 500, num.trees = 5000, ncores = 10, importance = 'none' ) {

  
# load data
###########
  
  load("~/epilepsie_preprocessing_descrStat/output/featureMatrix/DRUG.FEATMAT.RData") # drug raw features
  load("~/epilepsie_preprocessing_descrStat/output/featureMatrix/DISEASE.FEATMAT.RData") # disease raw features
  load("~/epilepsie_preprocessing_descrStat/output/featureMatrix/P.FEAT.ENROL.RData") # # patient features, contains age, sex, region, hospdays, metqty per aed, claims info (hltpln, plntyp, presc)
   setDT(P.FEAT.ENROL)
  load("~/epilepsie_preprocessing_descrStat/output/featureMatrix/censored.incident.cvsamp.RData") # enrolid, time, status, cv-strata
  load("~/epilepsie_preprocessing_descrStat/output/featureMatrix/P.QUART.SUBST.RData") # contains treatment by quarter
  load("~/epilepsie_preprocessing_descrStat/output/featureMatrix/P.QUART.DIAG.RData") # contains diagnosis by quarter
  
# load packages + functions
###########################
# load packages
  require(dplyr)
  require(tidyr)
  library(data.table)
  library(mRMRe)
  set.thread.count(ncores) # allow up to 10 cores for parallelization
  
# load functions
  source("functions/Lib.featSelAndRF.R")

# feature selection
###################
  features <- 
    GetTrainingFeatureMatrix(
      trainingData =         train.data, 
      rawDrugFeatures =      drug.features, 
      patQuartTreat =        P.QUART.SUBST, 
      rawDiseaseFeatures =   DISEASES.FEATMAT, 
      patQuartDiag =         P.QUART.DIAG, 
      patFeatures =          P.FEAT.ENROL
    )
  
  features$mRMR <- featureSelectionMRMR(featureMatrix = features$featureMatrix, feature_count = feature_count) # mRMR feature selection

# training and test feature matrices
####################################
  # feature collection
  survival_features  <- c("time","status")
  character_features <- c("SEXC","REGIONC","SEXC","COVERAGE_OF_PRESCRIPTIONS", "HLTHPLNC", "PLANTYPC")
  mRMR_features <- features$mRMR$featureNames
  all_features <- unique(c(survival_features, character_features, mRMR_features))
  
  # training feature matrix: add "GENDER" and "REGIONC" to feature matrix
  features$fm.training <- features$featureMatrix[,all_features, with = FALSE]
  
  # test feature matrix
  features$fm.test <- 
    GetTestFeatureMatrix(
      # trainingFeatures =     setdiff(all_features, c("time","status")),
      trainingFeatures =     all_features,
      testData =             test.data, 
      rawDrugFeatures =      drug.features, 
      patQuartTreat =        P.QUART.SUBST, 
      rawDiseaseFeatures =   DISEASES.FEATMAT, 
      patQuartDiag =         P.QUART.DIAG, 
      patFeatures =          P.FEAT.ENROL
    )
  
  
  features$fm.test$featureMatrix$ENROLID <- c() # ENROLID useless for ranger
  

# survival random forest
########################
  library(ranger)
  library(survival)
    
# train without weights
 time.train <- system.time(x.ranger <- ranger(Surv(time, status) ~ ., data = features$fm.training, num.trees = num.trees, importance = importance)) # train
 time.predict <- system.time(x.predict <- predict(x.ranger, dat = features$fm.test$featureMatrix)) # predict

# return
  return(list(features = features,
              x.ranger = x.ranger,
              x.predict = x.predict,
              time.train = time.train,
              time.predict = time.predict))

}