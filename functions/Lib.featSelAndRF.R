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

AddTreatmentFeatures <- function(rawDrugFeatures, patQuartTreat, enrolment, trainingFeatures = NULL ) {
  
  if(length(trainingFeatures) == 0) {
  # get x dataset
  idx <- patQuartTreat$ENROLID %in% enrolment$ENROLID
  x  <- patQuartTreat[idx, ] 
  
  # pre-filtering: drop rare features
  
  ## threshold below features will be dropped
  nPat          <- n_distinct(x$ENROLID)
  nPatFeatThres <- ceiling(nPat * 0.05) 
  
  ## maximum occurrence of a substance in any quarter in the x dataset
  setDT(x)
  treatMaxN <- 
    x %>% 
    group_by(SUBSTANCENAME, QUARTER) %>% 
    tally() %>% # n substance per quarter
    ungroup() %>% 
    group_by(SUBSTANCENAME) %>% 
    mutate(max = max(n)) %>%
    dplyr::select(SUBSTANCENAME, max) %>%
    distinct() # max(n) substance in any quarter
  
  ## get non-sparse drug features (feature occurs in at least nPatFeatThres patients in *any* quarter)
  ## -> drop features which don't fulfill this
  setDT(rawDrugFeatures)
  setDT(treatMaxN)
  x.maxN <- treatMaxN[rawDrugFeatures, on = "SUBSTANCENAME", nomatch=0] # data.table inner join
  setDF(x.maxN)
  omitCols <- which(colnames(x.maxN) %in% c("SUBSTANCENAME","max"))
  x.maxN[,-omitCols] <- x.maxN[,-omitCols] * x.maxN$max
  x.maxN$max <- c()
  omitCols <- which(colnames(x.maxN) %in% c("SUBSTANCENAME"))
  nonSparseTreatFeatures <<- colSums(x.maxN[,-omitCols]) >= nPatFeatThres
  nonSparseTreatFeatures  <<- c(TRUE, nonSparseTreatFeatures) # 1st TRUE = SUBSTANCENAME, assignment to global environment with <<- for recycling in the "else if" part below
  rawDrugFeatures.nonSparse <- rawDrugFeatures[,nonSparseTreatFeatures] %>% filter(SUBSTANCENAME %in% x.maxN$SUBSTANCENAME)
  
  ## match pre-filtered drug features to x dataset
  setDT(rawDrugFeatures.nonSparse)
  setDT(x)
  x <- x[rawDrugFeatures.nonSparse, on = "SUBSTANCENAME", nomatch = 0]
  x[,SUBSTANCENAME := NULL]
  
  # aggregate by .(enrolid, quarter) to retrieve quantitative features using sum()
  names <- setdiff(colnames(x),c("ENROLID","QUARTER"))
  x.feat <- dcast(x , ENROLID ~ QUARTER, fun.aggregate = sum, sep = ".x.", value.var = names, fill = 0) # summarise frequency for quantitative features
  
  # drop features with don't occur in at least nPatFeatThres cases
  colnames <- x.feat[, lapply(.SD, function(x) sum(x>0) < nPatFeatThres) , .SDcols = !"ENROLID"]
  delCols  <- as.numeric(which(unlist(colnames)))
  delCols  <- delCols +1 # shift by 1 because column ENROLID was omitted 2 rows above
  cat("\n","Treatment features:\n",ncol(x.feat)- 1 - length(delCols),"of",ncol(x.feat)-1,"feature columns appear in at least", nPatFeatThres,"patients","\n",sep = " ")
  x.feat <- x.feat[, -delCols, with = FALSE]
  
  return(x.feat)
  } else if(length(trainingFeatures)>0) {
    # filter data
    idx <- patQuartTreat$ENROLID %in% enrolment$ENROLID
    x <- patQuartTreat[idx, ] 
    
    # join drug features to treatment  
    setDT(x)
    rawDrugFeatures.nonSparse <- rawDrugFeatures[,nonSparseTreatFeatures] # speed boost: nonSparseTreatFeatures is from global environment (assigned above via <<-)
    rm(nonSparseTreatFeatures, envir = .GlobalEnv) # delete global variable
    x <- x[rawDrugFeatures.nonSparse, on = "SUBSTANCENAME", nomatch=0] # data.table inner join
    
    # aggregate by .(enrolid, quarter) to retrieve quantitative features using sum()
    x[,SUBSTANCENAME := NULL]
    names <- setdiff(colnames(x),c("ENROLID","QUARTER"))
    x.feat <- dcast(x,ENROLID ~ QUARTER, fun.aggregate = sum, sep = ".x.", value.var = names, fill = 0)  # summarise frequency for quantitative features
    
    # omit cols, which were not defined as trainingFeatures
    cols <- c("ENROLID", trainingFeatures)
    x.feat <- x.feat[,colnames(x.feat) %in% cols, with = FALSE]
    
    return(x.feat)
  }  
}

####################################################################################################

# efficient attachment of raw drug features to patients treatment table

AddDiagnosisFeatures <- function(rawDiseaseFeatures, patQuartDiag, enrolment, trainingFeatures = NULL) {
  
  if(length(trainingFeatures) == 0) {
  idx <- patQuartDiag$ENROLID %in% enrolment$ENROLID
  x  <- patQuartDiag[idx, ] 
  
  # pre-filtering: drop rare features
  nPat          <- n_distinct(x$ENROLID)
  nPatFeatThres <- ceiling(nPat * 0.05)
  
  setDT(x)
  setDT(rawDiseaseFeatures)
  
  ## qualitative aggregation
  suppressWarnings(rawDiseaseFeatures$ICD9 <- as.numeric(rawDiseaseFeatures$ICD9))
  rawDiseaseFeatures      <- rawDiseaseFeatures[, lapply(.SD, max), by = ICD9]
  
  ## maximum occurrence of a diagnosis in any quarter in the x dataset
  diagMaxN <-
    x %>% 
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
  
  ## get non-sparse disease features (feature occurs in at least nPatFeatThres patients in *any* quarter)
  x.maxN <- merge(diagMaxN,rawDiseaseFeatures, by.x = "DIAG", by.y="ICD9", all.x = 0) 
  setDF(x.maxN)
  omitCols <- which(colnames(x.maxN) %in% c("DIAG","max"))
  x.maxN[,-omitCols] <- x.maxN[,-omitCols] * x.maxN$max
  x.maxN$max <- c()
  omitCols <- which(colnames(x.maxN) %in% c("DIAG"))
  nonSparseDiagFeatures <<- colSums(x.maxN[,-omitCols]) >= nPatFeatThres
  nonSparseDiagFeatures <<- c(TRUE, nonSparseDiagFeatures) # 1st TRUE = DIAG
  rawDiseaseFeatures.nonSparse <- rawDiseaseFeatures[,nonSparseDiagFeatures, with = FALSE] %>% filter(ICD9 %in% x.maxN$DIAG)
  
  ## match pre-filtered disease features to x dataset
  setDT(rawDiseaseFeatures.nonSparse)
  setDT(x)
  suppressWarnings(x$DIAG <- as.numeric(x$DIAG))
  x <- merge(x, rawDiseaseFeatures.nonSparse, by.x = "DIAG", by.y="ICD9", all.x = 0) # data.table inner join
  x[,DIAG := NULL]
  
  # aggregate by .(enrolid, quarter) to retrieve qualitative features using max()
  names <- setdiff(colnames(x),c("ENROLID","QUARTER"))
  x.feat <- dcast(x, ENROLID ~ QUARTER, fun.aggregate = max, sep = ".x.", value.var = names, fill = 0) # take max() of the boolean features to retrieve qualitative aggregation
  
  ## drop features with don't occur in at least nPatFeatThres cases
  delCols <- x.feat[, lapply(.SD, function(x) sum(x>0) < nPatFeatThres) , .SDcols = !"ENROLID"]
  delCols <- as.numeric(which(unlist(delCols)))
  delCols <- delCols +1 # shift by 1 because column ENROLID was omitted 2 rows above
  cat("\n","Diagnosis features: \n",ncol(x.feat)- 1 - length(delCols),"of",ncol(x.feat)-1,"feature columns appear in at least", nPatFeatThres,"patients \n",sep = " ")
  x.feat <- x.feat[, -delCols, with = FALSE]
  
  return(x.feat)
  } else if(length(trainingFeatures)>0) {
    
    # filter data
    idx <- patQuartDiag$ENROLID %in% enrolment$ENROLID
    x  <- patQuartDiag[idx, ]
    
    # coerce format
    suppressWarnings(x$DIAG <- as.numeric(x$DIAG))
    x <- x[complete.cases(x),]
    
    # qualitative aggregation
    setDT(x)
    setDT(rawDiseaseFeatures)
    suppressWarnings(rawDiseaseFeatures$ICD9 <- as.numeric(rawDiseaseFeatures$ICD9))
    rawDiseaseFeatures <- rawDiseaseFeatures[, lapply(.SD, max), by = ICD9] %>% na.omit()
    
    # use only non-spares features. the non-sparse features "nonSparseDiagFeatures" were computed above and <<- was utilized to assign it to the global environment
    rawDiseaseFeatures.nonSparse <- rawDiseaseFeatures[,nonSparseDiagFeatures, with = FALSE]
    rm(nonSparseDiagFeatures, envir = .GlobalEnv) # delete global variable
    
    # get non-sparse disease features (feature occurs in at least nPatFeatThres patients in *any* quarter)
    x <- merge(x,rawDiseaseFeatures.nonSparse, by.x = "DIAG", by.y="ICD9", all.x = 0) 
    x[,DIAG := NULL]
    
    # aggregate by .(enrolid, quarter) to retrieve qualitative features using max()
    names <- setdiff(colnames(x),c("ENROLID","QUARTER"))
    x.feat <- dcast(x, ENROLID ~ QUARTER, fun.aggregate = max, sep = ".x.", value.var = names, fill = 0) # take max() of the boolean features to retrieve qualitative aggregation
    
    # omit cols, which were not defined as trainingFeatures
    cols <- c("ENROLID", trainingFeatures)
    x.feat <- x.feat[,colnames(x.feat) %in% cols, with = FALSE]
    
    return(x.feat)
  }
}

####################################################################################################

# This is a convenience wrapper to compute the training feature matrices for the particular main-comorbidities

GetTrainingFeatureMatrix <- function(rawDrugFeatures, patQuartTreat, trainingData, rawDiseaseFeatures, patQuartDiag, patFeatures) {
  # treatment features
  cat("\n Add treatment features...\n")
  mainComorb.train.treat <- 
    AddTreatmentFeatures(
      rawDrugFeatures = rawDrugFeatures,
      patQuartTreat = patQuartTreat,
      enrolment = trainingData)
  
  # disease features
  cat("\n Add disease features...\n")
  mainComorb.train.diag <- 
    AddDiagnosisFeatures(
      rawDiseaseFeatures = rawDiseaseFeatures, 
      patQuartDiag = patQuartDiag,
      enrolment = trainingData)
  
  # patient features (age, sex, region, MHSAFL, HOSPFL, ...)
  cat("\n Filter patient features...\n")
  mainComorb.train.pat <- patFeatures[ENROLID %in% trainingData$ENROLID,]
  
  # patient survival data (ENROLID, time, status)
  cat("\n Retrieve survival features...\n")
  mainComorb.train.surv <- trainingData
  
  # create feature matrix
  cat("\n Join all features...\n")
  mainComorb.featMat <- 
    mainComorb.train.surv %>% 
    inner_join(mainComorb.train.pat) %>%
    inner_join(mainComorb.train.diag) %>%
    inner_join(mainComorb.train.treat)
  
  return(list(featureMatrix = mainComorb.featMat))
}


####################################################################################################
# This is a convenience wrapper to compute the test feature matrices for the particular main-comorbidities

GetTestFeatureMatrix <- function(trainingFeatures, rawDrugFeatures, patQuartTreat, testData, rawDiseaseFeatures, patQuartDiag, patFeatures) {
  
  # treatment features
  cat("\n","Add treatment Features...","\n")
  mainComorb.test.treat <- 
    AddTreatmentFeatures(
      rawDrugFeatures = rawDrugFeatures,
      patQuartTreat = patQuartTreat,
      enrolment = testData, 
      trainingFeatures = trainingFeatures)
  
  # disease features
  cat("\n Add disease features...\n")
  mainComorb.test.diag <- 
    AddDiagnosisFeatures(
      rawDiseaseFeatures = rawDiseaseFeatures, 
      patQuartDiag = patQuartDiag,
      enrolment = testData, 
      trainingFeatures = trainingFeatures)
  
  # patient features (age, sex, region, MHSAFL, HOSPFL, ...)
  cat("\n Filter patient features...\n")
  setDT(patFeatures)
  cols <- c("ENROLID", trainingFeatures)
  mainComorb.test.pat <- patFeatures[ENROLID %in% testData$ENROLID, colnames(patFeatures) %in% cols, with = FALSE]
  
  # patient survival data (ENROLID, time, status)
  cat("\n Retrieve survival features...\n")
  mainComorb.test.surv <- testData
  
  # create feature matrix
  cat("\n Join all features...\n")
  mainComorb.featMat <- 
    mainComorb.test.surv %>% 
    inner_join(mainComorb.test.pat) %>%
    inner_join(mainComorb.test.diag) %>%
    inner_join(mainComorb.test.treat)
  
 #
  return(list(featureMatrix = mainComorb.featMat))
}

####################################################################################################

# featureSelectionMRMR <- function(featureMatrix, feature_count = 500){
#   
#   # type conversion for compatiblity
#   featureMatrix$AGE <- as.numeric(featureMatrix$AGE)
#   featureMatrix$time <- as.numeric(featureMatrix$time)
#   
#   setDF(featureMatrix)
#   
#   # SEXC, REGIONC, ... are inappropiate for mRMRe feature selection, because type must be either numeric, survival or ordered factor
#   # ENROLID is no feature, time and status are response variables
#   c1 <- which(colnames(featureMatrix) %in% c("REGIONC","SEXC","COVERAGE_OF_PRESCRIPTIONS", "HLTHPLNC", "PLANTYPC"))
#   c2 <- which(colnames(featureMatrix) %in% c("ENROLID"))
#   cc <- c(c1,c2)
#   featureMatrix.fs <- featureMatrix[,-cc] # select columns appropiate for mRMRe algorithm
#   
#   # create mRMR.data object
#   dd             <- mRMR.data(data = featureMatrix.fs)
#   
#   # uncomment to stratify by status for bootstrap
#     # sampleStrata(dd) <-  as.factor(featureMatrix.fs$status)
#   
#   # select time and status  as reference variables to select features which correlated most to status and time,
#   # but a minimum correlated to each other (= most redundant) 
#   target_indices <- which(colnames(featureMatrix.fs) %in% c("time","status"))
#   exect <- system.time(fs <- mRMR.ensemble(data = dd, target_indices = target_indices, feature_count = feature_count, solution_count = 1))
#   # see also: http://stackoverflow.com/questions/36502796/using-mrmre-in-r
#   
#   ## print the names of the selected features for (each distinct) mRMR solutions
#   featureNames <- print(apply(solutions(fs)[[1]], 2, function(x, y) { return(y[x]) }, y=featureNames(dd)))
#   
#   return(list(exec.time = exect, featureSelectionObject = fs, featureNames = featureNames))
# }

####################################################################################################

featureSelectionMRMR2 <- function(featureMatrix){
  
  # survival object as endpoint
  featureMatrix$Surv <- with(featureMatrix, Surv(time, status, type = "right"))
  featureMatrix$time <- featureMatrix$status <- NULL
  
  # run mRMR
  feature_count <- ncol(featureMatrix) -1 # -1 to exclude the "Surv" column
  dd <- mRMR.data(data = featureMatrix)
  target_indices <- which(colnames(featureMatrix) %in% c("Surv"))
  fs <- mRMR.ensemble(data = dd, target_indices = target_indices, feature_count = feature_count, solution_count = 1)
  featureNames <- print(apply(solutions(fs)[[1]], 2, function(x, y) { return(y[x]) }, y=featureNames(dd)))
  
  # - drop "Surv" columns: this is not feature! It's a bug: MRMR fills up to number of <feature_count> features with the
  # target index feature name
  # - keep only features with score > 0
  fs <- data.frame(scores = unlist(fs@scores), featureNames) %>% filter(scores > 0) %>% filter(featureNames != "Surv")
  return(fs)
}

##############

dropSparseFeatures <- function(cv.samp, featureMatrix, nCrossValidation){
  setDF(featureMatrix)
  cat("drop sparse features","\n")
  cat("  Original dimension: ",dim(featureMatrix),"\n")
  # drop features from feature matrix which don't occur in at least 5% of a particular CV schema.
  # stay conservative: take the smallest CV subset
  setDT(cv.samp)
  # cv.samp[, .N, by = .(main.comorb, cv.samp)] # overview
  minSamp     <- cv.samp[, .N, by = .(main.comorb, cv.samp)][,min(N)]
  cutOff      <- minSamp * 0.05 * (nCrossValidation-1)
  numericCols <- which(sapply(featureMatrix, class) == "numeric")
  colsums     <- apply(featureMatrix[,numericCols] >0, MARGIN = 2, sum)
  dropCols    <- numericCols[colsums < cutOff]
  featureMatrix  <- featureMatrix[,-dropCols]
  cat("  Dimension without sparse features: ",dim(featureMatrix),"\n")
  return(setDT(featureMatrix))
}

##############

# scaleTo01 <- function(featureMatrix) {
#   # in except of enrolid, scale every (numeric) feature to a [0,1] range
#   cat("scaling numeric features...\n")
#   numericCols <- which(sapply(featureMatrix, class) %in% c("numeric","integer"))
#   dropEnrolid <- which(names(featureMatrix) == "ENROLID")
#   numericCols <- setdiff(numericCols, dropEnrolid)
#   maxFeatVals <- apply(featureMatrix[,numericCols,with=F], MARGIN = 2, function(x) max(x, na.rm = T))
#   
#   if(any(maxFeatVals==0)) {
#     maxIsZero <- which(maxFeatVals == 0)
#     maxFeatVals[maxIsZero] <- 1
#   }
#   
#   # finally, scaling. then limit to 6 digits
#   setDF(featureMatrix)
#   featureMatrix[,numericCols] <- sweep(featureMatrix[,numericCols],2,maxFeatVals,"/") %>% round(6)
#   return(featureMatrix)
# }


###############

#' Title
#'
#' @param i 
#' @param cv.samp 
#' @param savepath 
#' @param FM 
#' @param iterator 
#'
#' @param 
#' @return
#' @export
#'
#' @examples
MRMRWrapperForParallelization <- function(i, iterator, cv.samp, FM, savepath) {
  # set "looping" variables
  mc <- iterator$MC[i]
  k <- iterator$CV[i]
  # get test + training dataset
  cv.samp.mc <- copy(cv.samp)
  cv.samp.mc[main.comorb != mc, c("status","time","main.comorb") := list(0, maxixday,"censored") ]
  cv.samp.mc <- cv.samp.mc %>% select(-maxixday) %>% distinct()
  train <- cv.samp.mc %>% filter(cv.samp != k) %>%  select(ENROLID, time, status)
  test <- cv.samp.mc %>% filter(cv.samp == k) %>%  select(ENROLID, time, status)
  
  # run MRMR
  GetMRMRTestAndTrainingMatrices(
    train = train,
    test = test,
    featureMatrix = FM,
    mc = mc, 
    k = k, 
    savepath = savepath
  ) 
}


#' Title
#'
#' @param MC 
#' @param CV 
#' @param cv.samp 
#' @param FM 
#' @param savepath 
#' @param mc.cores 
#'
#' @return
#' @export
#'
#' @examples
ParallelMRMR <- function(MC, CV, cv.samp, FM, savepath, mc.cores = 4) {
  
  iterator <- expand.grid(MC, CV, stringsAsFactors = F)
  colnames(iterator) <- c("MC","CV")
  mclapply(1:nrow(iterator), function(i) MRMRWrapperForParallelization(i, iterator, cv.samp, FM, savepath), mc.preschedule = F, mc.cores = mc.cores)
}