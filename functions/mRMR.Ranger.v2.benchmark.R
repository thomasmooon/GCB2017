mRMR.Ranger2 <-
  function(train,
           test,
           feature_count = 500,
           featureMatrix,
           num.trees = 5000,
           ncores = 10,
           mc,
           k,
           importance = 'none',
           scale.permutation.importance = F,
           ranger.save.memory = T) {
    
  
  # setup
  #######
  featureSelectionMRMR2 <- function(featureMatrix, feature_count = 500){
    dd             <- mRMR.data(data = featureMatrix)
    target_indices <- which(colnames(featureMatrix) %in% c("time","status"))
    exect <- system.time(fs <- mRMR.ensemble(data = dd, target_indices = target_indices, feature_count = feature_count, solution_count = 1))
    featureNames <- print(apply(solutions(fs)[[1]], 2, function(x, y) { return(y[x]) }, y=featureNames(dd)))
    return(list(exec.time = exect, featureSelectionObject = fs, featureNames = featureNames))
  }
  
  library(mRMRe)
  
  # create train / test feature matrices
  # - with dummy coding
  # - with normalized column values
  #########################################
  setDT(train)
  setDT(test)
  
  # feature selection
  predictors <- c("time","status")
  
  # add features to cross-validation subsample
  train            <- train[featureMatrix,  on = "ENROLID", nomatch=0]
  train.predictors <- train[,predictors, with = F]
  train.features   <- train[,c("ENROLID",predictors) := NULL, with = F]
  
  test            <- test[featureMatrix,  on = "ENROLID", nomatch=0]
  test.predictors <- test[,predictors, with = F]
  test.features   <- test[,c("ENROLID",predictors) := NULL, with = F]
  
  # rm(train,test) # clean memory
  
  # feature preselection
    # train data
    cat("\n feature preselection... \n")
    thres       <- ceiling(0.05 * nrow(train.features)) # features must occur in at least 5% of patients
    numericCols <- which(sapply(train.features, class) == "numeric")
    colsums     <- apply(train.features[,numericCols, with = FALSE] >0, MARGIN = 2, sum)
    dropCols    <- names(numericCols[colsums < thres])
    train.features[, (dropCols) := NULL]
    
    # test data (utilizing train data feature preselection)
    test.features[, (dropCols) := NULL]
    
  # dummy coding (categorical to binary variables)
  cat("\n dummy coding... \n ")
  train.features <- dummy.data.frame(train.features,sep=".x.")
  test.features  <- dummy.data.frame(test.features,sep=".x.")
  
  # # normalize values to a range between [0,1]
  cat("\n normalizing to [0,1]... \n ")
  setDF(train.features)
  setDF(test.features)
  
  maxFeatVal <- apply(train.features, 2, function(x) max(x))
  train.features <- sweep(x = train.features, MARGIN = 2, STATS = maxFeatVal, FUN = "/")
  train.features[is.na(train.features)] <- 0
  train.features <- as.data.table(train.features)
  
  test.features <- sweep(x = test.features, MARGIN = 2, STATS = maxFeatVal, FUN = "/")
  test.features[is.na(test.features)] <- 0
  test.features <- as.data.table(test.features)
  
  #  mRMR feature selection
  ################################################################
  cat("\n mRMRe feature selection... \n ")
  # train SDAE on train features 
  start     <- proc.time()
  train.predictors <- train.predictors + 0.0 # convert to numeric
  train.features <- train.features + 0.0 # convert to numeric
  
  train.names <- colnames(train.predictors)
  train.predictors <- apply(train.predictors,2, function(x) round(x,4)) %>% data.frame()
  names(train.predictors) <- train.names
  
  time.mrmr <-
    system.time (
      featureSelection <-
        featureSelectionMRMR2(
          featureMatrix = cbind(train.predictors, train.features),
          feature_count = feature_count
        )
    ) # mRMR feature selection
  
  sparsefeatures <- setdiff(featureSelection$featureNames, predictors)
  time.train.model <- round(((proc.time() - start)[3])/60,2)
  
  # retrieve sparse features
    
  # bind response and sparse features
  setDF(train.features)
  setDF(test.features)
  train.sparse.features <- cbind(train.predictors, train.features[,sparsefeatures])
  test.sparse.features  <- cbind(test.predictors, test.features[,sparsefeatures])
  
  cat("\n save MRMR feature matrix...")
  save(train.sparse.features, file = "data/mrmr.normalized.dummy.fm.RData", compress = T)
  
  # # survival random forest
  # ########################
  # library(ranger)
  # library(survival)
  # cat("\n train survival random forest... \n ")
  # 
  # time.train.forest   <-
  #   system.time(
  #     ranger.forest <-
  #       ranger(dependent.variable.name = "time", status.variable.name = "status",
  #         data = train.sparse.features,
  #         num.trees = num.trees,
  #         importance = importance,
  #         num.threads = ncores,
  #         save.memory = ranger.save.memory,
  #         scale.permutation.importance = scale.permutation.importance
  #       )
  #   )
  # 
  # # train
  # time.predict.forest <-
  #   system.time(ranger.predict <-
  #                 predict(ranger.forest, dat = test.sparse.features)) # predict
  # 
  # # mail reports
  # times <- c(time.mrmr[3], time.train.forest[3], time.predict.forest[3])
  # times <- round(times/60,2)
  # event <- c("mRMR","SRF training","SRF prediction")
  # body <- paste(event,as.character(times),sep=": ",collapse = " min\n")
  # StatusEmail(subject = paste("RStudio status email: MC / CV / ncores ",mc,k,ncores,"training time","min"),body = body)
  # 
  # 
  # # results container
  # ###################
  # return(list(train.featureMatrix = train.sparse.features,
  #             test.featureMatrix  = test.sparse.features,
  #             ranger.forest       = ranger.forest,
  #             ranger.predict      = ranger.predict,
  #             time.train.model     = time.train.model,
  #             time.train.forest   = time.train.forest,
  #             time.predict.forest = time.predict.forest)
  # )
  
}