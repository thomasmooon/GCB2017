GetMRMRTestAndTrainingMatrices <- 
  function(train,
         test,
         featureMatrix,
         mc,
         k,
         modelDescription = "modelDescription",
         savepath) {
  
  
 
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
  rm(train)
  rm(test)
  rm(featureMatrix)

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

  ## in some cases the dummy coding results in different "dummy columns" depending
  ## on which attributes of a feature appears (appears not) in the particular data (test, training).
  ## to avoid errors and to harmonize this, keep only columns (after dummy coding) which appear in both (test & training dummies)
  intCols <- intersect(colnames(train.features),colnames(test.features))
  test.features <- test.features[,intCols]
  train.features <- train.features[,intCols]


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
  train.predictors <- train.predictors + 0.0 # convert to numeric
  train.features <- train.features + 0.0 # convert to numeric

  train.names <- colnames(train.predictors)
  train.predictors <- apply(train.predictors,2, function(x) round(x,4)) %>% data.frame()
  names(train.predictors) <- train.names

  # mRMR feature selection
  time.mrmr <-
    system.time (
      featureSelection <-
        featureSelectionMRMR2(
          featureMatrix = cbind(train.predictors, train.features)
        )
    )


  StatusEmail(subject = paste("RStudio status email: proj.GCBPaperFix MRMR ",", MC=",mc,", k=",k),body = paste("runtime = ",round(time.mrmr[3]/3600,1)," hours"))

  # bind response and sparse features
  setDF(train.features)
  setDF(test.features)
  train.sparse.features <- cbind(train.predictors, train.features[,featureSelection$featureNames])
  test.sparse.features  <- cbind(test.predictors, test.features[,featureSelection$featureNames])

  # create container
  mRMRTestAndTrainingMatrices <- list(train.sparse.features, test.sparse.features)
  
  # save
  save(mRMRTestAndTrainingMatrices, file = paste(savepath,mc,k,"RData",sep = "."), compress = T)
 
  return(NULL)
  
}
