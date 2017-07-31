SDAE.Ranger <- function(train, test, featureMatrix, num.trees = 5000, ncores = 10,mc,k, ranger.save.memory = T) {
  
  # setup
  #######
  setDT(train)
  setDT(test)
  
  # create train / test feature matrices
  # - with dummy coding
  # - with normalized column values
  #########################################
  
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
  
  #  stacked denoising autoencoder (SDAE): deep feature selection
  ################################################################
  cat("\n train SDAE... \n ")
  # train SDAE on train features 
  start     <- proc.time()
  train.h2o <- na.omit(as.h2o(data.matrix(train.features))) # na.omit prevents a bug which adds a NA row to the data
  hidden    <- c(500,500,500)
  train.sdae.model <- 
    h2o.deeplearning(
      x = 1:h2o.ncol(train.h2o),
      training_frame = train.h2o, 
      activation = "Tanh",
      input_dropout_ratio = 0.05,
      mini_batch_size = 100,
      autoencoder = TRUE,
      hidden = hidden,
      loss = "Automatic")
  time.train.sdae.model <- round(((proc.time() - start)[3])/60,2)
  
  # retrieve deep features
  
    # training
    train.deepfeatures <- as.data.frame(h2o.deepfeatures(train.sdae.model, train.h2o, layer=length(hidden)))
    
    # test  
    test.h2o <- na.omit(as.h2o(test.features))
    test.deepfeatures <- as.data.frame(h2o.deepfeatures(train.sdae.model, test.h2o, layer=length(hidden)))
    
  # bind response and SDAE deep features
  train.deepfeatures <- cbind(train.predictors, train.deepfeatures)
  test.deepfeatures  <- cbind(test.predictors, test.deepfeatures)
  
  # status mail: SDAE runtime
  StatusEmail(subject = paste("RStudio status email: ",mc,k,"SDAE training time",time.train.sdae.model,"min"),body = "xxx") 
  
  # survival random forest
  ########################
  library(ranger)
  library(survival)
  
  time.train.forest   <-
    system.time(
      ranger.forest <-
        ranger(
          Surv(time, status) ~ .,
          data = train.deepfeatures,
          num.trees = num.trees,
          save.memory = ranger.save.memory
        )
    ) 
  # train
  time.predict.forest <- system.time(ranger.predict <- predict(ranger.forest, dat = test.deepfeatures)) # predict
  
  
  # results container
  ###################
  return(list(train.featureMatrix = train.features,
              test.featureMatrix  = test.features,
              train.sdae.model    = train.sdae.model,
              ranger.forest       = ranger.forest,
              ranger.predict      = ranger.predict,
              time.minutes.train.sdae.model     = time.train.sdae.model,
              time.train.forest   = time.train.forest,
              time.predict.forest = time.predict.forest)
  )
  
}