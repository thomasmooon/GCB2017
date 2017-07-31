# run survival random forest
# - mRMR feature selection
# - k-fold crossvalidation
############################

# setup
#######

# librarys
  require(dplyr)
  require(tidyr)
  library(data.table)
  library(dummies)

# load cv-data
  load("~/epilepsie_preprocessing_descrStat/output/featureMatrix/censored.incident.cvsamp.RData")
  load("~/epilepsie_preprocessing_descrStat/output/featureMatrix/FM.all.RData") # full feature matrix (all features, all patients) 
  FM <- FM$featureMatrix
  
# parameter setup
  nCrossValidation <- max(unique(cv.samp$cv.samp))
  MC <- setdiff(unique(cv.samp$main.comorb),"censored")
  num.trees <- 4000
  ncores = 9
  
# functions
  source("~/epilepsie_preprocessing_descrStat/functions/mRMR.Ranger.v2.R")
  source("~/epilepsie_preprocessing_descrStat/functions/StatusEmail.R")
  source("~/epilepsie_preprocessing_descrStat/functions/Lib.featSelAndRF.R")
  
# preprocessing
################
  FM <- dropSparseFeatures(cv.samp,FM, nCrossValidation)
  

# kernel
########
# MC <- MC[2:4]
MC <- MC[6:9]


  for(mc in MC) {
    for(k in 1:1) { # no CV's : full model, k is just a placeholder
       cat(mc,k,sep = ",")
       # get test + training dataset
       train <- cv.samp %>% filter(main.comorb == "censored" | main.comorb == mc) %>%  select(ENROLID, time, status)
       test <- cv.samp %>% filter(main.comorb == "censored" | main.comorb == mc, cv.samp == k) %>%  select(ENROLID, time, status)
       start <- proc.time()
       # run core
       
       results <-
         mRMR.Ranger2(
           train = train,
           test = test,
           featureMatrix = FM,
           feature_count = 500,
           num.trees = num.trees,
           ncores = ncores,
           importance = 'permutation',
           scale.permutation.importance = T,
           mc = mc, 
           k = k
         ) 
       
       runtime <- round(((proc.time() - start)[3])/60,2)
       # save results
       save(results, file = paste("~/epilepsie_preprocessing_descrStat/MRMR.SRF.fullmodel/data/",mc,k,num.trees,"RData",sep = "."), compress = T)
       # save(runtime, file = paste("~/epilepsie_preprocessing_descrStat/MRMR.SRF.fullmodel/runtimes/",mc,k,num.trees,"RData",sep = "."), compress = T)
       
       # clean memory
       rm(train,test,results)
     }
   }


    