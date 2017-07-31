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
  ncores = 18
  
# functions
  source("~/epilepsie_preprocessing_descrStat/functions/mRMR.Ranger.v2.R")
  source("~/epilepsie_preprocessing_descrStat/functions/StatusEmail.R")
  source("~/epilepsie_preprocessing_descrStat/functions/Lib.featSelAndRF.R")
  
# preprocessing
################
  FM <- dropSparseFeatures(cv.samp,FM, nCrossValidation)
  

# kernel
########
# MC <- MC[1:3]
# MC <- MC[4:6]
# MC <- MC[7:9]


  for(mc in MC) {
    cv.samp.mc <- copy(cv.samp)
    cv.samp.mc[main.comorb != mc, c("status","time","main.comorb") := list(0, maxixday,"censored") ]
    cv.samp.mc <- cv.samp.mc %>% select(-maxixday) %>% distinct()
    
    for(k in 1:nCrossValidation) {
       cat(mc,k,sep = ",")
      
      # get test + training dataset
      train <- cv.samp.mc %>% filter(cv.samp != k) %>%  select(ENROLID, time, status)
      test <- cv.samp.mc %>% filter(cv.samp == k) %>%  select(ENROLID, time, status)
       
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
           mc = mc, 
           k = k
         ) 
       
       runtime <- round(((proc.time() - start)[3])/60,2)
       # save results
       save(results, file = paste("~/epilepsie_preprocessing_descrStat/MRMR.SRF/data/importance_4000/ranger",mc,k,num.trees,"RData",sep = "."), compress = T)
       # mail reports
       StatusEmail(subject = paste("RStudio status email: ",mc,num.trees,"finished"),body = paste(unlist(runtime), collapse = " min /")) 
       # clean memory
       rm(train,test,results)
     }
   }


    