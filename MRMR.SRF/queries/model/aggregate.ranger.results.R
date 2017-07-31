# get essential ranger data and store them in a single list for loading perfomance
##################################################################################


# create container
###################
# get main comorbidity list
load("~/epilepsie_preprocessing_descrStat/output/featureMatrix/censored.incident.cvsamp.RData")
MC <- setdiff(unique(cv.samp$main.comorb),"censored")
survRF <- vector("list",length(MC))
names(survRF) <- MC
for(i in seq_along(MC)) {survRF[[i]] <- list(training = NULL, test = NULL)}


#################

path <- "~/epilepsie_preprocessing_descrStat/MRMR.SRF/data/importance_4000/"
allfiles <- dir(path)
mc <- paste("ranger.",MC, sep="")

for(m in seq_along(mc)){
  mcfiles <- grep(mc[m], allfiles, value = TRUE)
  for(i in seq_along(mcfiles)){
    cat(m,"/",length(mc),i,"/",length(mcfiles),"\n")
    load(paste(path, mcfiles[i],sep=""))
    # set training
    survRF[[MC[m]]]$training[[i]] <- list(fm = results$train.featureMatrix,
                                 surv = results$ranger.forest$survival,
                                 chf = results$ranger.forest$chf,
                                 unique.death.times = results$ranger.forest$unique.death.times,
                                 importance = results$ranger.forest$variable.importance)
    # set test
    survRF[[MC[m]]]$test[[i]] <- list(fm = results$test.featureMatrix,
                                      unique.death.times = results$ranger.predict$unique.death.times,
                                      surv = results$ranger.predict$survival,
                                      chf = results$ranger.predict$chf)
  }
}

save(survRF, file = paste(path,"survRF_summary.RData",sep=""), compress = T)
