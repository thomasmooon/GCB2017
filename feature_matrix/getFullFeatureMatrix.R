#################
# setup         #
#################
require(dplyr)
require(tidyr)
library(data.table)
library(dummies)

source("functions/StatusEmail.R")

load("~/epilepsie_preprocessing_descrStat/output/featureMatrix/DRUG.FEATMAT.RData") # drug raw features
load("~/epilepsie_preprocessing_descrStat/output/featureMatrix/DISEASE.FEATMAT.RData") # disease raw features
load("~/epilepsie_preprocessing_descrStat/output/featureMatrix/P.FEAT.ENROL.RData") # # patient features, contains age, sex, region, hospdays, metqty per aed, claims info (hltpln, plntyp, presc)
setDT(P.FEAT.ENROL)
load("~/epilepsie_preprocessing_descrStat/output/featureMatrix/P.QUART.SUBST.RData") # contains treatment by quarter
load("~/epilepsie_preprocessing_descrStat/output/featureMatrix/P.QUART.DIAG.RData") # contains diagnosis by quarter


###################
# start functions #
###################

GetTrainingFeatureMatrix <- function(rawDrugFeatures, patQuartTreat, enrolids, rawDiseaseFeatures, patQuartDiag, patFeatures) {
  # treatment features
  cat("\n Add treatment features...\n")
  mainComorb.train.treat <- 
    AddTreatmentFeatures(
      rawDrugFeatures = rawDrugFeatures,
      patQuartTreat = patQuartTreat)
  
  # disease features
  cat("\n Add disease features...\n")
  mainComorb.train.diag <- 
    AddDiagnosisFeatures(
      rawDiseaseFeatures = rawDiseaseFeatures, 
      patQuartDiag = patQuartDiag)
  
  
  # patient survival data (ENROLID, time, status)
  cat("\n Retrieve survival features...\n")
  mainComorb.train.surv <- enrolids
  
  # create feature matrix
  cat("\n Join all features...\n")
  mainComorb.featMat <- 
    mainComorb.train.surv %>% 
    inner_join(patFeatures) %>%
    inner_join(mainComorb.train.diag) %>%
    inner_join(mainComorb.train.treat)
  
  return(list(featureMatrix = mainComorb.featMat))
}

############

AddTreatmentFeatures <- function(rawDrugFeatures, patQuartTreat) {
  # add treatment features to treatment
  setDT(rawDrugFeatures)
  setDT(patQuartTreat)
  x <- patQuartTreat[rawDrugFeatures, on = "SUBSTANCENAME", nomatch=0] # data.table inner join
  x[,SUBSTANCENAME := NULL]
  # aggregate by .(enrolid, quarter) to retrieve quantitative features using sum()
  names <- setdiff(colnames(x),c("ENROLID","QUARTER"))
  x.feat <- dcast(x , ENROLID ~ QUARTER, fun.aggregate = sum, sep = ".x.", value.var = names, fill = 0) # summarise frequency for quantitative features
  return(x.feat)
}

############

AddDiagnosisFeatures <- function(rawDiseaseFeatures, patQuartDiag) {
  # add diagnosis features to diagnosis
  rawDiseaseFeatures <- unique(rawDiseaseFeatures)
  setDT(rawDiseaseFeatures)
  names(rawDiseaseFeatures)[names(rawDiseaseFeatures) == "ICD9"] = "DIAG"
  setDT(patQuartDiag)
  x <- patQuartDiag[rawDiseaseFeatures, on = "DIAG", nomatch=0] # data.table inner join
  x[,DIAG := NULL]
  # aggregate by .(enrolid, quarter) to retrieve qualitative features using max()
  names <- setdiff(colnames(x),c("ENROLID","QUARTER"))
  x.feat <- dcast(x, ENROLID ~ QUARTER, fun.aggregate = max, sep = ".x.", value.var = names, fill = 0) # take max() of the boolean features to retrieve qualitative aggregation
  return(x.feat)
}


####################################################
# feature matrix for all patients and all features #
####################################################

enrolids <- P.QUART.SUBST %>% select(ENROLID) %>% distinct()

# feature matrix for all patients and all features (no preselection)
start <- proc.time()
FM <- 
  GetTrainingFeatureMatrix(
    enrolids =             enrolids, 
    rawDrugFeatures =      drug.features, 
    patQuartTreat =        P.QUART.SUBST, 
    rawDiseaseFeatures =   DISEASES.FEATMAT, 
    patQuartDiag =         P.QUART.DIAG, 
    patFeatures =          P.FEAT.ENROL
  )
stop <- proc.time() - start

save(FM,file = "~/epilepsie_preprocessing_descrStat/output/featureMatrix/FM.all.RData",compress = TRUE)


StatusEmail(subject = paste("full feature matrix finished"),body = paste("runtime: ",stop[3],"sec" ,collapse = " ")) 

##################
# postprocessing #
##################

# # dummy encoding of non-numeric variables
# FM.dummy <- dummy.data.frame(FM$featureMatrix,sep=".x.")
# setDT(FM.dummy)
# save(FM.dummy,file = "~/epilepsie_preprocessing_descrStat/output/featureMatrix/FM.all.dummy.RData",compress = TRUE)
# StatusEmail(subject = paste("dummied matrix finished"),body = paste("dummy encoding finished" ,collapse = " ")) 
# 
# # normalize values to a range between [0,1]
# FM.dummy.norm <- FM.dummy
# setDF(FM.dummy.norm)
# # FM.dummy.norm[,-1] <- apply(FM.dummy.norm[,-1], 2, function(x) x/max(x))
# FM.dummy.norm[,-1] <- apply(FM.dummy.norm[,-1], 2, function(x) round(x/max(x),6))
# FM.dummy.norm[is.na(FM.dummy.norm)] <- 0
# setDT(FM.dummy.norm)
# save(FM.dummy.norm,file = "~/epilepsie_preprocessing_descrStat/output/featureMatrix/FM.all.dummy.norm.RData",compress = TRUE)
# StatusEmail(subject = paste("normalised dummied matrix finished"),body = paste("normalising finished" ,collapse = " ")) 

