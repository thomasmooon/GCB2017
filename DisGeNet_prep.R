###############################################################################
# HeadURL:  DisGeNet_prep.R
# Date:     2016-10-21
# Author:   Thomas Gerlach
# -----------------------------------------------------------------------------
# CATEG: data preprocessing  
# DESCR: disease gene net data preprocessing
# KEYS : 
# INPUT: curatedAndBefree_gene_disease_associations_2016-10-19.csv
# OUT  : 
# REQ  : 
# NOTES: origin http://www.disgenet.org/web/DisGeNET/menu/downloads
###############################################################################
# CHANGELOG:
#
###############################################################################

# load source
##############
source("z_start.R")
disGeNet <- read.csv("~/epilepsie_preprocessing_descrStat/data/diseaseDescriptors/curatedAndBefree_gene_disease_associations_2016-10-19.csv", sep=";")

# preprocessing
###############
disGeNet <- sapply(disGeNet,as.character)
disGeNet <- as_tibble(disGeNet)
disGeNet$diseaseId  <- gsub(pattern = "umls:",x = disGeNet$diseaseId,replacement = "")
disGeNet <- unique(disGeNet[,c(-7,-9)])
glimpse(disGeNet)

# save / export
###############
name.csv  <- paste("output/disGenNet_",Sys.Date(),".csv",sep = "")
name.rdat <- paste("output/disGenNet_",Sys.Date(),".RData",sep="")
write.csv2(disGeNet,file = name.csv,row.names=FALSE)
save(disGeNet,file = name.rdat, compress = TRUE)
