###############################################################################
# HeadURL:  join_DisOnt_disGenNet.R
# Date:     2016-10-21
# Author:   Thomas Gerlach
# -----------------------------------------------------------------------------
# CATEG: 
# DESCR: create matching table: UMLS,ICD9, entrez
# KEYS : 
# INPUT: disGenNet_2016-10-21.RData, disOnt_ext2_2016-10-24.RData
# OUT  : disease2genes_<DATE>.Rdata and .csv
# REQ  : 
# NOTES: origin http://www.disgenet.org/web/DisGeNET/menu/downloads
###############################################################################
# CHANGELOG:
#
###############################################################################

# load source
##############
source("z_start.R")
load("~/epilepsie_preprocessing_descrStat/output/disGenNet_2016-10-24.RData")
load("~/epilepsie_preprocessing_descrStat/output/disease_ontology/disOnt_ext2_2016-10-20.RData")

# left join disGenNet with DO, by diseaseId=UMLS
j.umls <- left_join(disGeNet,disOnt_ext2 %>% select(ICD9,UMLS),by=c("diseaseId"="UMLS"))
glimpse(j.umls)
sum(!is.na(j.umls$ICD9))

# left join disGenNet with DO, by diseseName=NAME
j.name <- left_join(disGeNet,disOnt_ext2 %>% select(ICD9,name),by=c("diseaseName"="name"))
glimpse(j.name)
sum(!is.na(j.name$ICD9))

# left join disGenNet with DO, by diseseName=MEDDRA
j.meddra <- left_join(disGeNet,disOnt_ext2 %>% select(ICD9,MEDDRA),by=c("diseaseName"="MEDDRA"))
glimpse(j.meddra)
sum(!is.na(j.meddra$ICD9))

# concatenate
j <- rbind(j.umls,j.name,j.meddra)
j <- j[!is.na(j$ICD9),]
j <- unique(j)
colnames(j) <- c("UMLS","entrez","score","geneName","description","diseaseName",
                 "NofPmids","source_descr","ICD9")
glimpse(j)

# ICD9 to PHEWAS
ICD9GROUPING <- tbl(src,"REFERENCE.ICD9GROUPING") %>% collect()
icd2phew     <- ICD9GROUPING %>% select(ICD9, PHEWAS_STRING) %>% distinct()
icd2phew     <- as_tibble(sapply(icd2phew,as.character))
j            <- left_join(j,icd2phew,by="ICD9")

# save / export
###############
disease2genes <- j
name.csv  <- paste("output/disease2genes_",Sys.Date(),".csv",sep = "")
name.rdat <- paste("output/disease2genes_",Sys.Date(),".RData",sep="")
write.csv2(disease2genes,file = name.csv,row.names=FALSE)
save(disease2genes,file = name.rdat, compress = T)
