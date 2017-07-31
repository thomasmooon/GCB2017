###############################################################################
# HeadURL:  join_DRUG2SIDEFFECTS_DisOnt.R
# Date:  
# Author:  
# -----------------------------------------------------------------------------
# CATEG:   
# DESCR: 
# KEYS : 
# INPUT: DISEASE2SIDEFFECTS, Disease Ontology (disOnt_ext1_....RData)
# OUT  : expanded Disease Ontology: add MEDDRA names
# REQ  : 
# NOTES: 
###############################################################################
# CHANGELOG:
#
###############################################################################

# load data
###########
source("z_start.R")
DRUG2SIDEFFECTS <- tbl(src,"REFERENCE.DRUG2SIDEEFFECTS") %>% collect()
D2S <- DRUG2SIDEFFECTS %>% select(UMLS,MEDDRA) %>% distinct()
D2S <- as_tibble(sapply(D2S,as.character))

# left join disease ontology to D2S by UMLS
load("output/disease_ontology/disOnt_ext1_2016-10-20.RData")
disOnt_ext1 <- sapply(disOnt_ext1,as.character) %>% as_tibble()
parseMe(disOnt_ext1$UMLS)
parseMe(D2S$UMLS)
disOnt_ext2 <- left_join(disOnt_ext1,D2S,by="UMLS")

# export / save
###############
name.csv  <- paste("output/disease_ontology/disOnt_ext2_",Sys.Date(),".csv",sep = "")
name.rdat <- paste("output/disease_ontology/disOnt_ext2_",Sys.Date(),".RData",sep="")
write.csv2(disOnt_ext2,file = name.csv,row.names=FALSE)
save(disOnt_ext2,file = name.rdat)


