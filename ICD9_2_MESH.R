# convert the csv source to an appropiate table format and save it as RData-file
rm(list=ls())
MESH2ICD9         <- as_tibble(read.csv2(file="data/doid_MESH2ICD9_2016-09-15.csv",header = T))
MESH2ICD9[,2:3]   <- sapply(MESH2ICD9[,2:3],as.character)
id_mesh           <- MESH2ICD9 %>% group_by(id) %>% select(mesh) %>% filter(mesh != "") %>% distinct()
id_icd9           <- MESH2ICD9 %>% group_by(id) %>% select(icd9) %>% filter(icd9 != "") %>% distinct()
ICD9_2_MESH       <- left_join(id_icd9,id_mesh,by="id")
ICD9_2_MESH       <- ICD9_2_MESH[complete.cases(ICD9_2_MESH),c(2:3)] %>% rename(DIAG = icd9)
ICD9_2_MESH$DIAG  <- gsub("\\.","",ICD9_2_MESH$DIAG) # remove delimiter

save(ICD9_2_MESH,file="data/ICD9_2_MESH.RData")
