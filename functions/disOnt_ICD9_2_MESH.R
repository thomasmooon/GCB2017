###############################################################################
# $HeadURL:  $
# $Rev:  $
# $Date: 2016-10-05 $
# $Author:  $
# -----------------------------------------------------------------------------
# CATEG:  ICD9 to MESH 
# DESCR:  match Mesh terms from DISEASE ONTOLOGY DATABASE via ICD9 terms to PATIENT table
# KEYS : 
# INPUT: ICD9_2_MESH.RData (see also H:\MeSH)
# OUT  : 
# REQ  : 
# NOTES: H:\MeSH\readme_DO.txt, https://github.com/DiseaseOntology/HumanDiseaseOntology/tree/master/src/ontology
###############################################################################

disOnt_ICD9_2_MESH <- function(ICD9) {

load("data/diseaseDescriptors/ICD9_2_MESH.RData")
ICD9 <- left_join(ICD9,ICD9_2_MESH,by="DIAG")
nM                  <- ICD9 %>% filter(mesh != "") %>% tally()
nD                  <- ICD9 %>% filter(DIAG != "") %>% tally()
cat(round((nM$n / nD$n) * 100,0),"% of ICD9 Codes could be matched to MESH terms")

return(ICD9)
}