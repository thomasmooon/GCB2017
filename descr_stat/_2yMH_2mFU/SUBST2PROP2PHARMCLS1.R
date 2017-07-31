# title: "SUBST2PROP2PHARMCLS1"
# subtitle: "for drugRelatedDescriptors"
# author: "Thomas Gerlach"
# date: "2017 JAN 26"

# purpose: 
# retrieve table SUBSTANCENAME | PROPRIETARYNAME | PHARMCLS1 for all substances
# as input for the drugRelatedDescriptors.Rmd to generate the drug-features
  
# setup
rm(list=setdiff(ls(), "TABLE04SAS7BDAT"))
source("z_start.R") # initalize netezza DB, load core data
if(!exists("TABLE04SAS7BDAT")) load("./data/TABLE04SAS7BDAT.RData")
FILTER_PATIENTS_AGE_REGION()

# retrieve SUBSTANCENAME, PROPRIETARYNAME, PHARMCLS1
SUBST.PROP.PHARM <- 
  TABLE04SAS7BDAT %>% 
  filter(SUBSTANCENAME != "") %>% 
  dplyr::select(SUBSTANCENAME, PROPRIETARYNAME, PHARMCLS1) %>% 
  distinct()

SUBST.PROP.PHARM$SUBSTANCENAME <- as.character(SUBST.PROP.PHARM$SUBSTANCENAME )

SUBST.PROP.PHARM <- parseColByIdx(SUBST.PROP.PHARM,"SUBSTANCENAME",sep = ";") %>% distinct()

SUBST.PROP.PHARM <- as_tibble(apply(SUBST.PROP.PHARM,2,function(x) trimws(toupper(x))))

# omit emtpy PHARMCLS1 entries, if drug has at least an non-empty  entry
x1 <- 
  SUBST.PROP.PHARM %>% 
  filter(PHARMCLS1 != "") %>%
  dplyr::select(SUBSTANCENAME) %>%
  distinct()

x2 <- 
  SUBST.PROP.PHARM %>%
  filter(SUBSTANCENAME %in% x1$SUBSTANCENAME) %>%
  dplyr::select(SUBSTANCENAME, PHARMCLS1) %>% distinct() %>% 
  group_by(SUBSTANCENAME) %>% 
  tally() %>% 
  filter(n>1)

SUBST.PROP.PHARM <- 
  SUBST.PROP.PHARM %>%
  filter( !(PHARMCLS1 == "" & SUBSTANCENAME %in% x2$SUBSTANCENAME) ) %>%
  distinct()

# save
save(SUBST.PROP.PHARM, file="output/SUBST.PROP.PHARM.RData", compress = TRUE)
