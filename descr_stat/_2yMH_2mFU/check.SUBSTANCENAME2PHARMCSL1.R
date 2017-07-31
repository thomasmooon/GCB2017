load("~/epilepsie_preprocessing_descrStat/output/SUBST.PROP.PHARM.RData")

require(dplyr)

x <- SUBST.PROP.PHARM %>% select(SUBSTANCENAME, PHARMCLS1) %>% distinct() %>% group_by(SUBSTANCENAME) %>% tally(sort = TRUE) %>% filter(n>= 2)

# SUBSTANCENAME     n
# 1                FOLIC ACID    13
# 2             ASCORBIC ACID     9
# 3       HYDROCHLOROTHIAZIDE     9
# 4   METFORMIN HYDROCHLORIDE     8
# 5  PYRIDOXINE HYDROCHLORIDE     8
# 6           CHOLECALCIFEROL     7
# 7         CODEINE PHOSPHATE     7
# 8                PYRIDOXINE     7
# 9                RIBOFLAVIN     7


SUBST.PROP.PHARM %>% 
  filter(SUBSTANCENAME == "FOLIC ACID") %>% 
  select(PHARMCLS1) %>% distinct() 

SUBST.PROP.PHARM %>% 
  filter(SUBSTANCENAME == "POTASSIUM CHLORIDE") %>% 
  select(PHARMCLS1) %>% distinct() 

SUBST.PROP.PHARM %>% 
  filter(SUBSTANCENAME == "FOLIC ACID") %>% 
  select(PHARMCLS1) %>% 
  group_by(PHARMCLS1) %>% 
  tally()


  SUBST.PROP.PHARM %>% 
    filter(SUBSTANCENAME == "ASCORBIC ACID") %>% 
    select(PHARMCLS1) %>% 
    group_by(PHARMCLS1) %>% 
    tally()
  
  
  SUBST.PROP.PHARM %>% 
    filter(SUBSTANCENAME == "COPPER") %>% 
    select(PHARMCLS1) %>% 
    group_by(PHARMCLS1) %>% 
    tally()
  
  
  
  SUBST.PROP.PHARM %>% 
    filter(SUBSTANCENAME == "N") %>% 
    select(PHARMCLS1) %>% 
    group_by(PHARMCLS1) %>% 
    tally()
  
  
  
  
  
  
  SUBST.PROP.PHARM %>% 
    filter(SUBSTANCENAME == "TOPIRAMATE") %>% 
    select(PHARMCLS1) %>% 
    group_by(PHARMCLS1) %>% 
    tally()