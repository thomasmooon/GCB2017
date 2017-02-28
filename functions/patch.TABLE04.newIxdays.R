# patch notes:
##############

# load patch table newIxdays.RData (created with TABLE04selectionCriteria.Rmd)
# load TABLE04SASB7DAT.RData
# apply patch: 
## delete old INDEXDATE from TABLE04SASB7DAT
## join  newIxdays to TABLE04SASB7DAT to retrieve new ixdays
## delete patients with no new ixdays (= don't fulfill any criteria)

load("data/TABLE04SAS7BDAT.RData")
load("data/patches/newIxdays.RData")
require(dplyr)

newIxdays <- newIxdays %>% rename(INDEXDT = ixdate)

# how man STARTDT are empty?
TABLE04SAS7BDAT %>% select(STARTDT) %>% filter(is.na(STARTDT)) %>% tally()
TABLE04SAS7BDAT %>% select(IXDAYS) %>% filter(is.na(IXDAYS)) %>% tally()

TABLE04.2 <- 
  TABLE04SAS7BDAT %>% 
  select(-INDEXDT, -IXDAYS) %>% 
  inner_join(newIxdays, by = "ENROLID") %>% 
  mutate(IXDAYS = STARTDT - INDEXDT)


TABLE04.2 %>% select(IXDAYS) %>% filter(is.na(IXDAYS)) %>% tally()
TABLE04.2 <- TABLE04.2 %>% select(-NDCNUM, -(PHARMCLS2:PHARMCLS5), -THERCLS, -THERGRP) # omit needless columns

save(TABLE04.2, file="data/patches/TABLE04.2.RData", compress = TRUE)



# some stats
#############
n_distinct(TABLE04SAS7BDAT$ENROLID)
n_distinct(TABLE04.2$ENROLID)
sum(is.na(TABLE04SAS7BDAT$IXDAYS))
sum(is.na(TABLE04SAS7BDAT$IXDAYS[TABLE04SAS7BDAT$SUBSTANCENAME == ""]))
sum(is.na(TABLE04SAS7BDAT$IXDAYS[TABLE04SAS7BDAT$SUBSTANCENAME != ""]))

# number of patients with false indexdates
##########################################
T.old <- TABLE04SAS7BDAT %>% select(ENROLID, INDEXDT) %>% distinct() %>% rename(old = INDEXDT)
T.new <- TABLE04.2 %>% select(ENROLID, INDEXDT) %>% distinct() %>% rename(new = INDEXDT)
T.oldVsNew <- inner_join(T.old, T.new, by = "ENROLID")
T.oldVsNew$isEqual = T.oldVsNew$old == T.oldVsNew$new
sum(!T.oldVsNew$isEqual)
