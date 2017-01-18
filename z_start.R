# librarys
##########

# perfomance testing
library(microbenchmark)
library(compiler)
library(profvis)

# essential
library(dplyr)
library(nzdplyr)
library(tidyr)

# productive, plots
library(tibble)
#library(sas7bdat)
library(ggplot2)
library(grid)
library(gplots)

#############
# functions #
#############

KeepIncidentAndCensored <- function(PATIENTS_AGE_REGION) {
  # - keep only patients which have
  # -- an incident main-comorbidity (= event subpopulation)
  # -- or no main-comorbidity at any time (= censored subpopulation)
  
  #  A: patients without any main-comorbidity
  anyMc  <- PATIENTS_AGE_REGION %>% filter(!is.na(MAIN.COMORB)) %>% dplyr::select(ENROLID) %>% distinct()
  allPat <- PATIENTS_AGE_REGION %>% dplyr::select(ENROLID) %>% distinct()
  A   <- setdiff(allPat$ENROLID, anyMc$ENROLID)
  
  # patients with main-comorbidities
  main.comorb.first.ixday <- PATIENTS_AGE_REGION %>% filter(MAIN.COMORB != "") %>% dplyr::select(ENROLID, IXDAYS,  MAIN.COMORB) %>% distinct()
  main.comorb.first.ixday <- aggregate(IXDAYS ~., data = main.comorb.first.ixday, FUN = min)
  
  # B: patients with incident main-comorbidities
  B <- main.comorb.first.ixday %>% filter(IXDAYS >= 180) %>% dplyr::select(ENROLID) %>% distinct()
  
  # combine A and B to retrieve relevant population
  relPop <- c(A, B$ENROLID)
  
  PATIENTS_AGE_REGION <- PATIENTS_AGE_REGION %>% filter(ENROLID %in% relPop)
  return(PATIENTS_AGE_REGION)
}

####################

AddMainComorb <- function(PATIENTS_AGE_REGION) {
  # - flag the particular main-comorbidity
  
  # list of PHEWAS_STRINGS with their assignments to co-morbidiets
  MAIN.COMORB <- c("Hypertension","Diabetes","Hyperlipidemia","Anxiety","Migraine","Depression","Stroke.IschemAttack")
  MAIN.COMORB.PHEWAS <- vector("list",length(MAIN.COMORB))
  names(MAIN.COMORB.PHEWAS) <- MAIN.COMORB
  MAIN.COMORB.PHEWAS$Hypertension <- c("Essential hypertension")
  MAIN.COMORB.PHEWAS$Diabetes <- c("Type 2 diabetes")
  MAIN.COMORB.PHEWAS$Hyperlipidemia <- c("Hyperlipidemia","Mixed hyperlipidemia")
  MAIN.COMORB.PHEWAS$Anxiety <- c("Anxiety disorder","Generalized anxiety disorder","Anxiety, phobic and dissociative disorders","Agorophobia, social phobia, and panic disorder")
  MAIN.COMORB.PHEWAS$Stroke.IschemAttack <- c("Ischemic stroke", "Transient cerebral ischemia")
  MAIN.COMORB.PHEWAS$Migraine <- c("Migraine","Migrain with aura")
  MAIN.COMORB.PHEWAS$Depression <- c("Depression","Major depressive disorder")
  
  # comorb-list to df, join PHEWAS frequency
  MAIN.COMORB.PHEWAS <-
    data.frame (cbind(
      MAIN.COMORB = as.character(unlist(mapply(rep, MAIN.COMORB, unlist(lapply( MAIN.COMORB.PHEWAS, length))))),
      PHEWAS_STRING = as.character(unlist(MAIN.COMORB.PHEWAS))
    ), row.names = NULL, stringsAsFactors = FALSE)
  
  knitr::kable(MAIN.COMORB.PHEWAS)
  
  # add main-comorbidity to patients table
  PATIENTS_AGE_REGION <- left_join(PATIENTS_AGE_REGION, MAIN.COMORB.PHEWAS)
  return(PATIENTS_AGE_REGION)
}



##################
NormalizeStrings <- function(y) sapply(y, function(x) trimws(toupper(as.character(x)))) %>% as_tibble()

###############
# detect number of delimiters
parseMe <- function(x) {
  delim <- c(";",",","|")
  n <- c(sum(grepl(";", x)), sum(grepl(",", x)), sum(grepl("\\|", x)))
  n.delim <- data.frame(delim,n)
  return(n.delim)
}

###############
# count number of patients per DIAG (ICD9)

Diag_frequency <- function(P) {
  nPat <- P %>% dplyr::select(ENROLID) %>% distinct() %>% tally() %>% as.numeric()
  freq.DIAG <- P %>% 
    filter(DIAG != "") %>% 
    dplyr::select(ENROLID, DIAG) %>% 
    distinct() %>% 
    group_by(DIAG) %>% 
    tally(sort = TRUE) %>% 
    mutate(freq = round(n/nPat * 100,1))
  return(freq.DIAG)
}

#####################

# standardized filter for patients table
FILTER_PATIENTS_AGE_REGION <- function(TABLE04SAS7BDAT) {
  
  PATIENTS_AGE_REGION <- TABLE04SAS7BDAT %>% dplyr::select(-NDCNUM, -PHARMCLS2, -PHARMCLS3, -PHARMCLS4, -PHARMCLS5, -THERCLS, -THERGRP)
  
  # correct / complete IXDAYS
  newIXDAYS                  <- PATIENTS_AGE_REGION$STARTDT - PATIENTS_AGE_REGION$INDEXDT
  PATIENTS_AGE_REGION$IXDAYS <- as.numeric(newIXDAYS)
  
  source("functions/n_patient_per_period.R")
  source("functions/patient_monthly_enrolment.R")
  source("functions/patient_nonNegative_DAYSUPP.R")
  source("functions/setAEDFlag.R")
  source("functions/aggregate_AED_substancenames.R")
  source("functions/add_Phew_PhewHighlevel.R")
  
  # filtering and parsing
  ##################
  
  # Keep only patients within valid time period 
  PATIENTS_AGE_REGION <- filter_patient_medHist_follUp(PATIENTS_AGE_REGION,lbound=-2*365,ubound = 0*365)
  # only patients with non-negative DAYSUP
  PATIENTS_AGE_REGION <- patient_nonNegative_DAYSUPP(PATIENTS_AGE_REGION)
  # Add Age and Regional Information
  PATIENTS_AGE_REGION <- addAgeRegion(PATIENTS_AGE_REGION)
  # adult only (AGE >= 18)
  PATIENTS_AGE_REGION <- PATIENTS_AGE_REGION %>% filter(AGE >= 18) %>% distinct()
  
  # deseparate substancenames
  PATIENTS_AGE_REGION <- parseColByIdx(PATIENTS_AGE_REGION,"SUBSTANCENAME",sep = ";")
  # set AED flag
  PATIENTS_AGE_REGION <- setAEDFlag(PATIENTS_AGE_REGION)
  # only patients which were treated with AEDs
  pat.with.aeds <- PATIENTS_AGE_REGION %>% filter(isAED == TRUE) %>% dplyr::select(ENROLID) %>% distinct()
  PATIENTS_AGE_REGION <- PATIENTS_AGE_REGION %>% filter(ENROLID %in% pat.with.aeds$ENROLID)
    # add phewas and phewas_hl
  PATIENTS_AGE_REGION <- add_Phew_PhewHighlevel(PATIENTS_AGE_REGION)
  
  # add main comorbidities
  PATIENTS_AGE_REGION <- AddMainComorb(PATIENTS_AGE_REGION)
  
  # Patch ICD9 Code V12.54
  source("functions/Patch_V12.54.R")
  PATIENTS_AGE_REGION <- Patch_V12.54(PATIENTS_AGE_REGION)
  
  # return
  return(PATIENTS_AGE_REGION)
}

####################

parseColByIdx <- function(data, colname,sep=";") {
  
  keyIdx    <- which(colnames(data) %in% colname)
  
  isEmpty   <- data[,keyIdx] == "" | is.na(data[,keyIdx])
  otherData <- data[isEmpty,] # +
  data      <- data[!isEmpty,]
  
  parse         <- data[,keyIdx]
  parsed.list   <- sapply(parse, function(x) strsplit(x,split = sep))
  reps          <- sapply(parsed.list,length)
  
  parsed        <- unlist(parsed.list)
  cnames        <- c(colnames(data[,-keyIdx]),colname) # *
  
  # reorder columns of otherData
  o         <- match(cnames,colnames(otherData))
  otherData <- otherData[,o]
  
  row.reps      <- unlist(mapply(rep, 1:length(reps), reps))
  data.new      <- cbind(data[row.reps, - keyIdx], parsed)
  colnames(data.new) <- cnames
  data.new      <- rbind(data.new, otherData) # +
  
  data.new <- data.new %>% as_tibble() %>% distinct()
  
  return(data.new)
}

####################

# count number of patients per SUBSTANCENAME

SUBSTANCE_FREQUENCY <- function(PATIENTS_TABLE) {
  
  nPat <- n_distinct(PATIENTS_TABLE$ENROLID)
  
  freq.subst <- PATIENTS_TABLE %>% 
    filter(SUBSTANCENAME != "") %>% 
    dplyr::select(ENROLID, SUBSTANCENAME) %>% 
    distinct() %>% 
    group_by(SUBSTANCENAME) %>% 
    tally(sort = TRUE) %>% 
    mutate(freq = round(n/nPat * 100,1)) 
  
  return(freq.subst)
}


###################

# count nPat und nObs

nPatObs <- function(x) {
  nPat <- n_distinct(x$ENROLID) 
  nObs <- nrow(x)
  knitr::kable(cbind(nPat,nObs))
}

##################

ParseICD9 <- function(df) {
  # parse ICD9 Codes, e.g 0010 -> 001.0
  # two leading zeros -> add dot: 00xxx -> 00.xxx
  # one leading zero -> add dot: 0xxxx -> 0.xxxxx
  ###############################################
  
  x00 <- grep("^[0]{2}",df$DIAG)
  x0  <- grep("^[0]{1}[1-9]",df$DIAG)
  
  df$DIAG[x00] <- paste("00",substr(df$DIAG[x00],3,3),".",substr(df$DIAG[x00],4,10),sep="")
  df$DIAG[x0]  <- paste("0",substr(df$DIAG[x0],2,3),".",substr(df$DIAG[x0],4,10),sep="")
  
  # parse ICD9 Codes, nnnxx -> nnn.xx
  # -> after 3 leading numbers insert a point, if length(xx) >0
  #############################################################
  xnnn     <- setdiff( 1:nrow(df), c(x00,x0))
  xnnn.num <- substr(df$DIAG[xnnn],1,3)
  xnnn.dig <- substr(df$DIAG[xnnn],4,10)
  xnnn.digdot <- unlist(sapply(xnnn.dig, FUN = function(x) if (nchar(x)>0) paste(".",x,sep="") else ""))
  xnnn.new    <- paste(xnnn.num, xnnn.digdot, sep="")
  df$DIAG[xnnn] <- xnnn.new
  return(df)
}

# transform result from list to data table
###########################################
listToDf <- function(mylist) {
  
  ncols     <- dim(mylist[[1]])[2]
  rows.each <- unlist(lapply(mylist,nrow))
  rows      <- sum(rows.each)
  y         <- data.frame(matrix(NA,nrow = rows,ncol = ncols))
  
  L         <- length(mylist)
  l.start   <- 1
  l.stop    <- c()
  
  for(l in 1:L) {
    l.stop             <- l.start + rows.each[l] -1
    y[l.start:l.stop,] <- sapply(mylist[[l]],as.character)
    l.start            <- l.start + rows.each[[l]]
  }
  
  return(y)
}

# subplot/multiplot support functions
#####################################
vplayout <-
  function(x, y) {
    viewport(layout.pos.row = x, layout.pos.col = y)
  }

subplot <-
  function(x, y) {
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(x, y)))
  }

# get the indices of the top <n> elements of <x>
###############################################
idTopX <- function(x, n) {
  rows <- (order(x, decreasing = T))[1:n]
  rows <- rows[complete.cases(rows)]
}

# normalize to colSums
######################
normalizeToColSums <- function(m){
  m         <- round(t(t(m) / colSums(m,na.rm = TRUE)),2)
}

# normalize to rowSums
######################
normalizeToRowSums <- function(m){
  m         <- round(m / rowSums(m,na.rm=TRUE),2)
}

# add age and regional information to PATIENT_AGE_REGION
########################################################
addAgeRegion <- function(PATIENTS_AGE_REGION) {
  TABLE01SAS7BDAT     <- tbl(src,"TABLE01SAS7BDAT") # origin source / copy of: table01sas7bdat
  TABLE01SAS7BDAT     <- TABLE01SAS7BDAT %>% dplyr::select(ENROLID, AGE, REGION, REGIONC, SEX, GENDER) %>% collect()
  PATIENTS_AGE_REGION <- dplyr::select(PATIENTS_AGE_REGION, -REGION, -REGIONC, -SEX, -GENDER )
  PATIENTS_AGE_REGION <- left_join(PATIENTS_AGE_REGION, TABLE01SAS7BDAT, by = "ENROLID") 
  return(PATIENTS_AGE_REGION)
}

# filter patient by medical history and follow up span
######################################################
filter_patient_medHist_follUp <- function(PATIENTS_AGE_REGION, lbound=-1e6, ubound=1e6){
  load("./output/patDayMinMax.RData")
  patDayMinMax            <- patDayMinMax %>% filter(ixMin <= lbound,ixMax >= ubound) 
  PATIENTS_AGE_REGION     <- inner_join(PATIENTS_AGE_REGION,patDayMinMax %>% dplyr::select(ENROLID),by="ENROLID")
  return(PATIENTS_AGE_REGION)
}

# ranking plot
##############

plotRanks <- function(a ,a.text, b,b.text, labels.offset=0.1, arrow.len=0.1,cex=0.75)
{
  
  # Find the length of the vectors
  len.1 <- length(a)
  len.2 <- length(b)
  
  # Plot two columns of equidistant points
  plot(rep(1, len.1), 1:len.1, pch=20, cex=0.8, 
       xlim=c(0, 3), ylim=c(0, max(len.1, len.2)),
       axes=F, xlab="", ylab="") # Remove axes and labels
  points(rep(2, len.2), 1:len.2, pch=20, cex=0.8)
  
  # Put labels next to each observation
  text(rep(1-labels.offset, len.1), 1:len.1, a.text,cex = cex)
  text(rep(2+labels.offset, len.2), 1:len.2, b.text,cex = cex)
  
  # Now we need to map where the elements of a are in b
  # We use the match function for this job
  a.to.b <- match(a, b)
  
  # Now we can draw arrows from the first column to the second
  arrows(rep(1.02, len.1), 1:len.1, rep(1.98, len.2), a.to.b, 
         length=arrow.len, angle=20)
  
  # plot a box around
  box()
}

# aed ranking table
###################
aed_ranking <- function(patient_table) {
  
  # compute frequency and rank of AEDs by the number of patients whom received them
  nPat <- 
    PATIENTS_AGE_REGION %>% 
    dplyr::select(ENROLID) %>% 
    distinct() %>% 
    tally()
  
  print(paste("number of patients:",nPat))
  
  a <- 
    patient_table %>%
    filter(isAED) %>%
    dplyr::select(SUBSTANCENAME, ENROLID) %>%
    distinct() %>%
    group_by(SUBSTANCENAME) %>%
    tally(sort=TRUE) %>%
    rename(nPatients = n) %>%
    mutate(nPatients_frac = round(nPatients/nPat$n,2)) %>%
    mutate(rank_n = rank(-nPatients))
  
  return(a)
}



###########
# netezza #
###########

odbcinst_content <- "
[ODBC Drivers]
NetezzaSQL = Installed

[NetezzaSQL]
Driver           = /opt/nz/lib64/libnzodbc.so
Setup            = /opt/nz/lib64/libnzodbc.so
APILevel         = 1
ConnectFunctions = YYN
Description      = Netezza ODBC driver
DriverODBCVer    = 03.51
DebugLogging     = false
LogPath          = /tmp
UnicodeTranslationOption = utf8
CharacterTranslationOption = all
PreFetch         = 256
Socket           = 16384
"
writeLines(odbcinst_content, "~/.odbcinst.ini")

# specify connection information
driver   ="NetezzaSQL"
database ="HLT_CLM_AA"
server   ="10.1.4.12"
uid      ="NZDW_HLT_CLM_AA_RW"
pwd      ="jkyB6Ew84fOpq"
port     ="5480"

src         <- src_odbc_string(uid, pwd, database, server, port, driver) # connect to the database
tables_list <- dplyr::src_tbls(src) # list available tables

# delete unessential variables
rm(list=c("database","driver","odbcinst_content","port","pwd","server","uid"))


