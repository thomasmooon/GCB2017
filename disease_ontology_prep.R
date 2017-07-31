###############################################################################
# $HeadURL: disease_ontology_prep.R
# $Rev:   
# $Date:    19 OCT 2016
# $Author:  Thomas Gerlach
# -----------------------------------------------------------------------------
# CATEG: ICD9, UMLS
# DESCR: disease ontology extension
# KEYS : 
# INPUT: doid_2016-09-15.txt, ICD9GROUPING (netezza)
# OUT  : disOnt_ext1
# REQ  : 
# NOTES:
###############################################################################
# CHANGELOG:
#
###############################################################################
source("z_start.R")

# disGenNet <- 
#   read.csv(
#     "~/epilepsie_preprocessing_descrStat/data/curatedAndBefree_gene_disease_associations_2016-10-19.csv",
#     sep=";", stringsAsFactors=FALSE)

disOnt <- 
  read.delim("~/epilepsie_preprocessing_descrStat/data/doid_2016-09-15.txt",
  header=FALSE, quote="", stringsAsFactors=FALSE)


# Preprocessing disease Ontology
################################

# identify [Term] blocks, store each block in a list element
str1   <- "Term"
str1.i <- which(grepl(disOnt$V1,pattern = str1,ignore.case = FALSE))
disOnt <- disOnt[ str1.i[1]:nrow(disOnt),]
str1.i <- which(grepl(disOnt,pattern = str1,ignore.case = FALSE))
term.start <- str1.i+1
str2   <- "Typedef"
str2.i <- which(grepl(disOnt,pattern = str2,ignore.case = FALSE))
term.end   <- c( str1.i[-1]-1, str2.i[1]-1)
terms.list <- vector("list",length = length(term.start))
for(n in 1:length(term.start)) {
  terms.list[[n]] <- disOnt[term.start[n] : term.end[n]]
}

# keep only list elements which fullfill all filters
# - contain "ICD9"
# - contain  "UMLS
F.ICD9 <- function(x) any(grepl(pattern = "xref: ICD9",x = x))
F.UMLS <- function(x) any(grepl(pattern = "xref: UMLS_CUI",x = x))
has.icd9 <- unlist(lapply(terms.list, FUN = F.ICD9))
has.umls <- unlist(lapply(terms.list, FUN = F.UMLS))
terms.list <- terms.list[has.icd9 & has.umls]

# only keep these attributes: doid, icd9, umls, mesh, name
keys <- c("id: DOID:","name: ","xref: ICD9CM:","xref: UMLS_CUI:")
KeyElements <- function(l) { idx <- as.numeric(unlist(sapply(keys,FUN = function(key) which(grepl(pattern = key,x = l))))) }
terms.IdsKeyEl <- lapply(terms.list,FUN = KeyElements)
for(n in 1:length(terms.IdsKeyEl)) {terms.list[[n]] <- terms.list[[n]][ terms.IdsKeyEl[[n]] ]}
NoTrashElements <- function(l) { idx <- as.numeric(unlist(sapply("alt_id: DOID",FUN = function(key) which(!grepl(pattern = key,x = l))))) }
terms.IdsNoTrash <- lapply(terms.list, FUN = NoTrashElements)
for(n in 1:length(terms.IdsNoTrash)) {terms.list[[n]] <- terms.list[[n]][ terms.IdsNoTrash[[n]] ]}


# reshape list elements to dataframes for subsequent concatenation
TermsToDf <- function(term) {
  # reformat
  term <- gsub(pattern = "xref: ",replacement = "",x = term)
  term <- gsub(pattern = "id: DOID:",replacement = "DOID:",x = term)
  # reshape
  term.df <- matrix(trimws(unlist(strsplit(term,":"))),ncol = 2,byrow=T)
  term.df <- as.data.frame(term.df,stringsAsFactors=FALSE)
  colnames(term.df) <- c("var","value")
  # separate
  icd9     <- term.df$value[term.df$var=="ICD9CM"]
  umls    <- term.df$value[term.df$var=="UMLS_CUI"]
  term.df <- term.df %>% filter(!(var %in% c("ICD9CM","UMLS_CUI")))
  # reshape
  term.df <- spread(term.df,var,value)
  row.names(term.df) <- NULL
  # combine
  if(length(icd9) == 0) icd9 <- NA
  if(length(umls) == 0) umls <- NA
  result <- cbind(term.df,expand.grid(ICD9=icd9,UMLS=umls))
}

terms.list.mat <- lapply(X = terms.list,FUN = TermsToDf)
terms.mat <- listToDf(terms.list.mat)

# # if multiple icd9 (e.g. 1-9.99)for a particular DOID -> split
  icd9.ranged <- which(grepl(pattern = "-",x = terms.mat$ICD9))
  terms.mat.icd9R   <- terms.mat[icd9.ranged,]
  terms.mat.noIcd9R <- terms.mat[-icd9.ranged,]
  
  ICD9 <- tbl(src,"REFERENCE.ICD9GROUPING") %>%  filter(ICD9 != "") %>%  
    select(ICD9) %>% distinct() %>% collect()
  ICD9 <- as.numeric(as.character(ICD9$ICD9))
  ICD9 <- ICD9[complete.cases(ICD9)]
  
  SplitICD9 <- function(icd9Interval,ICD9Space) {
    s <- strsplit(as.character(icd9Interval),"-")
    s.lbound <- as.numeric(s[[1]][1])
    s.rbound <- as.numeric(s[[1]][2])
    icd9.new <- between(ICD9Space, s.lbound, s.rbound)
    icd9.new <- ICD9Space[icd9.new]
    icd9.all <- unique(c(s.lbound,icd9.new,s.rbound))
    return(icd9.all)
  }

# # # split, merge split
terms.mat.icd9parsed <- NULL
for(i in 1:nrow(terms.mat.icd9R)) {
  icd9.interv <- SplitICD9(terms.mat.icd9R$ICD9[i],ICD9)
  tmp         <- terms.mat.icd9R[i,]
  terms.mat.icd9parsed <- 
    rbind(
      terms.mat.icd9parsed,
      cbind(DOID=tmp$DOID, name=tmp$name, ICD9=icd9.interv, UMLS=tmp$UMLS))
}

# # # combine 
disOnt_ext1 <- rbind(terms.mat.noIcd9R,terms.mat.icd9parsed)

# save / export
###############

name.csv  <- paste("output/disease_ontology/disOnt_ex1_",Sys.Date(),".csv",sep = "")
name.rdat <- paste("output/disease_ontology/disOnt_ext1_",Sys.Date(),".RData",sep="")
write.csv2(disOnt_ext1,file = name.csv,row.names=FALSE)
save(disOnt_ext1,file = name.rdat)
