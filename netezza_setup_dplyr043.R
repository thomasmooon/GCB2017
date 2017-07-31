
# force installing dplyr 0.4.3
  pkg_url <- "https://cran.rstudio.com/src/contrib/Archive/dplyr/dplyr_0.4.3.tar.gz"
  install.packages(pkg_url, repos = NULL, type = "source")
  library(dplyr)

devtools::install_url("http://brasvnmap001.dir.ucb-group.com/svn/advanced_analytics_program/trunk/dplyrNetezza/nzdplyr_0.1.7.tar.gz",
                      upgrade_dependencies = FALSE, dependencies = TRUE)


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

#' specify connection information
driver="NetezzaSQL"
database="HLT_CLM_AA"
server="10.1.4.12"
uid="NZDW_HLT_CLM_AA_RW"
pwd="jkyB6Ew84fOpq"
port="5480"

# mount Biocdm


#' connect to the database
library(nzdplyr)
src <- src_odbc_string(uid, pwd, database, server, port, driver)

# list available tables
tables_list <- dplyr::src_tbls(src)

# patient table
pat <- tbl(src, "REFERENCE.PATIENTS")
pat.idxdays <- mutate(pat, quarter=ceiling(IXDAYS / 90))

install.packages("sas7bdat")
library(sas7bdat)
saspath <- "~/Biocdm/Products/Not-Product-specific/Epidemiology/gsp/analysis_epd183/data/sdtm/"
tab1 = read.sas7bdat(paste(saspath,"table01.sas7bdat",sep="")) # contains age and REGION of patients! TODO: join with other data in patient table

library(reshape2)
pat1 <- pat.idxdays %>% group_by(ENROLID, quarter) %>% select(SEX, HOSPFL, MHSAFL) %>% summarise(SEXq=max(SEX), HOSPq=sum(as.numeric(HOSPFL)), MHABUSEq=sum(as.numeric(MHSAFL))) %>% distinct()

# TODO: put these variables in columns (one for each value)
ivars = c("SUBSTANCENAME", "PHARMCLS1", "PHARMCLS2", "PHARMCLS3", "PHARMCLS4", "PHARMCLS5", "THERCLS", "THERGRP", "METQTY", "DAYSUPP")

# TODO: join table with matching information in other DB tables; for substance names: text matching


