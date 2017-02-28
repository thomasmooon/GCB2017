diagToICD9 <- function(DIAG) {
  
  # transform  DIAG Codes to ICD9 Codes
  d3                <- which(DIAG/1e4<1)
  d4                <- which(DIAG/1e4>=1)
  DIAG[d3] <- DIAG[d3]/10
  DIAG[d4] <- DIAG[d4]/100
  
  # rename column name
  # DIAG          <- rename(DIAG, ICD9 = DIAG)
  
  # convert to factor for comatibility (in case of joining operations in the netezza environment)
  ICD9     <- as.factor(DIAG)
  return(ICD9)
  
}