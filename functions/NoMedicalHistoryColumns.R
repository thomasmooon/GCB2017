NoMedicalHistoryColumns <- function(data) {
  
  # regular expression patterns for quarters in medical history (between -2y and +1 quarter relative
  # to indexdate)
  pattern1 <- paste("\\.x\\.m",9:1,sep="")
  pattern2 <- paste("\\.x\\.",1,sep="")
  pattern  <- c(pattern1,pattern2)
  pattern  <- paste("(",pattern,")$",sep="")
  cnames   <- colnames(data)
  
  # features appearing in quarters not in medical history
  notMedicalHistory <- !sapply(cnames, function(x) any(sapply(pattern, function(y) grepl(y,x))) )
  sum(notMedicalHistory)
  
  # return column names which aren't associated with the medical history
  cat("#-----     No-medical-history columns:     -----#\n")
  cat(cnames[notMedicalHistory])
  return(cnames[notMedicalHistory])
}