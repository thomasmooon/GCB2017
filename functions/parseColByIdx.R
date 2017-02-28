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