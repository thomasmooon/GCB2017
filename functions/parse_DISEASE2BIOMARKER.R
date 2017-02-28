parse_DISEASE2BIOMARKER <- function(DISEASE2BIOMARKER_R, ICD9All) {
  str1       <- which(grepl(x = DISEASE2BIOMARKER_R$ICD9,pattern = ","))
  str2       <- which(grepl(x = DISEASE2BIOMARKER_R$ICD9,pattern = "-"))
  idsToParse <- unique(str1,str2)
  ICD9AllR   <- ICD9All %>% collect() 
  ICD9AllR   <- suppressWarnings(as.numeric(as.character(ICD9AllR[[1]]))) # factor to numeric
  ICD9AllR   <- ICD9AllR[complete.cases(ICD9AllR)]
  parsed     <- data.frame(unparsed=character(0),parsed=numeric(0)) # container, will be filled in for-loop
  
  for (k in 1:length(idsToParse)) {
    
    unparsed     <- as.character(DISEASE2BIOMARKER_R$ICD9[idsToParse[k]])
    split        <- unlist(strsplit(x = unparsed, split = ","))
    isInterval   <- grepl(pattern = "-", x = split)
    idsIntervals <- which(isInterval)
    
    
    # attach the remaining not-interval-elements to the container
    if (any(!isInterval)) {
      split1 <- as.numeric(split[which(!isInterval)])
      parsed <- rbind(parsed, data.frame(unparsed, parsed = split1))
    }
    
    if (any(isInterval)) {
      for (l in 1:length(idsIntervals)) {
        lthInterval <- as.numeric(unlist(strsplit(split[idsIntervals[l]], "-")))
        lBound      <- lthInterval[1]
        
        #' if the right bound has no digits larger than 0, e.g. 149 (which is equal to 149.0) and not 149.x (with x >0)then assume
        #' that all values beginning with 149 are meant and not only the 149.0.
        #' but in the case that x not equals zero, take 149.x als right bound.
        ifelse (
          lthInterval[2] == floor(lthInterval[2]),
          rBound <- lthInterval[2] + 1 - 1e-4,
          rBound <- lthInterval[2]
        )
        matchedICD9Codes <- unique(which(between(ICD9AllR, lBound, rBound))) %>% (function(x)
          ICD9AllR[x])
        # concatenate results
        parsed <- rbind(parsed, data.frame(unparsed, parsed = matchedICD9Codes))
      }
    }
  }
  parsed <- unique(parsed)
}