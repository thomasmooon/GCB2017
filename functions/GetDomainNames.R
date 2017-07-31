getDomain <- function(x) sapply(x,function(k) strsplit(k,"\\.x\\.") %>% lapply(function(s) s[1]) %>% unlist() )

getSubDomain <-  function(x) {
  q <- c("1",paste("m",1:8,sep=""))
  subdomain <- sapply(x, function(k) strsplit(k, "\\.x\\.") %>% lapply(function(s) s[2]) %>% unlist())
  subdomain[subdomain %in% q] <- NA
  return(subdomain)
}
getQuarter <-  function(x) {
  q <- c("1",paste("m",1:8,sep=""))
  qq <- sapply(x, function(k)  strsplit(k, "\\.x\\.") %>% lapply(function(s)  s[length(s)]) %>% unlist())
  qq[!(qq %in% q)] <- NA
  return(qq)
}
getAEDfromSubdomain <- function(d) {
  id <- d$domain == "QUARTQTY"
  new.subdomain <- sapply(d$subdomain[id], function(x) strsplit(x,"_")) %>% lapply(function(x) x[1]) %>% unlist()
  d$subdomain[id] <- new.subdomain
  return(d)
}

# wrapper
getDomainTotal <- function(df,colname) {
  df$domain <- getDomain(df[,colname]) %>% as.character()
  df$subdomain <- getSubDomain(df[colname]) %>% as.character()
  df$domain.q <- getQuarter(df[colname]) %>% as.character()
  df <- getAEDfromSubdomain(df)
  return(df)
}
