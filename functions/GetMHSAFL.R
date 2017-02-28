GetMHSAFL.count <- function(x) {
  
  y <-
    x %>% 
    filter(MHSAFL == 1) %>% 
    filter(SUBSTANCENAME != "") %>% 
    dplyr::select(ENROLID, QUARTER, MHSAFL) %>% 
    distinct() 

  # create feature name
  y$colnames <- paste("MHSAFL", y$QUARTER, sep = ".x.")
  y <- y %>% dplyr::select(-QUARTER) %>% distinct()
  
  # reshape
  y <- spread(y, key=colnames, value = MHSAFL, fill = 0)
  
  
  return(y)
}