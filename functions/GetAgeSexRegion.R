GetAgeSexRegion <- function(x) {
  # convenient wrapper
  y <- x %>% dplyr::select(ENROLID, AGE, GENDER, REGIONC) %>% distinct()
}