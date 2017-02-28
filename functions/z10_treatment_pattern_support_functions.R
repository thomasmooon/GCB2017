# split_enrol_by_treat_quarterly <- function(samples_AEDs){
# 
#   # quarter START and STOP Dates
#   q_start_stop <-
#     samples_AEDs %>%
#     dplyr::select(quarter,quart_start,quart_end) %>%
#     distinct()
# 
#   samples_AED_quarter <- NULL # this one will be filled with the splitted permutations
#   e_ids               <- samples_AEDs %>%    dplyr::select(ENROLID) %>%    distinct()
# 
#   for(i in 1:length(e_ids$ENROLID)) {
# 
#     if(i %% 10 == 0) print(i)
#     
#     e_id <- e_ids$ENROLID[i]
#     
#     e_treat_seq <-
#       samples_AEDs %>%
#       filter(ENROLID == e_id) %>%
#       dplyr::select(SUBSTANCENAME, quarter) %>%
#       distinct()
# 
#     e_treat_seq <- aggregate(SUBSTANCENAME ~ quarter, data=e_treat_seq,FUN=list)
#     e_new_seq   <- expand.grid(e_treat_seq$SUBSTANCENAME)
# 
#     if (ncol(e_new_seq) > 1) {
#       colnames(e_new_seq)   <- e_treat_seq$quarter
#       e_new_seq_trf         <- suppressWarnings(gather(e_new_seq,key=quarter,value=SUBSTANCENAME))
#       e_new_seq_trf         <- cbind(ENROLID = e_id, e_new_seq_trf)
#       e_new_seq_trf$ENROLID <- paste(e_new_seq_trf$ENROLID,"_",1:nrow(e_new_seq),sep = "")
#       samples_AED_quarter   <- rbind(samples_AED_quarter,e_new_seq_trf)
#     } else {
#       e_treat_seq         <- cbind(ENROLID = e_id, e_treat_seq)
#       samples_AED_quarter <- rbind(samples_AED_quarter,e_treat_seq)
#     
# 
#     }
#   }
#   # add quarter start and quarter stop dates
#   samples_AED_quarter$quarter <- as.numeric(samples_AED_quarter$quarter)
#   samples_AED_quarter <- left_join(samples_AED_quarter,q_start_stop,by="quarter")
#   #
#   return(samples_AED_quarter)
#   
# }


############################################################
# 
# split_enrol_by_treat_halfyear  <- function(samples_AEDs){
#   
#   # quarter START and STOP Dates
#   hy_start_stop <- 
#     samples_AEDs %>% 
#     dplyr::select(half_year,half_year_start,half_year_end) %>% 
#     distinct()
#   
#   samples_AED_hy <- NULL # this one will be filled with the splitted permutations
#   e_ids         <- samples_AEDs %>%    dplyr::select(ENROLID) %>%    distinct()
#   
#   for(e_id in e_ids$ENROLID) {
#     
#     e_treat_seq <- 
#       samples_AEDs %>% 
#       filter(ENROLID == e_id) %>% 
#       dplyr::select(SUBSTANCENAME, half_year) %>% 
#       distinct()
#     
#     e_treat_seq <- aggregate(SUBSTANCENAME ~ half_year, data=e_treat_seq,FUN=list)
#     e_new_seq   <- expand.grid(e_treat_seq$SUBSTANCENAME)
#     
#     if (ncol(e_new_seq) > 1) {
#       colnames(e_new_seq)   <- e_treat_seq$half_year
#       e_new_seq_trf         <- suppressWarnings(gather(e_new_seq,key=half_year,value=SUBSTANCENAME))
#       e_new_seq_trf         <- cbind(ENROLID = e_id, e_new_seq_trf)
#       e_new_seq_trf$ENROLID <- paste(e_new_seq_trf$ENROLID,"_",1:nrow(e_new_seq),sep = "")
#       samples_AED_hy   <- rbind(samples_AED_hy,e_new_seq_trf)
#     } else {
#       e_treat_seq         <- cbind(ENROLID = e_id, e_treat_seq)
#       samples_AED_hy <- rbind(samples_AED_hy,e_treat_seq)
#     }
#     
#   } 
#   # add half_year start and half_year stop dates
#   samples_AED_hy$quarter <- as.numeric(samples_AED_hy$quarter)
#   samples_AED_hy <- left_join(samples_AED_hy,hy_start_stop,by="half_year")
#   #
#   return(samples_AED_hy)
# } 




#######################################################
# 
# split_enrol_by_treat_halfyear_par  <- function(samples_AEDs,ncores=10){
#   
#   # parallel computing
#     library(doParallel)
#     registerDoParallel(ncores)
# 
#   
#   # half-year START and STOP Dates
#   hy_start_stop <- 
#     samples_AEDs %>% 
#     dplyr::select(half_year,half_year_start,half_year_end) %>% 
#     distinct()
#   
#   e_ids <- samples_AEDs %>% dplyr::select(ENROLID) %>% distinct()
#   
#   # start foreach
#   samples_AED_hy <- 
#     foreach(
#       i = 1:length(e_ids$ENROLID),
#       .combine = rbind, 
#       .packages=c("tidyr","dplyr")
#     ) %dopar% {
#       
#       e_id <- e_ids$ENROLID[i] # i-th patient ENROLID
#       
#       e_treat_seq <- 
#         samples_AEDs %>% 
#         filter(ENROLID == e_id) %>% 
#         dplyr::select(SUBSTANCENAME, half_year) %>% 
#         distinct()
#       
#       e_treat_seq <- aggregate(SUBSTANCENAME ~ half_year, data=e_treat_seq,FUN=list)
#       e_new_seq   <- expand.grid(e_treat_seq$SUBSTANCENAME)
#       
#       if (ncol(e_new_seq) > 1) {
#         colnames(e_new_seq)   <- e_treat_seq$half_year
#         e_new_seq_trf         <- suppressWarnings(gather(e_new_seq,key=half_year,value=SUBSTANCENAME))
#         e_new_seq_trf         <- cbind(ENROLID = e_id, e_new_seq_trf)
#         e_new_seq_trf$ENROLID <- paste(e_new_seq_trf$ENROLID,"_",1:nrow(e_new_seq),sep = "")
#         output                <- e_new_seq_trf
#       } else {
#         output                <- cbind(ENROLID = e_id, e_treat_seq)
#       }
#     } 
#   # end foreach
#   #stopCluster(cl)
#   
#   # add half_year start and half_year stop dates
#   samples_AED_hy <- left_join(samples_AED_hy,hy_start_stop,by="half_year")
#   #
#   return(samples_AED_hy)
# } 




####################################
# mclapply version
split_enrol_by_treat_hy_mclapply <- function(x) {
  
  # This function splits patients into "subpatient" datasets.
  # As a result, not only the most frequency treatment in the alluvial or sunflower
  # plots are considered, rather if an patient gets multiple treatments within a sequence,
  # then the treatments are splitted to subunits to get aware of underlying treatment patterns.
  #
  # This example clarifies the schema - "Origin" |-> "Origin_transformed" :
  #
  #' Origin:
  #' row | ENROLID | SUBSTANCENAME | SEQUENCE
  #' ----------------------------------
  #' 1   | E1      | s1            | 0
  #' 2   | E1      | s2            | 0
  #' 3   | E1      | s1            | 1
  #' 
  #' Origin_transformed:
  #' row | ENROLID_trf | SUBSTANCENAME | SEQUENCE
  #' ----------------------------------
  #' 1   | E1_0        | s1            | 0
  #' 2   | E1_0        | s1            | 1
  #' 3   | E1_1        | s2            | 0
  #' 4   | E1_1        | s2            | 1
  #' 
  #' So, what exactly happend?
  #' 
  #' If you would utilize the "Origin" Sample, then the txvis package takes
  #' only one SUBSTANCENAME per Sequence. Therefore in "Origin" either row 1 or row 2 will be omitted.
  #' So the other one will get lost.
  #' 
  #' If you would utilize "Origin_transformed", then for each ENROLID_trf only exactly 1 SUBSTANCENAME
  #' per SEQUENCE exists.
  #' So nothing gets lost but you must be aware of some redundance.
  #' The regarding plots must now be interpreted as "Consider patients who get drug X, which drugs did they
  #' (also) take in the following sequences?"
  #' 
  
  
  e_treat_seq <- 
    x %>% 
    dplyr::select(SUBSTANCENAME, half_year) %>% 
    distinct()
  
  e_id <- unique(x$ENROLID)
  
  #  e_treat_seq <- aggregate(SUBSTANCENAME ~ half_year, data=e_treat_seq,FUN=list)
  e_treat_seq <- aggregate(SUBSTANCENAME ~ half_year, data=e_treat_seq,FUN=list)
  e_new_seq   <- expand.grid(e_treat_seq$SUBSTANCENAME)
  
  #if (ncol(e_new_seq) > 1) {
  colnames(e_new_seq)   <- e_treat_seq$half_year
  e_new_seq_trf         <- suppressWarnings(gather(e_new_seq,key=half_year,value=SUBSTANCENAME))
  e_new_seq_trf         <- cbind(ENROLID = e_id, e_new_seq_trf)
  e_new_seq_trf$ENROLID <- paste(e_new_seq_trf$ENROLID,"_",1:nrow(e_new_seq),sep = "")
  output                <- e_new_seq_trf
  # } else {
  #   output                <- cbind(ENROLID = e_id, e_treat_seq)
  # }
}








##################################################
# split_enrol_by_treat_quarterly_par <- function(samples_AEDs,ncores=10){
#   
#   # parallel computing
#   library(doParallel)
#   registerDoParallel(ncores)
#   
#   
#   # quarter START and STOP Dates
#   q_start_stop <- 
#     samples_AEDs %>% 
#     dplyr::select(quarter,quart_start,quart_end) %>% 
#     distinct()
#   
#   e_ids <- samples_AEDs %>%    dplyr::select(ENROLID) %>%    distinct()
#   
#   # start foreach
#   samples_AED_quarter <- 
#     foreach(
#       i = 1:length(e_ids$ENROLID),
#       .combine = rbind, 
#       .packages=c("tidyr","dplyr")
#     ) %dopar% {
#       
#       e_id <- e_ids$ENROLID[i] # i-th patient ENROLID
#       
#       e_treat_seq <- 
#         samples_AEDs %>% 
#         filter(ENROLID == e_id) %>% 
#         dplyr::select(SUBSTANCENAME, quarter) %>% 
#         distinct()
#       
#       e_treat_seq <- aggregate(SUBSTANCENAME ~ quarter, data=e_treat_seq,FUN=list)
#       e_new_seq   <- expand.grid(e_treat_seq$SUBSTANCENAME)
#       
#       if (ncol(e_new_seq) > 1) {
#         colnames(e_new_seq)   <- e_treat_seq$quarter
#         e_new_seq_trf         <- suppressWarnings(gather(e_new_seq,key=quarter,value=SUBSTANCENAME))
#         e_new_seq_trf         <- cbind(ENROLID = e_id, e_new_seq_trf)
#         e_new_seq_trf$ENROLID <- paste(e_new_seq_trf$ENROLID,"_",1:nrow(e_new_seq),sep = "")
#         output                <- e_new_seq_trf
#       } else {
#         output                <- cbind(ENROLID = e_id, e_treat_seq)
#       }
#       
#     } 
# 
#   # end foreach
#   
#   # add quarter start and quarter stop dates
#   samples_AED_quarter$quarter <- as.numeric(samples_AED_quarter$quarter)
#   samples_AED_quarter         <- left_join(samples_AED_quarter,q_start_stop,by="quarter")
#   #
#   return(samples_AED_quarter)
# } 











############################ 
# variant for mclappy paralellization
# split_enrol_by_treat_quarterly_mclapply <- function(x) {
#   
#   e_treat_seq <- 
#     x %>% 
#     dplyr::select(SUBSTANCENAME, quarter) %>% 
#     distinct()
#   
#   e_id <- unique(x$ENROLID)
#   
#   
#   e_treat_seq <- aggregate(SUBSTANCENAME ~ quarter, data=e_treat_seq,FUN=list)
#   e_new_seq   <- expand.grid(e_treat_seq$SUBSTANCENAME)
#   
#   if (ncol(e_new_seq) > 1) {
#     colnames(e_new_seq)   <- e_treat_seq$quarter
#     e_new_seq_trf         <- suppressWarnings(gather(e_new_seq,key=quarter,value=SUBSTANCENAME))
#     e_new_seq_trf         <- cbind(ENROLID = e_id, e_new_seq_trf)
#     e_new_seq_trf$ENROLID <- paste(e_new_seq_trf$ENROLID,"_",1:nrow(e_new_seq),sep = "")
#     output                <- e_new_seq_trf
#   } else {
#     output                <- cbind(ENROLID = e_id, e_treat_seq)
#   }
# }



################
# rbind list elements to data.frame
# - list elements have unequal length, so a simple data.frame(l) doesn't work
listToDf <- function(l) {

  N <- length(l)
  df <- NULL
  
  for (k in 1:N) {
    df <- rbind(df, l[[k]])
  }
  
  return(df)

}