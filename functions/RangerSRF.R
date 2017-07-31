#' Compute Ranger Survival Random Forest
#'
#' @param train <data.frame> Training data set (from MRMR), contains time, status and feature columns
#' @param test <data.frame> Test data set (from MRMR), contains time, status and feature columns
#' @param num.trees <int> Number of trees to trees to grow
#' @param ncores <int> Number of cores to use for parallelization
#' @param min.node.size <int> Regularization parameter, default is 3, bigger values (e. g. 15 grow trees with less nodes).
#' @param importance <char> inherited from ranger(). For SRF use 'permutation' only.
#' @param scale.permutation.importance <logical> set to TRUE if importance != 'none'
#' @param mc <char> comorbidity description for email status report
#' @param k <int> the k in the k-fold cross-validation for email status report
#' @param modelDescription Model description for email status report
#'
#' @return A list with 2 objects: ranger.forest, ranger.predict.
#' @export
#'
#' @examples

RangerSRF <-
  function(train,
           test,
           num.trees = 5000,
           ncores = 10,
           min.node.size = 3,
           mc,
           k,
           importance = 'none',
           scale.permutation.importance = F,
           modelDescription = "modelDescription") {

   
    # survival random forest
    ########################
    library(ranger)
    library(survival)
    cat("\n train survival random forest... \n ")

    # training
    time.test   <-
      system.time(
        ranger.forest <-
          ranger(dependent.variable.name = "time", 
                 status.variable.name = "status",
                 data = train,
                 num.trees = num.trees,
                 importance = importance,
                 num.threads = ncores, 
                 min.node.size = min.node.size, 
                 scale.permutation.importance = scale.permutation.importance
                 )
        )
    
    # train
    time.predict <- system.time(ranger.predict <- predict(ranger.forest, dat = test, num.threads = ncores)) # predict
    
    # mail reports
    times <- c(time.test[3], time.predict[3])
    times <- round(times/60,2)
    event <- c("SRF training","SRF prediction")
    body <- paste(event,as.character(times),sep=": ",collapse = " min\n")
    StatusEmail(
      subject = paste(
        "RStudio status email:",
        ", model =", modelDescription,
        ", MC = ", mc,
        ", CV:" , k,
        ", ncores: ", ncores,
        ", min.node.size: ", min.node.size,
        "training time minutes"),
      body = body
    ) 
    
    # results container
    ###################
    return(
      list(
        train = train,
        test = test,
        ranger.forest = ranger.forest,
        ranger.predict = ranger.predict
      )
    )
    
  }