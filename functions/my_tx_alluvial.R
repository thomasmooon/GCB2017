my_tx_alluvial <- function (txvis, nsequ = NULL, sequence = TRUE, start = NULL, 
          end = NULL, interval = "4 months", conflict = "majority", 
          tx_cw = 0.05, ...) 
{
  if (!"txvis" %in% class(txvis)) {
    stop("You must pass a txvis object.")
  }
  if (!requireNamespace("alluvial", quietly = TRUE)) {
    message("This function requires the non-CRAN package `alluvial` installed from GitHub.")
    user_inp <- readline(prompt = "Do you want to install this package? (y/n)")
    if (user_inp == "y") {
      devtools::install_github("mbojan/alluvial")
      requireNamespace(alluvial)
    }
    else {
      stop(paste0("You must install the package `alluvial` for this function to work.\n", 
                  " You can install the package directly from GitHub using:\n\n", 
                  "> library(devtools)\n> devtools::install_github('mbojan/alluvial')"))
    }
  }
  if (sequence == TRUE) {
    txvis.ref <- data.frame(t(apply(reform_seq(txvis, nsequ), 
                                    1, function(x) {
                                      x[is.na(x)] <- "None"
                                      (x)
                                    })), stringsAsFactors = F)
  }
  else {
    txvis.ref <- data.frame(t(apply(reform_dates(txvis, nsequ, 
                                                 start, end, interval, conflict), 1, function(x) {
                                                   x[is.na(x)] <- "None"
                                                   (x)
                                                 })), stringsAsFactors = F)
  }
  seq.cols <- paste0(rep("seq_", (ncol(txvis.ref) - 1)), c(1:(ncol(txvis.ref) - 
                                                                1)))
  seq.fun <- paste0(seq.cols, collapse = " + ")
  input_agged_seq <- stats::aggregate(data = txvis.ref, stats::as.formula(paste0("pt_id ~ ", 
                                                                                 seq.fun)), FUN = length)
  colnames(input_agged_seq)[ncol(input_agged_seq)] <- "freq"
  output <- alluvial::alluvial(input_agged_seq[, 1:(ncol(txvis.ref) - 
                                                      1)], freq = input_agged_seq$freq, blocks = TRUE, cw = tx_cw, ...)
  
}
