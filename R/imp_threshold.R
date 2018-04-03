#'Improvement based Threshold
#'
#'Estimates threshold level of given function based on given paramters.
#'\cr
#'Improvement based threshold is a point where subject's performance grows the most.
#'@param y hit percentage of experiment data
#'@param p hit percentage of estimated data
#'@param rmNaN If true all NaN values are ignored during the computatioin. If true may severly alter result. Implycitly is rmNaN FALSE.
#'@return Error
#'@export
imp_threshold <- function(gama, lambda , sigmoid, core, ...){

  fitFunc <- function(level){
    return(-PSfunction(gama, lambda, sigmoid, core, level, ..., type = "pdf"))
  }

  fit <- optim(c(1), fitFunc)

  return(unlist(fit$par))
}
