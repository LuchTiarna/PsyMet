#'Performance based Threshold
#'
#'Estimates threshold level of given function based on given paramters.
#'\cr
#'Performance based threshold is a point where subjects performance is halfway between gamma and 1-lambda.
#'@param y hit percentage of experiment data
#'@param p hit percentage of estimated data
#'@param rmNaN If true all NaN values are ignored during the computatioin. If true may severly alter result. Implycitly is rmNaN FALSE.
#'@return Error
#'@export
perf_threshold <- function(gama, lambda , sigmoid, core, ...){
  halfway <- (1 + gama - lambda)*0.5
  halfway <- PSfunction(gama, lambda, sigmoid, core, halfway, ..., inverse=TRUE)

  return(halfway)
}
