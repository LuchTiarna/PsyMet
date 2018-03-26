#'Log-likelihood estimate
#'
#'Estimates likelihood for certain paramters of an function.
#'\cr
#'Formula for likelihood estimate computation:
#'\deqn{LE = 2*\sum{log(ni!/((ni*yi)!*(ni - ni*yi)!)) + ni * yi * log(pi) + ni * (1-yi) * log(1-pi)}}
#'Where n indicates number of trials per level, y hit percentage of experiment data, p hit percentage of estiamted data and i indicates the level.
#'@param y hit percentage of experiment data
#'@param p hit percentage of estimated data
#'@param rmNaN If true all NaN values are ignored during the computatioin. If true may severly alter result. Implycitly is rmNaN FALSE.
#'@return Error
#'@export
likelihood_log <- function(y, p, n, rm.nan=FALSE){

  if(length(y) != length(p) || length(n) != length(p) )
  {warning("All vectors must have the same length."); return(NaN)}

  pe <- base::log(base::choose(n,(n*y)))
  pe <- pe + n*y*base::log(p)
  pe <- pe + (1 - y)*n*base::log(1-p)

  if(rm.nan==TRUE){
    pe <- pe[!is.nan(pe)]
  }

  return(base::sum(pe))
}
