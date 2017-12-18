#'@name exponetial
#'@rdname exponetial
#'@aliases
#'exponetial
#'exponetial.cdf
#'exponetial.pdf
#'
#'@title Exponetial
#'@description
#'Exponential function is a sigmoid type function.
#'\cr
#'It's CDF formula is: y = 1 - exp(-x))
#'\cr
#'It's PDF formula is: y = exp(-x))
#'@param x Vector of x parametres
#'
#'@return Vector of result vaues
NULL

#'@rdname exponential
#'@export
exponential <- function(x) {
  UseMethod("exponential")
}
#'@rdname exponential
#'@export
exponential.cdf <- function(x) {
  return(1 - exp(-x))
}
#'
#'@rdname exponential
#'@export
exponential.pdf <- function(x) {
  return(exp(-x))
}
