#'@name logistic
#'@rdname logistic
#'@aliases
#'logistic
#'logistic.cdf
#'logistic.pdf
#'
#'@title Logistic
#'@description
#'Logistic function is a sigmoid type function. It is the part of CDF.
#'\cr
#'It's CDF formula is: y = 1 / ( 1 + exp(-x))
#'\cr
#'It's PDF formula is: y = exp(x) / ( 1 + exp(x))^2
#'
#'@param x Vector of x parametres
#'@return Vector of result vaues
#'@export

#'@rdname logistic
#'@export
logistic <- function(x) {
  UseMethod("logistic")
}
#'@rdname logistic
#'@export
logistic.cdf <- function(x) {
  return(1 / ( 1 + exp(-x)))
}
#'@rdname logistic
#'@export
logistic.pdf <- function(x) {
  return(exp(x) / ( 1 + exp(x))^2 )
}
