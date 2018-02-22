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
logistic.orig <- function(x) {
  UseMethod("logistic.orig")
}
#'@rdname logistic
#'@export
logistic.inverse <- function(x) {
  UseMethod("logistic.inverse")
}
#'@rdname logistic
#'@export
logistic.orig.cdf <- function(x) {
  return(1 / ( 1 + exp(-x)))
}
#'@rdname logistic
#'@export
logistic.orig.pdf <- function(x) {
  return(exp(x) / ( 1 + exp(x))^2 )
}
#'@rdname logistic
#'@export
logistic.inverse.cdf <- function(x) {
  return(-log(1/x -1))
}
#'@rdname logistic
#'@export
logistic.inverse.pdf <- function(x) {
  return((x*(x -1)))
}
