#'@name gauss
#'@rdname gauss
#'@aliases
#'gauss
#'gauss.cdf
#'gauss.pdf
#'
#'@title Gauss
#'@description
#'Gauss function is a sigmoid type function.
#'\cr
#'It's CDF formula is: y = pnorm(x,mean = 0, sd=1, log=FALSE)
#'\cr
#'It's PDF formula is: y = exp(-x^2/2) / (2*pi)^(1/2)
#'@param x Vector of x parametres
#'
#'@return Vector of result vaues
NULL

#'@rdname gauss
#'@export
gauss <- function(x) {
  UseMethod("gauss")
}
#'@rdname gauss
#'@export
gauss.orig <- function(x) {
  UseMethod("gauss.orig")
}
#'@rdname gauss
#'@export
gauss.inverse <- function(x) {
  UseMethod("gauss.inverse")
}
#'@rdname gauss
#'@export
gauss.orig.cdf <- function(x){
  return(pnorm(x, mean = 0, sd = 1, log=FALSE))
}
#'
#'@rdname gauss
#'@export
gauss.orig.pdf <- function(x){
  return(exp(-x^2/2) / (2*pi)^(1/2))
}
#'@rdname gauss
#'@export
gauss.inverse.cdf <- function(x){
  return(qnorm(x, mean = 0, sd = 1, log=FALSE))
}
#'@rdname gauss
#'@export
gauss.inverse.pdf <- function(x){
  return(1/gauss.orig.pdf(x))
}
