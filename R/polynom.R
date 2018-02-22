#'@name polynom
#'@rdname polynom
#'@aliases
#'poly
#'poly.cdf
#'poly.pdf
#'
#'@title Polynom
#'@description
#'Polynomial function is a core type function.
#'\cr
#'It's CDF formula is: y = (x / a) ^ b
#'\cr
#'It's PDF formula is: y = (x / a) ^ (b- 1) * b / a
#'@param x Vector of x parametres
#'@param a adjusts the curve slope
#'@param b adjusts the curve slope
#'
#'@return Vector of result vaues
NULL

#'@rdname polynom
#'@export
polynom <- function(x, a, b) {
  UseMethod("polynom")
}
#'@rdname polynom
#'@export
polynom.orig <- function(x, a, b) {
  UseMethod("polynom.orig")
}
#'@rdname polynom
#'@export
polynom.inverse <- function(x, a, b) {
  UseMethod("polynom.inverse")
}
#'@rdname polynom
#'@export
polynom.orig.cdf <- function(x, a=NULL, b=NULL) {
  if(is.null(b) && length(a) > 1){
    params <- a
    a=as.double(params[1])
    b=as.double(params[2])
  }
  return((x / a) ^ b)
}
#'@rdname polynom
#'@export
polynom.orig.pdf <- function(x, a=NULL, b=NULL) {
  if(is.null(b) && length(a) > 1){
    params <- a
    a=as.double(params[1])
    b=as.double(params[2])
  }
  return((x / a) ^ (b- 1) * b / a)
}
#'@rdname polynom
#'@export
polynom.inverse.cdf <- function(x, a=NULL, b=NULL) {
  if(is.null(b) && length(a) > 1){
    params <- a
    a=as.double(params[1])
    b=as.double(params[2])
  }
  return(a * (x ^ (1/b)))
}
#'@rdname polynom
#'@export
polynom.inverse.pdf <- function(x, a=NULL, b=NULL) {
  if(is.null(b) && length(a) > 1){
    params <- a
    a=as.double(params[1])
    b=as.double(params[2])
  }
  return(a/b * x ^ (1/b-1))
}
