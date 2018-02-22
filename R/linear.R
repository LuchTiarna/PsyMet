#'@name linear
#'@rdname linear
#'@aliases
#'linear
#'linear.cdf
#'linear.pdf
#'
#'@title Linear
#'@description
#'Linear function is a core type function.
#'\cr
#'It's CDF formula is: y = a * x + b
#'\cr
#'It's PDF formula is: y = a
#'
#'@param x Vector of x parametres
#'@param a relative parameter
#'@param b absolute parameter
#'
#'@return Vector of result vaues
NULL

#'@rdname linear
#'@export
linear <- function(x, a, b) {
  UseMethod("linear")
}
#'@rdname linear
#'@export
linear.orig <- function(x, a, b) {
  UseMethod("linear.orig")
}
#'@rdname linear
#'@export
linear.inverse <- function(x, a, b) {
  UseMethod("linear.inverse")
}
#'@rdname linear
#'@export
linear.orig.cdf <- function(x, a=NULL, b=NULL) {
  if(is.null(b) && length(a) > 1){
    params <- a
    a=as.double(params[1])
    b=as.double(params[2])
  }
  return(a * x + b)
}
#'@rdname linear
#'@export
linear.orig.pdf <- function(x, a=NULL, b=NULL) {
  if(is.null(b) && length(a) > 1){
    params <- a
    a=as.double(params[1])
    b=as.double(params[2])
  }
  return(a)
}
#'@rdname linear
#'@export
linear.inverse.cdf <- function(x, a=NULL, b=NULL) {
  if(is.null(b) && length(a) > 1){
    params <- a
    a=as.double(params[1])
    b=as.double(params[2])
  }
  return((x - b)/a)
}
#'@rdname linear
#'@export
linear.inverse.pdf <- function(x, a=NULL, b=NULL) {
  if(is.null(b) && length(a) > 1){
    params <- a
    a=as.double(params[1])
    b=as.double(params[2])
  }
  return(1/a)
}
