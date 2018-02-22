#'@name logy
#'@rdname logy
#'@aliases
#'logy
#'logy.cdf
#'logy.pdf
#'
#'@title Logy
#'@description
#'Log function is a core type function. It is the part of PDF.
#'\cr
#'It's CDF formula is: y = (x - a) / b
#'\cr
#'It's PDF formula is: y = a / x
#'@param x Vector of x parametres
#'@param a moves whole curve to the left towards lesser values.
#'@param b rotates the line closer to the x axis and movelogys it towards y axis
#'@return Vector of result vaues
#'@export
#'
#'@rdname logy
#'@export
logy <- function(x, a, b) {
  UseMethod("logy")
}
#'@rdname logy
#'@export
logy.orig <- function(x, a, b) {
  UseMethod("logy.orig")
}
#'@rdname logy
#'@export
logy.inverse <- function(x, a, b) {
  UseMethod("logy.inverse")
}
#'@rdname logy
#'@export
logy.orig.cdf <- function(x, a=NULL, b=NULL) {
  if(is.null(b) && length(a) > 1){
    params <- a
    a=as.double(params[1])
    b=as.double(params[2])
  }
  return(a * log(x) + b)
}
#'@rdname logy
#'@export
logy.orig.pdf <- function(x, a=NULL, b=NULL) {
  if(is.null(b) && length(a) > 1){
    params <- a
    a=as.double(params[1])
    b=as.double(params[2])
  }
  return(a / x)
}
#'@rdname logy
#'@export
logy.inverse.cdf <- function(x, a=NULL, b=NULL) {
  if(is.null(b) && length(a) > 1){
    params <- a
    a=as.double(params[1])
    b=as.double(params[2])
  }
  return(exp((x-b)/a))
}
#'@rdname logy
#'@export
logy.inverse.pdf <- function(x, a=NULL, b=NULL) {
  if(is.null(b) && length(a) > 1){
    params <- a
    a=as.double(params[1])
    b=as.double(params[2])
  }
  return(exp((x-b)/a)*(x/a))
}
