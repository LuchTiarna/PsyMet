#'@name weibull
#'@rdname weibull
#'@aliases
#'weibull
#'weibullcdf
#'weibull.pdf
#'
#'@title Weibul
#'@description
#'Weibull function is a core type function.
#'\cr
#'It's CDF formula is: y = 2*m*s*(log(x) - log(m)) / log(2) + log(log(2))
#'\cr
#'It's PDF formula is: y = 2*m*s/log(2)/x
#'@param x Vector of x parametres
#'@param m marks the x coordinate where function reaches the midpoint.
#'@param s adjusts the slope of the fuction
#'
#'@return Vector of result vaues
NULL

#'@rdname weibull
#'@export
weibull <- function(x, m, s) {
  UseMethod("weibull")
}
#'@rdname weibull
#'@export
weibull.orig <- function(x, m, s) {
  UseMethod("weibull.orig")
}
#'@rdname weibull
#'@export
weibull.inverse <- function(x, m, s) {
  UseMethod("weibull.inverse")
}
#'@rdname weibull
#'@export
weibull.orig.cdf <- function(x, m=NULL, s=NULL) {
  if(is.null(s) && length(m) > 1){
    params <- m
    m=as.double(params[1])
    s=as.double(params[2])
  }
  return(2*m*s*(log(x) - log(m)) / log(2) + log(log(2)))
}
#'@rdname weibull
#'@export
weibull.orig.pdf <- function(x, m=NULL, s=NULL) {
  if(is.null(s) && length(m) > 1){
    params <- m
    m=as.double(params[1])
    s=as.double(params[2])
  }
  return(2*m*s/log(2)/x)
}
#'@rdname weibull
#'@export
weibull.inverse.cdf <- function(x, m=NULL, s=NULL) {
  if(is.null(s) && length(m) > 1){
    params <- m
    m=as.double(params[1])
    s=as.double(params[2])
  }
  return(exp(log(2)/(2*m*s)*(x - log(log(2))) + log(m)))
}
#'@rdname weibull
#'@export
weibull.inverse.pdf <- function(x, m=NULL, s=NULL) {
  if(is.null(s) && length(m) > 1){
    params <- m
    m=as.double(params[1])
    s=as.double(params[2])
  }
  return(exp(log(2)/(2*m*s)*(x - log(log(2))) + log(m))*log(2)/(2*m*s))
}
