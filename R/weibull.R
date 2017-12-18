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
weibull.cdf <- function(x, m, s) {
  return(2*m*s*(log(x) - log(m)) / log(2) + log(log(2)))
}
#'@rdname weibull
#'@export
weibull.pdf <- function(x, m, s) {
  return(2*m*s/log(2)/x)
}
