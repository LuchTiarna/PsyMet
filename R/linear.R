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
linear.cdf <- function(x, a, b) {
  return(a * x + b)
}
#'@rdname linear
#'@export
linear.pdf <- function(x, a, b) {
  return(a)
}
