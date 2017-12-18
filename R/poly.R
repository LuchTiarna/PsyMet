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
polynom.cdf <- function(x, a, b) {
  return((x / a) ^ b)
}
#'@rdname polynom
#'@export
polynom.pdf <- function(x, a, b) {
  return((x / a) ^ (b- 1) * b / a)
}
