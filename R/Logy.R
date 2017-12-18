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
logy.cdf <- function(x, a, b) {
  return(a * log(x) + b)
}
#'@rdname logy
#'@export
logy.pdf <- function(x, a, b) {
  return(a / x)
}
