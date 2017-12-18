#'@name cauchy
#'@rdname cauchy
#'@aliases
#'cauchy
#'cauchy.cdf
#'cauchy.pdf
#'
#'@title Cauchy
#'@description
#'Cauchy function is a sigmoid type function.
#'\cr
#'It's CDF formula is: y = atan(x) / pi + 0.5
#'\cr
#'It's PDF formula is: y = 1/ (1 + x^2) / pi
#'@param x Vector of x parametres
#'
#'@return Vector of result vaues
NULL
#'@rdname cauchy
#'@export
cauchy <- function(x) {
  UseMethod("cauchy")
}
#'@rdname cauchy
#'@export
cauchy.cdf <- function(x) {
  return(atan(x) / pi + 0.5)
}
#'
#'@rdname cauchy
#'@export
cauchy.pdf <- function(x) {
  return( 1/ (1 + x^2) / pi)
}



