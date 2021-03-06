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
cauchy.orig <- function(x) {
  UseMethod("cauchy.orig")
}
#'@rdname cauchy
#'@export
cauchy.inverse <- function(x) {
  UseMethod("cauchy.inverse")
}
#'@rdname cauchy
#'@export
cauchy.orig.cdf <- function(x) {
  return(atan(x) / pi + 0.5)
}
#'@rdname cauchy
#'@export
cauchy.orig.pdf <- function(x) {
  return( 1/ (1 + x^2) / pi)
}
#'@rdname cauchy
#'@export
cauchy.inverse.cdf <- function(x) {
  return(tan((x - 0.5)* pi))
}
#'@rdname cauchy
#'@export
cauchy.inverse.pdf <- function(x) {
  return(1/(cos((x-0.5) * pi) ^ 2))
}
