#'@name gumbel_l
#'@rdname gumbel_l
#'@aliases
#'gumbel_l
#'gumbel_l.cdf
#'gumbel_l.pdf
#'
#'@title Gumbel_l
#'@description
#'Left gumbel function is a sigmoid type function.
#'\cr
#'It's CDF formula is: y = 1 - exp(- exp(x))
#'\cr
#'It's PDF formula is: y = exp(- exp(x) + x)
#'
#'@param x Vector of x parametres
#'
#'@return Vector of result vaues
NULL

#'@rdname gumbel_l
#'@export
gumbel_l <- function(x) {
  UseMethod("gumbel_l")
}
#'@rdname gumbel_l
#'@export
gumbel_l.cdf <- function(x) {
  return(1 - exp(- exp(x)))
}
#'
#'@rdname gumbel_l
#'@export
gumbel_l.pdf <- function(x) {
  return(exp(- exp(x) + x))
}




