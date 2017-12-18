#'@name gumbel_r
#'@rdname gumbel_r
#'@aliases
#'gumbel_r
#'gumbel_r.cdf
#'gumbel_r.pdf
#'
#'@title Gumbel_r
#'@description
#'Right gumbel function is a sigmoid type function.
#'\cr
#'It's CDF formula is: exp(- exp(-x))
#'\cr
#'It's PDF formula is: y = exp(- exp(-x) - x)
#'
#'@param x Vector of x parametres
#'@return Vector of result vaues
NULL

#'@rdname gumbel_r
#'@export
gumbel_r <- function(x) {
  UseMethod("gumbel_r")
}
#'@rdname gumbel_r
#'@export
gumbel_r.cdf <- function(x) {
  return(exp(- exp(-x)))
}
#'
#'@rdname gumbel_r
#'@export
gumbel_r.pdf <- function(x) {
  return(exp(- exp(-x) - x))
}
