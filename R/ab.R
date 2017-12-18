#'@name ab
#'@rdname ab
#'@aliases
#'ab
#'ab.cdf
#'ab.pdf
#'
#'@title ab
#'@description
#'AB function is a core type function.
#'\cr
#'It's CDF formula is: y = (x - a) / b
#'\cr
#'It's PDF formula is: y = 1/b
#'
#'@param x Vector of x parametres
#'@param a linear parameter
#'@param b nominater parameter
#'
#'@return Vector of result vaues
NULL
#'@rdname ab
#'@export
ab <- function(x, a, b) {
  UseMethod("ab")
}
#'@rdname ab
#'@export
ab.cdf <- function(x, a=NULL, b=NULL) {
  if(is.null(b) && length(a) > 1){
    params <- a
    a=params[1]
    b=params[2]
  }
  return((x - a) / b)
}
#'@rdname ab
#'@export
ab.pdf <- function(x, a=NULL, b=NULL){
  if(is.null(b) && length(a) > 1){
    params <- a
    a=params[1]
    b=params[2]
  }
  return (1 / b)
}
