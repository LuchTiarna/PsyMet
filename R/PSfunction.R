#'PSfunction
#'
#'Provides the frame for psycometrics functions. Combines the sigmoid and core function.
#'@param type specifies, whether function is CDF of PDF type
#'@param gama sets the loves boundary of function
#'@param lambda sets the highes boundary of function
#'@param sigmoid determines the sigmoid of the fuction
#'@param core dermines the core of the function
#'@param x the vector of level values
#'@param ... specifies the parametres or core function
#'@return vector of return values
#'@export
PSfunction <- function(gamma, lambda, sigmoid, core, x, ..., type="cdf"){

  if(gamma <0 || lambda < 0) {stop("Gamma and lambda must be a least 0")}
  if(type=="cdf" || type == "CDF"){
    class(x) <- "cdf"

    xp <-  do.call(core, c(list(x), list(...)))

    y <- (1 - gamma - lambda) * do.call(sigmoid, list(x))
    class(y) <- NULL
    return(y)
  }
  else if(type=="pdf" || type == "PDF"){
    class(x) <- "cdf"
    xs <- do.call(core, list(list(x), list(...)))
    class(xs) <- "pdf"

    class(x) <- "pdf"
    xp <- do.call(core, list(list(x), list(...)))

    y <- (1 - gamma - lambda) * do.call(sigmoid, list(xs)) * xp
    class(y) <- NULL

    return(y)
  } else {
    stop("invalid function type, must be rather cdf of pdf")
  }
}
