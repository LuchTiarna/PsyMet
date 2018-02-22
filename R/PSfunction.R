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
PSfunction <- function(gamma, lambda, sigmoid, core, x, ... , type="cdf", inverse=FALSE){
  gamma <-  as.double(gamma)
  lambda <- as.double(lambda)

  if(gamma < 0) {stop("Gamma must be a least 0.")}
  if(lambda < 0) {stop("Lambda must be a least 0.")}
  if((gamma + lambda) > 1) {stop("Summ of gamma and lambda must be lesser than 1.")}

  if(is.character(sigmoid)){
    sigmoidName <- sigmoid
  }else{
    sigmoidName <- as.character(substitute(sigmoid))
  }

  if(is.character(core)){
    coreName <- core
  }else{
    coreName <- as.character(substitute(core))
  }

  if(!inverse){
    sigmoidName <- paste(sigmoidName, "orig",sep=".")
    coreName <- paste(coreName, "orig",sep=".")
  }else{
    sigmoidName <- paste(sigmoidName, "inverse",sep=".")
    coreName <- paste(coreName, "inverse",sep=".")
  }

  if(tolower(type)=="cdf"){
    coref <- function(){eval(body(paste(coreName , ".cdf", sep="")))}
    formals(coref) <- formals(coreName)

    sigmoidf <- function(){eval(body(paste(sigmoidName, ".cdf", sep="")))}
    formals(sigmoidf) <- formals(sigmoidName)

    if(!inverse){
      y <- gamma + (1 - gamma - lambda) * sigmoidf(coref(x, ...))
    }else{
      y <- coref(sigmoidf((x-gamma)/(1-lambda-gamma)), ...)
    }

  }else if(tolower(type)=="pdf"){
    coref <- function(){eval(body(paste(coreName, ".cdf", sep="")))}
    formals(coref) <- formals(coreName)

    corePdf <- function(){eval(body(paste(coreName, ".pdf", sep="")))}
    formals(corePdf) <- formals(coreName)

    sigmoidf <- function(){eval(body(paste(sigmoidName, ".cdf", sep="")))}
    formals(sigmoidf) <- formals(sigmoidName)

    sigmoidPdf <- function(){eval(body(paste(sigmoidName, ".pdf", sep="")))}
    formals(sigmoidPdf) <- formals(sigmoidName)

    if(!inverse){
      y <- gamma + (1 - gamma - lambda) * sigmoidPdf(coref(x, ...))*corePdf(x, ...)
    }else{
      adjustedX <- x-gamma/(1-lambda-gamma)
      y <- corePdf(sigmoidf(adjustedX), ...)*sigmoidPdf(adjustedX)
      y <- y/(1-lambda-gamma)
    }

  } else {
    stop("invalid function type, must be either cdf or pdf")
  }
    return(y)
}
