#'@name multifit
#'@rdname multifit
#'@aliases
#'fit
#'@title MultiFit
#'@description
#'Generates multiple functions with multiple parametres on same data you are able to adjust noise properties.
#'
#'@param origins lists of functions and their parametres
#'@param levels  levels stimuli
#'@param trials
#'
#'@return data.frame with all values with distinguished origin functions and their parametres
NULL
#'@rdname multifit
#'@export
#'
#'

multifit <- function(fitted, fitters)
{
  if(!is.list(fitters)){
    stop("Origins has to be a list.")
  }

  #if(length(levels) != length(trials)){
  #  stop("Every level must have defined number of trials")
  #}

  results <- NULL
  id <- 0

  for(n in 1:nrow(fitted)){
    for(fitter in fitters){
      id <- id + 1

      params <- formals(fitter[[2]])
      params <- params[-1]

      fitData <- fitted$data[[n]]

      if(is.character(fitter[[1]])){
        sigmoidName <- fitter[[1]]
      }else{
        sigmoid <- fitter[[1]]
        sigmoidName <- as.character(substitute(sigmoid))
      }

      if(is.character(fitter[[2]])){
        coreName <- fitter[[2]]
      }else{
        core <- eval(fitter[[2]])
        coreName <- as.character(substitute(core))
      }

      paramNames <- ""
      for(l in 1:length(params)){
        paramNames <- paste(paramNames, paste("param", as.character(l), sep="."), sep=", ")
      }

      fitFormula <- paste("simulatedHitPercentage ~ gama + (1 - gama - lambda) * ", sigmoidName, ".orig.cdf", "(", coreName, ".orig.cdf", "(", "level", paramNames ,")",")", sep="")

      tryCatch({
        fit <- NULL
        fit <- nls( formula = as.formula(fitFormula), data=fitData, lower=c(gama=0, lambda=0),upper=c(gama = 1, lambda = 1), algorithm = "port")
      },error = function(e){fit <- NULL})

      if(is.null(fit)){
        next
      }

      responses <- unnest(fitted[n,], data)
      responses$fitedHitPercentage <- predict(fit, responses$level)

      responses$id <- rep(id, nrow(responses))
      responses$fitSigmoid <-  rep(fitter[1],nrow(responses))
      responses$fitCore <-  rep(fitter[2],nrow(responses))

      params <- fit$m$getPars()

      responses$fitGamma <- rep(params[[1]], nrow(responses))
      responses$fitLambda <- rep(params[[2]], nrow(responses))
      for(i in 3:length(params))
      {
        responses[[paste("fitParam", as.character(i-2), sep="")]] <-  rep(params[[i]], nrow(responses))
      }

      if(is.null(results)){
        results <- nest(responses, c(level, obsNumber, hitPercentage, simulatedHitPercentage, fitedHitPercentage))
      }else{
        row <- nest(responses, c(level, obsNumber, hitPercentage, simulatedHitPercentage, fitedHitPercentage))
        results <-bind_rows(results, row)
      }
      }
  }
  return(results)
}
