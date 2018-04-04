#'@name multifit
#'@rdname multifit
#'@aliases
#'fit
#'@title MultiFit
#'@description
#'Generates multiple functions with multiple parametres on same data you are able to adjust noise properties.
#'
#'@param levels stimuli levels
#'@param hitPercentage percentage of hits on a level
#'@param trials  number of trials per level
#'@param fitters list of functions to fit the data
#'
#'@return data.frame with all values with distinguished origin functions and their parametres
NULL
#'@rdname multifit
#'@export
#'
#'



multifit <- function(levels, hitPercentage, obsNumber, fitters)
{
  results <- NULL

  for(fitter in fitters){

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

    fitfunction <- function(params){
      x <- PSfunction(params[1], params[2], sigmoidName, coreName, levels, params[3], params[4])
      LE <- likelihood_log(hitPercentage,  x, obsNumber, rm.nan = FALSE)

      return(-LE)
    }



    fit <- NULL
    tryCatch({
      fit <- optim(c(0.05,0.05,1,1),fn=fitfunction)
    },error = function(e){warning(e);fit <- NULL})

    if(is.null(fit)){
      next
    }



    responses <- data.frame(level = levels,obsNumber=obsNumber, hitPercentage=hitPercentage)
    responses <- responses[complete.cases(responses),]
    params <- fit$par
    responses$fitedHitPercentage <- PSfunction(params[1], params[2], sigmoidName, coreName, responses$level, params[3], params[4])



    responses$sigmoid_fit <-  rep(fitter[1],nrow(responses))
    responses$core_fit <-  rep(fitter[2],nrow(responses))
    responses$gamma_fit <- rep(params[1], nrow(responses))
    responses$lambda_fit <- rep(params[2], nrow(responses))

    for(l in 3:length(params))
    {
      responses[[paste("param", as.character(l-2), "_fit", sep="")]] <-  rep(params[l], nrow(responses))
    }

    if(is.null(results)){
      results <- nest(responses, c(level, obsNumber, hitPercentage, fitedHitPercentage))
    }else{
      row <- nest(responses, c(level, obsNumber, hitPercentage, fitedHitPercentage))
      results <-bind_rows(results, row)
    }
  }
  return(results)
}


