#'@name multigenerate
#'@rdname multigenerate
#'@aliases
#'generate
#'@title MultiGenerate
#'@description
#'Generates multiple functions with multiple parametres on same data you are able to adjust noise properties.
#'
#'@param origins lists of functions and their parametres
#'@param levels  levels os stimulus
#'@param trials  numbers of trials on each level of stimulus
#'@param examplesPerFunction number of simulated sessions per one origin function, implicit value is 1
#'
#'@return data.frame with all values with distinguished origin functions and their parametres
NULL
#'@rdname multigenerate
#'@export
#'
#'
multigenerate <- function(origins, levels, trials, examplesPerFunction=1)
{
  if(!is.list(origins)){
    stop("Origins has to be a list.")
  }

  if(length(levels) != length(trials)){
    stop("Every level must have defined number of trials")
  }

  results <- NULL

  for(func in origins){
    gama = as.integer(func[3])
    lambda = as.integer(func[4])

    if(gama < 0) {stop("Gamma must be a least 0.")}
    if(lambda < 0) {stop("Lambda must be a least 0.")}
    if((gama + lambda) > 1) {stop("Summ of gamma and lambda must be lesser than 1.")}

    if(is.character(func[1])){
      sigmoidName <- func[1]
    }else{
      sigmoidName <- as.character(substitute(func[1]))
    }

    if(is.character(func[2])){
      coreName <- func[2]
    }else{
      coreName <- as.character(substitute(func[2]))
    }

    xc <- do.call(paste(coreName , ".orig.cdf", sep=""), c(list(levels), list(func[5:length(func)])))
    hits <- gama + (1 - gama - lambda) * do.call(paste(sigmoidName, ".orig.cdf", sep=""), list(xc))

    responses <- data.frame(level = levels, obsNumber = trials, hitPercentage = hits)

    for(i in 1:examplesPerFunction){
      simulatedResp <- simulateResponse(responses)
      simulatedResp <- processResponses(simulatedResp)
      responses$simulatedHitPercentage <- simulatedResp$hitPercentage

      responses$sigmoid <-  rep(func[1],nrow(responses))
      responses$core <-  rep(func[2],nrow(responses))
      responses$gamma <- rep(as.double(func[3]), nrow(responses))
      responses$lambda <- rep(as.double(func[4]), nrow(responses))
      for(i in 5:length(func))
      {
        responses[[paste("param", as.character(i-4), sep="")]] <-  rep(as.double(func[i]), nrow(responses))
      }

      if(is.null(results))
      {
        results <- nest(responses, c(level, obsNumber, hitPercentage, simulatedHitPercentage))
      }else{
        row <- nest(responses, c(level, obsNumber, hitPercentage, simulatedHitPercentage))
        results <- bind_rows(results, row)
      }
    }
  }
  return(results)
}
