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
    gama = base::as.double(func[3])
    lambda = base::as.double(func[4])

    if(gama < 0) {stop("Gamma must be a least 0.")}
    if(lambda < 0) {stop("Lambda must be a least 0.")}
    if((gama + lambda) > 1) {stop("Summ of gamma and lambda must be lesser than 1.")}

    if(is.character(func[1])){
      sigmoidName <- func[1]
    }else{
      sigmoidName <- base::as.character(substitute(func[1]))
    }

    if(is.character(func[2])){
      coreName <- func[2]
    }else{
      coreName <- base::as.character(substitute(func[2]))
    }

    xc <- base::do.call(paste(coreName , ".orig.cdf", sep=""), c(list(levels), list(func[5:length(func)])))
    hits <- gama + (1 - gama - lambda) * base::do.call(paste(sigmoidName, ".orig.cdf", sep=""), list(xc))

    responses <- data.frame(level = levels, obsNumber = trials, hitPercentage = hits)
    responses <- dplyr::distinct(dplyr::mutate(dplyr::group_by(responses, level), obsNumber=sum(obsNumber), hitPercentage=sum(hitPercentage*obsNumber)/sum(obsNumber)))
    responses <- dplyr::ungroup(responses)
    responses <- responses[order(responses$level),]

    for(i in 1:examplesPerFunction){
      simulatedResp <- PsyMet::simulateResponse(responses)
      simulatedResp <- PsyMet::processResponses(simulatedResp)
      responses$simulatedHitPercentage <- simulatedResp$hitPercentage

      responses$sigmoid <-  base::rep(func[1],base::nrow(responses))
      responses$core <-  base::rep(func[2],base::nrow(responses))
      responses$gamma <- base::rep(base::as.double(func[3]), base::nrow(responses))
      responses$lambda <- base::rep(base::as.double(func[4]), base::nrow(responses))
      for(i in 5:base::length(func))
      {
        responses[[base::paste("param", base::as.character(i-4), sep="")]] <-  base::rep(as.double(func[i]), base::nrow(responses))
      }

      if(base::is.null(results))
      {
        results <- tidyr::nest(responses, c(level, obsNumber, hitPercentage, simulatedHitPercentage))
      }else{
        row <- tidyr::nest(responses, c(level, obsNumber, hitPercentage, simulatedHitPercentage))
        results <- dplyr::bind_rows(results, row)
      }
    }
  }
  return(results)
}
