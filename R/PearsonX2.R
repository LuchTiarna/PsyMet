#'PearsonX2
#'
#'Estimates errors between two sets of data.
#'\cr
#'Formula of deviance:
#'\deqn{D = 2*\sum{ni * yi * log(yi/pi) + ni * (1-yi) * log((1-yi)/(1-pi)}}
#'Where n indicates number of trials in estimatedData, y hit percentage of generatedData, p hit percentage of originalData and i indicates the level.
#'@param originalData dataTable with defined level and hitPercentage
#'@param estimatedData dataTable with defined level and hitPercentage
#'@param rmNaN If true all NaN values are ignored during the computatioin. If true may severly alter result. Implycitly is rmNaN FALSE.
#'@return Error
#'@export
PearsnoX2 <- function(originalData, estimatedData, rmNaN=FALSE){
error <- 0
for(rowA in 1:nrow(originalData)){
  for(rowB in 1:nrow(estimatedData)){
    if(originalData[rowA,1]==estimatedData[rowB,1]){
      currentError <- estimatedData[rowB,2]*(estimatedData[rowB,3] - originalData[rowA,3])^2 / (originalData[rowA,3]*(1-originalData[rowA,3]))

      if(!rmNaN || !is.nan(currentError)){
        error <- error + currentError
      }
    }
  }
}
return(error)
}
