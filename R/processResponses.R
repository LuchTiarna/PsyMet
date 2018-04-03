#'processResponses
#'
#'Creates new table having with level, number of observations and hitpercentage according to given table of observations contatining column leven and hit.
#' @param responseTable Table of responses. Every row represents one response (observation) and contains columns level(level of stimulation) and hit(indicates whether response was positive(0=negative, 1=positive).)
#' @return Returns table with row for every level specifing number of observations and hit percentage in every level.
#' @export
processResponses <- function(responseTable){
  responseTable <- tibble::as_tibble(responseTable)
  generatedTable <- dplyr::summarize( dplyr::group_by(responseTable, level), obsNumber=n(), hitPercentage=mean(hit))
  return(base::as.data.frame(generatedTable))
}
