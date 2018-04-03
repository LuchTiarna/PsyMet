#' simulateResponse
#'
#' Creates new table with responses of fictional subject, who's responses respect distribution of given function.
#'
#'@param dataTable Table with level of stimulation as a first column, number of observations as the second column and hit percentage as a third column.
#'@return Table of observations where first column is level of stimulations and second indicates whether it was hit.
#'@export

simulateResponse <- function(dataTable){
  dataTable <- tibble::as_tibble(dataTable)
  responseTable <- tibble::tibble(level = as.double(), hit = as.integer())

  for(rown in 1:nrow(dataTable)){
    if(dataTable[[rown,2]] >= 1){
      responseTable <- tibble::add_row(responseTable,
                               level=dataTable[[rown,1]], hit=rbinom(dataTable[[rown,2]], 1,dataTable[[rown,3]]))
    }
  }
  return(as.data.frame(responseTable))
}
