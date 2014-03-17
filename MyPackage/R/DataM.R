#' Data-like object to be used by the dataMining() function
#' 
#' Objects of class \code{DataM} contain 2 slots, the first of which contains the values for the dependent variable and the second of which contains a matrix of values for the predictor variables.
#'
#' An object of the class `DataM' has the following slots:
#' \itemize{
#' \item \code{depvar} The added or subtracted squared values
#' \item \code{covariates} The added or subtracted squared values
#' }
#'
#' @author Thomas Carroll: \email{thomasscarroll89@gmail.com}
#' @rdname DataM-class
#' @export
setClass(Class="DataM",
         representation = representation(
           depvar = "numeric", 
           covariates = "matrix"
         ),    
         prototype = prototype(
           depvar = c(), 
           covariates = matrix()
         )
)

#' @export
setValidity("DataM", function(object){
  observations <- length(object@depvar)
  if(length(object@depvar)!=nrow(object@covariates)){
    return("Number of observations in dependent variable and independent variables are not equal")
  }
})

#' @export
setMethod("initialize", "DataM", function(.Object, ...){
  value = callNextMethod()
  validObject(value)
  return(value)
})