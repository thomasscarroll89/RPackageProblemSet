#' Matrix of coefficient estimates and R^2 values from data mining
#' 
#' Objects of class \code{Mined} are created by the \code{dataMining} function. 
#' 
#' Objects of class \code{Mined} have the following slots:
#' \itemize{
#'  \item \code{depvar} A (numeric) vector for length n containing the dependent variable data 
#'  \item \code{covariates} A (numeric) matrix with n rows and any number of columns; contains the independent variable data, with each column corresponding to a different independent variable
#' }
#' 
#' @author Thomas Carroll: \email{thomasscarroll89@gmail.com}
#' @rdname Mined
#' @export
setClass(Class="Mined",
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
setValidity("Mined", function(object){
  observations <- length(object@depvar)
  if(length(object@depvar)!=nrow(object@covariates)){
    return("Number of observations in dependent variable and independent variables are not equal")
  }
})

#' @export 
setMethod("initialize", "Mined", function(.Object, ...){
  value = callNextMethod()
  validObject(value)
  return(value)
})