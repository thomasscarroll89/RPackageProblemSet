#' Matrix of coefficient estimates and R^2 values from data mining; also contains a parameter for the variable names
#' 
#' Objects of class \code{Mined2} are created by the \code{dataMining} function. 
#' 
#' Objects of class \code{Mined2} have the following slots:
#' \itemize{
#'  \item \code{depvar} A (numeric) vector for length n containing the dependent variable data 
#'  \item \code{covariates} A (numeric) matrix with n rows and any number of columns; contains the independent variable data, with each column corresponding to a different independent variable
#'  \item \code{variable.names} A (character) vector containing the variable names for the k independent varables in the covariates matrix
#' }
#' 
#' @author Thomas Carroll: \email{thomasscarroll89@gmail.com}
#' @rdname Mined2
#' @export
setClass(Class="Mined2",
         representation = representation(
           depvar = "numeric", 
           covariates = "matrix",
           variable.names = "character"
         ),    
         prototype = prototype(
           depvar = c(), 
           covariates = matrix(),
           variable.names = c()
         )
)

#' @export
setValidity("Mined2", function(object){
  observations <- length(object@depvar)
  if(length(object@depvar)!=nrow(object@covariates)){
    return("Number of observations in dependent variable and independent variables are not equal")
  }
  if(length(object@variable.names)!=ncol(object@covariates)){
    return("Number of variable names does not match number of independent variables")
  }
})

#' @export
setMethod("initialize", "Mined2", function(.Object, ...){
  value = callNextMethod()
  validObject(value)
  return(value)
})