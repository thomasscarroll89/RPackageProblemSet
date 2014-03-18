#' A subclass of DataM which includes a slot containing the names for the independent variables. 
#' 
#' Objects of class \code{DataM2} contain three slots. The first two are identical to the DataM class object (i.e. a vector and matrix of dependent and independent variable values, respectively). However it also contains a slot for a character vector containing the variable names. 
#' 
#' An object of class \code{DataM2} has the following slots:
#' \itemize{
#'  \item \code{depvar} A (numeric) vector for length n containing the dependent variable data 
#'  \item \code{covariates} A (numeric) matrix with n rows and any number of columns; contains the independent variable data, with each column corresponding to a different independent variable
#'  \item \code{variable.names} A (character) vector containing the variable names for the k independent varables in the covariates matrix
#' }
#' 
#' @author Thomas Carroll: \email{thomasscarroll89@gmail.com}
#' @rdname DataM2-class
#' @export
setClass(Class="DataM2",
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
setValidity("DataM2", function(object){
  observations <- length(object@depvar)
  if(length(object@depvar)!=nrow(object@covariates)){
    return("Number of observations in dependent variable and independent variables are not equal")
  }
  if(length(object@variable.names)!=ncol(object@covariates)){
    return("Number of variable names does not match number of independent variables")
  }
})

#' @export
setMethod("initialize", "DataM2", function(.Object, ...){
  value = callNextMethod()
  validObject(value)
  return(value)
})