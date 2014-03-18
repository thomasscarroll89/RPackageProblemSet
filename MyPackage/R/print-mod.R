#' Print DataM Object
#' 
#' Modifies the "print" function to take objects of class \code{DataM} (or any of its subclasses) and print out a matrix where the first column is the dependent variable and the remaining columns are the independent variables. 
#' 
#' @param DataM An object of class DataM
#' 
#' @author Thomas Carroll: \email{thomasscarroll89@gmail.com}
#' @rdname print
#' @export
setMethod("print", 
          signature(x="DataM"),
          function(x, ...){
            print(cbind(x@depvar, x@covariates))
          }
)
getMethod("print", signature="DataM")