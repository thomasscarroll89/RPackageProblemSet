#' Show DataM Object
#' 
#' Modifies the "show" function to take objects of class \code{DataM} (or any of its subclasses) and summarizes each of the (in)dependent variables in the class
#' 
#' @param DataM An object of class DataM, or any of its subclasses
#' 
#' @author Thomas Carroll: \email{thomasscarroll89@gmail.com}
#' @rdname show
#' @export
setMethod("show", 
          signature(object="DataM"),
          function(object){
            summary(cbind(object@depvar, object@covariates))
          }
) #end setMethod() function
getMethod("show", signature="DataM")