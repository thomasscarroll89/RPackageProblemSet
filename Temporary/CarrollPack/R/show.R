#' Show Mined Object
#' 
#' Modifies the "show" function to take objects of class \code{Mined} (or any of its subclasses) and summarizes each of the (in)dependent variables in the class
#' 
#' @param Mined An object of class Mined
#' 
#' @author Thomas Carroll: \email{thomasscarroll89@gmail.com}
#' @rdname show
#' @export
setMethod("show", 
        signature(object="Mined"),
        function(object){
          summary(cbind(object@depvar, object@covariates))
        }
)
