#' Show Mined Object
#' 
#' Modifies the "plot" function to take objects of class \code{Mined} (or any of its subclasses) and plot the dependent variable against each independent variable
#' 
#' @param Mined An object of class Mined
#' 
#' @author Thomas Carroll: \email{thomasscarroll89@gmail.com}
#' @rdname plot
#' @export
setMethod("plot", 
    signature(object="Mined"),
    function(object){
      plot(x=object@covariates[,1], y=object@depvar)
    }
)
