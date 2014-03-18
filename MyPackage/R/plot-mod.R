#' Plot DataM Object
#' 
#' Modifies the "plot" function to take objects of class \code{DataM} (or any of its subclasses) and plot the dependent variable against each independent variable in a separate graph. It records these graphs in a separate plotting window. 
#' 
#' @param DataM An object of class DataM
#' 
#' @author Thomas Carroll: \email{thomasscarroll89@gmail.com}
#' @rdname plot
#' @export
setMethod("plot", 
          signature(x="DataM", y="missing"), #IMPORTANT NOTE: the arguments in signature must match the arguments in the original function
          function(x, y, ...){
            windows(record=TRUE)
            for(i in 1:ncol(x@covariates)){
              plot(x@covariates[,i], x@depvar) 
            }
          }
)
getMethod("plot", signature=c("DataM", "missing"))