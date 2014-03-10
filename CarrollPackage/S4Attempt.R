setClass(Class="Mined",
   representation = representation(
     depvar = "numeric",
     covariates = "matrix",
     output = "data.frame"
     ),    
   prototype = prototype(
     depvar = c(),
     covariates = matrix(),
     output = data.frame()
     )
)

setValidity("Mined", function(object){
  observations <- length(object@depvar)
  if(length(object@depvar)!=nrow(object@covariates)){
    return("Number of observations in dependent variable and independent variables are not equal")
  }
})

setMethod("initialize", "Mined", function(.Object, ...){
 value = callNextMethod()
 validObject(value)
 return(value)
})

new("Mined", depvar=Y, covariates=cbind(X1, X2, X3), output=dataMining(depvar=Y, covariates=cbind(X1, X2, X3)))
new("Mined", depvar="hello", covariates=cbind(X1, X2, X3), output=dataMining(depvar=Y, covariates=cbind(X1, X2, X3)))

setGeneric("dataMining", function(object="Mined"){
  
})