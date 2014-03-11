#' Matrix of coefficient estimates and R^2 values from data mining
#' 
#' Objects of class \code{Mined} are created by the \code{dataMining} function. 
#' 
#' Objets of class \code{Mined} have the following slots:
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
setMethod("initialize", "Mined", function(.Object, ...){
  value = callNextMethod()
  validObject(value)
  return(value)
})

#' @export
setGeneric("dataMining", function(object="Mined"){
  standardGeneric("dataMining")
})

#' @export
setMethod("dataMining", "Mined", function(object="Mined"){
  k <- ncol(object@covariates) #Let k be the number of predictor variables (not including intercept)
  colnames(object@covariates) <- paste(rep("X", length=k), 1:k, sep="")
  model.combinations <- vector(mode="list") #Create object "model.combinations", which is a list containing all of the different combinations of variables 
  number.of.models <- c(1) #do 1, for the model with just an intercept
  for(i in 1:k){
    model.combinations[[i]] <- combn(c(1:k), m=i)
    number.of.models <- append(number.of.models, values=ncol(model.combinations[[i]]), after=length(number.of.models))
  }
  #CREATE basic structure of the output matrix
  output2 <- matrix(NA, nrow=k+2, ncol=sum(number.of.models))
  rownames(output2) <- c("Intercept", colnames(object@covariates), "R^2")
  colnames(output2) <- paste(rep("Model", length=sum(number.of.models)), 1:sum(number.of.models), sep=" ")
  #BEGIN running linear models; start with model using only an intercept
  model.base <- lm(object@depvar ~ 1)
  model.base.coef <- model.base$coef
  output2[1,1] <- model.base.coef
  r.squared <- summary(model.base)$r.squared  
  count <- 1
  for(i in 1:length(model.combinations)){
    for(j in 1:ncol(model.combinations[[i]])){
      count <- count + 1
      variable.numbers <- model.combinations[[i]][,j]
      model <- lm(object@depvar ~ object@covariates[,c(variable.numbers)])
      coefficients <- model$coef
      output2[c(1, variable.numbers + 1),count] <- coefficients
      r.squared <- append(r.squared, values=summary(model)$r.squared, after=length(r.squared))
    }
  }
  output2[k+2,] <- r.squared  
  output <- as.data.frame(output2)
  output <- round(output, 4)
  output[is.na(output)] <- ""
  return(output)
})