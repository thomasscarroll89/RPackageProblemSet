#' Determining the Importance of Variables
#' 
#' Takes objects of class \code{DataM} (or any of its subclasses) and runs an OLS regression for every possible combination of variables, and returns the p-values  for each of these regressions as well as the "average" p-value that each variable was run in.
#' 
#' @param DataM An object of class DataM, which must first be created using the "new" function. 
#' 
#' @return A matrix of p-values for each of the regressions run by the function, and also a vector containing the "average" p-value for each of the predictor variables.  
#' @author Thomas Carroll: \email{thomasscarroll89@gmail.com}
#' @rdname importantVariables
#' @export
setGeneric("importantVariables", function(object="DataM"){
  standardGeneric("importantVariables")
})

setMethod("importantVariables", "DataM", function(object="DataM"){
  k <- ncol(object@covariates) #Let k be the number of predictor variables (not including intercept)
  colnames(object@covariates) <- paste(rep("X", length=k), 1:k, sep="")
  model.combinations <- vector(mode="list") #Create object "model.combinations", which is a list containing all of the different combinations of variables 
  number.of.models <- c(1) #do 1, for the model with just an intercept
  for(i in 1:k){
    model.combinations[[i]] <- combn(c(1:k), m=i)
    number.of.models <- append(number.of.models, values=ncol(model.combinations[[i]]), after=length(number.of.models))
  }
  #CREATE basic structure of the output matrix
  output2 <- matrix(NA, nrow=k+1, ncol=sum(number.of.models))
  rownames(output2) <- c("Intercept", colnames(object@covariates))
  colnames(output2) <- paste(rep("Model", length=sum(number.of.models)), 1:sum(number.of.models), sep=" ")
  #BEGIN running linear models; start with model using only an intercept
  model.base <- lm(object@depvar ~ 1)
  model.base.pvalues <- summary(model.base)$coef[,4]
  output2[1,1] <- model.base.pvalues
  count <- 1
  for(i in 1:length(model.combinations)){
    for(j in 1:ncol(model.combinations[[i]])){
      count <- count + 1
      variable.numbers <- model.combinations[[i]][,j]
      model <- lm(object@depvar ~ object@covariates[,c(variable.numbers)])
      pvalues <- summary(model)$coef[,4]
      output2[c(1, variable.numbers + 1),count] <- pvalues
    }
  }
  output <- vector(mode="list")
  output[[1]] <- round(apply(output2, 1, mean, na.rm=TRUE), 6)
  output[[2]] <- as.data.frame(output2)
  output[[2]] <- round(output[[2]], 6)
  output[[2]][is.na(output[[2]])] <- ""
  return(output)
})