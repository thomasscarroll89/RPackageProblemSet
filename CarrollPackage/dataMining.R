#' Data Mining
#' 
#' Runs regressions for every possible combination of variables.
#'
#' @param depvar A vector containing the "dependent variable" data
#' @param covariates A matrix or dataframe containing the "independent variable" data, where each column corresponds to a different variable
#'
#' @return A matrix where each column corresponds to a different model, and each row contains the coefficients for each independent variable
#'
#' @author Thomas Carroll
#' @note The function is not yet designed to take factor-class independent variables. All variables should be in numeric form. 
#' @examples
#' 
#' X1 <- rnorm(1000, mean=50, sd=10)
#' X2 <- rnorm(1000, mean=0, sd=5)
#' X3 <- rnorm(1000, mean=5, sd=3)
#' Y <- 17 + 4*X1 + (-12)*X2 + 10*X3 + rnorm(1000, mean=0, sd=10)
#' dataMining(depvar=Y, covariates=cbind(X1, X2, X3))

dataMining <- function(depvar, covariates){
  k <- ncol(covariates) #Let k be the number of predictor variables (not including intercept)
  if(class(covariates)=="matrix"){ 
    #Create column names for a matrix object  
    colnames(covariates) <- paste(rep("X", length=k), 1:k, sep="")
  }
  model.combinations <- vector(mode="list") #Create object "model.combinations", which is a list containing all of the different combinations of variables 
  number.of.models <- c(1) #do 1, for the model with just an intercept
  for(i in 1:k){
    model.combinations[[i]] <- combn(c(1:k), m=i)
    number.of.models <- append(number.of.models, values=ncol(model.combinations[[i]]), after=length(number.of.models))
  }
  #CREATE basic structure of the output matrix
  output <- matrix(NA, nrow=k+2, ncol=sum(number.of.models))
  rownames(output) <- c("Intercept", colnames(covariates), "R^2")
  colnames(output) <- paste(rep("Model", length=sum(number.of.models)), 1:sum(number.of.models), sep=" ")
  #BEGIN running linear models; start with model using only an intercept
  model.base <- lm(depvar ~ 1)
  model.base.coef <- model.base$coef
  output[1,1] <- model.base.coef
  r.squared <- summary(model.base)$r.squared  
  count <- 1
  for(i in 1:length(model.combinations)){
    for(j in 1:ncol(model.combinations[[i]])){
      count <- count + 1
      variable.numbers <- model.combinations[[i]][,j]
      model <- lm(depvar ~ covariates[,c(variable.numbers)])
      coefficients <- model$coef
      output[c(1, variable.numbers + 1),count] <- coefficients
      r.squared <- append(r.squared, values=summary(model)$r.squared, after=length(r.squared))
    }
  }
  output[k+2,] <- r.squared  
  output <- as.data.frame(output)
  output <- round(output, 4)
  output[is.na(output)] <- ""
  return(output)
} 