\name{MyPackage-package}
\alias{MyPackage-package}
\alias{MyPackage}
\docType{package}
\title{
Data Mining
}
\description{
Perform data mining on a user-supplied dataset by running a basic OLS regression on every possible combination of the available independent variables. Then return the coefficients and R^2 values for each of these regressions in a matrix form. There is also a function that returns the p-values for each regression, as well as the average p-values for each covariate for all of the regressions that it was included in. 
}
\details{
\tabular{ll}{
Package: \tab MyPackage\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2014-03-17\cr
License: \tab GPL (>= 2)\cr
}
Begin by creating a DataM or DataM2 object using the new() function. Then use the funcation dataMining() or importantVariables() to create the matrix of coefficients or p-values, depending on which you want. 
}
\author{
Thomas Carroll <thomasscarroll89@gmail.com>
Maintainer: Thomas Carroll <thomasscarroll89@gmail.com>
}
\examples{
X1 <- rnorm(100, 50, 20)
X2 <- runif(100)
X3 <- rnorm(100, 0, 10)
Y <- 10 + 3*X1 - 5*X2 + X3 + rnorm(100, 10, 10)
Temp <- new("DataM", depvar=Y, covariates=cbind(X1, X2, X3))
dataMining(Temp)
importantVariables(Temp)
}
