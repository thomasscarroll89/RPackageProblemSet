#Create a new object of class DataM; this object has one dependent variable vector and 3 independent variables
X1 <- rnorm(100, 50, 20)
X2 <- runif(100)
X3 <- rnorm(100, 0, 10)
Y <- 10 + 3*X1 - 5*X2 + X3 + rnorm(100, 10, 10)
Temp <- new("DataM", depvar=Y, covariates=cbind(X1, X2, X3))

#Demonstrate the print, show, and plot functions
print(Temp)
show(Temp)
plot(Temp)

#Demonstrate the dataMining and importantVariables function
dataMining(Temp)
importantVariables(Temp)

#Use getMethod to see what the function is for plot, show, and print
getMethod("plot", signature=c("DataM", "missing"))
getMethod("show", signature="DataM")
getMethod("print", signature="DataM")

#The setMethod examples can be seen in the show, print, and plot R files under the R folder