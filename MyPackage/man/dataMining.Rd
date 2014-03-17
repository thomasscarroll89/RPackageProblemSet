\name{dataMining}
\alias{dataMining}
\title{Data Mining}
\usage{
dataMining(object = "DataM")
}
\arguments{
  \item{DataM}{An object of class DataM, which must first
  be created using the "new" function.}
}
\value{
A matrix of coefficient estimates for each model that was
run, along with the R^2 value for each of these models.
}
\description{
Takes objects of class \code{DataM} (or any of its
subclasses) and runs an OLS regression for every possible
combination of variables, and returns a matrix of
coefficient estimates for each model, as well as the R^2
value for each model.
}
\author{
Thomas Carroll: \email{thomasscarroll89
}

