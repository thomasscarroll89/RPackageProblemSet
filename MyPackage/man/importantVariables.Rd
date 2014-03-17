\name{importantVariables}
\alias{importantVariables}
\title{Determining the Importance of Variables}
\usage{
importantVariables(object = "DataM")
}
\arguments{
  \item{DataM}{An object of class DataM, which must first
  be created using the "new" function.}
}
\value{
A matrix of p-values for each of the regressions run by the
function, and also a vector containing the "average"
p-value for each of the predictor variables.
}
\description{
Takes objects of class \code{DataM} (or any of its
subclasses) and runs an OLS regression for every possible
combination of variables, and returns the p-values for each
of these regressions as well as the "average" p-value that
each variable was run in.
}
\author{
Thomas Carroll: \email{thomasscarroll89
}

