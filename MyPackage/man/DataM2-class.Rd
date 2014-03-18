\docType{class}
\name{DataM2-class}
\alias{DataM2-class}
\title{A subclass of DataM which includes a slot containing the names for the independent variables.}
\description{
Objects of class \code{DataM2} contain three slots. The
first two are identical to the DataM class object (i.e. a
vector and matrix of dependent and independent variable
values, respectively). However it also contains a slot for
a character vector containing the variable names.
}
\details{
An object of class \code{DataM2} has the following slots:
\itemize{ \item \code{depvar} A (numeric) vector for length
n containing the dependent variable data \item
\code{covariates} A (numeric) matrix with n rows and any
number of columns; contains the independent variable data,
with each column corresponding to a different independent
variable \item \code{variable.names} A (character) vector
containing the variable names for the k independent
varables in the covariates matrix }
}
\author{
Thomas Carroll: \email{thomasscarroll89
}

