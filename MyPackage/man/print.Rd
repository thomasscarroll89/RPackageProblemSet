\docType{methods}
\name{print,DataM-method}
\alias{print,DataM-method}
\title{Print DataM Object}
\usage{
\S4method{print}{DataM}(x, ...)
}
\arguments{
  \item{DataM}{An object of class DataM}

  \item{x}{an object used to select a method.}

  \item{...}{further arguments passed to or from other
  methods.}
}
\description{
Modifies the "print" function to take objects of class
\code{DataM} (or any of its subclasses) and print out a
matrix where the first column is the dependent variable and
the remaining columns are the independent variables.
}
\author{
Thomas Carroll: \email{thomasscarroll89
}

