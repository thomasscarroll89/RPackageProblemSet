\name{Mined-class}
\Rdversion{1.1}
\docType{class}
\alias{Mined-class}
\alias{dataMining,Mined-method}
\alias{importantVariables,Mined-method}
\alias{initialize,Mined-method}

\title{Class \code{"Mined"}}
\description{
The Mined class is the superclass to class Mined2, and contains only two parameters: a vector of dependent variable data and a matrix of independent variable data}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("Mined", ...)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{depvar}:}{Object of class \code{"numeric"}; a vector of length n containing dependent variable data}
    \item{\code{covariates}:}{Object of class \code{"matrix"}; dimensions are n by k and contains independent variable data, where each column corresponds to a different independent varibale}
  }
}
\section{Methods}{
  \describe{
    \item{dataMining}{\code{signature(object = "Mined")}: ... }
    \item{importantVariables}{\code{signature(object = "Mined")}: ... }
    \item{initialize}{\code{signature(.Object = "Mined")}: ... }
	 }
}
\references{
%%  ~~put references to the literature/web site here~~
}
\author{
Thomas Carroll
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\linkS4class{Mined2}}
%%  ~~objects to See Also as \code{\link{~~fun~~}}, ~~~
%%  ~~or \code{\linkS4class{CLASSNAME}} for links to other classes ~~~
}
\examples{
showClass("Mined")
}
\keyword{classes}
