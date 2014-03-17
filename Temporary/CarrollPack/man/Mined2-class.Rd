\name{Mined2-class}
\Rdversion{1.1}
\docType{class}
\alias{Mined2-class}
\alias{initialize,Mined2-method}

\title{Class \code{"Mined2"}}
\description{
Mined2 is a subclass of Mined, containing three parameters. These include the two parameters found in Mined classes (e.g. a depvar parameter containing dependent variable data, and a covariates parameter containing a matrix of independent variable data), as well as a third parameter containing a vector of independent variables' names. 
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("Mined2", ...)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{depvar}:}{Object of class \code{"numeric"}; should contain dependent variable data (length n)}
    \item{\code{covariates}:}{Object of class \code{"matrix"}; should contain independent variable data (rows n, columns k)}
    \item{\code{variable.names}:}{Object of class \code{"character"} should contain k strings containing the independent variables' names}
  }
}
\section{Methods}{
  \describe{
    \item{initialize}{\code{signature(.Object = "Mined2")}: ... }
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
\code{\linkS4class{Mined}}
%%  ~~objects to See Also as \code{\link{~~fun~~}}, ~~~
%%  ~~or \code{\linkS4class{CLASSNAME}} for links to other classes ~~~
}
\examples{
showClass("Mined2")
}
\keyword{classes}
