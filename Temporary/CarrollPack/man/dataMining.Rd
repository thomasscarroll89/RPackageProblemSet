\name{dataMining}
\alias{dataMining}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Data Mining
}
\description{
Runs a linear regression using all possible combinations of supplied independent variables.
}
\usage{
dataMining(object = "Mined")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{depvar}{ A n-length numeric vector containing the dependent variable data}
  \item{covariates} {An n-by-k matrix where each column represents the data for one independent variable}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Returns a matrix containing all of the coefficient estimates and R^2 values for each OLS model run
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Thomas Carroll
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
X1 <- rnorm(100, 50, 20)
X2 <- runif(100)
X3 <- rnorm(100, 0, 10)
Y <- 12 + 3*X1 - 6*X2 + rnorm(100)
test1 <- new(Class="Mined", depvar=Y, covariates=c(X1, X2, X3))
dataMining(test1)
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
structure(function (object = "Mined") 
{
    standardGeneric("dataMining")
  }, generic = structure("dataMining", package = "CarrollPack"), package = "CarrollPack", group = list(), valueClass = character(0), signature = "object", default = `\001NULL\001`, skeleton = (function (object = "Mined") 
stop("invalid call in method dispatch to 'dataMining' (no default method)", 
    domain = NA))(object), class = structure("nonstandardGenericFunction", package = "methods"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
