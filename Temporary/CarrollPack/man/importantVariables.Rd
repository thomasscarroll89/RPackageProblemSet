\name{importantVariables}
\alias{importantVariables}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate which Variables are "important"}
\description{
Determines which variables from a data mined object are "important", based on their average p-values for every model they were run in. 
}
\usage{
importantVariables(object = "Mined")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{ A "Mined" object or one of its subclasses
%%     ~~Describe \code{object} here~~
}
}
\details{
After running separate linear regressions for every possible combination of variables, this function returns the different models p-values and average p-values for each variable. 
}
\value{
  Returns a list, output, with elements:
  \item{output[[1]]} {A vector containing the average p-value for each variable}
  \item{output[[2]]} {A matrix containing all of the p-values for each variable in each model}
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Thoms Carroll
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
importantVariables(test1)
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
structure(function (object = "Mined") 
{
    standardGeneric("importantVariables")
  }, generic = structure("importantVariables", package = "CarrollPack"), package = "CarrollPack", group = list(), valueClass = character(0), signature = "object", default = `\001NULL\001`, skeleton = (function (object = "Mined") 
stop("invalid call in method dispatch to 'importantVariables' (no default method)", 
    domain = NA))(object), class = structure("nonstandardGenericFunction", package = "methods"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
