\name{print.blca}
\alias{print.blca.boot}
\alias{print.blca.em}
\alias{print.blca.gibbs}
\alias{print.blca.vb}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Bayesian Latent Class Analysis
}
\description{
Print \code{\link{blca}} object.
}
\usage{
\method{print}{blca.boot}(x, ...)
\method{print}{blca.em}(x, ...)
\method{print}{blca.gibbs}(x, ...)
\method{print}{blca.vb}(x, ...)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
An object of class \code{\link{blca.boot}}, \code{\link{blca.em}}, \code{\link{blca.gibbs}} or \code{\link{blca.vb}}.
}
  \item{\dots}{
Additional arguments to be passed onto lower-level functions at a later stage of development.
}
}
\details{
 currently prints parameter maximum posterior (map) and standard error estimates.
}
\value{
The blca object itself.
}
\author{
Arthur White
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
data(Alzheimer)
fit<- blca(Alzheimer, 2, method="em")
class(fit)
print(fit)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ blca }
\keyword{ print }% __ONLY ONE__ keyword per line
