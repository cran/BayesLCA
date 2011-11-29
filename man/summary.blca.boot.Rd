\name{summary.blca}
\alias{summary.blca.boot}
\alias{summary.blca.em}
\alias{summary.blca.gibbs}
\alias{summary.blca.vb}
\alias{print.summary.blca.boot}
\alias{print.summary.blca.em}
\alias{print.summary.blca.gibbs}
\alias{print.summary.blca.vb}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Bayesian Latent Class Analysis}
\description{
Summary method for class "blca".
}
\usage{
\method{summary}{blca.boot}(object, ...)
\method{summary}{blca.em}(object, ...)
\method{summary}{blca.gibbs}(object, ...)
\method{summary}{blca.vb}(object, ...)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
Object of class \code{\link{blca.boot}}, \code{\link{blca.em}}, \code{\link{blca.gibbs}} or \code{\link{blca.vb}}.
}
 \item{\dots}{
Additional arguments to be passed onto lower-level functions at a later stage of development.
}
}
\details{
Provides a \code{\link{summary}} for blca objects. This includes a description of the hyper-parameters and other terms specified for the model, and where appropriate, model selection criteria such as BIC. The output is different depending on  the model class, e.g., \code{\link{summary.blca.vb}} doesn't provide the BIC for the model in its output.
}
\value{
A brief summary of model diagnostics and the prior values specified to the model are returned.
}
\references{
%% ~put references to the literature/web site here ~
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
\code{\link{summary}}
}
\examples{
data(Alzheimer)
summary(blca.em(Alzheimer, 2))
summary(blca.em(Alzheimer, 3))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ blca }
\keyword{ summary }% __ONLY ONE__ keyword per line
