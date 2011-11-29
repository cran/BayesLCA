\name{blca2mcmc}
\alias{blca2mcmc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Converts blca Objects to mcmc 
}
\description{
Converts blca objects to mcmc objects. This is only to be used when using the Gibbs sampling method.
}
\usage{
blca2mcmc(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
An object of class blca.gibbs. An error is returned if this is not the case.
}
}
\details{
Whenever a Gibbs sampler is employed, it is always a good idea to ensure that parameter samples are being obtained correctly - that burn-in has been achieved, and that appropriate mixing is taking place, for example. blca2mcmc converts an object of class blca to that of mcmc to avail of the diagnostic  checks available in other R packages.
}
\value{
An \eqn{N \times G*(M+1)} matrix of class mcmc, where N is the number of data points, M the number of columns and G the number of classes. The first G columns (labelled Tau 1 , \ldots, Tau G) are membership samples, the next G*M columns (labelled Theta 1 1 , Theta 1 2, \ldots, Theta G 1, \ldots, Theta G M) are item response samples.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Arthur White
}
\note{
While plot and summary methods exist for the function, their scope is currently somewhat limited. The function\code{\link{blca2mcmc}} makes \code{blca.gibbs} objects compatible with functions such as \code{\link[coda]{summary.mcmc}} and \code{\link[coda]{raftery.diag}} allowing for more detailed analytical analysis.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{blca.gibbs}}, \code{\link[coda]{raftery.diag}}
}
\examples{
data(Alzheimer)

fit.gibbs<- blca.gibbs(Alzheimer, 2)
raftery.diag(blca2mcmc(fit.gibbs)) 

\dontrun{ fit.gibbs<- blca.gibbs(Alzheimer, 2, K=100000, accept=0.1, burn.in=100) }
\dontrun{ raftery.diag(blca2mcmc(fit.gibbs)) }

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ blca }
\keyword{ mcmc }% __ONLY ONE__ keyword per line
