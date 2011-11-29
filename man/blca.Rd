\name{blca}
\alias{blca}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Bayesian Latent Class Analysis with one of several methods
}
\description{
Latent class analysis (LCA) attempts to find G hidden classes in binary data X. blca utilises one of: an EM algorithm, a variational Bayes approximation, Gibbs sampling or boot-strapping techniques to find maximum {\emph{ a posteriori}} (MAP), standard error and density estimates of the parameters.
}
\usage{
blca(X, G, method = c("em", "gibbs", "boot", "vb"), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
The data matrix. This may take one of several forms, see \code{\link{data.blca}}.

}
  \item{G}{
The number of classes to run lca for.
}
  \item{method}{
The method with which to perform lca on the data. Four methods are currently available, "em", "gibbs", "boot" or "vb". Defaults to "em", with a warning.
}
  \item{\dots}{
Additional arguments to be passed on, depending on the method. See additional help files for details.
}
}
\details{
The function calls to one of \code{\link{blca.em}}, \code{\link{blca.boot}}, \code{\link{blca.gibbs}}, \code{\link{blca.vb}}, depending on the method specified.
}
\value{

A list of class "blca" is returned. All methods return the following items:
\item{classprob}{The class probabilities.}
\item{itemprob}{The item probabilities, conditional on class membership.}
\item{Z}{Estimate of class membership for each unique datapoint.}
\item{prior}{A list containing the prior values specified for the model.}

See additional help files for  details.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{blca.em}}, \code{\link{blca.boot}}, \code{\link{blca.gibbs}}, \code{\link{blca.vb}}
}
\examples{
type1 <- c(0.8, 0.8, 0.2, 0.2)
type2 <- c(0.2, 0.2, 0.8, 0.8)
x<- rlca(1000, rbind(type1,type2), c(0.6,0.4))

set.seed(1)
fit <- blca(x, 2) ## EM algorithm used, warning returned
print(fit)	  ## No standard errors returned
summary(fit)

set.seed(1)
fit2 <- blca(x, 2, method="em", se=TRUE) ##No warning - same fit
print(fit2) 				 ##Standard Errors returned

set.seed(1)
##Variational Bayes approximation, with priors specified.
fit3 <- blca(x, 2, method="vb", delta=c(5,5), alpha=2, beta=1) 
print(fit3)	##Standard errors returned also.
par(mfrow=c(3,2))			  
plot(fit3)
par(mfrow=c(1,1))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ blca }
\keyword{ lca }% __ONLY ONE__ keyword per line
