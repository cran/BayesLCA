\name{blca.em.se}
\alias{blca.em.se}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Standard Error Estimates for Bayesian Latent Class Analysis  via an EM Algorithm
}
\description{
Returns standard error estimates for point estimates returned by \code{\link{blca.em}}.
}
\usage{
blca.em.se(fit, X, counts.n)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fit}{
An object of class "blca.em"
}
  \item{X}{
A binary matrix, consisting of all unique data patterns.
}
  \item{counts.n}{
A vector which supplies the corresponding number of times each pattern in X occurs in the data.
}
}
\details{
This function is primarily intended for use in conjunction with \code{\link{blca.em}}. It may also be used separately if the user wishes.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
A list containing:
\item{itemprob }{Standard error estimates of the item probabilities.}
\item{classprob }{Standard error estimates of the class probabilities.}
\item{convergence}{An integer value denoting whether the algorithm has converged correctly. 1 denotes acceptable convergence, 2 denotes that it converged at a saddle point, 3 that the algorithm ended before it converged and 4 denotes that at least one parameter value converged at a boundary value.}
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Arthur White}
\note{
The standard error estimates are derived asymptotically, i.e., by inverting the information matrix of the parameters. These values are known to be unreliable in cases where parameters estimates are close to  1 or 0, so caution is advised when checking their values. Bootstrapping methods may provide better estimates.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
 \code{\link{blca.em}}, \code{\link{blca.boot}}
}
\examples{
type1 <- c(0.8, 0.8, 0.2, 0.2)
type2 <- c(0.2, 0.2, 0.8, 0.8)
x<- rlca(1000, rbind(type1,type2), c(0.6,0.4))

set.seed(1)
fit1 <- blca.em(x, 2, se=TRUE)
print(fit1$itemprob.se)
print(fit1$classprob.se)

dat<- data.blca(x)
set.seed(1)
fit2<- blca.em(x, 2, se=FALSE)
fit2.se<- blca.em.se(fit2, dat$data, dat$counts.n)
print(fit2.se$itemprob)
print(fit2.se$classprob)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ standarderror }
\keyword{ blca }% __ONLY ONE__ keyword per line
