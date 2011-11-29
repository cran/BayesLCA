\name{BayesLCA-package}
\alias{BayesLCA-package}
\alias{BayesLCA}
\docType{package}
\title{
Bayesian Latent Class Analysis
}
\description{
Bayesian latent class analysis using multiple inference techniques.
}
\details{
\tabular{ll}{
Package: \tab BayesLCA\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2011-11-10\cr
License: \tab GPL (>= 2)\cr
LazyLoad: \tab yes\cr
}
}
\author{
Arthur White and Brendan Murphy
Maintainer: Arthur White <arthur.white@ucdconnect.ie>
}
\references{

}
\keyword{ package }
\seealso{
}
\examples{
type1 <- c(0.8, 0.8, 0.2, 0.2)
type2 <- c(0.2, 0.2, 0.8, 0.8)
x<- rlca(1000, rbind(type1, type2), c(0.4,0.6))

fit.em <- blca.em(x, 2)
summary(fit.em)
data(Alzheimer)
fit.vb<- blca(Alzheimer, 2, method="vb")
par(mfrow=c(3,3))
plot(fit.vb)
summary(fit.vb)

}
