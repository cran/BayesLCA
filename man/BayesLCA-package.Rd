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
Version: \tab 1.1\cr
Date: \tab 2013-02-01\cr
License: \tab GPL (>= 2)\cr
LazyLoad: \tab yes\cr
}
}
\author{
Arthur White and Brendan Murphy
Maintainer: Arthur White <arthur.white@ucdconnect.ie>
}

\keyword{ package }

\examples{
type1 <- c(0.8, 0.8, 0.2, 0.2)
type2 <- c(0.2, 0.2, 0.8, 0.8)
x <- rlca(1000, rbind(type1, type2), c(0.4,0.6))
fit.em <- blca.em(x, 2)
plot(fit.em, which=1)
print(fit.em)
summary(fit.em)
data(Alzheimer)
fit.vb <- blca(Alzheimer, 2, method="vb")
par(mfrow=c(3,3))
plot(fit.vb, which=3:4)
summary(fit.vb)
par(mfrow=c(1,1))
}
