\name{plot.blca}
\alias{plot.blca.boot}
\alias{plot.blca.em}
\alias{plot.blca.gibbs}
\alias{plot.blca.vb}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot Parameter Summaries, Density Estimates and Model Diagnostics for Bayesian Latent Class Analysis 
}
\description{
Five plots are selectable: a plot summarising item and class probability, a mosaic plot representing classification uncertainty, item probability density estimates, conditional class probability density estimates, and a diagnostics plot. The default setting is for the first four plots to be displayed, with the exception of plot.blca.em, which cannot produce density plots and so only produces the first two plots by default.
}
\usage{
\method{plot}{blca.boot}(x, which = c(1L, 3L), main = "", ...)
\method{plot}{blca.em}(x, which = 1L, main = "", ...)
\method{plot}{blca.gibbs}(x, which = c(1L, 3L), main = "", ...)
\method{plot}{blca.vb}(x, which = c(1L, 3L), main = "", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
An object of class \code{\link{blca.boot}}, \code{\link{blca.em}}, \code{\link{blca.gibbs} } or \code{\link{plot.blca.vb}}.
}
  \item{which}{
Which plots to select.
}
  \item{main}{
An overall title for the plot: see \code{\link{title}}.
}
  \item{\dots}{
Further arguments to be passed onto \code{\link{plot}}, or in the case of which=2, \code{\link{mosaicplot}}.
}
}
\details{
Some plots are currently unavailable to certain classes. No density estimates are possible for objects of class \code{\link{blca.em}}, and no diagnostic plots are currently available for objects of class \code{\link{blca.boot}}.
}
\value{
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
type1 <- c(0.8, 0.8, 0.2, 0.2)
type2 <- c(0.2, 0.2, 0.8, 0.8)
x<- rlca(1000, rbind(type1,type2), c(0.6,0.4))

fit <- blca.em(x, 2)
plot(fit) ## No density etimates

fit2<- blca.vb(x,2)
par(mfrow=c(3,4))
plot(fit2, which=1:3)
par(mfrow=c(1,1))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ plot }
\keyword{ blca }% __ONLY ONE__ keyword per line
\keyword{bootstrap}
\keyword{em}
\keyword{gibbs}
\keyword{variational}
