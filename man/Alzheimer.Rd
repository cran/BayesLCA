\name{Alzheimer}
\alias{Alzheimer}
\docType{data}
\title{
Symptoms of Patients Suffering from Alzheimer's Syndrome
}
\description{
Presence or absence of 6 symptoms of Alzheimer's disease (AD) in 240 patients diagnosed with early onset AD conducted in the Mercer Institute in St. James's Hospital, Dublin.
}
\usage{data(Alzheimer)}
\format{
A binary matrix, consisting of 240 rows and 6 columns, with each row denoting an individual and each column denoting one of the 6 symptoms.
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
Moran M, Walsh C, Lynch A, Coen RF, Coakley D, Lawlor BA (2004).``Syndromes of behavioural and psychological symptoms in mild Alzheimer's disease.'' International Journal 
of Geriatric Psychiatry, 19(4), 359-364. ISSN 1099-1166. doi:10.1002/gps.1091. URL 
http://dx.doi.org/10.1002/gps.1091. 

Walsh C (2006). ``Latent Class Analysis Identification of Syndromes in Alzheimer's Disease: A 
Bayesian Approach.'' metodoloyski zvezki - Advances in Methodology and Statistics, 3(1), pp.147 -- 162. URL mrvar.fdv.uni-lj.si/pub/mz/mz3.1/walsh.pdf
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(Alzheimer)
fit<- blca.em(Alzheimer, 2)
plot(fit)
}
\keyword{datasets}
\keyword{Alzheimer}
\keyword{Saint}
\keyword{James}
\keyword{Alzheimers}
\keyword{Syndrome}
