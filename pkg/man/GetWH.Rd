% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GetWH.R
\name{GetWH}
\alias{GetWH}
\title{Occurrence of compound wet-hot events}
\usage{
GetWH(mp,mt,threp,thret)
}
\arguments{
\item{mp}{Precipitation}

\item{mt}{Temperature}

\item{threp}{Threshold of precipitation (e.g., 80th percentile)}

\item{thret}{Threshold of temperature (e.g., 80th percentile)}
}
\value{
The occurrence of compound wet-hot event (0-1 binary variable)
}
\description{
Extract compound wet-hot occurrences based on thresholds of precipitation and temperature.The binary variable of the wet and warm (WW) event can be otained.
}
\examples{
mp=matrix(rnorm(120,0,1),ncol=1)
mt=matrix(rnorm(120,0,1),ncol=1)
threp=80
thret=80
WH<-GetWH(mp,mt,threp,thret)
}
\references{
Hao, Z. et al (2013). Changes in concurrent monthly precipitation and temperature extremes. Environ. Res. Lett., 8(3): 034014.
}
