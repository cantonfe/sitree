\name{stand.west.st}
\alias{stand.west.st}
\docType{data}
\title{Stand and plot characteritics for stand.west.tr}
\description{
Dataset with stand and plot data for stand.west.tr
}
\usage{stand.west.st}
\format{
  A data frame with 4 observations on the following 7 variables.
  \describe{
    \item{\code{plot.id}}{unique ID for each plot}
    \item{\code{SI.m}}{site index in meters}
    \item{\code{kom}}{municipality code}
    \item{\code{SI.spp}}{species for which site index is measured}
    \item{\code{plot.size.m2}}{plot size in m2}
    \item{\code{tree2ha}}{multiplier to convert values per tree to per hectare}
    \item{\code{lat.deg}}{latitude of the plots in degrees}
    \item{\code{subplot.size.m2}}{plot size in m2, this is a dummy variable needed for some of the calculations}
  }
}

\examples{
stand.west.st
str(stand.west.st) 
}
\keyword{datasets}
