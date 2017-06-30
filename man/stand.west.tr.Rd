\name{stand.west.tr}
\alias{stand.west.tr}
\docType{data}
\title{
A whole stand dataset
}
\description{
This dataset describes a whole stand of 1019 m2 and spacing 1.25x1.25m. The stand is divided in 4 plots of approximately 250 m2. All trees are Norway spruce.
}
\usage{stand.west.tr}
\format{
  A data frame with 651 observations on the following 5 variables.
  \describe{
    \item{\code{dbh}}{diameter at breast height in mm}
    \item{\code{height}}{tree height in dm}
    \item{\code{plot.id}}{a unique ID for each plot}
    \item{\code{treeid}}{a unique ID for each tree}
    \item{\code{tree.sp}}{tree species, a factor with levels \code{1} \code{2} \code{3} \code{10} \code{11} \code{20} \code{21} \code{29} \code{30} \code{31} \code{32} \code{40} \code{41} \code{42} \code{43} \code{44} \code{48} \code{49} \code{50} \code{51} \code{52} \code{53} \code{54} \code{55} \code{56} \code{57} \code{58} \code{59} \code{70}}
  }
}

\examples{
stand.west.tr
str(stand.west.tr) 
}
\keyword{datasets}
