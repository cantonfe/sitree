\name{tree.age}
\alias{tree.age}

\title{
EStimation of individual tree age
}
\description{
Estimates individual tree age of trees within a plot based on basal
area, site index, species, and development class.
}
\usage{
tree.age(stand.age.years, ustandID, tree.BA.m2, dbh.mm,
SI.spp, SI.m, spp, dev.class, apply.correction = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{stand.age.years}{
The age of the stand in years.
}
  \item{ustandID}{
The unique ID of the stand
}
  \item{tree.BA.m2}{
A vector with the basal areas of each tree in m2.
}
  \item{dbh.mm}{
A vector with the DBH in mm of each tree.
}
  \item{SI.spp}{
Species for which SI has been calculated (1, 2, 3).
}
  \item{SI.m}{
Site index (SI) in m.
}
  \item{spp}{
Species group classification.
}
  \item{dev.class}{
Development class.
}
  \item{apply.correction}{
TRUE/FALSE. If a correction to age should be applied 
}
}

\value{
A vector with the estimated ages of trees
}
\author{
Clara Anton Fernandez \email{caf@nibio.no}
}

\examples{

tree.age(stand.age.years = 40,
        ustandID = c(1,1), tree.BA.m2 = c(0.05, 0.5),dbh.mm
        = c(50,150), SI.spp = 2, 
        SI.m = 11, spp = c(1,1), dev.class = 3, apply.correction = TRUE) 

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
