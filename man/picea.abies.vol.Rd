\name{picea.abies.vol}
\alias{picea.abies.vol}
\alias{picea.abies.volV}
\alias{pinus.sylvestris.vol}
\alias{pinus.sylvestris.volV}
\alias{sitka.vol}
\alias{harw.nor.vol}

\title{
Volumes for Norwegian species
}
\description{
These functions calculate tree volume with or without bark for the main
species in Norway, that is, Norway spruce, Scots pine, Sitka spruce,
birch, and
... following Braastad (1966), Brantseg (1967), and Vestjordet (1967).
}
\usage{
picea.abies.vol(dbh, trh, bark, units)
picea.abies.volV(dbh, trh, bark, units)
pinus.sylvestris.vol(dbh, trh, bark, units)
pinus.sylvestris.volV(dbh, trh, bark, units)
sitka.vol(dbh, trh, bark, units)
harw.nor.vol(tsl, dbh, trh, bark, units)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dbh}{
A vector with diameters at breast height in mm with bark.
}
  \item{trh}{
A vector with tree heights in cm.
}
  \item{bark}{
If tree volume should be calculated with bark, "mb", or wihtout bark, "ub".
}
  \item{units}{
"l" in liters, "c" in cubic centimeters.
}
  \item{tsl}{
Tree species  30 (), 31(), 32 (), 40(), 41(), 50(), NA. This only affects
the calculations of diameter without bark.
}
}

\value{
Returns tree volume in the selected units.
}

\references{
Braastad, H. 1966. Volumtabeller for bjoerk [Volume tables for birch]. Meddelelser fra Det norske Skogforsøksvesen 21: 23–78.
Brantseg, A. 1967. Furu sønnafjells. Kubering av staaende skog. Funksjoner og tabeller [Volume functions and tables for Scots pine. South Norway]. Meddelelser fra Det norske Skogforsøksvesen 22: 695–739.
Vestjordet, E. 1967. Funksjoner og tabeller for kubering av staaende gran [Functions and tables for volume of standing trees. Norway spruce]. Meddelelser fra Det norske Skogforsøksvesen 22. Ås, Norway: Norwegian Forest and Landscape Institute.
}


\examples{
harw.nor.vol(tsl = c(30,31), dbh = c(45, 25), trh = c(120, 80), bark = "ub", units = "l")
}
