\name{FuruVol}
\alias{FuruVol}
\alias{FuruVolV}
\alias{GranVol}
\alias{GranVolV}
\alias{LauvVol}
\alias{SitkaVol}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Volume for the Main Species in Norway
}
\description{
Calculates tree volume for Scots pine (FuruVol, and FuruVolV for the west
coast), Spruce (GranVol, and GranVolV for the west coast), 
hardwoods (LauvVol), and Sitka spruce (SitkaVol) under Norwegian
conditions following Braastad (1966), Brantseg(1967),  Vestjordet (1967).
}
\usage{
FuruVol(dbh, trh, bark, enhet)
FuruVolV(dbh, trh, bark, enhet)
GranVol(dbh, trh, bark, enhet)
GranVolV(dbh, trh, bark, enhet)
LauvVol(tsl, dbh, trh, bark, enhet)
SitkaVol(dbh, trh, bark, enhet) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{

  \item{dbh}{
diameter at breast height in mm
}
  \item{trh}{
tree height in dm
}
  \item{bark}{
		'ub' without bark, 'mb' with bark
	}
  \item{enhet}{
units for the output. 'l' for volume in liters, and 'cm' for volume in
cubic meters
}
  \item{tsl}{
tree species
}

}

\value{
A vector with the individual tree volumes.
}
\references{
	
Braastad, H. 1966 Volumtabeller for bjørk [Volume tables for
birch]. \emph{Meddelelser fra Det norske Skogforsøksvesen} \bold{21}, 23-–78.

Brantseg, A. 1967 Furu sønnafjells. Kubering av stående
skog. Funksjoner og tabeller [Volume functions and tables for Scots
pine. South Norway]. \emph{Meddelelser fra Det norske Skogforsøksvesen}
\bold{22}, 695–-739.

Vestjordet, E. 1967 Funksjoner og tabeller for kubering av stående gran
[Functions and tables for volume of standing trees. Norway spruce].”
\emph{Meddelelser fra Det norske Skogforsøksvesen}. Ås, Norway:
Norwegian Forest and Landscape Institute.


}
\author{
Clara Anton Fernandez \email{caf@nibio.no}
}

\examples{
FuruVol (dbh = 80, trh = 120, bark = 'ub', enhet = 'l') 
FuruVolV(dbh = 80, trh = 120, bark = 'ub', enhet = 'l')
GranVol(dbh = c(80, 85), trh = c(120, 140), bark = 'ub', enhet = 'l')
GranVolV(dbh = 80, trh = 120, bark = 'ub', enhet = 'l')
LauvVol(tsl = c('30', '31'), dbh = c(80, 85), trh = c(80, 90),
  bark = 'mb', enhet = 'l')
SitkaVol(dbh = 80, trh = 120, bark = 'ub', enhet = 'l')

}

