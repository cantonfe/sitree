\name{QMD.cm.f}
\alias{QMD.cm.f}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Quadratic Mean Diameter
}
\description{
Calculates quadratic mean diameter.
}
\usage{
QMD.cm.f(x.mm)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x.mm}{
DBH in mm.
}
}
\details{
It ignores not finite values like NA, and calculates the QMD for the
rest of the trees.
}
\value{
  The QMD in cm of x.mm.
}

\author{
Clara Anton Fernandez \email{caf@nibio.no}
}


\examples{
QMD.cm.f(50)
QMD.cm.f(c(51, NA))
QMD.cm.f(c(51, 42, 120, 57))
}
