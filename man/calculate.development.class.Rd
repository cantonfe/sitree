\name{calculate.development.class}
\alias{calculate.development.class}

\title{
Calculate Development Class of the Stand
}
\description{
Calculates the development class of the stand according to the Norwegian National
Forest Inventory.
}
\usage{
calculate.development.class(SI.spp, SI.m, stand.age.years)
}
\arguments{
  \item{SI.spp}{
Species for which the SI is calculated (1 = spruce, 2 = pine, 3 = deciduous).
}
  \item{SI.m}{
Site index (SI) in m.
}
  \item{stand.age.years}{
Age of the stand in years.
}
}
\details{
Development classes 1 (regeneration) to 5 (mature).
}
\value{
It returns a vector with the development class of the stands (values 1 to 5).
}

\author{
Clara Anton Fernandez \email{caf@nibio.no}
}

\examples{
calculate.development.class (SI.spp = c(1, 1, 2),
   SI.m = c(23,11,11),
   stand.age.years = c(120, 80, 40)) 

}


