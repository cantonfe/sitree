\name{sp.classification}
\alias{sp.classification}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Species classifcation
}
\description{
Classifies the tree species into four species groups
}
\usage{
sp.classification(tree.sp, treslag.gran, treslag.furu, treslag.lauv)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tree.sp}{
Tree species code.
}
 \item{treslag.gran}{
A vector with the codes used to define 'spruce' group .
}
 \item{treslag.furu}{
A vector with the codes used to define 'pine' group .
}
 \item{treslag.lauv}{
   A vector with the codes used to define the 'birch' group .
 }
  
}

\value{
Returns a vector with the classification of tree.sp into one of these
four groups: spruce, pine, birch, and other. spruce group is defined by
treslag.gran, pine group is defined by treslag.furu, and birch is
defined by treslag.lauv. If a species does not fall within any of these
three groups is classified as 'other'.
}

\author{
Clara Anton Fernandez \email{caf@nibio.no}
}

\examples{
sp.classification(tr$tree.sp, treslag.gran = c(1,15),
                           treslag.furu = c(2,34),
                           treslag.lauv = c(3,4))
}
