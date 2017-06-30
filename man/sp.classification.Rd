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
sp.classification(tree.sp, species.spruce, species.pine, species.harw)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tree.sp}{
Tree species code.
}
 \item{species.spruce}{
A vector with the codes used to define 'spruce' group .
}
 \item{species.pine}{
A vector with the codes used to define 'pine' group .
}
 \item{species.harw}{
   A vector with the codes used to define the 'birch' group .
 }
  
}

\value{
Returns a vector with the classification of tree.sp into one of these
four groups: spruce, pine, birch, and other. spruce group is defined by
species.spruce, pine group is defined by species.pine, and birch is
defined by species.harw. If a species does not fall within any of these
three groups is classified as 'other'.
}

\author{
Clara Anton Fernandez \email{caf@nibio.no}
}

\examples{
sp.classification(tr$tree.sp, species.spruce = c(1,15),
                           species.pine = c(2,34),
                           species.harw = c(3,4))
}
