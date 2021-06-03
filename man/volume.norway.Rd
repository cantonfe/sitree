\name{volume.norway}
\alias{volume.norway}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Tree volume for Norwegian conditions
}
\description{
Calculates tree volume following the equations used in the Norwegian national forest inventory 
}
\usage{
volume.norway(dbh.mm, height.dm, tree.sp, kom, vol.reduksjon = NULL, 
vol.w = TRUE, vol.wo = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dbh.mm}{
tree dbh in mm
}
  \item{height.dm}{
tree height in dm
}
  \item{tree.sp}{
tree species following the same codification as the Norwegian NFI
}
  \item{kom}{
municipality code
}
  \item{vol.reduksjon}{
volume reduction in 100%
}
  \item{vol.w}{
TRUE/FALSE if volume with bark needs to be calculated
}
  \item{vol.wo}{
TRUE/FALSE if volume without bark needs to be calculated
}
}

\value{
It returns a list with up to two elements:
 \item{vol.w.tr.m3}{volume with bark per tree in m3}
 \item{vol.wo.tr.m3}{volume without bark per tree in m3}

}


\examples{
volume.norway(dbh.mm = c(50,70), height.dm = c(17,20), tree.sp = c(1, 10), kom = c(623, 623))

}
