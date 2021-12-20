\name{dead.trees.growth}
\alias{dead.trees.growth}
\title{
Growth of Dead Trees
}
\description{
A very simple function to estimate the growth of dead trees between the
last measurement and their death. Dead trees are assumed to die in the
middle of the period. The growth before the death of the tree is assumed
to be half of the growth for the whole period, both in terms of DBH and height.
}
\usage{
dead.trees.growth(dt, growth, mort, this.period)
}
\arguments{
  \item{dt}{
A list of trees with at least two elements: \var{dbh.mm} and \var{height.dm}.
}
  \item{growth}{
A data frame with columns \var{dbh.inc.mm} and \var{hgt.inc.dm}.
}
   \item{mort}{
A TRUE/FALSE vector with the predicted mortality for each living tree in
this period.
}
  \item{this.period}{
The period where the dead trees were last seen alive (last measured DBH).
}
} 

\value{
Returns a data frame of same length as \emph{dt} with two elements
  \item{dbh.mm }{DBH in mm}
  \item{height.dm }{Height in dm}

}

\author{
Clara Anton Fernandez \email{caf@nibio.no}
}
\examples{
dead.trees.growth(
  dt = list(dbh.mm = data.frame   (t0 = c(75, 90, 25, 24)),
            height.dm = data.frame(t0 = c(190, 210, 110, 90))),
  growth = data.frame(dbh.inc.mm = c(7, 9, 2, 1),
                      hgt.inc.dm = c(11, 12, 8, 10)),
  this.period = "t0"
) 

}
