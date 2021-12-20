\name{mort.B2007}
\alias{mort.B2007}
\title{
Mortality Function by Bollandsas (2007)
}
\encoding{UTF-8}
\description{
Implementation of the individual tree mortality function published by Bollandsås (2007).
}
\usage{
mort.B2007(tr, fl, common.vars, this.period, ...)
}
\arguments{
  \item{tr}{
A \emph{trList} class object.
}
  \item{fl}{
A list describing the plot data.
}
  \item{common.vars}{
A list with at least variables spp (species classification in
categories: spruce, pine, birch and other), and SBA.m2.ha.
}
  \item{this.period}{
The period for which to calculate mortality.
}

  \item{\dots}{
Ignored.
}
}

\value{
A TRUE/FALSE vector for all trees in \emph{tr}.
}
\references{
 Bollandsås O. Uneven-aged Forestry in Norway: Inventory and Management Models. [Ås, Norway]: Norwegian University of Life Sciences, Department of Ecology and Natural Resource Management; 2007. 

}
\author{
Clara Anton Fernandez \email{caf@nibio.no}
}

\examples{
foo.matrix <-  matrix(0, nrow = length(tr$dbh), ncol = (5 +1))
colnames(foo.matrix) <- paste("t", 0:5, sep = "")
foo.dbh <- foo.height <- foo.matrix
foo.dbh[,1] <- tr$dbh
foo.height[,1] <- tr$height

trl <- list(
  plot.id  = tr$plot.id,
  treeid    = tr$treeid,
  dbh.mm    = foo.dbh,
  height.dm = foo.height,
  yrs.sim   = rep(0, nrow(tr)),
  tree.sp   = factor(tr$tree.sp)
)
tr.i <- trList$new(data = trl, nperiods = as.integer(5))

common.vars <-  prep.common.vars.fun(
  tr = tr.i,
  fl = fl,
  this.period    = "t0",
  i.period       = 0,
  common.vars    = "NULL",
  mng.options    = NA,
  vars.required  = c("spp", "SBA.m2.ha"),
  period.length = 5
)
dead.trees <- mort.B2007(tr.i, fl, common.vars$res, this.period = 't0')
sum(dead.trees)

}

