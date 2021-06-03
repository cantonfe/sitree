\name{dbhi.BN2009}
\alias{dbhi.BN2009}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
DBH Increment Function by Bollandsas and Naesset (2009)
}
\encoding{UTF-8}
\description{
Implementation of the diameter at breast height function published by Bollandsås and Næsset (2009).
}
\usage{
dbhi.BN2009(tr, fl, common.vars, this.period, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tr}{
A \emph{trList} class object.
}
  \item{fl}{
A list describing the plot data.
}
  \item{common.vars}{
		A list with at least variables \emph{QMD.cm}, \emph{i.stand},
		and \emph{SBA.m2.ha}.
}
  \item{this.period}{
The period for which to calculate DBH increment.
}
  \item{\dots}{
    Ignored
}
}

\value{
A vector with DBH increments in mm for all trees in \emph{tr}.
}
\references{
Bollandsås, O. M and Næsset, E. (2009). Weibull Models for Single-Tree
Increment of Norway Spruce, Scots Pine, Birch and Other Broadleaves in Norway. \emph{Scandinavian Journal of Forest Research} \strong{24} (1): 54.

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
  i.period       = 0,
  this.period    = "t0",
  common.vars    = "NULL",
  vars.required  = c("QMD.cm", "i.stand"),
  period.length = 5
)
 
dbhi.BN2009(tr.i, fl, common.vars$res , this.period = "t0",
            species.spruce = c(1, 2, 3),
            species.pine =  c(10, 11, 20, 21, 29),
            species.harw =  c(30, 31))
}

