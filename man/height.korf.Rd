\name{height.korf}
\alias{height.korf}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Simple height function
}
\description{
  A simple tree height function based on tree species, and DBH. It returns the
difference between the height at the current period (this.period) and
the next period.
}
\usage{
height.korf(common.vars, this.period, tr,  dbh.inc.mm, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{common.vars}{
A list with at least an element named 'spp' with the species group composition.
}
  \item{this.period}{
The period for which to calculate the height of the trees.
}
  \item{tr}{
A \emph{trList}.
}  

\item{dbh.inc.mm}{
A vector with the projected dbh increment.
}
  \item{\dots}{
Currently not used
}
}

\value{
A vector with tree heights increments in dm.
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
  ustandID  = tr$ustandID,
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
  vars.required  = c("spp"),
  period.length = 5,
  treslag.gran = c(1, 2, 3),
  treslag.furu = c(10, 11, 20, 21, 29),
  treslag.lauv = c(30, 31)
)
height.korf(common.vars = common.vars$res, this.period = 't0', 
    tr.i, fl, dbh.inc.mm = runif(nrow(tr.i$data$dbh.mm), 5, 80)) 
}

