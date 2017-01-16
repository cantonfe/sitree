\name{mng.tree.removal}
\alias{mng.tree.removal}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to remove trees
}
\description{
Define how trees are going to be removed in plots with some sort of
management that involves removal (e.g final felling, thinning).
}
\usage{
mng.tree.removal(tr, fl, common.vars, this.period, next.period, ...)
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
A list with at least variable \emph{vol.wo.tr.m3.ha}
}
 
 \item{this.period}{
The period for which to calculate final felling and thinning probability.
}
\item{next.period}{
The next period to the one for which final felling and thinning probability are to be calculated.
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}

\value{
Returns a TRUE/FALSE vector of length equal to the number of trees in \emph{tr}.
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
  i.period       = 0,
  this.period    = "t0",
  common.vars    = "NULL",
  vars.required  = c("vol.wo.tr.m3.ha", "vuprha.m3.h"),
  period.length = 5,
  treslag.gran = c(1, 2, 3),
  treslag.furu = c(10, 11, 20, 21, 29),
  treslag.lauv = c(30, 31)
)

fl$management <- data.frame(matrix(NA, ncol = tr.i$nperiods,
                                   nrow = nrow(tr.i$data$dbh.mm)))
names(fl$management) <- paste0("t", 1:tr.i$nperiods)

management <- management.prob(tr.i,
                fl,
                fun.final.felling = "harv.prob",
                fun.thinning = "thin.prob",
                common.vars = common.vars$res,
                this.period = "t0",
                next.period = "t1")
fl$management[, "t1"] <- management$management
remove.tree <- mng.tree.removal(tr = tr.i,
                                fl = fl,
                                common.vars = common.vars$res,
                                this.period = "t0",
                                next.period = "t1",
                                per.vol.harv =  0.83 )
sum(remove.tree)
}

