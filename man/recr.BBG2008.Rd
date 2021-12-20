\name{recr.BBG2008}
\alias{recr.BBG2008}
\encoding{UTF-8}
\alias{BBG2008}
\title{
Recruitment function following BBG2008
}
\description{
It creates a list of new trees based on the functions from BBG2008 (see
references). BBG2008 functions estimates recruitment on a plot based on
stand basal area, SI, and pine, spruce, birch, and other broadleaved
species abundance. The implementation of this function is stochastic,
not deterministic. So, if run several times without setting a seed, the
results will vary. 
}
\usage{
recr.BBG2008(tr, fl, common.vars, i.period, this.period, next.period, ...)
BBG2008 (SBA.m2.ha, SI.m, pr.spru.ba, pr.pine.ba, pr.birch.ba, 
    pr.other.ba) 
}
\arguments{
  \item{tr}{
A trList object
}
  \item{fl}{
A list describing the plot information
}
  \item{common.vars}{
}
  \item{i.period}{
The number of period (from 0 to nperiods) for which to calculate recruitment.
}
  \item{this.period}{
Period to use to estimate next period recruitment.
}
\item{next.period}{
	Period for which to calculate recruitment.
}

  \item{SBA.m2.ha}{
Stand basal area in m2 per ha.
}
  \item{SI.m}{
Site index (SI) in m.
}
  \item{pr.spru.ba}{
Percentatge of spruce in terms of basal area.
}
  \item{pr.pine.ba}{
Percentatge of pine in terms of basal area.
}
  \item{pr.birch.ba}{
Percentatge of birch in terms of basal area.
}
  \item{pr.other.ba}{
Percentatge of other species in terms of basal area.
}
  \item{\dots}{
Not used.
}
}

\value{
  Returns a data frame with two columns for each of the four species,
  one for the probability of positive recruitment (spruce.p, pine.p,
  birch.p, other.p), and one for the conditional expected recruitment
  (spruce.e, pine.e, birch.e, other.e).
}
\references{
Bollandsås, O. M., Buongiorno, J., and Gobakken T. (2008). Predicting the Growth of Stands of Trees of Mixed Species and Size: A Matrix Model for Norway. \emph{Scandinavian Journal of Forest Research} \strong{23}(2). 167-178.
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

## create a fake dead trees and removed trees.

common.vars <-  prep.common.vars.fun(
  tr = tr.i,
  fl = fl,
  this.period    = "t0",
  i.period       = 0,
  common.vars    = "NULL",
  mng.options    = NA,
  vars.required  = c("spp", "SBA.m2.ha", "pr.spru.ba"),
  period.length = 5,
  species.spruce = c(1, 2, 3),
  species.pine = c(10, 11, 20, 21, 29),
  species.harw = c(30, 31)
)

recr <- recr.BBG2008(tr.i, fl, common.vars$res, i.period = 0, this.period = "t0",
                     next.period = "t1")
str(recr)
bbg <- BBG2008 (common.vars$res$SBA.m2.ha[common.vars$res$i.tree],
                fl$SI.m,
                common.vars$res$pr.spp.ba$spru[common.vars$res$i.tree],
                common.vars$res$pr.spp.ba$pine[common.vars$res$i.tree],
                common.vars$res$pr.spp.ba$birch[common.vars$res$i.tree], 
                common.vars$res$pr.spp.ba$other[common.vars$res$i.tree])
str(bbg)
}

