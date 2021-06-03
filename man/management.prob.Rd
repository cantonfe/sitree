\name{management.prob}
\alias{harv.prob}
\alias{management.prob}
\alias{thin.prob}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Final felling and thinning functions for Norwegian forest
}
\description{
Estimates de probability of a stand to be harvested or thinning
following Anton-Fernandez et al. (20012).
}
\usage{
management.prob(tr, fl, fun.final.felling = "harv.prob",
fun.thinning = "thin.prob", common.vars, this.period, next.period, ...)

harv.prob(region, skidding.distance.100m, AgeTo5, vuprha.m3.ha,
slope.per, SI.m, SI.spp)

thin.prob(region, skidding.distance.100m, AgeTo5, vuprha.m3.ha, slope.per, SI.m, SI.spp)


}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{tr}{
A \emph{trList} class object.
}
\item{fl}{
A list describing the plot data.
}
\item{fun.final.felling}{
Function to use to calculate final felling.
}
\item{fun.thinning}{
Function to use to calculate thinning.
}
\item{common.vars}{
A list with at least variables dev.class and vuprha.m3.ha.
}
\item{this.period}{
The period for which to calculate final felling and thinning probability.
}
\item{next.period}{
The next period to the one for which final felling and thinning probability are to be calculated.
}
\item{...}{
%%     ~~Describe \code{region} here~~
}

  \item{region}{
A vector containing the region in Norway where every plot is situated.
}
  \item{skidding.distance.100m}{
A vector containing skidding for each plot. 
}
  \item{AgeTo5}{
A vector containing number of years to development class 5 for each plot.
}
  \item{vuprha.m3.ha}{
Volume per ha in cubic meters per ha for each plot.
}
  \item{slope.per}{
Slope, in percentatge, for each plot.
}
  \item{SI.m}{
Site index (SI) in m.
}
  \item{SI.spp}{
Species for which the SI is calculated (1 = spruce, 2 = pine, 3 = deciduous).
}
}

\value{
	It returns a list with one element: 
	\item{mng}{a vector with the management to apply to each plot.}

}
\references{
Antón-Fernández, C. and Astrup, R. 2012 Empirical harvest models and
their use in regional business-as-usual scenarios of timber supply  and carbon stock development. \emph{Scandinavian Journal of Forest Research}, \emph{27}, 4, 379--392.}
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
  vars.required  = c("spp", "SBA.m2.ha", "QMD.cm", "vuprha.m3.ha", "AgeTo5"),
  period.length = 5
)
fl$management <- data.frame(matrix(NA, ncol = tr.i$nperiods,
                                   nrow = nrow(tr.i$data$dbh.mm)))
names(fl$management) <- paste0("t", 1:tr.i$nperiods)

management.prob(tr.i,
                fl,
                fun.final.felling = "harv.prob",
                fun.thinning = "thin.prob",
                common.vars = common.vars$res,
                this.period = "t0",
                next.period = "t1")

harv.prob(region = fl$region[1:3],
          skidding.distance.100m = fl$skidding.distance.100[1:3],
          AgeTo5 = c(50, 20, 15),
          vuprha.m3.ha = common.vars$res$vuprha.m3.ha[1:3],
          slope.per = fl$slope.per[1:3],
          SI.m = fl$SI.m[1:3],
          SI.spp = fl$SI.spp[1:3])

thin.prob(region = fl$region[1:3],
          skidding.distance.100m = fl$skidding.distance.100[1:3],
          AgeTo5 = c(50, 20, 15),
          vuprha.m3.ha = common.vars$res$vuprha.m3.ha[1:3],
          slope.per = fl$slope.per[1:3],
          SI.m = fl$SI.m[1:3],
          SI.spp = fl$SI.spp[1:3]
)

}

