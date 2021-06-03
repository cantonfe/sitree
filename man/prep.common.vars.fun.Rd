\name{prep.common.vars.fun}
\alias{prep.common.vars.fun}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to Calculate Common Variables
}
\description{
  A function to calculate common variables needed to estimate growth,
  death, etc.
}
\usage{
prep.common.vars.fun(tr, fl, i.period, this.period, common.vars, vars.required,
period.length, n.periods, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
   \item{tr}{
A \emph{trList} class object.
}
  \item{fl}{
A list describing the plot data.
}
  \item{i.period}{
The number of period (from 0 to nperiods) for which to calculate the
comon variables.
}
  \item{this.period}{
The period for which to calculate the common variables.
}
  \item{common.vars}{
A list with existing common variables.
}
  \item{vars.required}{
Variables to be calculated.
}
  \item{period.length}{
Length of the periods, in years.
}
  \item{n.periods}{
Number of periods.
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}
\details{
This is an example of a prep.common.vars file. A prep.common.vars
(prepare common variables) is needed for the simulation. It is in this
file that additional variables needed for the particular functions used
in the simulation are calculated. For example, to calculate quadratic mean diameter, basal area, or volume.

}
\value{
A list
  \item{res}{A list with the common variables.}
  \item{fl }{Updated \emph{fl}}
%% ...
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
  vars.required  = c("vol.wo.tr.m3.ha", "vuprha.m3.h"),
  period.length = 5
)
str(common.vars)
}
