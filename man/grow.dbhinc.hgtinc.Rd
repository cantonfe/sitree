\name{grow.dbhinc.hgtinc}
\alias{grow.dbhinc.hgtinc}
\title{
Growth and Height Increment Function Wrapper
}
\description{
A growth and height increment function wrapper that can be used with or
without a height increment function.
}
\usage{
grow.dbhinc.hgtinc(tr, fl, common.vars, this.period, functions, ...)
}

\arguments{
  \item{tr}{A \emph{trList} class object.}
  \item{fl}{A list describing the plot data.}
  \item{common.vars}{A list with at least variables spp (species classification in categories: spruce, pine, birch and other), and SBA.m2.ha.}
  \item{this.period}{
The period for which to calculate growth.}
  \item{functions}{A list defining the functions to be used in sitree.}
  \item{\dots}{Further arguments to be passed to DBH increment and height increment functions.}
}
\details{
A growth function should calculate both dbh and height increment. This
  function calls two functions, defined in \emph{functions} as \emph{fn.dbh.inc} and \emph{fn.hgt.in}c to  estimate those.
}
\value{
  A data frame with two elements
  {dbh.inc.mm } {DBH increment in mm}
  {hgt.inc.dm } {Height increment in dm}
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
  vars.required  = c("spp", "SBA.m2.ha", "QMD.cm"),
  period.length = 5,
  species.spruce = c(1, 2, 3),
  species.pine = c(10, 11, 20, 21, 29),
  species.harw = c(30, 31)
)


grow.dbhinc.hgtinc (tr = tr.i,
                    fl = fl,
                    common.vars = common.vars$res,
                    this.period = "t0",
                    functions = list(
                        fn.growth     = 'grow.dbhinc.hgtinc',
                        fn.mort       = 'mort.B2007',
                        fn.recr       = 'recr.BBG2008',
                        fn.management = 'management.prob',
                        fn.tree.removal = 'mng.tree.removal',
                        fn.modif      = NULL, #'ext.modif.fun',
                        fn.prep.common.vars = 'prep.common.vars.fun'
                    ),
		    fn.dbh.inc = "dbhi.BN2009",
                    fn.hgt.inc =  "height.korf", 
                    species.spruce = c(1, 2, 3),
                    species.pine =  c(10, 11, 20, 21, 29),
                    species.harw =  c(30, 31)
                    )

}

