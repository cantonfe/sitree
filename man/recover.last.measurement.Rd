\name{recover.last.measurement}
\alias{recover.last.measurement}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Recover dbh and height at time of death
}
\description{
Removes all dbh and height data that corresponds to the live of the
tree, and adds the dbh and height at time of death to the dbh.mm and
height.dm in the \emph{data} field.
}
\usage{
recover.last.measurement(tr.list.dead)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tr.list.dead}{
a trListDead object
}
}
\details{
It removes all dbh and height data from the \emph{data} field, adds the
dbh and height at time of death, and converts any potential NAs created
in the process to 0.
}
\value{
	It returns the same object (a trListDead object) with all data on dbh
	and height removed. For each tree there is only one observation of dbh and
	height in the  \emph{data} field, the estimated dbh and height at time
	of death.
}

\author{
Clara Anton Fernandez \email{caf@nibio.no}
}

\examples{
set.seed(2017)
res <- sitree (tree.df   = tr,
                stand.df  = fl,
                functions = list(
                    fn.growth     = 'grow.dbhinc.hgtinc',
                    fn.mort       = 'mort.B2007',
                    fn.recr       = 'recr.BBG2008',
                    fn.management = 'management.prob',
                    fn.tree.removal = 'mng.tree.removal',
                    fn.modif      = NULL, #'ext.modif.fun',
                    fn.prep.common.vars = 'prep.common.vars.fun'
                ),
                n.periods = 5,
                period.length = 5,
                mng.options = NA,
                print.comments = FALSE,
		fn.dbh.inc = "dbhi.BN2009",
                fn.hgt.inc =  "height.korf", 
                species.spruce = c(1, 2, 3),
                species.pine = c(10, 11, 20, 21, 29),
                species.harw = c(30, 31),
                fun.final.felling = "harv.prob",
                fun.thinning      = "thin.prob",
                'BN2009',
                'BBG2008', 'SBA.m2.ha', 'spp','pr.spru.ba', 'QMD.cm',
                per.vol.harv = 0.83
                )
dead <- recover.last.measurement(res$dead)
str(dead)
removed <- recover.last.measurement(res$removed)
str(removed)
}

