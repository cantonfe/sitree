\name{sitree2dataframe}
\alias{sitree2dataframe}
\alias{sitree2dataframe.all}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
sitree2data.frame and sitree2dataframe.all
}
\description{
sitree2data.frame converts trList and trListDead class objects to data.frames.
sitree2data.frame converts all output of sitree to a data.frame
}
\usage{
sitree2dataframe(tr.dt)
sitree2dataframe.all(sitree.res)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tr.dt}{
A trList or trListDead object.
}
  \item{sitree.res}{
An object resulting from running sitree.
}
}
\details{
As implemented, it expects to have a 'removed' element on sitree2dataframe.all. For an alternative when the simulation has no harvest see the exaples below.
}
\value{
sitree2dataframe return a data.frame with dbh, height, and species by period.
sitree2dataframe.all returns a list  
  \item{live}{A data.frame with dbh, height, and species of the trees alive at each period.}
  \item{dead}{A data.frame with dbh, height, and species of the trees found dead at each period.}
\item{removed}{A data.frame with dbh, height, and species of the trees removed at each period.}
}

\author{
Clara Anton Fernandez
}

\examples{

n.periods <- 40

# Run a simulation with the example functions
res <- sitree (tree.df   = tr,
               stand.df  = fl,
               functions = list(
                     fn.growth     = 'grow.dbhinc.hgtinc',
                     fn.dbh.inc = "dbhi.BN2009",
                     fn.hgt.inc =  "height.korf", 
                     fn.mort       = 'mort.B2007',
                     fn.recr       = 'recr.BBG2008',
                     fn.management = 'management.prob',
                     fn.tree.removal = 'mng.tree.removal',
                     fn.modif      = NULL, 
                     fn.prep.common.vars = 'prep.common.vars.fun'
                 ),
                 n.periods = n.periods,
                 period.length = 5,
                 mng.options = NA,
                 print.comments = FALSE,
                 species.spruce = c(1, 2, 3),
                 species.pine = c(10, 11, 20, 21, 29),
                 species.harw = c(30, 31),
                 fun.final.felling = "harv.prob",
                 fun.thinning      = "thin.prob",
                 'BN2009',
                 'BBG2008', 'SBA.m2.ha', 'spp','pr.spru.ba', 'QMD.cm',
                 per.vol.harv = 0.83
                 )

dbh.mm <- sitree2dataframe(res$live)
res.dt <- sitree2dataframe.all(res)



## The function is currently defined as


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
