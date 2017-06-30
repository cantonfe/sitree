\name{sitree}
\alias{sitree}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Individual Tree Simulator
}
\description{
Simulates the growth of individual trees in several plots, including
management and keeping record of dead and removed trees. It can also
include the effect of external modifiers such as climate change.
}
\usage{
sitree(tree.df, stand.df, functions, n.periods = 5, period.length,
mng.options = NA, print.comments = FALSE,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tree.df}{
A data frame with individual tree data. It should include four columns
named 'plot.id' (an ID for the plot/stand that the tree belongs to),
treeid (a unique ID for the tree), dbh (a measure of horizontal
dimension), height (a measure of vertical dimension), tree.sp (tree species)
}
  \item{stand.df}{
A data frame or list with neccessary data on the stand (e.g. slope, site
index). It should include a 'plot.id' that matches the one in tree.df
and a 'plot.size.m2', the plot size in m2. 'plot.size.m2' is only used
in the summary.sitree function to calculate figures by ha.
}
  \item{functions}{
A list describing the basic functions. It should include a minimum of a
function for growth (fn.growth), for diameter (or basal area) increment,
for height increment (height.korf), mortality (fn.mort), recruitment
(fn.recr), management (fn.management), tree removal (fn.tree.removal),
and external modifiers (fn.modif). fn.management, fn.tree.removal, and
fn.modif, can be missing.
}
  \item{n.periods}{
Number of periods to simulte (numeric).
}
  \item{period.length}{
The length of the periods (e.g. 5 years).
}
  \item{mng.options}{
Management options and definitions to be passed further.
}
  \item{print.comments}{
TRUE/FALSE Print comments. It is meant to help debugging new functions.
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}

\value{
Returns a list with all live, dead, removed trees, and some plot
information like stand age in years at each period, and management
applied at each plot at each period.
}
\author{
Clara Anton Fernandez \email{caf@nibio.no}
}



\examples{
result.sitree <- sitree (tree.df   = tr,
                           stand.df  = fl,
                           functions = list(
                             fn.growth     = 'grow.dbhinc.hgtinc',
                             fn.dbh.inc = "dbhi.BN2009",
                             fn.hgt.inc =  "height.korf", 
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
                           species.spruce = c(1, 2, 3),
                           species.pine = c(10, 11, 20, 21, 29),
                           species.harw = c(30, 31),
                           fun.final.felling = "harv.prob",
                           fun.thinning      = "thin.prob",
                           'BN2009',
                           'BBG2008', 'SBA.m2.ha', 'spp','pr.spru.ba', 'QMD.cm',
                           per.vol.harv = 0.83
                           )
str(result.sitree)
set.seed(2017)
result.sitree <- sitree (tree.df   = stand.west.tr,
                           stand.df  = stand.west.st,
                           functions = list(
                             fn.growth     = 'grow.dbhinc.hgtinc',
                             fn.dbh.inc = "dbhi.BN2009",
                             fn.hgt.inc =  "height.korf", 
                             fn.mort       = 'mort.B2007',
                             fn.recr       = 'recr.BBG2008',
                             fn.management = NULL,
                             fn.tree.removal = NULL,
                             fn.modif      = NULL, 
                             fn.prep.common.vars = 'prep.common.vars.fun'
                           ),
                           n.periods = 20,
                           period.length = 5,
                           mng.options = NA,
                           print.comments = FALSE,
                           species.spruce = c(1, 2, 3),
                           species.pine = c(10, 11, 20, 21, 29),
                           species.harw = c(30, 31),
                           'BN2009',
                           'BBG2008', 'SBA.m2.ha', 'spp','pr.spru.ba', 'QMD.cm'
                         )

}

