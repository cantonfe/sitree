\name{sitree.summary}
\alias{sitree.summary}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Summary plots
}
\description{
  It produces 5 summary plots (stand basal area, number of stems, average
  height of tallest 10 trees, number of dead trees, and number of harvested trees)
}
\usage{
sitree.summary(sitrees.res, plots, by.stand = TRUE, plot = FALSE,
                           plot.all.together = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sitrees.res}{
An object result of running sitree().
}
  \item{plots}{
Plots to be printed (1: stand basal area, 2: number of stems, 3: average
height of tallest 10 trees, 4: number of dead trees, 5: number of
harvested trees)
}
  \item{by.stand}{
TRUE/FALSE TRUE prints plot by stand (figures by ha), if FALSE it prints
plots with totals (divides values per ha by 'plot.size' and multiplies
by 10.000 to obtain total values)
}
  \item{plot}{
TRUE/FALSE if plots are printed
}

  \item{plot.all.together}{
if TRUE all plots are plotted in a single page
}
}

\value{
It passes the required plots in a named list. It plot is TRUE it also
prints the required plots.
}

\author{
Clara Anton Fernandez \email{caf@nibio.no}
}

\seealso{
\code{\link{sitree}}
}
\examples{

require(sitree)
require(lattice)
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

summary.plots <- sitree.summary(res, plots = 1:6, by.stand = FALSE, plot = TRUE)



}

