\name{fn.vars.required}
\alias{fn.vars.required}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Finds Variables Required on Functions
}
\description{
Tries to find the variables to be calculated through the parameters of
the functions.
}
\usage{
fn.vars.required(my.functions, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{my.functions}{A list of functions to be used during the simulation} 
  \item{\dots}{Use this to add any funciton that might not be included
		in my.functions}
}

\value{
It returns a vector with the name of the potential variables required.
}

\author{
Clara Anton Fernandez \email{caf@nibio.no}
}

\examples{
fn.vars.required (my.functions = list(
                             fn.growth     = "grow.dbhinc.hgtinc",
                             fn.dbh.inc    = "dbhi.BN2009",
                             fn.hgt.inc    = "height.korf", 
                             fn.mort       = "mort.B2007",
                             fn.recr       = "recr.BBG2008",
                             fn.management = "management.prob",
                             fn.tree.removal = "mng.tree.removal",
                             fn.modif      = NULL, 
                             fn.prep.common.vars = "prep.common.vars.fun"
                           ),
                      "BBG2008")

}

