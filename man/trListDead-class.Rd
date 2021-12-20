\name{trListDead-class}
\Rdversion{1.1}
\docType{class}
\alias{trListDead-class}
\alias{trListDead}

\title{Class \code{"trListDead"}}
\description{
A Reference Class for dead trees.
}
\section{Extends}{
Class \code{"\linkS4class{trList}"}, directly.

All reference classes extend and inherit methods from \code{"\linkS4class{envRefClass}"}.

}

\author{
Clara Anton Fernandez \email{caf@nibio.no}
}
\note{
This class is meant to keep the data of the dead trees in a similar way
as the live trees, but with some extra information such as diameter and height at death.
}

\seealso{
\code{\link{trList}}, ~~~
}
\examples{
showClass("trListDead")
}
\keyword{classes}
\section{Fields}{
  \describe{
    \item{\code{data}:}{Object of class \code{list} containing the
			unique stand ID, the tree ID, and dbh and height for all the
			periods. Only data from the periods were the tree was alive is
			stored in this field}
    \item{\code{nperiods}:}{Object of class \code{integer} containing
			the number of period that should be simulated }
    \item{\code{last.measurement}:}{Object of class \code{data.frame}
			containing tree ID, dbh, height, and period when the tree was "found" dead }
  }
}
\section{Methods}{
  \describe{
    \item{\code{last.time.alive()}:}{ It estimates when the trees where
			last "seen" alive}
    \item{\code{addTrees(value)}:}{ It add trees to the trlistDead object }
    \item{\code{remove.next.period(next.period)}:}{ It removed the data
			from the period when the tree was seen "dead". The next period is
			calculated so the dbh and height at time of death can be estimated }
  }

The following methods are inherited (from the corresponding class):
extractTrees ("trList"), as.list ("trList"), show ("trList"), getTrees ("trList"), addTrees ("trList")
}
