\name{trList-class}
\Rdversion{1.1}
\docType{class}
\alias{trList-class}
\alias{trList}

\title{Class \code{"trList"}}
\description{
	Describes the Reference Class \emph{trList}, which is used to store
	data on live trees.
}
\section{Extends}{

All reference classes extend and inherit methods from \code{"\linkS4class{envRefClass}"}.

}

\author{
Clara Anton Fernandez \email{caf@nibio.no}
}
\note{
\emph{trList} objects should not be modified inside any user function. Any
change should be made by the \code{sitree} function. Reference class objects
are mutable, they don’t use R’s usual copy-on-modify semantics, but are
modified in place. So, if you want to modify, for example for a
posterior analysis of the results, any \emph{trList} object I recommend
to make a copy of the object first, using \code{$copy()}, e.g. my.tr.list$copy().
}

\seealso{
  \code{\linkS4class{trListDead}}
}
\examples{
showClass("trList")
}
\keyword{classes}
\section{Fields}{
  \describe{
    \item{\code{data}:}{Object of class \code{list} containing the
			unique stand ID (plot.id), the tree ID (treeid), and dbh (dbh.mm)
			and height (height.dm) for all the periods }
    \item{\code{nperiods}:}{Object of class \code{integer} containing
			the number of period that should be simulated }
  }
}
\section{Methods}{
  \describe{
    \item{\code{extractTrees(i)}:}{ Removes trees from the objectd, for
  example when trees die or are harvested }
    \item{\code{as.list()}:}{converts this class to a list }
    \item{\code{show()}:}{ shows the first 20 trees }
    \item{\code{getTrees(i, j)}:}{get the information for i trees and j
  periods without deleting the data  }
    \item{\code{addTrees(value)}:}{adds trees to the object }
  }
}
