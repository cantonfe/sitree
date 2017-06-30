\name{tr}
\alias{tr}
\docType{data}
\title{
Individual Tree Data
}
\description{
A data frame with data to create the trList object needed to run \emph{sitree}.
}
\usage{data("tr")}
\format{
  The data frame contains the following columns
	\describe{
	\item{plot.id}{unique ID for each stand that corresponds to plot.id
		in the \emph{fl} dataset}
	\item{treeid}{unique ID for each tree}
	\item{dbh}{dbh in mm}
	\item{height}{height of the tree in dm}
	\item{tree.sp}{tree species code}
	}
}

\source{
This dataset is derived from the Norwegian National Forest Inventory.
}

\examples{
data(tr)
hist(tr$dbh)
}
\keyword{datasets}
