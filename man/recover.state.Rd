\name{recover.state}
\alias{recover.state}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Recovers the State
}
\description{
It adds dead and removed trees to \emph{tr}. This is an internal
function that should not be run, in general, by a regular user. 
}
\usage{
recover.state(tr, dead.trees, removed.trees)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tr}{
A \emph{trList} class object.
}
  \item{dead.trees}{
A \emph{trListDead} class object.
}
  \item{removed.trees}{
A \emph{trListDead} class object.
}
}
\details{
In order to make calculations easier and lighter memory wise, at each
cycle (period) of the simulation all removed (harvested) and dead trees
are removed from \emph{tr}, which only keeps alive trees at the current
period. Once the simulation is finished recover.state returns all dead
and removed trees to the last \emph{tr}, so \emph{sitree} can return a
full list, including all trees alive at any point during the simulation, of trees.
}
\value{
A \emph{trList} class object.
}

\author{
Clara Anton Fernandez \email{caf@nibio.no}
}

