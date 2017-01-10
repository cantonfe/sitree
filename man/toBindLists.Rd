\name{toBindLists}
\alias{toBindLists}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Binding lists
}
\description{
This function is only used internally to bind vectors, data frames, and lists.
}
\usage{
toBindLists(x, y)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
A vector, data frame or list. 
}
  \item{y}{
Object of the same type as x.
}
}

\value{
It returns a vector, a data frame or a list depending on \emph{x}.
}

\author{
Clara Anton Fernandez \email{caf@nibio.no}
}

\examples{
## Example of how toBindLists works with data frames
x <- data.frame(1:10, ncol = 2)
names(x) <- c('a', 'b')
y <- data.frame(1:20, ncol = 2)
names(y) <- names(x)
j <- toBindLists(x, y)
j

## with vectors
x <- c(1:3)
y <- (8:15)
j <- toBindLists(x, y)
j

## with lists
x <- data.frame(1:10, ncol = 2)
names(x) <- c('a', 'b')
y <- data.frame(1:20, ncol = 2)
names(y) <- names(x)

x <- list(x = x, y = y)

xx <- data.frame(1:10, ncol = 3)
names(xx) <- c('a', 'b')
yy <- data.frame(1:20, ncol = 3)
names(yy) <- names(xx)

y <- list(xx = xx, yy= yy)

j <- toBindLists(x, y)
j
}

