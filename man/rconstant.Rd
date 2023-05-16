\name{rconstant}
\alias{rconstant}

\title{
Use a single constant value for a root node
}
\description{
This is a small convenience function that simply returns the value passed to it, in order to allow the use of a constant node as root node in the \code{\link{sim_from_dag}} function.
}
\usage{
rconstant(n, constant)
}
\arguments{
  \item{n}{
The number of times the constant should be repeated.
  }
  \item{constant}{
A single value of any kind which is used as the only value of the resulting variable.
  }
}
\author{
Robin Denz
}
\examples{
rconstant(n=10, constant=7)

rconstant(n=4, constant="Male")
}
