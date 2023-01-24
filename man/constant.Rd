\name{constant}
\alias{constant}

\title{
Use a single constant value for a root node
}
\description{
This is a small convenience function that simply returns the value passed to it, in order to allow the use of a constant node as root node in the \code{\link{sim_from_dag}} function.
}
\usage{
constant(constant)
}
\arguments{
  \item{constant}{
A single value of any kind which is used as the only value of the resulting variable.
  }
}
\author{
Robin Denz
}
\examples{
\dontrun{
constant(7)

constant("Male")
}
}
