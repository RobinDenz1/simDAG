\name{long2start_stop}
\alias{long2_start_stop}

\title{
Transform a \code{data.table} in the long-format to a \code{data.table} in the start-stop format
}
\description{
This function transforms a \code{data.table} in the long-format (one row per person per time point) to a \code{data.table} in the start-stop format (one row per person-specific period in which no variables changed).
}
\usage{
long2start_stop(data, id, time, varying)
}
\arguments{
  \item{data}{
A \code{data.table} including data in the long-format.
  }
  \item{id}{
A single character string specifying a unique person identifier included in in \code{data}.
  }
  \item{time}{
A single character string specifying a time variable included in in \code{data} coded as integers.
  }
  \item{varying}{
A character vector specifying names of variables included in in \code{data} that may change over time.
  }
}
\details{
This function relies on \code{data.table} syntax to make the data transformation as RAM efficient and fast as possible.
}
\value{
Returns a \code{data.table} containing the columns \code{.id} (the unique person identifier) and \code{.simulation_time} (an integer variable encoding the simulation time) and all other variables created by the \code{sim_discrete_time} function in the long format.
}
\author{
Robin Denz
}
\examples{
## add some examples
}
