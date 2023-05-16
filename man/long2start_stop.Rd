\name{long2start_stop}
\alias{long2start_stop}

\title{
Transform a \code{data.table} in the long-format to a \code{data.table} in the start-stop format
}
\description{
This function transforms a \code{data.table} in the long-format (one row per person per time point) to a \code{data.table} in the start-stop format (one row per person-specific period in which no variables changed).
}
\usage{
long2start_stop(data, id, time, varying, check_inputs=TRUE)
}
\arguments{
  \item{data}{
A \code{data.table} including data in the long-format.
  }
  \item{id}{
A single character string specifying a unique person identifier included in in \code{data}.
  }
  \item{time}{
A single character string specifying a time variable included in in \code{data} coded as integers starting from 1.
  }
  \item{varying}{
A character vector specifying names of variables included in in \code{data} that may change over time.
  }
  \item{check_inputs}{
Whether to check if the user input is correct or not. Can be turned off by setting it to \code{FALSE} to save computation time.
  }
}
\details{
This function relies on \code{data.table} syntax to make the data transformation as RAM efficient and fast as possible.
}
\value{
Returns a \code{data.table} containing the columns \code{.id} (the unique person identifier), \code{.time} (an integer variable encoding the time) and all other variables included in the input \code{data} in the long format.
}
\author{
Robin Denz
}
\examples{
library(simDAG)
library(data.table)

# generate example data in long format
long <- data.table(.id=rep(seq_len(10), each=5),
                   .time=rep(seq_len(5), 10),
                   A=c(rep(FALSE, 43), TRUE, TRUE, rep(FALSE, 3), TRUE,
                           TRUE),
                   B=FALSE)
setkey(long, .id, .time)

# transform to start-stop format
long2start_stop(data=long, id=".id", time=".time", varying=c("A", "B"))
}
