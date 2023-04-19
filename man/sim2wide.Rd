\name{sim2wide}
\alias{sim2wide}

\title{
Transform \code{sim_discrete_time} output into the wide-format
}
\description{
This function transforms the output of the \code{\link{sim_discrete_time}} function into a single \code{data.table} structured in the wide-format. See details.
}
\usage{
sim2wide(sim, warn=TRUE)
}
\arguments{
  \item{sim}{
An object created with the \code{\link{sim_discrete_time}} function.
  }
  \item{warn}{
Whether to print a warning message if the resulting \code{data.table} contains inaccuracies due to not all relevant data being stored. Set to \code{FALSE} to ignore the warning message. See details for more information.
  }
}
\details{
The raw output of the \code{sim_discrete_time} function may be difficult to use for further analysis. Using this function, it is straightforward to transform that ouput to the wide-format, e.g. a \code{data.table} containing multiple columns per person, where each column corresponds to a variable at a specific point in time and there is only one row per person. Some caution needs to be applied when using this function:

Currently it only works if \code{save_states="all"} was used in the original call to the \code{sim_discrete_time} function. Internally, it calls the \code{\link{sim2long}} function first and then simply uses the fast \code{dcast} function of the \code{data.table} package to transform it into the wide-format. For more details see \code{\link{sim2long}}.

\strong{\emph{Other formats}:}

Instead of transforming the output to the wide-format, it may also be transformed to the long-format using the \code{sim2long} function or to the start-stop format using the \code{\link{sim2start_stop}} function.

}
\value{
Returns a \code{data.table} containing one row per \code{.id} (the unique person identifier) with one column for each time-constant variable and \code{max_t} columns for each time-varying variable.
}
\author{
Robin Denz
}
\examples{
## add some examples
}
