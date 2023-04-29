\name{sim2wide}
\alias{sim2wide}

\title{
Transform \code{sim_discrete_time} output into the wide-format
}
\description{
This function transforms the output of the \code{\link{sim_discrete_time}} function into a single \code{data.table} structured in the wide-format. See details.
}
\usage{
sim2wide(sim, use_saved_states=sim$save_states=="all", check_inputs=TRUE)
}
\arguments{
  \item{sim}{
An object created with the \code{\link{sim_discrete_time}} function.
  }
  \item{use_saved_states}{
Whether the saved simulation states (argument \code{save_states} in \code{sim_discrete_time} function) should be used to construct the resulting data or not. See details.
  }
  \item{check_inputs}{
Whether to perform input checks (\code{TRUE} by default). Prints warning messages if the output may be incorrect due to missing information.
  }
}
\details{
This is only a convenience function which first transforms the data created using the \code{sim_discrete_time} function into the long-format and then into the wide-format. Some cautions apply, please see the documentation of the \code{\link{sim2long}} or \code{\link{sim2start_stop}} function for more details.

\strong{\emph{Other formats}:}

Instead of transforming the output to the wide-format, it may also be transformed to the long-format using the \code{\link{sim2long}} function or to the start-stop format using the \code{\link{sim2start_stop}} function.

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
