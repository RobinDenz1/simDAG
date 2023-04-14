\name{sim2long}
\alias{sim2long}

\title{
Transform \code{sim_discrete_time} output into the long-format
}
\description{
CURRENTLY MAY NOT WORK AS DESCRIBED

This function transforms the output of the \code{\link{sim_discrete_time}} function into a single \code{data.table} structured in the long-format. See details.
}
\usage{
sim2long(sim, warn=TRUE)
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
The raw output of the \code{sim_discrete_time} function may be difficult to use for further analysis. Using this function, it is straightforward to transform that ouput to the long-format, e.g. a \code{data.table} containing multiple rows per person, where each row corresponds to one point in time. Some caution needs to be applied when using this function:

\strong{\emph{Usage of the \code{save_states} argument}}:

The levels of detail of the output produced directly correspond to which information was saved when generating the data using the \code{sim_discrete_time} function. If \code{save_states="all"} was used, the output contains correct information about all variables considered in the simulation. If \code{save_states="last"} was used instead, the situation is a little more complicated. In this case, the only time-varying information available is that recorded by the \code{node_time_to_event} function. The output will therefore only be correct for time-fixed variables and \code{tx_nodes} of type \code{"time_to_event"}.

If there are other variables (with \code{type!="time_to_event"}) in \code{tx_nodes} that should be represented in the output of this function, the user needs to set \code{save_states} to \code{"all"} in the original function call to the \code{sim_discrete_time} function.

\strong{\emph{Usage of the \code{save_past_events} argument}}:

If \code{save_states="all"} was used in the creation of the \code{sim} object, the following concerns do not apply (see above). If \code{save_states="last"} was used instead and neither the \code{event_duration} and \code{immunity_duration} parameters of the \code{node_time_to_event} function were set to \code{Inf}, the \code{save_past_events} argument needs to be set to \code{TRUE} in order to obtain correct results.

\strong{\emph{Other formats}:}

Instead of transforming the output to the long format, it may also be transformed to the start-stop format using the \code{\link{sim2start_stop}} function.

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
