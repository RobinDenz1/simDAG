\name{sim2start_stop}
\alias{sim2start_stop}

\title{
Transform \code{sim_discrete_time} output into the start-stop format
}
\description{
This function transforms the output of the \code{\link{sim_discrete_time}} function into a single \code{data.table} structured in the start-stop format (also known as counting process format). See details.
}
\usage{
sim2start_stop(sim, tte_name, warn=TRUE)
}
\arguments{
  \item{sim}{
An object created with the \code{\link{sim_discrete_time}} function.
  }
  \item{tte_name}{
A single character string containing the name of a node of type \code{time_to_event}. This node will be used as the outcome of interest and the rest of the dataset will be structured around it. This means that the \code{start} and \code{stop} columns will be in respect to the event time of that node.
  }
  \item{warn}{
Whether to print a warning message if the resulting \code{data.table} contains inaccuracies due to not all relevant data being stored. Set to \code{FALSE} to ignore the warning message. See details for more information.
  }
}
\details{
The raw output of the \code{sim_discrete_time} function may be difficult to use for further analysis. Using this function, it is straightforward to transform that ouput to the start-stop format, e.g. a \code{data.table} containing one or multiple rows per person, where each row corresponds a range of time defined by the \code{start} and \code{stop} columns. Some caution needs to be applied when using this function:

\strong{\emph{Usage of the \code{save_states} argument}}:

The levels of detail of the output produced directly correspond to which information was saved when generating the data using the \code{sim_discrete_time} function. If \code{save_states="all"} was used, the output contains correct information about all variables considered in the simulation. If \code{save_states="last"} was used instead, the situation is a little more complicated. In this case, the only time-varying information available is that recorded by the \code{node_time_to_event} function. The output will therefore only be correct for time-fixed variables and \code{tx_nodes} of type \code{"time_to_event"}.

If there are other variables (with \code{type!="time_to_event"}) in \code{tx_nodes} that should be represented in the output of this function, the user needs to set \code{save_states} to \code{"all"} in the original function call to the \code{sim_discrete_time} function.

\strong{\emph{Usage of the \code{save_past_events} argument}}:

If \code{save_states="all"} was used in the creation of the \code{sim} object, the following concerns do not apply (see above). If \code{save_states="last"} was used instead and neither the \code{event_duration} and \code{immunity_duration} parameters of the \code{node_time_to_event} function were set to \code{Inf}, the \code{save_past_events} argument needs to be set to \code{TRUE} in order to obtain correct results.

\strong{\emph{Other formats}:}

Instead of transforming the output to the start-stop format, it may also be transformed to the long format using the \code{\link{sim2long}} function.
}
\value{
Returns a \code{data.table} containing the columns \code{.id} (the unique person identifier), \code{start} (an integer variable encoding the time at which the observation period begins), \code{stop} (an integer variable encoding the time at which the observation period stops) and all other variables created by the \code{sim_discrete_time} function. Whenever a variable changes over time, a new period is defined by the \code{start} and \code{stop} columns.
}
\author{
Robin Denz
}
\examples{
## add some examples
}
