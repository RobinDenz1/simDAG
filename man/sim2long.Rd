\name{sim2long}
\alias{sim2long}

\title{
Transform \code{sim_discrete_time} output into the long-format
}
\description{
This function transforms the output of the \code{\link{sim_discrete_time}} function into a single \code{data.table} structured in the long-format. See details.
}
\usage{
sim2long(sim, use_saved_states=sim$save_states=="all", check_inputs=TRUE)
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
The raw output of the \code{sim_discrete_time} function may be difficult to use for further analysis. Using this function, it is straightforward to transform that ouput to the long-format, e.g. a \code{data.table} containing multiple rows per person, where each row corresponds to one point in time. Some caution needs to be applied when using this function:

\strong{\emph{Output with \code{use_saved_states=TRUE}}:}

If \code{use_saved_states=TRUE}, this function will use only the data that is stored in the \code{past_states} list of the \code{sim} object to construct the resulting \code{data.table}. This results in the following behavior, depending on which \code{save_states} option was used in the original \code{sim_discrete_time} function call:

\itemize{
  \item{\code{save_states="all"}: A complete \code{data.table} in the long-format with information for \strong{all observations} at \strong{all points in time} for \code{all variables} will be created. This is the safest option, but also uses the most RAM and computational time.}
  \item{\code{save_states="at_t"}: A \code{data.table} in the long-format with correct information for \strong{all observations} at the \strong{user specified times} (\code{save_states_at} argument) for \code{all variables} will be created. The state of the simulation at all other times will be ignored, because it wasn't stored. This may be useful in some scenarios, but is generally discouraged unless you have good reasons to use it. A warning message about this is printed if \code{check_inputs=TRUE}.}
  \item{\code{save_states="last"}: Since only the last state of the simulation was saved, an error message is returned. \strong{No} \code{data.table} is produced.}
}

\strong{\emph{Output with \code{use_saved_states=FALSE}}:}

If \code{use_saved_states=FALSE}, this function will use only the data that is stored in the final state of the simulation (\code{data} object in \code{sim}) and information about \code{node_time_to_event} objects. If all \code{tx_nodes} are \code{time_to_event} nodes or if all the user cares about are the \code{time_to_event} nodes and time-fixed variables, this is the best option.

A \code{data.table} in the long-format with correct information about \code{all observations} at \code{all times} is produced, but only with correct entries for \strong{some time-varying variables}, namely \code{time_to_event} nodes. Note that this information will also only be correct if the user used \code{save_past_events=TRUE} in all \code{time_to_event} nodes. Support for \code{competing_events} nodes will be implemented in the future as well.

The other time-varying variables specified in the \code{tx_nodes} argument will still appear in the output, but it will only be the value that was observed at the last state of the simulation.

\strong{\emph{Other formats}:}

Instead of transforming the output to the long-format, it may also be transformed to the wide-format using the \code{\link{sim2wide}} format or to the start-stop format using the \code{\link{sim2start_stop}} function.

}
\value{
Returns a \code{data.table} containing the columns \code{.id} (the unique person identifier) and \code{.time} (an integer variable encoding the simulation time) and all other variables created by the \code{sim_discrete_time} function in the long format.
}
\author{
Robin Denz
}
\examples{
## add some examples
}
