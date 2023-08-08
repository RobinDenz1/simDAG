\name{sim2data}
\alias{sim2data}

\title{
Transform \code{sim_discrete_time} output into the start-stop, long- or wide-format
}
\description{
This function transforms the output of the \code{\link{sim_discrete_time}} function into a single \code{data.table} structured in the start-stop format (also known as counting process format), the long format (one row per person per point in time) or the wide format (one row per person, one column per point in time for time-varying variables). See details.
}
\usage{
sim2data(sim, to, use_saved_states=sim$save_states=="all",
         as_data_frame=FALSE, check_inputs=TRUE)
}
\arguments{
  \item{sim}{
An object created with the \code{\link{sim_discrete_time}} function.
  }
  \item{to}{
Specifies the format of the output data. Must be one of: \code{"start_stop"}, \code{"long"}, \code{"wide"}.
  }
  \item{use_saved_states}{
Whether the saved simulation states (argument \code{save_states} in \code{sim_discrete_time} function) should be used to construct the resulting data or not. See details.
  }
  \item{as_data_frame}{
Set this argument to \code{TRUE} to return a \code{data.frame} instead of a \code{data.table}.
  }
  \item{check_inputs}{
Whether to perform input checks (\code{TRUE} by default). Prints warning messages if the output may be incorrect due to missing information.
  }
}
\details{
The raw output of the \code{sim_discrete_time} function may be difficult to use for further analysis. Using this function, it is straightforward to transform that output into three different formats, which are described below. Note that some caution needs to be applied when using this function, which is also described below.

\strong{\emph{The start-stop format}:}

The start-stop format (\code{to="start_stop"}), also known as counting process or period format corresponds to a \code{data.table} containing multiple rows per person, where each row corresponds to a period of time in which no variables changed. These intervals are defined by the \code{start} and \code{stop} columns. The \code{start} column gives the time at which the period started, the \code{stop} column denotes the time when the period ended. These intervals are coded to be non-overlapping, meaning that the edges of the periods are included in the period itself. For example, if the respective period is exactly 1 point in time long, \code{start} will be equal to \code{stop}. If non-overlapping periods are desired, the user can simply add 1 to the \code{stop} column.

\strong{\emph{The long format}:}

The long format (\code{to="long"}) corresponds to a \code{data.table} in which there is one row per person per point in time. The unique person identifier is stored in the \code{.id} column and the unique points in time are given in the \code{.time} column.

\strong{\emph{The wide format}:}

The wide format (\code{to="wide"}) corresponds to a \code{data.table} with exactly one row per person and multiple columns per points in time for each time-varying variable. All time-varying variables are coded as their original variable name with an underscore and the time-point appended to the end. For example, the variable \code{sickness} at time-point 3 is named \code{"sickness_3"}.

\strong{\emph{Output with \code{use_saved_states=TRUE}}:}

If \code{use_saved_states=TRUE}, this function will use only the data that is stored in the \code{past_states} list of the \code{sim} object to construct the resulting \code{data.table}. This results in the following behavior, depending on which \code{save_states} option was used in the original \code{sim_discrete_time} function call:

\itemize{
  \item{\code{save_states="all"}: A complete \code{data.table} in the desired format with information for \strong{all observations} at \strong{all points in time} for \strong{all variables} will be created. This is the safest option, but also uses the most RAM and computational time.}
  \item{\code{save_states="at_t"}: A \code{data.table} in the desired format with correct information for \strong{all observations} at the \strong{user specified times} (\code{save_states_at} argument) for \strong{all variables} will be created. The state of the simulation at all other times will be ignored, because it wasn't stored. This may be useful in some scenarios, but is generally discouraged unless you have good reasons to use it. A warning message about this is printed if \code{check_inputs=TRUE}.}
  \item{\code{save_states="last"}: Since only the last state of the simulation was saved, an error message is returned. \strong{No} \code{data.table} is produced.}
}

\strong{\emph{Output with \code{use_saved_states=FALSE}}:}

If \code{use_saved_states=FALSE}, this function will use only the data that is stored in the final state of the simulation (\code{data} object in \code{sim}) and information about \code{node_time_to_event} objects. If all \code{tx_nodes} are \code{time_to_event} nodes or if all the user cares about are the \code{time_to_event} nodes and time-fixed variables, this is the best option.

A \code{data.table} in the desired format with correct information about \code{all observations} at \code{all times} is produced, but only with correct entries for \strong{some time-varying variables}, namely \code{time_to_event} nodes. Note that this information will also only be correct if the user used \code{save_past_events=TRUE} in all \code{time_to_event} nodes. Support for \code{competing_events} nodes will be implemented in the future as well.

The other time-varying variables specified in the \code{tx_nodes} argument will still appear in the output, but it will only be the value that was observed at the last state of the simulation.

\strong{\emph{Optional columns created using a \code{time_to_event} node}:}

When using a time-dependent node of type \code{"time_to_event"} with \code{event_count=TRUE} or \code{time_since_last=TRUE}, the columns created using either argument are \strong{not} included in the output if \code{to="start_stop"}, but will be included if \code{to} is set to either \code{"long"} or \code{"wide"}. The reason for this behavior is that including these columns would lead to nonsense intervals in the start-stop format, but makes sense in the other formats.

\strong{\emph{What about \code{tx_nodes} that are not \code{time_to_event} nodes?}:}

If you want the correct output for all \code{tx_nodes} and one or more of those are not \code{time_to_event} nodes, you will have to use \code{save_states="all"} in the original \code{sim_discrete_time} call. We plan to add support for \code{competing_events} with other \code{save_states} arguments in the near future. Support for arbitrary \code{tx_nodes} will probably take longer.

}
\note{
Using the node names \code{"start"}, \code{"stop"}, \code{".id"}, \code{".time"} or names that are automatically generated by time-dependent nodes of type \code{"time_to_event"} may break this function.
}
\value{
Returns a single \code{data.table} (or \code{data.frame}) containing all simulated variables in the desired format.
}
\author{
Robin Denz
}
\seealso{
\code{\link{sim_discrete_time}}
}
\examples{
library(simDAG)

set.seed(435345)

## exemplary car crash simulation, where the probability for
## a car crash is dependent on the sex, and the probability of death is
## highly increased for 3 days after a car crash happened
prob_car_crash <- function(data) {
  ifelse(data$sex==1, 0.001, 0.01)
}

prob_death <- function(data) {
  ifelse(data$car_crash_event, 0.1, 0.0001)
}

dag <- empty_dag() +
  node("sex", type="rbernoulli", p=0.5) +
  node_td("car_crash", type="time_to_event", prob_fun=prob_car_crash,
          parents="sex") +
  node_td("death", type="time_to_event", prob_fun=prob_death,
          parents="car_crash_event")

# generate some data, only saving the last state
# not a problem here, because the only time-varying nodes are
# time-to-event nodes where the event times are saved
sim <- sim_discrete_time(dag, n_sim=20, max_t=500, save_states="last")

# transform to start-stop format
d_start_stop <- sim2data(sim, to="start_stop")

# transform to long-format
d_long <- sim2data(sim, to="long")

# transform to wide-format
d_wide <- sim2data(sim, to="wide")
}
