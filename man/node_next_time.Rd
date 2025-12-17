\name{node_next_time}
\alias{node_next_time}

\title{
Generate the Next Time of an Event in Discrete-Event Simulation
}
\description{
This node essentially models a dichotomous time-dependent variable for which the time of the event will be important for later usage. Can only be used inside of the \code{\link{sim_discrete_event}} function, not outside of it or in other simulation functions. See details.
}
\usage{
node_next_time(data, prob_fun, ..., distr_fun=rtexp,
               distr_fun_args=list(), event_duration=Inf,
               immunity_duration=event_duration)
}
\arguments{
  \item{data}{
A \code{data.table} containing all columns specified by \code{parents}. Similar objects such as \code{data.frame}s are not supported.
  }
  \item{prob_fun}{
A function that returns a numeric vector of size \code{nrow(data)}. These numbers should be used to summarise the effect of the considered covariates per person. The summarised score is then used in the \code{distr_fun} function call that follows internally. For example, when using \code{distr_fun="rtexp"} (default), the \code{prob_fun} should generate the person-specific probability of experiencing the event during 1 time unit. Any function may be used, as long as it has a named argument called \code{data}. Alternatively this argument can be set to a single number, resulting in a fixed summary score being used for every simulated individual at every point in time.
  }
  \item{...}{
An arbitrary amount of additional named arguments passed to \code{prob_fun}. Ignore this if you do not want to pass any arguments. Also ignored if \code{prob_fun} is a single number.
  }
  \item{distr_fun}{
A function that returns the (left-truncated) next time at which the variable turns to \code{TRUE}. Any function that has at least three named arguments \code{n} (the number of times to draw), \code{rate} (the summary score returned by \code{prob_fun}) and \code{l} (the time of left-truncation) may be used. The function additionally needs to be vectorised over both \code{rate} and \code{l}, so that a vector of different values may be supplied. The left-truncation is required, so that it only generated times that are strictly larger than \code{l}. A classic example for such a function is the \code{\link{rtexp}} function (the default). See examples and the associated vignette.
  }
  \item{distr_fun_args}{
A list of named arguments that should be passed to the function specified in the \code{distr_fun} argument.
  }
  \item{event_duration}{
A single number > 0 specifying how long the event should last. During this period, the corresponding variable is set to \code{TRUE}.
  }
  \item{immunity_duration}{
A single number >= \code{event_duration} specifying how long the person should be immune to the event after it is over. The count internally starts when the event starts, so in order to use an immunity duration of 10 time units after the event is over \code{event_duration + 10} should be used. The corresponding variable is set to \code{FALSE} after the \code{event_duration} is up and until the \code{immunity_duration} is over.
  }
}
\details{

This function is the only time-dependent node type that may currently be used when conducting discrete-event simulations using the \code{\link{sim_discrete_event}} function. It is very similar to the \code{\link{node_time_to_event}} function in spirit, as it is used to model a binary variable over time. It is, however, not usable in \code{\link{sim_discrete_time}} calls. Use the \code{\link{node_time_to_event}} function there instead.

\strong{\emph{How it works}}:

At the beginning (\eqn{t = 0}) of the simulation, any variable added using this function is set to \code{FALSE} for all individuals. Then, the function supplied in the \code{prob_fun} argument is applied to all individuals in the current \code{data}, potentially using information from baseline covariates and other time-dependent nodes (the latter of which are all \code{FALSE} at this stage). The obtained summary score is then passed to the \code{distr_fun} in order to generate the time at which the variable changes from \code{FALSE} to \code{TRUE} for each individual.

For example, consider the situation in which only one time-dependent variable is includded. In this case, the simulation time for each individual jumps to the generated event time immediately. The variable is then set to \code{TRUE}. Afterwards, the simulation time jumps until the end of the \code{event_duration} (if that duration is \code{Inf}, the simulation is over). The variable is then set back to \code{FALSE}. Next, the simulation time jumps to the end of the \code{immunity_duration} (again, if this is \code{Inf}, the simulation is over).

With more then one time-dependent variable, the situation is a little more complicated. Consider two time-dependent variables \code{A} and \code{B}. At \eqn{t = 0}, both are \code{FALSE} for every individual. The \code{prob_fun} and subsequently the \code{distr_fun} of both variables are called to generate the time of the next event in each of them. Lets say those are 20 and 42, respectively. The simulation time is then advanced to 20, setting \code{A} to \code{TRUE}. At this point, the \code{prob_fun} and \code{distr_fun} arguments are called again for \code{B}, because \code{B} might be dependent on current values of \code{A}, drawing a new next event time for \code{B}. Cruicially, this time is drawn from a left-truncated \code{distr_fun}, so that it is always larger than the current time of 20. Lets say that new time is 53.

The simulation is then advanced again, but not necessarily to 53. Lets say the \code{event_duration} of \code{A} is only 10. In this case the simulation time is only advanced to 30. \code{A} is then set to \code{FALSE} again and the next time for \code{B} is re-computed using its \code{prob_fun} and \code{distr_fun}. At this point, if the \code{immunity_duration} of \code{A} is not \code{Inf}, the next time for \code{A} is also re-computed, left-truncated on the current simulation time + the \code{immunity_duration}. Again, the time is advanced to the next event and the cycle continues.

This process is repeated until either (1) all variables reach a terminal state, (2) the simulation time for each individual is >= \code{max_t}, (3) a break condition defined by \code{break_if} is reached or (4) no individuals are left after their removal through the \code{remove_if} argument. Otherwise, the simulation runs forever.

\strong{\emph{What can be done with it}}:

This type of node naturally supports the implementation of terminal and recurrent events that may be influenced by baseline variables and other such events over time dynamically. By specifying the \code{parents} and \code{prob_fun} arguments correctly, it is possible to create an event type that is dependent on past events of itself or other time-to-event variables and other variables in general, allowing non-markovian data to be generated. The user can include any amount of these nodes in their simulation. It may also be used to simulate any kind of binary time-dependent variable that one would usually not associate with the name "event" as well.

\strong{\emph{What can't be done with it}}:

Currently this function only allows binary events. Categorical event types or continuous time-dependent variables are currently not supported. The \code{event_duration} and \code{immunity_duration} can also only be fixed for each node, and are not allowed to vary per person.

}
\author{
Robin Denz
}
\value{
This function is never actually called. It is only used so that the node type \code{"next_time"} can be safely specified in \code{\link{node_td}} calls. It does not make sense to ever use it outside a \code{\link{node_td}} call, as it always returns \code{NULL}.
}
\seealso{
\code{\link{empty_dag}}, \code{\link{node_td}}, \code{\link{sim_discrete_event}}, \code{\link{node_time_to_event}}
}
\examples{

library(simDAG)

## a simple terminal time-to-event node, with a constant probability of
## occurrence, independent of any other variable
dag <- empty_dag() +
  node_td("death", type="next_time", prob_fun=0.0001,
          event_duration=Inf)

## a simple recurrent time-to-event node with a constant probability of
## occurrence, independent of any other variable
dag <- empty_dag() +
  node_td("car_crash", type="next_time", prob_fun=0.001, event_duration=1)

## a next-time node with a probability function dependent on a
## time-fixed variable
prob_car_crash <- function(data) {
  ifelse(data$sex==1, 0.001, 0.01)
}

dag <- empty_dag() +
  node("sex", type="rbernoulli", p=0.5) +
  node_td("car_crash", type="next_time", prob_fun=prob_car_crash,
          parents="sex")

## a little more complex car crash simulation, where the probability for
## a car crash is dependent on the sex, and the probability of death is
## highly increased for 3 days after a car crash happened
prob_car_crash <- function(data) {
  ifelse(data$sex==1, 0.001, 0.01)
}

prob_death <- function(data) {
  ifelse(data$car_crash, 0.1, 0.0001)
}

dag <- empty_dag() +
  node("sex", type="rbernoulli", p=0.5) +
  node_td("car_crash", type="next_time", prob_fun=prob_car_crash,
          parents="sex", event_duration=3) +
  node_td("death", type="next_time", prob_fun=prob_death,
          parents="car_crash", event_duration=Inf)

# use the sim_discrete_time function to simulate data from one of these DAGs:
sim <- sim_discrete_event(dag, n_sim=20, max_t=500)

## more examples can be found in the vignettes of this package
}
