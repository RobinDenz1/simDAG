\name{node_time_to_event}
\alias{node_time_to_event}

\title{
Simulate a Time-to-Event Node in Discrete-Time Simulation
}
\description{
This node essentially models a dichotomous time-dependent variable for which the time of the event will be important for later usage. It adds two columns to \code{data}: \code{name_event} (whether the person currently has an event) and \code{name_time} (the time at which the current event started). Past events are stored in a list. Can only be used inside of the \code{\link{sim_discrete_time}} function, not outside of it. See details.
}
\usage{
node_time_to_event(data, parents, sim_time, name,
                   prob_fun, ..., event_duration=1,
                   immunity_duration=event_duration,
                   time_since_last=FALSE, event_count=FALSE,
                   save_past_events=TRUE, check_inputs=TRUE,
                   envir)
}
\arguments{
  \item{data}{
A \code{data.table} containing all columns specified by \code{parents}. Similar objects such as \code{data.frame}s are not supported.
  }
  \item{parents}{
A character vector specifying the names of the parents that this particular child node has. Those child nodes should be valid column names in \code{data}. Because the state of this variable is by definition dependent on its previous states, the columns produced by this function will automatically be considered its parents without the user having to manually specify this.
  }
  \item{sim_time}{
The current time of the simulation.
  }
  \item{name}{
The name of the node. This will be used as prefix before the \code{_event}, \code{_time} columns. If the \code{time_since_last} or \code{event_count} arguments are set to \code{TRUE}, this will also be used as prefix for those respective columns.
  }
  \item{prob_fun}{
A function that returns a numeric vector of size \code{nrow(data)} containing only numbers between 0 and 1. These numbers specify the person-specific probability of experiencing the event modeled by this node at the particular point in time of the simulation. The corresponding event will be generated internally using the \code{\link{rbernoulli}} function. The function needs to have a named argument called \code{data}. If the function has an argument named \code{sim_time}, the current simulation time will also be passed to this function automatically, allowing time-dependent probabilities to be generated. Alternatively this argument can be set to a single number (between 0 and 1), resulting in a fixed probability of occurrence for every simulated individual at every point in time.
  }
  \item{...}{
An arbitrary amount of additional named arguments passed to \code{prob_fun}. Ignore this if you do not want to pass any arguments. Also ignored if \code{prob_fun} is a single number.
  }
  \item{event_duration}{
A single number > 0 specifying how long the event should last. The point in time at which an event occurs also counts into this duration. For example, if an event occurs at \eqn{t = 2} and it has a duration of 3, the event will be set to \code{TRUE} on \eqn{t \in \{2, 3, 4\}}. Therefore, all events must have a duration of at least 1 unit (otherwise they never happened).
  }
  \item{immunity_duration}{
A single number >= \code{event_duration} specifying how long the person should be immune to the event after it is over. The count internally starts when the event starts, so in order to use an immunity duration of 10 time units after the event is over \code{event_duration + 10} should be used.
  }
  \item{time_since_last}{
Either \code{TRUE} or \code{FALSE} (default), indicating whether an additional column should be generated that tracks the number of time units since the individual had its last event onset. For example, if the individual experienced a single event at \eqn{t = 10}, this column would be \code{NA} before time 10, 0 at time 10 and increased by 1 at each point in time. If another event happens, the time is set to 0 again. The column is named \code{paste0(name, "_time_since_last")}. The difference to the column ending with \code{"_time"} is that this column will not be set to \code{NA} again if the \code{immunity_duration} is over. It keeps counting until the end of the simulation, which may be useful when constructing event-time dependent probability functions.
  }
  \item{event_count}{
Either \code{TRUE} or \code{FALSE} (default), indicating whether an additional column should be generated that tracks the number of events the individual has already experienced. This column is 0 for all individuals at t = 0. Each time a new event occurs, the counter is increased by one. Note that only new events increase this counter. For example, an individual with an event at t = 10 that has an \code{event_duration} of 15 will have a value of 0 before t = 10, and will have a value of 1 at t = 10 and afterwards. The column will be named \code{paste0(name, "_event_count")}.
  }
  \item{save_past_events}{
When the event modeled using this node is recurrent (\code{immunity_duration < Inf & event_duration < Inf}), the same person may experience multiple events over the course of the simulation. Those are generally stored in the \code{tte_past_events} list which is included in the output of the \code{sim_discrete_time} function. This extends the runtime and increases RAM usage, so if you are not interested in the timing of previous events or if you are using \code{save_states="all"} this functionality can be turned off by setting this argument to \code{FALSE}.
  }
  \item{check_inputs}{
Whether to perform plausibility checks for the user input or not. Is set to \code{TRUE} by default, but can be set to \code{FALSE} in order to speed things up when using this function in a simulation study or something similar.
  }
  \item{envir}{
Only used internally to efficiently store the past event times. Cannot be used by the user.
  }
}
\details{
When performing discrete-time simulation using the \code{\link{sim_discrete_time}} function, the standard node functions implemented in this package are usually not sufficient because they don't capture the time-dependent nature of some very interesting variables. Often, the variable that should be modelled has some probability of occurring at each point in time. Once it does occur, it has some kind of influence on other variables for a period of time until it goes back to normal (or doesn't). This could be a car crash, a surgery, a vaccination etc. The \code{time_to_event} node function can be used to model these kinds of nodes in a fairly straightforward fashion.

\strong{\emph{How it Works}}:

At \eqn{t = 1}, this node will be initialized for the first time. It adds two columns to the data: \code{name_event} (whether the person currently has an event) and \code{name_time} (the time at which the current event started) where \code{name} is the name of the node. Additionally, it adds a list with \code{max_t} entries to the \code{tte_past_events} list returned by the \code{sim_discrete_time} function, which records which individuals experienced a new event at each point in time.

In a nutshell, it simply models the occurrence of some event by calculating the probability of occurrence at \eqn{t} and drawing a single bernoulli trial from this probability. If the trial is a "success", the corresponding event column will be set to \code{TRUE}, the time column will be set to the current simulation time \eqn{t} and the column storing the past event times will receive an entry.

The \code{_event} column will stay \code{TRUE} until the event is over. The duration for that is controlled by the \code{event_duration} parameter. When modeling terminal events such as death, one can simply set this parameter to \code{Inf}, making the event eternal. In many cases it will also be necessary to implement some kind of immunity after the event, which can be done using the \code{immunity_duration} argument. This effectively sets the probability of another occurrence of the event to 0 in the next \code{immunity_duration} time steps. During the immunity duration, the event may be \code{TRUE} (if the event is still ongoing) or \code{FALSE} (if the \code{event_duration} has already passed). The \code{_time} column is similarly set to the time of occurrence of the event and reset to \code{NA} when the \code{immunity_duration} is over.

The probability of occurrence is calculated using the function provided by the user using the \code{prob_fun} argument. This can be an arbitrary complex function. The only requirement is that it takes \code{data} as a first argument. The columns defined by the \code{parents} argument will be passed to this argument automatically. If it has an argument called \code{sim_time}, the current time of the simulation will automatically be passed to it as well. Any further arguments can be passed using the \code{...} syntax. A simple example could be a logistic regression node, in which the probability is calculated as an additive linear combination of the columns defined by \code{parents}. A more complex function could include simulation-time dependent effects, further effects dependent on past event times etc. Examples can be found below and in the vignettes.

\strong{\emph{How it is Used}}:

This function should never be called directly by the user. Instead, the user should define a DAG object using the \code{\link{empty_dag}} and \code{\link{node_td}} functions and set the \code{type} argument inside of a \code{node_td} call to \code{"time_to_event"}. This DAG can be passed to the \code{\link{sim_discrete_time}} function to generate the desired data. Many examples and more explanations are given below and in the vignettes of this package.

\strong{\emph{What can be done with it}}:

This type of node naturally supports the implementation of terminal and recurrent events that may be influenced by pretty much anything. By specifying the \code{parents} and \code{prob_fun} arguments correctly, it is possible to create an event type that is dependent on past events of itself or other time-to-event variables and other variables in general. The user can include any amount of these nodes in their simulation. It may also be used to simulate any kind of binary time-dependent variable that one would usually not associate with the name "event" as well. It is very flexible, but it does require the user to do some coding by themselves (e.g. creating a suitable function for the \code{prob_fun} argument).

\strong{\emph{What can't be done with it}}:

Currently this function only allows binary events. Categorical event types may be implemented using the \code{\link{node_competing_events}} function, which works in a very similar fashion.

}
\author{
Robin Denz, Katharina Meiszl
}
\value{
Returns a \code{data.table} containing at least two columns with updated values of the node.
}
\note{
This function cannot be called outside of the \code{\link{sim_discrete_time}} function. It only makes sense to use it as a type in a \code{\link{node_td}} function call, as described in the documentation and vignettes.
}
\seealso{
\code{\link{empty_dag}}, \code{\link{node_td}}, \code{\link{sim_discrete_time}}
}
\examples{
library(simDAG)

## a simple terminal time-to-event node, with a constant probability of
## occurrence, independent of any other variable
dag <- empty_dag() +
  node_td("death", type="time_to_event", prob_fun=0.0001,
          event_duration=Inf)

## a simple recurrent time-to-event node with a constant probability of
## occurrence, independent of any other variable
dag <- empty_dag() +
  node_td("car_crash", type="time_to_event", prob_fun=0.001, event_duration=1)

## a time-to-event node with a time-dependent probability function that
## has an additional argument
prob_car_crash <- function(data, sim_time, base_p) {
  return(base_p + sim_time * 0.0001)
}

dag <- empty_dag() +
  node_td("car_crash", type="time_to_event", prob_fun=prob_car_crash,
          event_duration=1, base_p=0.0001)

## a time-to-event node with a probability function dependent on a
## time-fixed variable
prob_car_crash <- function(data) {
  ifelse(data$sex==1, 0.001, 0.01)
}

dag <- empty_dag() +
  node("sex", type="rbernoulli", p=0.5) +
  node_td("car_crash", type="time_to_event", prob_fun=prob_car_crash,
          parents="sex")

## a little more complex car crash simulation, where the probability for
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

# use the sim_discrete_time function to simulate data from one of these DAGs:
sim <- sim_discrete_time(dag, n_sim=20, max_t=500)

## more examples can be found in the vignettes of this package
}
