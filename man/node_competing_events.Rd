\name{node_competing_events}
\alias{node_competing_events}

\title{
Simulate a Time-to-Event Node with Multiple Mutually Exclusive Events in Discrete-Time Simulation
}
\description{
This node essentially models a categorical time-dependent variable for which the time and the type of the event will be important for later usage. It adds two columns to \code{data}: \code{name_event} (which type of event the person is currently experiencing) and \code{name_time} (the time at which the current event started). Can only be used inside of the \code{\link{sim_discrete_time}} function, not outside of it. Past events and their kind are stored in two lists. See details.
}
\usage{
node_competing_events(data, parents, sim_time, name,
                      prob_fun, ..., event_duration=c(1, 1),
                      immunity_duration=max(event_duration),
                      save_past_events=TRUE, check_inputs=TRUE,
                      envir)
}
\arguments{
  \item{data}{
A \code{data.table} containing all columns specified by \code{parents}. Similar objects such as \code{data.frame}s are not supported.
  }
  \item{parents}{
A character vector specifying the names of the parents that this particular child node has.
  }
  \item{sim_time}{
The current time of the simulation.
  }
  \item{name}{
The name of the node. This will be used as prefix before the \code{_event}, \code{_time}, \code{_past_event_times} and \code{_past_event_kind} columns.
  }
  \item{prob_fun}{
A function that returns a numeric matrix with \code{nrow(data)} rows and one column storing probabilities of occurrence for each possible event type plus a column for no events. For example, if there are two possible events such as recurrence and death, the matrix would need to contain three columns. The first storing the probability of no-event and the other two columns storing probabilities for recurrence and death per person. Since the numbers are probabilities, the matrix should only contain numbers between 0 and 1 that sum to 1 in each row. These numbers specify the person-specific probability of experiencing the events modeled by this node at the particular point in time of the simulation. The corresponding event will be generated internally using the \code{\link{rcategorical}} function.
  }
  \item{...}{
An arbitrary number of additional named arguments passed to \code{prob_fun}. Ignore this if you do not want to pass any arguments.
  }
  \item{event_duration}{
A numeric vector containing one positive integer for each type of event of interest, specifying how long that event should last. For example, if we are interested in modelling the time to a cardiovascular event with death as competing event, this argument would need 2 entries. One would specify the duration of the cardiovascular event and the other would be \code{Inf} (because death is a terminal event).
  }
  \item{immunity_duration}{
A single number >= \code{max(event_duration)} specifying how long the person should be immune to all events after experiencing one. The count internally starts when the event starts, so in order to use an immunity duration of 10 time units after the event is over \code{max(event_duration) + 10} should be used.
  }
  \item{save_past_events}{
When the event modeled using this node is recurrent (\code{immunity_duration < Inf & any(event_duration < Inf)}), the same person may experience multiple events over the course of the simulation. Those are generally stored in the \code{ce_past_events} list and \code{ce_past_causes} list which are included in the output of the \code{sim_discrete_time} function. This extends the runtime and increases RAM usage, so if you are not interested in the timing of previous events or if you are using \code{save_states="all"} this functionality can be turned off by setting this argument to \code{FALSE}
  }
  \item{check_inputs}{
Whether to perform plausibility checks for the user input or not. Is set to \code{TRUE} by default, but can be set to \code{FALSE} in order to speed things up when using this function in a simulation study or something similar.
  }
  \item{envir}{
Only used internally to efficiently store the past event times. Cannot be used by the user.
  }
}
\details{
When performing discrete-time simulation using the \code{\link{sim_discrete_time}} function, the standard node functions implemented in this package are usually not sufficient because they don't capture the time-dependent nature of some very interesting variables. Often, the variable that should be modelled has some probability of occurring at each point in time. Once it does occur, it has some kind of influence on other variables for a period of time until it goes back to normal (or doesn't). This could be a car crash, a surgery, a vaccination etc. The \code{\link{node_time_to_event}} node function can be used to model these kinds of nodes in a fairly straightforward fashion.

This function is an extended version of the \code{\link{node_time_to_event}} function. Instead of simulating a binary event, it can generate multiple competing events, where the occurrence of one event at time \eqn{t} is mutually exclusive with the occurrence of an other event at that time. In other words, multiple events are possible, but only one can occur at a time.

\strong{\emph{How it Works}}:

At \eqn{t = 1}, this node will be initialized for the first time. It adds two columns to the data: \code{name_event} (whether the person currently has an event) and \code{name_time} (the time at which the current event started) where \code{name} is the name of the node. Additionally, it adds a list with \code{max_t} entries to the \code{ce_past_events} list returned by the \code{sim_discrete_time} function, which records which individuals experienced a new event at each point in time. The \code{ce_past_causes} list additionally records which kind of event happened at that time.

In a nutshell, it simply models the occurrence of some event by calculating the probability of occurrence at \eqn{t} and drawing a single multinomial trial from this probability. If the trial is a "success", the corresponding event column will be set to the drawn event type (described using integers, where 0 is no event and all other events are numbered consecutively), the time column will be set to the current simulation time \eqn{t} and the columns storing the past event times and types will receive an entry.

The event column will stay at its new integer value until the event is over. The duration for that is controlled by the \code{event_duration} parameter. When modeling terminal events such as death, one can simply set this parameter to \code{Inf}, making the event eternal. In many cases it will also be necessary to implement some kind of immunity after the event, which can be done using the \code{immunity_duration} argument. This effectively sets the probability of another occurrence of the event to 0 in the next \code{immunity_duration} time steps. During the immunity duration, the event may be \code{> 0} (if the event is still ongoing) or \code{0} (if the \code{event_duration} for that event type has already passed).

The probability of occurrence is calculated using the function provided by the user using the \code{prob_fun} argument. This can be an arbitrary complex function. The only requirement is that it takes \code{data} as a first argument. The columns defined by the \code{parents} argument will be passed to this argument automatically. If it has an argument called \code{sim_time}, the current time of the simulation will automatically be passed to it as well. Any further arguments can be passed using the \code{prob_fun_args} argument. A simple example could be a multinomial logistic regression node, in which the probabilities are calculated as an additive linear combination of the columns defined by \code{parents}. A more complex function could include simulation-time dependent effects, further effects dependent on past event times etc. Examples can be found below and in the vignettes.

\strong{\emph{What can be done with it}}:

This type of node naturally support the implementation of competing events, where some may be terminal or recurrent in nature and may be influenced by pretty much anything. By specifying the \code{parents} and \code{prob_fun} arguments correctly, it is possible to create an event type that is dependent on past events of itself or other time-to-event variables and other variables in general. The user can include any amount of these nodes in their simulation. It may also be used to simulate any kind of binary time-dependent variable that one would usually not associate with the name "event" as well. It is very flexible, but it does require the user to do some coding by themselves.

\strong{\emph{What can't be done with it}}:

This function may only be used to generate competing events, meaning that the occurrence of event 1 at \eqn{t = 1} makes it impossible for event 2 at \eqn{t = 1} to occur. If the user wants to generate multiple events that are not mutually exclusive, he or she may add multiple \code{\link{node_time_to_event}} based nodes to the \code{dag} argument of the \code{\link{sim_discrete_time}} function.

In fact, a competing events node may be simulated using multiple calls to the \code{\link{node_time_to_event}} based nodes as well, by defining the \code{prob_fun} argument of these nodes in such a way that the occurrence of event A makes the occurrence of event B impossible. This might actually be easier to implement in some situations, because it doesn't require the user to manually define a probability function that outputs a matrix of subject-specific probabilities.

}
\author{
Robin Denz
}
\value{
Returns a \code{data.table} containing the updated columns of the node.
}
\note{
This function cannot be called outside of the \code{\link{sim_discrete_time}} function. It only makes sense to use it as a type in a \code{\link{node_td}} function call, as described in the documentation and vignettes.
}
\seealso{
\code{\link{empty_dag}}, \code{\link{node}}, \code{\link{node_td}}, \code{\link{sim_from_dag}}, \code{\link{sim_discrete_time}}
}
\examples{
library(simDAG)

## a competing_events node with only terminal events, all with a constant
## probability of occurrence, independent of any other variable
prob_death_illness <- function(data) {

  # simply repeat the same probabilities for everyone
  n <- nrow(data)
  p_mat <- matrix(c(rep(0.9, n), rep(0.005, n), rep(0.005, n)),
                  byrow = FALSE, ncol=3)

  return(p_mat)
}

dag <- empty_dag() +
  node_td("death_illness", type="competing_events", prob_fun=prob_death_illness,
          event_duration=c(Inf, Inf))

## making one of the event-types terminal and the other recurrent
dag <- empty_dag() +
  node_td("death_illness", type="competing_events", prob_fun=prob_death_illness,
          event_duration=c(15, Inf))

## call the sim_discrete_time function to generate data from it
sim <- sim_discrete_time(dag, n_sim=100, max_t=500)

## more examples on how to use the sim_discrete_time function can be found
## in the documentation page of the node_time_to_event function and
## in the package vignettes
}
