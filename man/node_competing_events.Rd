\name{node_competing_events}
\alias{node_competing_events}

\title{
Simulate a Time-to-Event Node with Multiple Mutually Exclusive Events in Discrete-Time Simulation
}
\description{
This node essentially models a categorical time-dependent variable for which the time and the type of the event will be important for later usage. It adds four columns to \code{data}: \code{name_event} (whether the person currently has an event), \code{name_time} (the time at which the current event started), \code{name_past_event_times} (a string containing the times at which previous events happened) and \code{name_past_event_kind} (a string containing the kind of previous events happened). See details.
}
\usage{
node_competing_events(data, parents, sim_time, name, prob_fun,
                      prob_fun_args=list(), event_duration=c(0, 0),
                      immunity_duration=max(event_duration),
                      save_past_events=TRUE)
}
\arguments{
  \item{data}{
A \code{data.frame} containing all columns specified by \code{parents}.
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
A function that returns a numeric matrix with \code{nrow(data)} rows and one column storing probabilities of occurence for each possible event type plus a column for no events. For example, if there are two possible events such as recurrence and death, the matrix would need to contain three columns. The first storing the probability of no-event and the other two columns storing probabilities for recurrence and death per person. Since the numbers are probabilities, the matrix should only contain numbers between 0 and 1 that sum to 1 in each row. These numbers specify the person-specific probability of experiencing the events modeled by this node at the particular point in time of the simulation. The corresponding event will be generated internally using the \code{\link{rcategorical}} function.
  }
  \item{prob_fun_args}{
A named list of arguments passed to \code{prob_fun}. Set to \code{list()} (default) if you do not want to pass any arguments.
  }
  \item{event_duration}{
A numeric vector containing one positive integer for each type of event of interest, specifying how long that event should last. For example, if we are interested in modelling the time to a cardiovascular event with death as competing event, this argument would need 2 entries. One would specify the duration of the cardiovascular event and the other would be \code{Inf} (because death is a terminal event).
  }
  \item{immunity_duration}{
A single number >= \code{max(event_duration)} specifying how long the person should be immune to all events after experiencin one. The count internally starts when the event starts, so in order to use an immunity duration of 10 time units after the event is over \code{max(event_duration) + 10} should be used.
  }
  \item{save_past_events}{
When the event modeled using this node is recurrent (\code{immunity_duration < Inf & event_duration < Inf}), the same person may experience multiple events over the course of the simulation. Those are generally stored in the \code{paste0(name, _past_event_times)} variable and the \code{paste0(name, _past_event_kind)} variable using a string for each person. For example, if the person experienced event "1"" at t = 10 and event "2" at t = 54, the value of that person would be \code{"10 54"} in the \code{paste0(name, _past_event_times)} column and \code{"1 2"} in the \code{paste0(name, _past_event_kind)} column. This can slow down the simulation considerably, so if you are not interested in the timing of previous events this can be turned off by setting this argument to \code{FALSE}.
  }
}
\details{
When performing discrete-time simulation using the \code{\link{sim_discrete_time}} function, the standard node functions implemented in this package are usually not sufficient because they don't capture the time-dependent nature of some very interesting variables. Often, the variable that should be modelled has some probability of occurring at each point in time. Once it does occurr, it has some kind of influence on other variables for a period of time until it goes back to normal (or doesn't). This could be a car crash, a surgery, a vaccination etc. The \code{\link{node_time_to_event}} node function can be used to model these kinds of nodes in a fairly straightforward fashion.

This function is an extended version of the \code{\link{node_time_to_event}} function. Instead of simulating a binary event, it can generate multiple competing events, where the occurence of one event at time \eqn{t} is mutually exclusive with the occurence of an other event at that time. In other words, multiple events are possible, but only one can occur at a time.

\strong{\emph{How it Works}}:

At \eqn{t = 1}, this node will be initialized for the first time. It adds four columns to the data: \code{name_event} (whether the person currently has an event), \code{name_time} (the time at which the current event started), \code{name_past_event_times} (a string containing the times at which previous events happened) and \code{name_past_event_kind} (a string containing the kind of previous events happened) where \code{name} is the name of the node. In a nutshell, it simply models the occurrence of some event by calculating the probability of occurence at \eqn{t} and drawing a single multinomial trial from this probability. If the trial is a "success", the corresponding event column will be set to the drawn event type (described using integers, where 0 is no event and all other events are numbered consecutively), the time column will be set to the current simulation time \eqn{t} and the columns storing the past event times and types will receive an entry.

The event column will stay at its new integer value until the event is over. The duration for that is controlled by the \code{event_duration} parameter. When modeling terminal events such as death, one can simply set this parameter to \code{Inf}, making the event eternal. In many cases it will also be neccessary to implement some kind of immunity after the event, which can be done using the \code{immunity_duration} argument. This effectively sets the probability of another occurence of the event to 0 in the next \code{immunity_duration} time steps. During the immunity duration, the event may be \code{> 0} (if the event is still ongoing) or \code{0} (if the \code{event_duration} for that event type has already passed).

The probability of occurence is calculated using the function provided by the user using the \code{prob_fun} argument. This can be an arbitrary complex function. The only requirement is that it takes \code{data} as a first argument. The columns defined by the \code{parents} argument will be passed to this argument automatically. If it has an argument called \code{sim_time}, the current time of the simulation will automatically be passed to it as well. Any further arguments can be passed using the \code{prob_fun_args} argument. A simple example could be a multinomial logistic regression node, in which the probabilities are calculated as an additive linear combination of the columns defined by \code{parents}. A more complex function could include simulation-time dependent effects, further effects dependent on past event times etc. Examples can be found below and in the vignettes.

\strong{\emph{What can be done with it}}:

This type of node naturally support the implementation of competing events, where some may be terminal or recurrent in nature and may be influenced by pretty much anything. By specifying the \code{parents} and \code{prob_fun} arguments correctly, it is possible to create an event type that is dependent on past events of itself or other time-to-event variables and other variables in general. The user can include any amount of these nodes in their simulation. It may also be used to simulate any kind of binary time-dependent variable that one would usually not associate with the name "event" as well. It is very flexible, but it does require the user to do some coding by themselves.

\strong{\emph{What can't be done with it}}:

This function may only be used to generate competing events, meaning that the occurence of event 1 at \eqn{t = 1} makes it impossible for event 2 at \eqn{t = 1} to occur. If the user wants to generate multiple events that are not mutually exclusive, he or she may add multiple \code{\link{node_time_to_event}} based nodes to the \code{tx_nodes} argument of the \code{\link{sim_discrete_time}} function.

}
\author{
Robin Denz
}
\examples{
# TODO: Add examples
}
