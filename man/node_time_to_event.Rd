\name{node_time_to_event}
\alias{node_time_to_event}

\title{
Simulate a Time-to-Event Node in Discrete-Time Simulation
}
\description{
This node essentially models a dichotomous time-dependent variable for which the time of the event will be important for later usage. It adds three columns to \code{data}: \code{name_event} (whether the person currently has an event), \code{name_time} (the time at which the current event started) and \code{name_past_event_times} (a string containing the times at which previous events happened). See details.
}
\usage{
node_time_to_event(data, parents, sim_time, name, prob_fun,
                   prob_fun_args=list(), event_duration=0,
                   immunity_duration=event_duration,
                   save_past_events=TRUE, check_inputs=TRUE)
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
The name of the node. This will be used as prefix before the \code{_event}, \code{_time} and \code{_past_event_times} columns.
  }
  \item{prob_fun}{
A function that returns a numeric vector of size \code{nrow(data)} containing only number between 0 and 1. These numbers specify the person-specific probability of experiencing the event modeled by this node at the particular point in time of the simulation. The corresponding event will be generated internally using the \code{\link{rbernoulli}} function.
  }
  \item{prob_fun_args}{
A named list of arguments passed to \code{prob_fun}. Set to \code{list()} if you do not want to pass any arguments.
  }
  \item{event_duration}{
A single number >= 0 specifying how long the event should last.
  }
  \item{immunity_duration}{
A single number >= \code{event_duration} specifying how long the person should be immune to the event after it is over. The count internally starts when the event starts, so in order to use an immunity duration of 10 time units after the event is over \code{event_duration + 10} should be used.
  }
  \item{save_past_events}{
When the event modeled using this node is recurrent (\code{immunity_duration < Inf & event_duration < Inf}), the same person may experience multiple events over the course of the simulation. Those are generally stored in the \code{paste0(name, _past_event_times)} variable using a string for each person. For example, if the person experienced an event at t = 10 and t = 54, the value of that person would be \code{"10 54"}. This can slow down the simulation considerably, so if you are not interested in the timing of previous events this can be turned off by setting this argument to \code{FALSE}.
  }
  \item{check_inputs}{
Whether to perform plausibility checks for the user input or not. Is set to \code{TRUE} by default, but can be set to \code{FALSE} in order to speed things up when using this function in a simulation study or something similar.
  }
}
\details{
When performing discrete-time simulation using the \code{\link{sim_discrete_time}} function, the standard node functions implemented in this package are usually not sufficient because they don't capture the time-dependent nature of some very interesting variables. Often, the variable that should be modelled has some probability of occurring at each point in time. Once it does occurr, it has some kind of influence on other variables for a period of time until it goes back to normal (or doesn't). This could be a car crash, a surgery, a vaccination etc. The \code{time_to_event} node function can be used to model these kinds of nodes in a fairly straightforward fashion.

\strong{\emph{How it Works}}:

At \eqn{t = 1}, this node will be initialized for the first time. It adds three columns to the data: \code{name_event} (whether the person currently has an event), \code{name_time} (the time at which the current event started) and \code{name_past_event_times} (a string containing the times at which previous events happened) where \code{name} is the name of the node. In a nutshell, it simply models the occurrence of some event by calculating the probability of occurence at \eqn{t} and drawing a single bernoulli trial from this probability. If the trial is a "success", the corresponding event column will be set to \code{TRUE}, the time column will be set to the current simulation time \eqn{t} and the column storing the past event times will receive an entry.

The event column will stay \code{TRUE} until the event is over. The duration for that is controlled by the \code{event_duration} parameter. When modeling terminal events such as death, one can simply set this parameter to \code{Inf}, making the event eternal. In many cases it will also be neccessary to implement some kind of immunity after the event, which can be done using the \code{immunity_duration} argument. This effectively sets the probability of another occurence of the event to 0 in the next \code{immunity_duration} time steps. During the immunity duration, the event may be \code{TRUE} (if the event is still ongoing) or \code{FALSE} (if the \code{event_duration} has already passed).

The probability of occurence is calculated using the function provided by the user using the \code{prob_fun} argument. This can be an arbitrary complex function. The only requirement is that it takes \code{data} as a first argument. The columns defined by the \code{parents} argument will be passed to this argument automatically. If it has an argument called \code{sim_time}, the current time of the simulation will automatically be passed to it as well. Any further arguments can be passed using the \code{prob_fun_args} argument. A simple example could be a logistic regression node, in which the probability is calculated as an additive linear combination of the columns defined by \code{parents}. A more complex function could include simulation-time dependent effects, further effects dependent on past event times etc. Examples can be found below and in the vignettes.

\strong{\emph{What can be done with it}}:

This type of node naturally support the implementation of terminal and recurrent events that may be influenced by pretty much anything. By specifying the \code{parents} and \code{prob_fun} arguments correctly, it is possible to create an event type that is dependent on past events of itself or other time-to-event variables and other variables in general. The user can include any amount of these nodes in their simulation. It may also be used to simulate any kind of binary time-dependent variable that one would usually not associate with the name "event" as well. It is very flexible, but it does require the user to do some coding by themselves.

\strong{\emph{What can't be done with it}}:

Currently this function only allows binary events. Categorical event types may be implemented using the \code{\link{node_competing_events}} function, which works in a very similar fashion.

}
\author{
Robin Denz, Katharina Meiszl
}
\examples{
# TODO: Add examples
}
