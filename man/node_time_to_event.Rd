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
                   immunity_duration=event_duration + 0,
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
The name of the node. This will be used as prefix before the \code{_event}, \code{_time} and \code{_past_event_times} columns.
  }
  \item{prob_fun}{
A function that returns a numeric vector of size \code{nrow(data)} containing only number between 0 and 1. These numbers specify the person-specific probability of experiencing the event modeled by this node at the particular point in time of the simulation. The corresponding event will be generated internally using the \code{rbernoulli} function.
  }
  \item{prob_fun_args}{
A named list of arguments passed to \code{prob_fun}. Set to \code{list()} if you do not want to pass any arguments.
  }
  \item{event_duration}{
A single number >= 0 specifying how long the event should last.
  }
  \item{immunity_duration}{
A single number >= event_duration specifying how long the person should be immune to the event after it is over. The count internally starts when the event starts, so in order to use an immunity duration of 10 time units after the event is over \code{event_duration + 10} should be used.
  }
  \item{save_past_events}{
When the event modeled using this node is recurrent (\code{immunity_duration < Inf & event_duration < Inf}), the same person may experience multiple events over the course of the simulation. Those are generally stored in the \code{paste0(name, _past_event_times)} variable using a string for each person. For example, if the person experienced an event at t = 10 and t = 54, the value of that person would be \code{"10 54"}. This can slow down the simulation considerably, so if you are not interested in the timing of previous events this can be turned off by setting this argument to \code{FALSE}.
  }
}
\details{
More explanation
}
\examples{
# TODO: Add examples
}
