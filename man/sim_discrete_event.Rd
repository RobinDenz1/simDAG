\name{sim_discrete_event}
\alias{sim_discrete_event}

\title{
Simulate Data from a \code{DAG} with Time-Dependent Variables in Continuous Time
}
\description{
EXPERIMENTAL: Similar to \code{\link{sim_discrete_time}}, this function allows users to generate complex data with time-varying variables from a \code{DAG} defined using \code{\link{node}} and \code{\link{node_td}} calls. In contrast to \code{\link{sim_discrete_time}}, time is modelled as a continuous variable using a discrete-event simulation approach. See details.
}
\usage{
sim_discrete_event(dag, n_sim=NULL, t0_sort_dag=FALSE,
                   t0_data=NULL, t0_transform_fun=NULL,
                   t0_transform_args=list(),
                   max_t=Inf, remove_if, break_if,
                   max_loops=10000, redraw_at_t=NULL,
                   allow_ties=FALSE,
                   censor_at_max_t=FALSE, target_event=NULL,
                   keep_only_first=FALSE, check_inputs=TRUE)
}
\arguments{
  \item{dag}{
A \code{DAG} object created using the \code{\link{empty_dag}} function with \code{\link{node_td}} calls added to it (see details and examples). If the \code{dag} contains root nodes and child nodes which are time-fixed (those who were added using \code{\link{node}} calls), data according to this DAG will be generated for time = 0. That data will then be used as starting data for the following simulation. Alternatively, the user can specify the \code{t0_data} argument directly. In either case, the supplied \code{dag} needs to contain at least one time-dependent node of type \code{"next_time"}, added using the \code{\link{node_td}} function. Other time-dependent node types are currently not supported.
  }
  \item{n_sim}{
A single number specifying how many observations should be generated. If a \code{data.table} is supplied to the \code{t0_data} argument, this argument is ignored. The sample size will then correspond to the number of rows in \code{t0_data}.
  }
  \item{t0_sort_dag}{
Corresponds to the \code{sort_dag} argument in the \code{\link{sim_from_dag}} function. Ignored if \code{t0_data} is specified.
  }
  \item{t0_data}{
An optional \code{data.table} like object (also accepts a \code{data.frame}, \code{tibble} etc.) containing values for all relevant variables at \eqn{t = 0}. This dataset will then be transformed over time according to the nodes specified using \code{\link{node_td}} calls in \code{dag}. Alternatively, data for \eqn{t = 0} may be generated automatically by this function if standard \code{\link{node}} calls were added to the \code{dag}.
  }
  \item{t0_transform_fun}{
An optional function that takes the data created at \eqn{t = 0} as the first argument. The function will be applied to the starting data and its output will replace the \code{data.table}. Can be used to perform arbitrary data transformations after the starting data was created. Set to \code{NULL} (default) to not use this functionality.
  }
  \item{t0_transform_args}{
A named list of additional arguments passed to the \code{t0_transform_fun}. Ignored if \code{t0_transform_fun=NULL}.
  }
  \item{max_t}{
A single number specifying the time needs to be reached for all individuals for the simulation to be terminated. This can be set to \code{Inf}, if the end of the simulation can be determinated through other means (e.g. the \code{remove_if} or \code{break_if} arguments, or when all variables have an event duration or immunity duration that is infinite).
  }
  \item{remove_if}{
A condition that will be evaluated directly on the generated \code{data.table} after each jump to the next event. All rows for which the condition is \code{TRUE} are removed from the \code{data} at this point in time. The condition may contain names of any variable that were generated. If all rows are removed through this condition, the simulation stops early. This argument may be useful to save computation time, if a large number of variables or many state changed need to be considered and the user only cares about the first time a condition is met for some individuals. Keep this argument unspecified (default) to not use this functionality.
  }
  \item{break_if}{
A condition that will be evaluated after each jump to the next event (but after subsetting, if \code{remove_if} was specified). If the condition is met, the simulation stops early. Contrary to the \code{remove_if} argument, this condition should return exactly one \code{TRUE} or \code{FALSE} value and is not directly evaluated on the \code{data}. To use variables generated in the simulation in this condition, users should use the \code{$} syntax (e.g. use \code{data$X} instead of just \code{X}). Keep this argument unspecified (default) to not use this functionality.
  }
  \item{max_loops}{
A single positive number, specifying the maximum amount of loops the simulation is allowed to run before it terminates. This argument exists so that if all of \code{max_t}, \code{remove_if} and \code{break_if} fail to terminate the simulation eventually, the code does not run forever. In nearly all cases it is, however, preferable to end the simulation using one of the other three arguments. A warning message is therefore returned whenever the simulation is stopped through reaching this limit.
  }
  \item{redraw_at_t}{
A numeric vector of positive values specifying times at which the time to the next event should be re-drawn, regardless of whether an event occurred at this time or not. This may be useful to specify effects or baseline probabilities that vary over discrete intervals of time. Note that using this argument potentially adds multiple additional rows to the output, in which no variables change. Set to \code{NULL} to not use this functionality (default).
  }
  \item{allow_ties}{
Either \code{TRUE} or \code{FALSE} (default), specifying whether multiple events (or changes from \code{TRUE} to \code{FALSE} in some variables) per individual at the exact same time should be allowed. If the times until the next event are continuous, the chances for an exact tie are astronomically small, so it is usually fine to keep this at \code{FALSE}. Should a tie be found anyways, an error will be returned. If some custom function is supplied to the \code{distr_fun} argument of one or more time-dependent nodes, which produce integer times, this argument should be set to \code{TRUE}. Note that this function is much faster with \code{allow_ties=FALSE}, especially with large \code{n_sim}.
  }
  \item{censor_at_max_t}{
Either \code{TRUE} or \code{FALSE}, specifying whether the last generated time should be censored at the user-specified value of \code{max_t}. Since the simulation jumps times at events, the last observed event time may often be larger than \code{max_t} initially. Setting this to \code{TRUE} censors these values appropriately and potentially discards the last state-change.
  }
  \item{target_event}{
By default (keeping this argument at \code{FALSE}) all time-varying nodes are treated equally when creating the start-stop intervals. This can be changed by supplying a single character string to this argument, naming one of the nodes. This node will then be treated as the outcome. The output then corresponds to what would be needed to fit a Cox proportional hazards model with that node as the outcome.
  }
  \item{keep_only_first}{
Only used when \code{target_event} is not \code{NULL}. Either \code{TRUE} or \code{FALSE} (default). If \code{TRUE}, all information after the first event per person will be discarded. Useful when \code{target_event} should be treated as a terminal variable.
  }
  \item{check_inputs}{
Whether to perform plausibility checks for the user input or not. Is set to \code{TRUE} by default, but can be set to \code{FALSE} in order to speed things up when using this function in a simulation study or something similar.
  }
}
\details{

This function is purely experimental. It currently needs more tests and checks. Use at your own risk.

\strong{\emph{What is Discrete-Event Simulation?}}:

In discrete-event simulations (DES), the system is modelled as a sequence of distinct events that occur over time any may influence each other. In contrast to discrete-time simulations, the time in a DES is only advanced by some amount whenever an event occurs. The state of the system is then updated according to this event and the next advancement is made. The possibly simplest example of a DES, as compared to a discrete-time simulation is a system with just one variable, \code{Y} for a single individual. At the start, \code{Y} is zero. We are interested only in the time at which \code{Y} turns 1 for the first time. The probability of 1 turning one in one unit of time is set to a fixed value of 0.01. In a discrete-time simulation, we would perform a single Bernoulli trial with probability 0.01. If this trial returns a 1, we are finished and save the current simulation time. If it is 0, we increase the time by 1 and repeat the process until \code{Y} is eventually 1. In DES on the other hand, we would simply draw the time until \code{Y} turns 1 from a suitable distribution (in this example a simple exponential distribution would be sufficient).

In such simple cases, using a discrete-time simulation approach is clearly a worse strategy. There is no reason to perform so many computations when drawing a single exponentially distributed random number is enough. With more complex data generation processes, for example include time-varying variables that influence each other over time, using DES gets more complicated.

\strong{\emph{How it Works}}:

Internally, this function works by first simulating data using the \code{\link{sim_from_dag}} function. Alternatively, the user can supply a custom \code{data.table} using the \code{t0_data} argument. This data defines the state of all entities at \eqn{t = 0}. Afterwards, the following algorithm is used for each simulated individual:

(1) The time of the next change in a variable is generated for each of the included time-varying variable separately, possibly dependent on the other variables.
(2) The minimum of these times is used as the new simulation time.
(3) The variable corresponding to the choosen time is updated.

This process is repeated until no changes are needed anymore (e.g. when all time-dependent variables have reached absorbing states, or \code{max_t} is reached), or when a user-specified break condition is reached (argument \code{break_if}), or when no individuals are left after conditional subsetting (argument \code{remove_if}). After the first iteration, the times that are sampled in step (1) have to be sampled from left-truncated distributions, where the truncation time is equal to the current simulation time. This has to be the case because that time has already passed for that individual, so the next event change must be at least some time afterwards. Users may specify any function to calculate the rate or probability used depending on the state. Users may also use any function to draw the time of the next change.

\strong{\emph{Specifying the \code{dag} argument}}:

The \code{dag} argument should be specified as described in the \code{\link{node}} documentation page. More examples specific to discrete-event simulations can be found in the vignettes and the examples. The only difference to specifying a \code{dag} for the \code{\link{sim_from_dag}} function is that the \code{dag} here should contain at least one time-dependent node added using the \code{\link{node_td}} function, that uses \code{type="next_time"}.

\strong{\emph{Networks-Based Simulation}}:

Currently not supported, but will be in future versions.

\strong{\emph{Speed Considerations}}:

In general, this function should be \emph{a lot faster} than a corresponding \code{\link{sim_discrete_time}} call, because it does not require going through all considered points in time directly. Its computation time therefore does not change substantially (or at all) with higher values of \code{max_t}. Instead, it only increases with higher values of \code{n_sim}, the amount of time-dependent variables and, most importantly, with the frequency by which these variables change. The more frequently a variable changes back and forth between \code{TRUE} and \code{FALSE}, the more iterations are needed and thus the more time is needed.

\strong{\emph{Current limitations}}:

Unlike the \code{\link{sim_discrete_time}} function, which does not assume any parametric distributions, this function requires the user to specify a function that may be used to generate the time of the next change in a binary variable. Multiple built-in options are provided, but it is nevertheless less flexible. Additionally, only binary time-dependent variables are supported (with no restrictions set on time-fixed variables). Some forms of dependencies are harder (but not impossible) to specify using the discrete-event approach.

For example, simulating effects of variables or overall event probabilities that are smooth functions of time is difficult. If the event probability is constant over time and only changes when some other variable changed, a simple left-truncated exponential distribution (see \code{\link{rtexp}}) may be used. If only the general event probability should vary over time, times may be generated using a Weibull or some other parametric functions. In any case, users will have to know very clearly what functions to use and then have to provide a function that is able to generate truncated random values from this distribution. If these requirements cannot be met, discrete-time simulation may be the only alternative.

}
\author{
Robin Denz
}
\value{
Returns a single \code{data.table} including at least the following columns:

\itemize{
\item{\code{.id}: The unique individual identifier, coded as integers.}
\item{\code{start}: The start of the time period, coded as a numeric value.}
\item{\code{stop}: The end of the time period, coded as a numeric value.}
}

Additionally, the returned data will include all time-constant and time-dependent variables that were generated. Some options on how this data should be formatted are given by the function itself (see \code{censor_at_max_t}, \code{target_event} and \code{keep_only_first}). The long- and wide-format are not supported, because the time is usually modelled as a continuous variable.
}
\seealso{
\code{\link{empty_dag}}, \code{\link{node}}, \code{\link{node_td}}, \code{\link{node_next_time}}, \code{\link{sim_discrete_time}}
}
\references{
Denz, Robin and Nina Timmesfeld (2025). Simulating Complex Crossectional and Longitudinal Data using the simDAG R Package. arXiv preprint, doi: 10.48550/arXiv.2506.01498.

Tang, Jiangjun, George Leu, und Hussein A. Abbass. 2020. Simulation and Computational Red Teaming for Problem Solving. Hoboken: IEEE Press.

Banks, Jerry, John S. Carson II, Barry L. Nelson, and David M. Nicol (2014). Discrete-Event System Simulation. Vol. 5. Edinburgh Gate: Pearson Education Limited.
}
\examples{
library(simDAG)

set.seed(454236)

## simulating death dependent on age, sex, bmi
## NOTE: this example is explained in detail in one of the vignettes

# initializing a DAG with nodes for generating data at t0
dag <- empty_dag() +
  node("age", type="rnorm", mean=50, sd=4) +
  node("sex", type="rbernoulli", p=0.5) +
  node("bmi", type="gaussian", parents=c("sex", "age"),
       betas=c(1.1, 0.4), intercept=12, error=2)

# a function to calculate the probability of death as a
# linear combination of age, sex and bmi on the log scale
prob_death <- function(data, beta_age, beta_sex, beta_bmi,
                       beta_sickness, intercept) {
  prob <- intercept + data$age*beta_age + data$sex*beta_sex +
    data$bmi*beta_bmi + data$sickness*beta_sickness
  prob <- 1/(1 + exp(-prob))
  return(prob)
}

# adding time-dependent nodes to the dag
dag <- dag +
  node_td("sickness", type="next_time", prob_fun=0.01,
          event_duration=50, immunity_duration=Inf) +
  node_td("death", type="next_time", parents=c("age", "sex", "bmi"),
          prob_fun=prob_death, beta_age=0.1, beta_bmi=0.3, beta_sex=-0.2,
          beta_sickness=1.1,
          intercept=-20, event_duration=Inf)

# run simulation for 100 people, until everyone died
sim_dt <- sim_discrete_event(n_sim=100, dag=dag, max_t=Inf,
                             remove_if=death==TRUE,
                             target_event="death")
}
