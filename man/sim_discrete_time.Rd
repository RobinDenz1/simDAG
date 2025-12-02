\name{sim_discrete_time}
\alias{sim_discrete_time}

\title{
Simulate Data from a \code{DAG} with Time-Dependent Variables
}
\description{
Similar to the \code{\link{sim_from_dag}} function, this function can be used to generate data from a given \code{DAG} created using the \code{\link{empty_dag}} and \code{\link{node}} or \code{\link{node_td}} functions (and possibly \code{\link{network}} or \code{\link{network_td}} functions). In contrast to the \code{\link{sim_from_dag}} function, this function utilizes a discrete-time simulation approach. This is not an "off-the-shelves" simulation function, it should rather be seen as a "framework-function", making it easier to create discrete-time-simulations. It usually requires custom functions written by the user. See details.
}
\usage{
sim_discrete_time(dag, n_sim=NULL, t0_sort_dag=FALSE,
                  t0_data=NULL, t0_transform_fun=NULL,
                  t0_transform_args=list(), max_t,
                  tx_nodes_order=NULL, tx_transform_fun=NULL,
                  tx_transform_args=list(),
                  remove_if, break_if,
                  save_states="last", save_states_at=NULL,
                  save_networks=FALSE,
                  verbose=FALSE, check_inputs=TRUE)
}
\arguments{
  \item{dag}{
A \code{DAG} object created using the \code{\link{empty_dag}} function with \code{\link{node_td}} calls added to it (see details and examples). If the \code{dag} contains root nodes and child nodes which are time-fixed (those who were added using \code{\link{node}} calls), data according to this DAG will be generated for time = 0. That data will then be used as starting data for the following simulation. Alternatively, the user can specify the \code{t0_data} argument directly. In either case, the supplied \code{dag} needs to contain at least one time-dependent node added using the \code{\link{node_td}} function.
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
A single integer specifying the final point in time to which the simulation should be carried out. The simulation will start at \eqn{t = 1} (after creating the starting data with the arguments above) and will continue until \code{max_t} by increasing the time by one unit at every step, updating the time-dependent nodes along the way.
  }
  \item{tx_nodes_order}{
A numeric vector specifying the order in which the time-dependent nodes added to the \code{dag} object using the \code{\link{node_td}} function should be executed at each time step. If \code{NULL} (default), the nodes will be generated in the order in which they were originally added.
  }
  \item{tx_transform_fun}{
An optional function that takes the data created after every point in time  \eqn{t > 0} as the first argument and the simulation time as the second argument. The function will be applied to that data after all node functions at that point in time have been executed and its output will replace the previous \code{data.table}. Can be used to perform arbitrary data transformations at every point in time. Set to \code{NULL} (default) to not use this functionality.
  }
  \item{tx_transform_args}{
A named list of additional arguments passed to the \code{tx_transform_fun}. Ignored if \code{tx_transform_fun=NULL}.
  }
  \item{remove_if}{
A condition that will be evaluated directly on the generated \code{data.table} at the beginning of each time-period. All rows for which the condition is \code{TRUE} are removed from the \code{data} at this point in time. The condition may contain names of any variable that were generated. If all individuals are removed through this condition, the simulation stops early. This argument may be useful to save computation time, if a large number of points in time should be considered and the user only cares about the first time a condition is met for some individuals. Keep this argument unspecified (default) to not use this functionality.
  }
  \item{break_if}{
A condition that will be evaluated at the beginning of each time-period (but after subsetting, if \code{remove_if} was specified). If the condition is met, the simulation stops early. Contrary to the \code{remove_if} argument, this condition should return exactly one \code{TRUE} or \code{FALSE} value and is not directly evaluated on the \code{data}. To use variables generated in the simulation in this condition, users should use the \code{$} syntax (e.g. use \code{data$X} instead of just \code{X}). Keep this argument unspecified (default) to not use this functionality.
  }
  \item{save_states}{
Specifies the amount of simulation states that should be saved in the output object. Has to be one of \code{"all"}, \code{"at_t"} or \code{"last"} (default). If set to \code{"all"}, a list of containing the \code{data.table} after every point in time will be added to the output object. If \code{"at_t"}, only the states at specific points in time specified by the \code{save_states_at} argument will be saved (plus the final state). If \code{"last"}, only the final state of the \code{data.table} is added to the output.
  }
  \item{save_states_at}{
The specific points in time at which the simulated \code{data.table} should be saved. Ignored if \code{save_states!="at_t"}.
  }
  \item{save_networks}{
Either \code{TRUE} or \code{FALSE}, specifying whether networks should be saved over time. Only relevant if \code{dag} contains one or more \code{\link{network}} or \code{\link{network_td}} calls. If set to \code{TRUE} all networks (including time-independent ones) are saved according to the specification of the \code{save_states} argument.
  }
  \item{verbose}{
If \code{TRUE} prints one line at every point in time before a node function is executed. This can be useful when debugging custom node functions. Defaults to \code{FALSE}.
  }
  \item{check_inputs}{
Whether to perform plausibility checks for the user input or not. Is set to \code{TRUE} by default, but can be set to \code{FALSE} in order to speed things up when using this function in a simulation study or something similar.
  }
}
\details{

Sometimes it is necessary to simulate complex data that cannot be described easily with a single DAG and node information. This may be the case if the desired data should contain multiple time-dependent variables or time-to-event variables in which the event has time-dependent effects on other events. An example for this is data on vaccinations and their effects on the occurrence of adverse events (see vignette). Discrete-Time Simulation can be an effective tool to generate these kinds of datasets.

\strong{\emph{What is Discrete-Time Simulation?}}:

In a discrete-time simulation, there are entities who have certain states associated with them that only change at discrete points in time. For example, the entities could be people and the state could be alive or dead. In this example we could generate 100 people with some covariates such as age, sex etc.. We then start by increasing the simulation time by one day. For each person we now check if the person has died using a bernoulli trial, where the probability of dying is generated at each point in time based on some of the covariates. The simulation time is then increased again and the process is repeated until we reach \code{max_t}.

Due to the iterative process it is very easy to simulate arbitrarily complex data. The covariates may change over time in arbitrary ways, the event probability can have any functional relationship with the covariates and so on. If we want to model an event type that is not terminal, such as occurrence of cardiovascular disease, events can easily be simulated to be dependent on the timing and number of previous events. Since Discrete-Time Simulation is a special case of Discrete-Event Simulation, introductory textbooks on the latter can be of great help in getting a better understanding of the former.

\strong{\emph{How it Works}}:

Internally, this function works by first simulating data using the \code{\link{sim_from_dag}} function. Alternatively, the user can supply a custom \code{data.table} using the \code{t0_data} argument. This data defines the state of all entities at \eqn{t = 0}. Afterwards, the simulation time is increased by one unit and the data is transformed in place by calling each node function defined by the time-dependent nodes which were added to the \code{dag} using the \code{\link{node_td}} function (either in the order in which they were added to the \code{dag} object or by the order defined by the \code{tx_nodes_order} argument). Usually, each transformation changes the state of the entities in some way. For example if there is an \code{age} variable, we would probably increase the age of each person by one time unit at every step. Once \code{max_t} is reached, the resulting \code{data.table} will be returned. It contains the state of all entities at the last step with additional information of when they experienced some events (if \code{\link{node_time_to_event}} was used as time-dependent node). Multiple in-depth examples can be found in the vignettes of this package.

\strong{\emph{Specifying the \code{dag} argument}}:

The \code{dag} argument should be specified as described in the \code{\link{node}} documentation page. More examples specific to discrete-time simulations can be found in the vignettes and the examples. The only difference to specifying a \code{dag} for the \code{\link{sim_from_dag}} function is that the \code{dag} here should contain at least one time-dependent node added using the \code{\link{node_td}} function. Usage of the \code{formula} argument with non-linear or interaction terms is discouraged for performance reasons.

\strong{\emph{Networks-Based Simulation}}:

As in the \code{\link{sim_from_dag}} function, networks-based simulations are also directly supported. Users may define static networks (using the \code{\link{network}} function) and / or dynamic networks that may evolve over time(using the \code{\link{network_td}} function). By using the \code{\link{net}} function inside the \code{formula} argument of \code{\link{node}} or \code{\link{node_td}} calls, complex dependencies among observations depending on the neighbors of each observation may then be simulated. More information is given in the associated vignette and the documentation pages of \code{\link{network}} and \code{\link{network_td}}.

\strong{\emph{Speed Considerations}}:

All functions in this package rely on the \code{data.table} backend in order to make them more memory efficient and faster. It is however important to note that the time to simulate a dataset increases non-linearly with an increasing \code{max_t} value and additional time-dependent nodes. This is usually not a concern for smaller datasets, but if \code{n_sim} is very large (say > 1 million) this function will get rather slow. Note also that using the \code{formula} argument is a lot more computationally expensive than using the \code{parents}, \code{betas} approach to specify certain nodes.

In some cases, the \code{remove_if} or \code{break_if} arguments may reduce the computation time considerably. For example, if the user is only interested in the first time that some variable \code{Y} turns \code{TRUE}, it may make sense to use \code{remove_if=Y==TRUE}. Under the hood, the function then removes any individual where \code{Y} is already \code{TRUE}, so that the \code{data} shrinks and no further computations are performed for these individuals. Unofrtunately, whether or not this actually does improve performance is dependent on multiple factors. With large \code{n_sim} and \code{max_t}, a constant or skewed probability distribution of \code{Y} and especially when expensive calculations are performed at each point in time, the performance gains may be very large. This is, however, not always the case. The added computational burden of actually doing the subsetting itself at each point in time may offset any performance gains or even deteriorate performance in other scenarios. We recommend checking the computation time on a single example with and without using either \code{remove_if} and/or \code{break_if} (if appropriate) and making the decision based on that small benchmark.

\strong{\emph{What do I do with the output?}}:

This function outputs a \code{simDT} object, not a \code{data.table}. To obtain an actual dataset from the output of this function, users should use the \code{\link{sim2data}} function to transform it into the desired format. Currently, the long-format, the wide-format and the start-stop format are supported. See \code{\link{sim2data}} for more information.

\strong{\emph{A Few Words of Caution}}:

In most cases it will be necessary for the user to write their own functions in order to actually use the \code{sim_discrete_time} function. Unlike the \code{\link{sim_from_dag}} function, in which many popular node types can be implemented in a re-usable way, discrete-time simulation will always require some custom input by the user. This is the price users have to pay for the almost unlimited flexibility offered by this simulation methodology.

}
\author{
Robin Denz, Katharina Meiszl
}
\value{
Returns a \code{simDT} object, containing some general information about the simulated data as well as the final state of the simulated dataset (and more states, depending on the specification of the \code{save_states} argument). In particular, it includes the following objects:

\itemize{
  \item{\code{past_states}: A list containing the generated data at the specified points in time.}
  \item{\code{past_networks}: A list containing the generated / updated networks at the specified points in time.}
  \item{\code{save_states}: The value of the \code{save_states} argument supplied by the user.}
  \item{\code{data}: The data at time \code{max_t}. Note that if \code{remove_if} was used, this data may not include all \code{n_sim} individuals.}
  \item{\code{data_t0}: The data at time 0. Only included if \code{remove_if} was specified.}
  \item{\code{tte_past_events}: A list storing the times at which events happened in variables of type \code{"time_to_event"}, if specified.}
  \item{\code{ce_past_events}: A list storing the times at which events happened in variables of type \code{"competing_events"}, if specified.}
  \item{\code{ce_past_causes}: A list storing the types of events which happened at in variables of type \code{"competing_events"}, if specified.}
  \item{\code{tx_nodes}: A list of all time-varying nodes, as specified in the supplied \code{dag} object.}
  \item{\code{max_t}: The value of \code{max_t}, as supplied by the user.}
  \item{\code{d_max_t}: A \code{data.table} containing \code{n_sim} rows and the two columns \code{.id} (unique person identifier) and \code{max_t} (the maximum time that an individual was actually included in the data generation). This is only included if \code{remove_if} was specified by the user.}
  \item{\code{break_t}: The time at which the simulation was stopped either because the break condition as defined in the \code{break_if} argument was first met, or the time at which no further individuals were included in the data because everyone was removed through the \code{remove_if} argument. If neither \code{break_if} nor \code{remove_if} were specified, this is simply equal to \code{max_t}.}
  \item{\code{t0_var_names}: A character vector containing the names of all variable names that do not vary over time.}
}

To obtain a single dataset from this function that can be processed further, please use the \code{\link{sim2data}} function.
}
\seealso{
\code{\link{empty_dag}}, \code{\link{node}}, \code{\link{node_td}}, \code{\link{sim2data}}, \code{\link{plot.simDT}}
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

# a function that increases age as time goes on
node_advance_age <- function(data) {
  return(data$age + 1/365)
}

# a function to calculate the probability of death as a
# linear combination of age, sex and bmi on the log scale
prob_death <- function(data, beta_age, beta_sex, beta_bmi, intercept) {
  prob <- intercept + data$age*beta_age + data$sex*beta_sex + data$bmi*beta_bmi
  prob <- 1/(1 + exp(-prob))
  return(prob)
}

# adding time-dependent nodes to the dag
dag <- dag +
  node_td("age", type="advance_age", parents="age") +
  node_td("death", type="time_to_event", parents=c("age", "sex", "bmi"),
          prob_fun=prob_death, beta_age=0.1, beta_bmi=0.3, beta_sex=-0.2,
          intercept=-20, event_duration=Inf, save_past_events=FALSE)

# run simulation for 100 people, 50 days long
sim_dt <- sim_discrete_time(n_sim=100,
                            dag=dag,
                            max_t=50,
                            verbose=FALSE)
}
