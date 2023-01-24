\name{sim_discrete_time}
\alias{sim_discrete_time}

\title{
Using Discrete-Time Simulation to Generate Complex Data from a Given DAG and Node Information
}
\description{
Similar to the \code{\link{sim_from_dag}} function, this function can be used to generate data from a given DAG. In contrast to the \code{sim_from_dag} function, this function utilizes a discrete-time simulation approach. This is not an "off-the-shelves" simulation function, it should rather be seen as a "framework-function", making it easier to create discrete-time-simulations. It usually needs custom functions written by the user. See details.
}
\usage{
sim_discrete_time(n_sim, t0_root_nodes, t0_child_nodes,
                  t0_sort_dag, t0_data, t0_transform_fun,
                  t0_transform_args, max_t,
                  tx_nodes, tx_nodes_order,
                  tx_transform_fun=NULL,
                  tx_transform_args=list(),
                  save_states="last", save_states_at=NULL,
                  verbose=FALSE, check_inputs=TRUE)
}
\arguments{
  \item{n_sim}{
A single number specifying how many observations should be generated. If a \code{data.frame} is supplied to the \code{t0_data} argument, this argument is ignored. The sample size will then correspond to the number of rows in \code{t0_data}.
  }
  \item{t0_root_nodes}{
This argument is passed to the \code{root_nodes} argument of the \code{\link{sim_from_dag}} function internally. By specifying this argument and the \code{t0_child_nodes} argument, the dataset containing values of all relevant variables before the first time step can be created. If such a dataset already exists, it can be simply supplied to the \code{t0_data} argument. In this case, \code{t0_root_nodes} and \code{t0_child_nodes} should be set to \code{NULL}.
  }
  \item{t0_child_nodes}{
Corresponds to the \code{child_nodes} argument in the \code{\link{sim_from_dag}} function. See argument \code{t0_root_nodes} or the details section for more details.
  }
  \item{t0_sort_dag}{
Corresponds to the \code{sort_dag} argument in the \code{\link{sim_from_dag}} function. Ignored if \code{t0_data} is specified.
  }
  \item{t0_data}{
A \code{data.frame} containing values for all relevant variables at t = 0. This dataset will then be transformed over time according to the node functions specified in \code{tx_nodes}. It is neccessary to specify either this argument or the \code{t0_root_nodes} and \code{t0_child_nodes} arguments.
  }
  \item{t0_transform_fun}{
An optional function that takes the data created at t = 0 as the first argument. The function will be applied to the starting data and its output will replace the \code{data.frame}. Can be used to perform arbitrary data transformations after the starting data was created. Set to \code{NULL} to not use this functionality.
  }
  \item{t0_transform_args}{
A named list of additional arguments passed to the \code{t0_transform_fun}. Ignored if \code{t0_transform_fun=NULL}.
  }
  \item{max_t}{
A single integer specifying the final point in time to which the simulation should be carried out. The simulation will start at t = 1 (after creating the starting data with the arguments above) and will continue with until \code{max_t} by increasing the time by one unit at every step.
  }
  \item{tx_nodes}{
A list of lists containing information about all nodes that change over time. Each list inside this object should contain a \code{name} object (singular string with the unique name of the variable), a \code{parents} object (character vector specifying the names of the variables in \code{data} that the respective node relies on) and a \code{type} object (name of a node function which creates the variable when called). May include aribitrary number of additional arguments which will be passed to the function specified by \code{type}. See examples and vignettes for more information.
  }
  \item{tx_nodes_order}{
A numeric vector specifying the order in which the nodes defined by the \code{tx_nodes} object should be executed at each time step. If \code{NULL} (default), the nodes will be generated in the order of appearance defined by the \code{tx_nodes} object.
  }
  \item{tx_transform_fun}{
An optional function that takes the data created after every point in time > 0 as the first argument and the simulation time as the second argument. The function will be applied to that data after all node functions at that point in time have been executed and its output will replace the previous \code{data.frame}. Can be used to perform arbitrary data transformations at every point in time. Set to \code{NULL} to not use this functionality.
  }
  \item{tx_transform_args}{
A named list of additional arguments passed to the \code{tx_transform_fun}. Ignored if \code{tx_transform_fun=NULL}.
  }
  \item{save_states}{
Specifys the amount of simulation states that should be saved in the output object. Has to be one of \code{"all"}, \code{"at_t"} or \code{"last"} (default). If set to \code{"all"}, a list of containing the \code{data.frame} after every point in time will be added to the output object. If \code{"at_t"}, only the states at specific points in time specified by the \code{save_states_at} argument will be saved (plus the final state). If \code{"last"}, only the final state of the \code{data.frame} is added to the output.
  }
  \item{save_states_at}{
The specific points in time at which the simulated \code{data.frame} should be saved. Ignored if \code{save_states!="at_t"}.
  }
  \item{verbose}{
If \code{TRUE} prints a one line at every point in time before a node function is executed. This can be useful when debugging custom node functions. Defaults to \code{FALSE}.
  }
  \item{check_inputs}{
Whether to perform plausibility checks for the user input or not. Is set to \code{TRUE} by default, but can be set to \code{FALSE} in order to speed things up when using this function in a simulation study or something similar.
  }
}
\details{

Sometimes it is neccessary to simulate complex data that cannot be described easily with a single DAG and node information. This may be the case if the desired data should contain multiple time-dependent variables or time-to-event variables in which the event has time-dependent effects on other events. An example for this is data on vaccinations and their effects on the occurence of adverse events (see vignette). Discrete-Time Simulation can be an effective tool to generate these kinds of datasets.

\strong{\emph{What is Discrete-Time Simulation?}}:

In a discrete-time simulation, there are entities who have certain states associated with them that only change at discrete points in time. For example, the entities could be people and the state could be alive or dead. In this example we could generate 100 people with some covariates such as age, sex etc.. We then start by increasing the simulation time by one day. For each person we now check if the person has died using a bernoulli trial, where the probability of dying is generated at each point in time based on some of the covariates. The simulation time is then increased again and the process is repeated until we reach \code{max_t}.

Due to the iterative process it is very easy to simulate arbitrarily complex data. The covariates may change over time in arbitrary ways, the event probability can have any functional relationship with the covariates and so on. If we want to model an event type that is not terminal, such as occurence of cardiovascular disease, events can easily be simulated to be dependent on the timing and number of previous events. Since Discrete-Time Simulation is a special case of Discrete-Event Simulation, introductory textbooks on the latter can be of great help in getting a better understanding of the former.

\strong{\emph{How it Works}}:

Internally, this function works by first simulating data using the \code{\link{sim_from_dag}} function. Alternatively, the user can supply a custom \code{data.frame} using the \code{t0_data} argument. This data defines the state of all entities at \eqn{t = 0}. Afterwards, the simulation time is increased by one unit and the data is transformed in place by calling each node function defined by the \code{tx_nodes} argument (either in turn or by the order defined by the \code{tx_nodes_order} argument). Usually, each transformation changes the state of the entities in some way. For example if there is an \code{age} variable, we would probably increase the age of each person by one time unit at every step. Once \code{max_t} is reached, the resulting \code{data.table} will be returned. It contains the state of all entities at the last step with additional information of when they experienced some events (if \code{\link{node_time_to_event}} was used in \code{tx_nodes}). Multiple in-depth examples can be found in the vignettes of this package.

\strong{\emph{Specifying the root_nodes, child_nodes and tx_nodes arguments}}:

Since the \code{t0_root_nodes} and \code{t0_child_nodes} arguments are simply passed to the \code{\link{sim_from_dag}} function, the documentation of that function is sufficient to explain how these arguments should be specified.

\strong{\emph{Speed Considerations}}:

All functions in this package rely on the \code{data.table} structure in order to make them more memory efficient and faster. It is however important to note that the time to simulate a dataset increases non-linearly with an increasing \code{max_t} value and additional nodes in \code{tx_nodes}. This is usually not a concern for smaller datasets, but if \code{n_sim} is very large (say > 1 million) this function will get rather slow.

\strong{\emph{A Few Words of Caution}}:

In most cases it will be neccessary for the user to write their own functions in order to actually use the \code{sim_discrete_time} function. Unlike the \code{sim_from_dag} function, in which many popular node types can be implemented in a re-usable way, discrete-time simulation will always require some custom input by the user. This is the price users have to pay for the almost unlimited flexibility offered by this simulation methodology.

}
\author{
Robin Denz, Katharina Meiszl
}
\examples{
library(simDAG)

## simulating death dependent on age, sex, bmi
## NOTE: this example is explained in detail in one of the vignettes

# nodes for generating data at t0
root_nodes <- list(list(name="age",
                        dist="rnorm",
                        params=list(mean=50, sd=4)),
                   list(name="sex",
                        dist="rbernoulli",
                        params=list(p=0.5)))
child_nodes <- list(list(name="bmi",
                         parents=c("sex", "age"),
                         type="gaussian",
                         betas=c(1.1, 0.4),
                         intercept=12,
                         error=2))

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

# defining the time-varying nodes
tx_nodes <- list(list(name="age",
                      type="advance_age",
                      parents="age"),
                 list(name="death",
                      type="time_to_event",
                      parents=c("age", "sex", "bmi"),
                      prob_fun=prob_death,
                      prob_fun_args=list(beta_age=0.1,
                                         beta_bmi=0.3,
                                         beta_sex=-0.2,
                                         intercept=-20),
                      event_duration=Inf,
                      save_past_events=FALSE))

# run simulation for 100 people, 50 days long
sim_dat <- sim_discrete_time(n_sim=100,
                             t0_root_nodes=root_nodes,
                             t0_child_nodes=child_nodes,
                             tx_nodes=tx_nodes,
                             max_t=50,
                             verbose=FALSE)$data
}
