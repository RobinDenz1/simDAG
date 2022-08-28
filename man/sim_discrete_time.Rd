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
                  filename=NULL)
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
  \item{filename}{
Either a single character string specifying the filename of a \code{.Rds} file or \code{NULL}. If a filename is supplied, the output object will be written to it.
  }
}
\details{

Sometimes it is neccessary to simulate complex data that cannot be described easily with a single DAG and node information. This may be the case if the desired data should contain multiple time-dependent variables or time-to-event variables in which the event has time-dependent effects on other events. An example for this is data on vaccinations and their effects on the occurence of adverse events (see vignette). Discrete-Time Simulation can be an effective tool to generate these kinds of datasets.

\strong{\emph{How it Works}}:

# TODO: write this

\strong{\emph{A Few Words of Caution}}:

In most cases it will be neccessary for the user to write their own functions in order to actually use the \code{sim_discrete_time} function. Unlike the \code{sim_from_dag} function, in which many popular node types can be implemented in a re-usable way, discrete-time simulation will always require some custom input by the user. This is the price users have to pay for the almost unlimited flexibility offered by this simulation methodology.

}
\examples{
# TODO: give very simple example
}
