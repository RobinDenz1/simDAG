# Simulate Data from a `DAG` with Time-Dependent Variables in Discrete Time

Similar to the
[`sim_from_dag`](https://robindenz1.github.io/simDAG/reference/sim_from_DAG.md)
function, this function can be used to generate data from a given `DAG`
created using the
[`empty_dag`](https://robindenz1.github.io/simDAG/reference/empty_dag.md)
and [`node`](https://robindenz1.github.io/simDAG/reference/node.md) or
[`node_td`](https://robindenz1.github.io/simDAG/reference/node.md)
functions (and possibly
[`network`](https://robindenz1.github.io/simDAG/reference/network.md) or
[`network_td`](https://robindenz1.github.io/simDAG/reference/network.md)
functions). In contrast to the
[`sim_from_dag`](https://robindenz1.github.io/simDAG/reference/sim_from_DAG.md)
function, this function utilizes a discrete-time simulation approach.
This is not an "off-the-shelves" simulation function, it should rather
be seen as a "framework-function", making it easier to create
discrete-time-simulations. It usually requires custom functions written
by the user. See details.

## Usage

``` r
sim_discrete_time(dag, n_sim=NULL, t0_sort_dag=FALSE,
                  t0_data=NULL, t0_transform_fun=NULL,
                  t0_transform_args=list(), max_t,
                  tx_nodes_order=NULL, tx_transform_fun=NULL,
                  tx_transform_args=list(),
                  remove_if, break_if,
                  save_states="last", save_states_at=NULL,
                  save_networks=FALSE,
                  verbose=FALSE, check_inputs=TRUE)
```

## Arguments

- dag:

  A `DAG` object created using the
  [`empty_dag`](https://robindenz1.github.io/simDAG/reference/empty_dag.md)
  function with
  [`node_td`](https://robindenz1.github.io/simDAG/reference/node.md)
  calls added to it (see details and examples). If the `dag` contains
  root nodes and child nodes which are time-fixed (those who were added
  using [`node`](https://robindenz1.github.io/simDAG/reference/node.md)
  calls), data according to this DAG will be generated for time = 0.
  That data will then be used as starting data for the following
  simulation. Alternatively, the user can specify the `t0_data` argument
  directly. In either case, the supplied `dag` needs to contain at least
  one time-dependent node added using the
  [`node_td`](https://robindenz1.github.io/simDAG/reference/node.md)
  function.

- n_sim:

  A single number specifying how many observations should be generated.
  If a `data.table` is supplied to the `t0_data` argument, this argument
  is ignored. The sample size will then correspond to the number of rows
  in `t0_data`.

- t0_sort_dag:

  Corresponds to the `sort_dag` argument in the
  [`sim_from_dag`](https://robindenz1.github.io/simDAG/reference/sim_from_DAG.md)
  function. Ignored if `t0_data` is specified.

- t0_data:

  An optional `data.table` like object (also accepts a `data.frame`,
  `tibble` etc.) containing values for all relevant variables at \\t =
  0\\. This dataset will then be transformed over time according to the
  nodes specified using
  [`node_td`](https://robindenz1.github.io/simDAG/reference/node.md)
  calls in `dag`. Alternatively, data for \\t = 0\\ may be generated
  automatically by this function if standard
  [`node`](https://robindenz1.github.io/simDAG/reference/node.md) calls
  were added to the `dag`.

- t0_transform_fun:

  An optional function that takes the data created at \\t = 0\\ as the
  first argument. The function will be applied to the starting data and
  its output will replace the `data.table`. Can be used to perform
  arbitrary data transformations after the starting data was created.
  Set to `NULL` (default) to not use this functionality.

- t0_transform_args:

  A named list of additional arguments passed to the `t0_transform_fun`.
  Ignored if `t0_transform_fun=NULL`.

- max_t:

  A single integer specifying the final point in time to which the
  simulation should be carried out. The simulation will start at \\t =
  1\\ (after creating the starting data with the arguments above) and
  will continue until `max_t` by increasing the time by one unit at
  every step, updating the time-dependent nodes along the way.

- tx_nodes_order:

  A numeric vector specifying the order in which the time-dependent
  nodes added to the `dag` object using the
  [`node_td`](https://robindenz1.github.io/simDAG/reference/node.md)
  function should be executed at each time step. If `NULL` (default),
  the nodes will be generated in the order in which they were originally
  added.

- tx_transform_fun:

  An optional function that takes the data created after every point in
  time \\t \> 0\\ as the first argument and the simulation time as the
  second argument. The function will be applied to that data after all
  node functions at that point in time have been executed and its output
  will replace the previous `data.table`. Can be used to perform
  arbitrary data transformations at every point in time. Set to `NULL`
  (default) to not use this functionality.

- tx_transform_args:

  A named list of additional arguments passed to the `tx_transform_fun`.
  Ignored if `tx_transform_fun=NULL`.

- remove_if:

  A condition that will be evaluated directly on the generated
  `data.table` at the beginning of each time-period. All rows for which
  the condition is `TRUE` are removed from the `data` at this point in
  time. The condition may contain names of any variable that were
  generated. If all individuals are removed through this condition, the
  simulation stops early. This argument may be useful to save
  computation time, if a large number of points in time should be
  considered and the user only cares about the first time a condition is
  met for some individuals. Keep this argument unspecified (default) to
  not use this functionality.

- break_if:

  A condition that will be evaluated at the beginning of each
  time-period (but after subsetting, if `remove_if` was specified). If
  the condition is met, the simulation stops early. Contrary to the
  `remove_if` argument, this condition should return exactly one `TRUE`
  or `FALSE` value and is not directly evaluated on the `data`. To use
  variables generated in the simulation in this condition, users should
  use the `$` syntax (e.g. use `data$X` instead of just `X`). Keep this
  argument unspecified (default) to not use this functionality.

- save_states:

  Specifies the amount of simulation states that should be saved in the
  output object. Has to be one of `"all"`, `"at_t"` or `"last"`
  (default). If set to `"all"`, a list of containing the `data.table`
  after every point in time will be added to the output object. If
  `"at_t"`, only the states at specific points in time specified by the
  `save_states_at` argument will be saved (plus the final state). If
  `"last"`, only the final state of the `data.table` is added to the
  output.

- save_states_at:

  The specific points in time at which the simulated `data.table` should
  be saved. Ignored if `save_states!="at_t"`.

- save_networks:

  Either `TRUE` or `FALSE`, specifying whether networks should be saved
  over time. Only relevant if `dag` contains one or more
  [`network`](https://robindenz1.github.io/simDAG/reference/network.md)
  or
  [`network_td`](https://robindenz1.github.io/simDAG/reference/network.md)
  calls. If set to `TRUE` all networks (including time-independent ones)
  are saved according to the specification of the `save_states`
  argument.

- verbose:

  If `TRUE` prints one line at every point in time before a node
  function is executed. This can be useful when debugging custom node
  functions. Defaults to `FALSE`.

- check_inputs:

  Whether to perform plausibility checks for the user input or not. Is
  set to `TRUE` by default, but can be set to `FALSE` in order to speed
  things up when using this function in a simulation study or something
  similar.

## Details

Sometimes it is necessary to simulate complex data that cannot be
described easily with a single DAG and node information. This may be the
case if the desired data should contain multiple time-dependent
variables or time-to-event variables in which the event has
time-dependent effects on other events. An example for this is data on
vaccinations and their effects on the occurrence of adverse events (see
vignette). Discrete-Time Simulation can be an effective tool to generate
these kinds of datasets.

***What is Discrete-Time Simulation?***:

In a discrete-time simulation, there are entities who have certain
states associated with them that only change at discrete points in time.
For example, the entities could be people and the state could be alive
or dead. In this example we could generate 100 people with some
covariates such as age, sex etc.. We then start by increasing the
simulation time by one day. For each person we now check if the person
has died using a bernoulli trial, where the probability of dying is
generated at each point in time based on some of the covariates. The
simulation time is then increased again and the process is repeated
until we reach `max_t`.

Due to the iterative process it is very easy to simulate arbitrarily
complex data. The covariates may change over time in arbitrary ways, the
event probability can have any functional relationship with the
covariates and so on. If we want to model an event type that is not
terminal, such as occurrence of cardiovascular disease, events can
easily be simulated to be dependent on the timing and number of previous
events. Since Discrete-Time Simulation is a special case of
Discrete-Event Simulation, introductory textbooks on the latter can be
of great help in getting a better understanding of the former.

***How it Works***:

Internally, this function works by first simulating data using the
[`sim_from_dag`](https://robindenz1.github.io/simDAG/reference/sim_from_DAG.md)
function. Alternatively, the user can supply a custom `data.table` using
the `t0_data` argument. This data defines the state of all entities at
\\t = 0\\. Afterwards, the simulation time is increased by one unit and
the data is transformed in place by calling each node function defined
by the time-dependent nodes which were added to the `dag` using the
[`node_td`](https://robindenz1.github.io/simDAG/reference/node.md)
function (either in the order in which they were added to the `dag`
object or by the order defined by the `tx_nodes_order` argument).
Usually, each transformation changes the state of the entities in some
way. For example if there is an `age` variable, we would probably
increase the age of each person by one time unit at every step. Once
`max_t` is reached, the resulting `data.table` will be returned. It
contains the state of all entities at the last step with additional
information of when they experienced some events (if
[`node_time_to_event`](https://robindenz1.github.io/simDAG/reference/node_time_to_event.md)
was used as time-dependent node). Multiple in-depth examples can be
found in the vignettes of this package.

***Specifying the `dag` argument***:

The `dag` argument should be specified as described in the
[`node`](https://robindenz1.github.io/simDAG/reference/node.md)
documentation page. More examples specific to discrete-time simulations
can be found in the vignettes and the examples. The only difference to
specifying a `dag` for the
[`sim_from_dag`](https://robindenz1.github.io/simDAG/reference/sim_from_DAG.md)
function is that the `dag` here should contain at least one
time-dependent node added using the
[`node_td`](https://robindenz1.github.io/simDAG/reference/node.md)
function. Usage of the `formula` argument with non-linear or interaction
terms is discouraged for performance reasons.

***Networks-Based Simulation***:

As in the
[`sim_from_dag`](https://robindenz1.github.io/simDAG/reference/sim_from_DAG.md)
function, networks-based simulations are also directly supported. Users
may define static networks (using the
[`network`](https://robindenz1.github.io/simDAG/reference/network.md)
function) and / or dynamic networks that may evolve over time(using the
[`network_td`](https://robindenz1.github.io/simDAG/reference/network.md)
function). By using the
[`net`](https://robindenz1.github.io/simDAG/reference/net.md) function
inside the `formula` argument of
[`node`](https://robindenz1.github.io/simDAG/reference/node.md) or
[`node_td`](https://robindenz1.github.io/simDAG/reference/node.md)
calls, complex dependencies among observations depending on the
neighbors of each observation may then be simulated. More information is
given in the associated vignette and the documentation pages of
[`network`](https://robindenz1.github.io/simDAG/reference/network.md)
and
[`network_td`](https://robindenz1.github.io/simDAG/reference/network.md).

***Speed Considerations***:

All functions in this package rely on the `data.table` backend in order
to make them more memory efficient and faster. It is however important
to note that the time to simulate a dataset increases non-linearly with
an increasing `max_t` value and additional time-dependent nodes. This is
usually not a concern for smaller datasets, but if `n_sim` is very large
(say \> 1 million) this function will get rather slow. Note also that
using the `formula` argument is a lot more computationally expensive
than using the `parents`, `betas` approach to specify certain nodes.

In some cases, the `remove_if` or `break_if` arguments may reduce the
computation time considerably. For example, if the user is only
interested in the first time that some variable `Y` turns `TRUE`, it may
make sense to use `remove_if=Y==TRUE`. Under the hood, the function then
removes any individual where `Y` is already `TRUE`, so that the `data`
shrinks and no further computations are performed for these individuals.
Unfortunately, whether or not this actually does improve performance is
dependent on multiple factors. With large `n_sim` and `max_t`, a
constant or skewed probability distribution of `Y` and especially when
expensive calculations are performed at each point in time, the
performance gains may be very large. This is, however, not always the
case. The added computational burden of actually doing the subsetting
itself at each point in time may offset any performance gains or even
deteriorate performance in other scenarios. We recommend checking the
computation time on a single example with and without using either
`remove_if` and/or `break_if` (if appropriate) and making the decision
based on that small benchmark.

If speed is of particular importance, it may make sense to use the
[`sim_discrete_event`](https://robindenz1.github.io/simDAG/reference/sim_discrete_event.md)
function instead. The discrete-event simulation framework is much
faster, but a little less flexible than the discrete-time framework. See
the relevant documentation page for more information.

***What do I do with the output?***:

This function outputs a `simDT` object, not a `data.table`. To obtain an
actual dataset from the output of this function, users should use the
[`sim2data`](https://robindenz1.github.io/simDAG/reference/sim2data.md)
function to transform it into the desired format. Currently, the
long-format, the wide-format and the start-stop format are supported.
See
[`sim2data`](https://robindenz1.github.io/simDAG/reference/sim2data.md)
for more information.

***A Few Words of Caution***:

In most cases it will be necessary for the user to write their own
functions in order to actually use the `sim_discrete_time` function.
Unlike the
[`sim_from_dag`](https://robindenz1.github.io/simDAG/reference/sim_from_DAG.md)
function, in which many popular node types can be implemented in a
re-usable way, discrete-time simulation will always require some custom
input by the user. This is the price users have to pay for the almost
unlimited flexibility offered by this simulation methodology.

## Author

Robin Denz, Katharina Meiszl

## Value

Returns a `simDT` object, containing some general information about the
simulated data as well as the final state of the simulated dataset (and
more states, depending on the specification of the `save_states`
argument). In particular, it includes the following objects:

- `past_states`: A list containing the generated data at the specified
  points in time.

- `past_networks`: A list containing the generated / updated networks at
  the specified points in time.

- `save_states`: The value of the `save_states` argument supplied by the
  user.

- `data`: The data at time `max_t`. Note that if `remove_if` was used,
  this data may not include all `n_sim` individuals.

- `data_t0`: The data at time 0. Only included if `remove_if` was
  specified.

- `tte_past_events`: A list storing the times at which events happened
  in variables of type `"time_to_event"`, if specified.

- `ce_past_events`: A list storing the times at which events happened in
  variables of type `"competing_events"`, if specified.

- `ce_past_causes`: A list storing the types of events which happened at
  in variables of type `"competing_events"`, if specified.

- `tx_nodes`: A list of all time-varying nodes, as specified in the
  supplied `dag` object.

- `max_t`: The value of `max_t`, as supplied by the user.

- `d_max_t`: A `data.table` containing `n_sim` rows and the two columns
  `.id` (unique person identifier) and `max_t` (the maximum time that an
  individual was actually included in the data generation). This is only
  included if `remove_if` was specified by the user.

- `break_t`: The time at which the simulation was stopped either because
  the break condition as defined in the `break_if` argument was first
  met, or the time at which no further individuals were included in the
  data because everyone was removed through the `remove_if` argument. If
  neither `break_if` nor `remove_if` were specified, this is simply
  equal to `max_t`.

- `t0_var_names`: A character vector containing the names of all
  variable names that do not vary over time.

To obtain a single dataset from this function that can be processed
further, please use the
[`sim2data`](https://robindenz1.github.io/simDAG/reference/sim2data.md)
function.

## See also

[`empty_dag`](https://robindenz1.github.io/simDAG/reference/empty_dag.md),
[`node`](https://robindenz1.github.io/simDAG/reference/node.md),
[`node_td`](https://robindenz1.github.io/simDAG/reference/node.md),
[`sim2data`](https://robindenz1.github.io/simDAG/reference/sim2data.md),
[`plot.simDT`](https://robindenz1.github.io/simDAG/reference/plot.simDT.md)

## References

Denz, Robin and Nina Timmesfeld (2025). Simulating Complex Crossectional
and Longitudinal Data using the simDAG R Package. arXiv preprint, doi:
10.48550/arXiv.2506.01498.

Tang, Jiangjun, George Leu, und Hussein A. Abbass. 2020. Simulation and
Computational Red Teaming for Problem Solving. Hoboken: IEEE Press.

Banks, Jerry, John S. Carson II, Barry L. Nelson, and David M. Nicol
(2014). Discrete-Event System Simulation. Vol. 5. Edinburgh Gate:
Pearson Education Limited.

## Examples

``` r
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
#> Error in get(paste0("node_", type)): object 'node_advance_age' not found

# run simulation for 100 people, 50 days long
sim_dt <- sim_discrete_time(n_sim=100,
                            dag=dag,
                            max_t=50,
                            verbose=FALSE)
#> Error: 'dag' must contain at least one time-varying node added using the node_td() function. For dag objects with no time-varying nodes, please use the sim_from_dag() function instead.
```
